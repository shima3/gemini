;;; gemini-mode.el --- A simple Emacs client for Google Gemini -*- lexical-binding: t; -*-

(require 'json)
(require 'url)
(require 'url-http)

;;; カスタム変数

(defcustom gemini-api-key nil
  "Google Gemini APIキー。Google AI Studioから取得してください。"
  :type 'string
  :group 'gemini)

(defcustom gemini-model "gemini-pro"
  "使用するGoogle Geminiのモデル名。"
  :type 'string
  :group 'gemini)

(defconst gemini-api-base-url "https://generativelanguage.googleapis.com/v1beta/models/"
  "Gemini APIのベースURL。")

(defconst gemini-message-separator "\n---\n"
  "メッセージの区切りを示す文字列。")

;;; 内部ヘルパー関数
(defun gemini--parse-buffer-and-get-messages ()
  "バッファを解析し、メッセージオブジェクトの「リスト」を確実に返す。"
  (let ((messages-list '()))
    ;;【変更点】split-string の不要な第4引数 `t` を削除。
    ;; 第4引数は正規表現を期待するため、`t` を渡すとエラーになるのが原因でした。
    (dolist (part (split-string (buffer-string) gemini-message-separator t))
      (let ((trimmed-part (string-trim part)) role content)
        (cond
         ((string-prefix-p "user:" trimmed-part)
          (setq role "user")
          (setq content (substring trimmed-part 5)))
         ((string-prefix-p "model:" trimmed-part)
          (setq role "model")
          (setq content (substring trimmed-part 6))))
        ;; 有効なメッセージのみをリストに追加
        (when (and role content (not (string-empty-p (string-trim content))))
          ;; 安定性のためキーは文字列のままにしておきます
          (push `(("role" . ,role) ("parts" . ((("text" . ,(string-trim content)))))) messages-list))))
    ;; リストを正しい順序にして返す
    (nreverse messages-list)))

(defun gemini--insert-response (response-text)
  "Geminiからの応答をバッファの最後に挿入する。"
  (goto-char (point-max))
  (when (looking-back "^user:[ \t]*$" (line-beginning-position))
    (delete-region (line-beginning-position) (point)))
  (delete-blank-lines)
  (unless (string-suffix-p gemini-message-separator (buffer-substring-no-properties (point-min) (point-max)))
    (insert gemini-message-separator))
  (insert (format "model: %s%suser: " response-text gemini-message-separator))
  (message "Geminiからの応答を受信しました。"))

(defun gemini--handle-response (status original-buffer)
  "url-retrieveの非同期コールバック。応答を処理する。
status変数が信頼できない環境があるため、応答バッファを直接読んで成否を判断する。"
  (let ((response-buffer (current-buffer)))
    (with-current-buffer response-buffer
      (goto-char (point-min))
      (if (re-search-forward "^HTTP/[0-9.]+[ \t]+200[ \t]" nil t)
          ;; --- 通信が成功した場合 (HTTP 200) ---
          (progn
            (goto-char (point-min))
            (unless (re-search-forward "\n\r?\n\r?" nil t)
              (error "Could not find end of HTTP headers in response"))
            (let* ((json-raw-text (buffer-substring-no-properties (point) (point-max)))
                   (json-text (decode-coding-string json-raw-text 'utf-8))
                   ;;【変更点】JSON配列をベクターではなくリストとして読み込むように指定
                   (json-object (let ((json-array-type 'list))
                                  (json-read-from-string json-text)))
                   (text "（応答の解析に失敗しました）"))
              (condition-case err
                  (let* ((candidates (cdr (assoc 'candidates json-object)))
                         (first-candidate (car candidates))
                         (content (cdr (assoc 'content first-candidate)))
                         (parts (cdr (assoc 'parts content)))
                         (first-part (car parts)))
                    (setq text (or (cdr (assoc 'text first-part)) "（空の応答）")))
                ('error (setq text (format "（JSON解析エラー: %s）" err))))
              (with-current-buffer original-buffer
                (gemini--insert-response text)))
            (kill-buffer response-buffer))

        ;; --- 通信が失敗した場合 (HTTP 200以外) ---
        (progn
          (goto-char (point-min))
          (let ((error-code-str
                 (if (re-search-forward "^HTTP/[0-9.]+[ \t]+\\([0-9]+\\)" nil t)
                     (match-string-no-properties 1)
                   "unknown")))
            (with-current-buffer original-buffer
              (message "Gemini APIエラー (HTTP %s)。詳細はバッファ '%s' を参照してください。"
                       error-code-str (buffer-name response-buffer))))
          (display-buffer response-buffer))))))

;;; メイン関数
(defun gemini-send ()
  "現在のバッファの内容をGemini APIに送信する。"
  (interactive)
  (unless (and gemini-api-key (not (string-empty-p gemini-api-key)))
    (error "Gemini APIキーが設定されていません。(setq gemini-api-key \"...\") で設定してください。"))

  (let* ((messages (gemini--parse-buffer-and-get-messages))
         ;;【変更点】キーをキーワードシンボルから文字列に変更
         (payload `(("contents" . ,messages)))
         ;;【デバッグ強化】payloadの内容を*Messages*バッファに表示
         (_ (message "---DEBUG PAYLOAD--- %S" payload))
         (json-payload (json-encode payload))
         ;;【デバッグ強化】json-encodeの結果を*Messages*バッファに表示
         (_ (message "---DEBUG JSON--- %S" json-payload))
         (request-data (encode-coding-string json-payload 'utf-8))
         (url (format "%s%s:generateContent?key=%s"
                      gemini-api-base-url
                      gemini-model
                      gemini-api-key))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json; charset=utf-8")))
         (url-request-data request-data))
    (message "Geminiにリクエストを送信中 (モデル: %s)..." gemini-model)
    (url-retrieve url 'gemini--handle-response (list (current-buffer)))))

;;; モード定義

(define-derived-mode gemini-mode text-mode "Gemini"
  "Google Geminiと対話するためのメジャーモード。"
  :syntax-table nil
  :abbrev-table nil
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (define-key gemini-mode-map (kbd "C-<return>") 'gemini-send))


;;; ユーザー向けコマンド

(defun gemini ()
  "Geminiとのチャット用の新しいバッファを作成または表示する。"
  (interactive)
  (let ((buffer (get-buffer-create "*gemini*")))
    (switch-to-buffer buffer)
    (gemini-mode)
    (when (= (point-min) (point-max))
      (insert "user: "))))

(provide 'gemini-mode)

;;; gemini-mode.el ends here
