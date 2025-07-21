;;; gemini-mode.el --- A simple Emacs client for Google Gemini -*- lexical-binding: t; -*-

;; - gemini-mode のバッファを作成する。
;; - バッファ内の全角マイナス3文字のみからなる行をメッセージの区切りと解釈する。
;; - メッセージの区切りの直後の行の先頭が「User:」ならばユーザからのメッセージ、「Gemini:」ならば Gemini からのメッセージと解釈する。
;; - ユーザが Ctrl+Enter を押すと、バッファにあるユーザからの最後のメッセージをプロンプトとし、その他のメッセージを履歴として Gemini に送信する。
;; - Gemini からの応答を受信したとき、バッファの最後にメッセージの区切り、「Gemini:」、Gemini からのメッセージ、メッセージの区切り、「User:」を挿入する。

(require 'json)
(require 'url)
(require 'url-http)

;;; カスタム変数

(defcustom gemini-api-key nil
  "Google Gemini APIキー。Google AI Studioから取得してください。環境変数 `GEMINI_API_KEY` が優先されます。"
  :type 'string
  :group 'gemini)

(defcustom gemini-model "gemini-1.5-flash-latest"
  "使用するGoogle Geminiのモデル名。環境変数 `GEMINI_MODEL` が優先されます。"
  :type 'string
  :group 'gemini)

(defconst gemini-api-base-url "https://generativelanguage.googleapis.com/v1beta/models/"
  "Gemini APIのベースURL。")

(defconst gemini-message-separator "\nーーー\n"
  "メッセージの区切りを示す文字列。")

;; ★★★ 変更点1: 待機メッセージを定数として定義 ★★★
(defconst gemini--waiting-message "Gemini: 返信中です。しばらくお待ちください。"
  "API応答待ちの間に表示するメッセージ。")

;;; 内部ヘルパー関数

(defun gemini--get-api-key ()
  "環境変数 `GEMINI_API_KEY` からAPIキーを取得する。
なければ `gemini-api-key` カスタム変数の値を使用する。"
  (or (getenv "GEMINI_API_KEY") gemini-api-key))

(defun gemini--get-model ()
  "環境変数 `GEMINI_MODEL` からモデル名を取得する。
なければ `gemini-model` カスタム変数の値を使用する。"
  (or (getenv "GEMINI_MODEL") gemini-model))

(defun gemini--parse-buffer-and-get-messages ()
  "バッファを解析し、メッセージオブジェクトの「リスト」を確実に返す。"
  (let ((messages-list '()))
    (dolist (part (split-string (buffer-string) gemini-message-separator t))
      (let ((trimmed-part (string-trim part)) role content)
        (cond
         ((string-prefix-p "User:" trimmed-part)
          (setq role "user")
          (setq content (substring trimmed-part 5)))
         ((string-prefix-p "Gemini:" trimmed-part)
          (setq role "model")
          (setq content (substring trimmed-part 6))))
        ;; 有効なメッセージのみをリストに追加
        (when (and role content (not (string-empty-p (string-trim content))))
          (push `(("role" . ,role) ("parts" . ((("text" . ,(string-trim content)))))) messages-list))))
    (nreverse messages-list)))

;; ★★★ 変更点2: 待機メッセージを削除するヘルパー関数を追加 ★★★
(defun gemini--clear-waiting-message ()
  "バッファの末尾から待機メッセージを削除する。"
  (goto-char (point-max))
  ;; `gemini--waiting-message` を後方検索して削除
  (when (search-backward gemini--waiting-message nil t)
    (delete-region (match-beginning 0) (point-max))))

(defun gemini--insert-response (response-text)
  "Geminiからの応答をバッファの最後に挿入する。"
  (goto-char (point-max))
  ;; (この部分は `gemini-send` 側で処理するため削除・変更)
  ;; (when (looking-back "^User:[ \t]*$" (line-beginning-position))
  ;;   (delete-region (line-beginning-position) (point)))
  (delete-blank-lines)
  (unless (string-suffix-p gemini-message-separator (buffer-substring-no-properties (point-min) (point-max)))
    (insert gemini-message-separator))
  (insert (format "Gemini: %s%sUser: " response-text gemini-message-separator))
  (message "Geminiからの応答を受信しました。"))

(defun gemini--handle-response (status original-buffer)
  "url-retrieveの非同期コールバック。応答を処理する。
status変数が信頼できない環境があるため、応答バッファを直接読んで成否を判断する。"
  (let ((response-buffer (current-buffer)))
    ;; ★★★ 変更点3: 応答処理の前に、まず待機メッセージを削除する ★★★
    (with-current-buffer original-buffer
      (gemini--clear-waiting-message))

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
              ;; オリジナルバッファに結果を挿入
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
            ;; ★★★ 変更点4: エラー時もオリジナルバッファでメッセージ表示 ★★★
            (with-current-buffer original-buffer
              (message "Gemini APIエラー (HTTP %s)。詳細はバッファ '%s' を参照してください。"
                       error-code-str (buffer-name response-buffer))
              ;; エラーメッセージの後に、再度 "User: " プロンプトを出す
              (goto-char (point-max))
              (unless (looking-at-p "\n$") (insert "\n"))
              (insert "User: ")))
          (display-buffer response-buffer))))))

;;; メイン関数
(defun gemini-send ()
  "現在のバッファの内容をGemini APIに送信する。"
  (interactive)
  (let ((api-key (gemini--get-api-key))
         (model (gemini--get-model)))
    (unless (and gemini-api-key (not (string-empty-p gemini-api-key)))
      (error "Gemini APIキーが設定されていません。環境変数 `GEMINI_API_KEY` または (setq gemini-api-key \"...\") で設定してください。"))

    ;; ★★★ 変更点5: リクエスト送信前に待機メッセージを挿入 ★★★
    ;; まず、末尾の不要な空白や、空の "User: " 行を整理する
    (goto-char (point-max))
    (when (looking-back "^User:[ \t]*$" (line-beginning-position))
        (delete-region (line-beginning-position) (point)))
    (delete-blank-lines)
    (unless (string-suffix-p gemini-message-separator (buffer-substring-no-properties (point-min) (point-max)))
      (insert gemini-message-separator))
    
    ;; メッセージをパースしてリクエストを作成
    (let* ((messages (gemini--parse-buffer-and-get-messages))
            (payload `(("contents" . ,messages)))
            (json-payload (json-encode payload))
            (request-data (encode-coding-string json-payload 'utf-8))
            (url (format "%s%s:generateContent?key=%s"
                   gemini-api-base-url
                   model
                   api-key))
            (url-request-method "POST")
            (url-request-extra-headers '(("Content-Type" . "application/json; charset=utf-8")))
            (url-request-data request-data))
      
      ;; 待機メッセージをバッファに挿入
      (insert gemini--waiting-message)

      ;; APIにリクエストを送信
      (message "Geminiにリクエストを送信中 (モデル: %s)..." model)
      (url-retrieve url 'gemini--handle-response (list (current-buffer))))))

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
      (insert "User: "))))

(provide 'gemini-mode)

;;; gemini-mode.el ends here
