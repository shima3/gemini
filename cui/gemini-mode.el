;;; gemini-mode.el --- A simple Emacs client for Google Gemini -*- lexical-binding: t; -*-

;; Gemini APIと通信するためのシンプルなモード。
;;
;; 使い方:
;; 1. (setq gemini-api-key "YOUR_API_KEY") を init.el に設定してください。
;; 2. (setq gemini-model "gemini-1.5-pro-latest") のようにモデル名も設定できます。
;; 3. M-x gemini でチャットバッファを開始します。
;; 4. プロンプトを入力し、C-<return> を押して Gemini に送信します。

(require 'json)
(require 'url)

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

(defconst gemini-message-separator "^---\n"
  "メッセージの区切りを示す正規表現。")

;;; 内部ヘルパー関数

(defun gemini--parse-buffer ()
  "現在のバッファを解析し、Gemini APIのフォーマットに合ったメッセージリストを返す。"
  (let ((messages '()))
    (save-excursion
      (goto-char (point-min))
      (let ((parts (split-string (buffer-string) gemini-message-separator t "[ \t\n]+")))
        (dolist (part parts)
          (with-temp-buffer
            (insert part)
            (goto-char (point-min))
            (let ((role (cond ((looking-at "user:") "user")
                              ((looking-at "model:") "model")
                              (t nil)))
                  (content ""))
              (when role
                (forward-line 1)
                (setq content (string-trim (buffer-substring-no-properties (point) (point-max))))
                (unless (string-empty-p content)
                  (push `(:role ,role :parts ((:text . ,content))) messages))))))))
    (nreverse messages)))

(defun gemini--insert-response (response-text)
  "Geminiからの応答をバッファの最後に挿入する。"
  (goto-char (point-max))
  ;; 最後の "user: " プロンプトが空なら削除
  (when (looking-back "^user:[ \t]*\n?$" (line-beginning-position))
    (delete-region (line-beginning-position) (point)))
  ;; 末尾の空白を削除
  (delete-blank-lines)
  (unless (eq (char-before) ?\n) (insert "\n"))
  (insert (format "\n---\nmodel: %s\n---\nuser: " response-text))
  (message "Geminiからの応答を受信しました。"))

(defun gemini--handle-response (status original-buffer)
  "url-retrieveの非同期コールバック。応答を処理する。"
  (with-current-buffer (url-retrieve-buffer)
    (let ((response-status (url-http-response-status status))
          (response-body (buffer-string)))
      (if (= 200 response-status)
          (let* ((json-object (json-read-from-string response-body))
                 (candidates (cdr (assoc 'candidates json-object)))
                 (first-candidate (car candidates))
                 (content (cdr (assoc 'content first-candidate)))
                 (parts (cdr (assoc 'parts content)))
                 (first-part (car parts))
                 (text (or (cdr (assoc 'text first-part)) "")))
            (with-current-buffer original-buffer
              (gemini--insert-response text)))
        (display-buffer (current-buffer))
        (error "Gemini APIエラー (HTTP %d): %s" response-status response-body))))
  (kill-buffer (url-retrieve-buffer)))


;;; メイン関数

(defun gemini-send ()
  "現在のバッファの内容をGemini APIに送信する。"
  (interactive)
  (unless (and gemini-api-key (not (string-empty-p gemini-api-key)))
    (error "Gemini APIキーが設定されていません。(setq gemini-api-key \"...\") で設定してください。"))

  (let* ((messages (gemini--parse-buffer))
         (payload `(:contents ,messages))
         (json-payload (json-encode payload))
         (url (format "%s%s:generateContent?key=%s"
                      gemini-api-base-url
                      gemini-model
                      gemini-api-key))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data json-payload))
    (message "Geminiにリクエストを送信中 (モデル: %s)..." gemini-model)
    (url-retrieve url 'gemini--handle-response (current-buffer))))


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
      (insert "---\nuser: "))))

(provide 'gemini-mode)

;;; gemini-mode.el ends here
