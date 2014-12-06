
;; user-customizable sections

;; only required for premium-features.
(defvar wsd-api-key "")

;; "svg" is also a permitted format, but this requires a premium account
;; and thus a api-key.
(defvar wsd-format "png")
(defvar wsd-style "modern-blue")

;; actual code

;; implementation based on documentation as found here:
;; http://www.websequencediagrams.com/embedding.html

(require 'url)
(require 'json)

(defconst wsd-base-url "http://www.websequencediagrams.com/")

(defun wsd-get-apikey-section ()
  (if wsd-api-key
      (concat "&apikey=" wsd-api-key)
    ""))

(defun wsd-encode (message)
  (let* ((encode1 (replace-regexp-in-string (regexp-quote "+")
                                            (regexp-quote "%2B")
                                            message))
         (encode2 (url-encode-url encode1)))
    encode2))

(defun wsd-get-request-data (message)
  (let* ((encoded (wsd-encode message))
         (apikey  (wsd-get-apikey-section)))
    (concat "apiVersion=1"
            "&format=png" wsd-format
            "&style=" wsd-style
            "&message=" encoded
            apikey)))

(defun wsd-get-json (message)
  (let* ((url-request-method        "POST")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data          (wsd-get-request-data message))
         (wsd-response              (url-retrieve-synchronously wsd-base-url)))
    (save-excursion
      (switch-to-buffer wsd-response)

      (goto-char (point-min))
      ;; move to beginning of JSON response
      (search-forward "{")
      (left-char)
      ;; parse and return json at point

      (let* ((json (json-read)))
        (kill-buffer wsd-response)
        json))))

(defun wsd-get-image-url (json)
  (let* ((url (concat wsd-base-url
                      (cdr (assoc 'img json)))))
    url))

(defun wsd-get-image-extension ()
  (concat "." wsd-format))

(defun wsd-get-temp-filename ()
  (make-temp-file "wsd-" nil (wsd-get-image-extension)))

(defun wsd-get-image-filename (name)
  (if name
      (concat (file-name-sans-extension name) (wsd-get-image-extension))
    (wsd-get-temp-filename)))

(defun wsd-get-image-buffer-name (buffer-name file-name)
  (if (not (buffer-name))
      (concat "wsd-temp-buffer." wsd-format)
    file-name))

(defun wsd-display-image-inline (buffer-name file-name)
  (save-excursion
    (switch-to-buffer buffer-name)
    (iimage-mode t)

    (read-only-mode -1)
    (kill-region (point-min) (point-max))
    ;; unless we clear the cache, the same cached image will
    ;; always get redisplayed.
    (clear-image-cache nil)
    (insert-image (create-image file-name))
    (read-only-mode t)))

(defun wsd-image-format-supported-p ()
  (image-type-available-p (intern wsd-format)))

;; for debugging
(setq wsd-json nil)

(defun wsd-show-diagram-inline ()
  "Attempts to show the diagram provided by the current buffer inside an Emacs-buffer.
   If emacs lacks format for the given graphics-format it will be delegated to the
   operating-system to open the local copy."
  (interactive)
  (let* ((orig-buffer (buffer-name))
         (buffer-name (buffer-file-name))
         (file-name   (wsd-get-image-filename buffer-name)))
    (let* ((message (buffer-substring-no-properties (point-min) (point-max)))
	   (json    (wsd-get-json message))
	   (url     (wsd-get-image-url json)))
      (save-excursion
	(setq wsd-json json)
	(url-copy-file url file-name t))

      (if (display-graphic-p)
	  (if (wsd-image-format-supported-p)
	      (let* ((image-buffer-name (wsd-get-image-buffer-name buffer-name file-name)))
		(wsd-display-image-inline image-buffer-name file-name)
		(switch-to-buffer orig-buffer))
	    (browse-url file-name))
	(message url)))))


(defun wsd-show-diagram-online ()
  "Shows the current buffer on www.websequencediagrams.com"
  (interactive)
  (let* ((message      (buffer-substring-no-properties (point-min) (point-max)))
	 (encoded      (wsd-encode message))
	 (url          (concat wsd-base-url "?m=" encoded)))
    (browse-url url)))

(provide 'wsd-core)
