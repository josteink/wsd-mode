
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
  "Returns a key-value pair for the API-key to be user in request data, delimiters included.
   If no api-key is used, returns nil."
  (if wsd-api-key
      (concat "&apikey=" wsd-api-key)
    ""))

(defun wsd-encode (message)
  "Encodes the provided message into something which can be transported over HTTP."
  (let* ((encode1 (replace-regexp-in-string (regexp-quote "+")
                                            (regexp-quote "%2B")
                                            message))
         (encode2 (url-encode-url encode1)))
    encode2))

(defun wsd-get-request-data (message)
  "Gets the request-data for a HTTP post to the wsd.com API."
  (let* ((encoded (wsd-encode message))
         (apikey  (wsd-get-apikey-section)))
    (concat "apiVersion=1"
            "&format=png" wsd-format
            "&style=" wsd-style
            "&message=" encoded
            apikey)))

(defun wsd-get-json (message)
  "Sends the provided message to the server and returns the server's JSON-response."
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
  "Based on the server's JSON-response, extracts the image-url to the resulting image."
  (let* ((url (concat wsd-base-url
                      (cdr (assoc 'img json)))))
    url))

(defun wsd-get-errors (json)
  "Based on the server's JSON-response, extracts error-elements."
  (let* ((errors (cdr (assoc 'errors json))))
    (append errors '())))

(defun wsd-parse-error (entry)
  "Parses a single error-response as returned by the WSD server."
  (let* ((line-num          '())
	 (error-description '()))
    (cons line-num error-description)))

(defun wsd-get-error-lines (error-list)
  "Processes and parses all the errors in the provided list."
  (mapcar 'wsd-parse-error error-list))

(defun wsd-get-image-extension ()
  "Returns the file-name extension to be used based on the current wsd-mode configuration."
  (concat "." wsd-format))

(defun wsd-get-temp-filename ()
  "Returns an appropriate corresponding image-filename for a given non-persisted buffer."
  (make-temp-file "wsd-" nil (wsd-get-image-extension)))

(defun wsd-get-image-filename (name)
  "Returns an appropriate corresponding image-filename for a given buffer."
  (if name
      (concat (file-name-sans-extension name) (wsd-get-image-extension))
    (wsd-get-temp-filename)))

(defun wsd-get-image-buffer-name (buffer-name file-name)
  "Returns an appropriate corresponding buffer name to display resulting image in."
  (if (not (buffer-name))
      (concat "wsd-temp-buffer." wsd-format)
    file-name))

(defun wsd-display-image-inline (buffer-name file-name)
  "Displays the provided image in the provided buffer. Buffer is created if non-existant."
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
  "Helper function to determine if we can display the image we're generating."
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
         (file-name   (wsd-get-image-filename buffer-name))
	 (message (buffer-substring-no-properties (point-min) (point-max)))
	 (json    (wsd-get-json message))
	 (url     (wsd-get-image-url json)))
    (save-excursion
      (setq wsd-json json)
      (url-copy-file url file-name t))

    (if (display-graphic-p)
	(if (wsd-image-format-supported-p)
	    (let* ((image-buffer-name (wsd-get-image-buffer-name buffer-name file-name))
		   (buffer-exists     (get-buffer image-buffer-name)))
	      (wsd-display-image-inline image-buffer-name file-name)
	      (switch-to-buffer orig-buffer)
	      (when (not buffer-exists)
		(switch-to-buffer-other-window image-buffer-name)))
	  (browse-url file-name))
      (message url))))


(defun wsd-show-diagram-online ()
  "Shows the current buffer on www.websequencediagrams.com"
  (interactive)
  (let* ((message      (buffer-substring-no-properties (point-min) (point-max)))
	 (encoded      (wsd-encode message))
	 (url          (concat wsd-base-url "?m=" encoded)))
    (browse-url url)))

(provide 'wsd-core)
