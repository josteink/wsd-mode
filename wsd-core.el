
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
      (concatenate 'string "&apikey=" wsd-api-key)
    ""))

(defun wsd-encode (message)
  (url-encode-url message))

(defun wsd-get-request-data (message)
  (let* ((encoded (wsd-encode message))
         (apikey  (wsd-get-apikey-section)))
    (concatenate 'string
                 "apiVersion=1"
                 "&format=png" wsd-format
                 "&style=" wsd-style
                 "&message=" encoded
                 apikey)))

(setf wsd-debug-json '())

(defun wsd-send (message)
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
	(setf wsd-debug-json json)
	(kill-buffer wsd-response)
	json))))

(defun wsd-get-image-url (json)
  (let* ((url (concatenate 'string
                           wsd-base-url
                           (cdr (assoc 'img json)))))
    url))

(defun wsd-process ()
  (interactive)
  (save-excursion
    (let* ((message (buffer-substring-no-properties (point-min) (point-max)))
           (json    (wsd-send message))
           (url     (wsd-get-image-url json)))
      (browse-url url))))


(provide 'wsd-core)
