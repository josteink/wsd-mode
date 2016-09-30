;;; wsd-core --- Summary
;;;
;;; Commentary:
;;;   Core code for wsd-mode
;;;
;;; Code:

;; user-customizable sections

;; only required for premium-features.

(defcustom wsd-api-key nil
  "Key for use with premium features."
  :type 'string
  :group 'wsd-mode)

;; "svg" is also a permitted format, but this requires a premium account
;; and thus a api-key.
(defcustom wsd-format "png"
  "Image format to use when generating the diagram.
The svg format is also permitted, but requires a premium
account."
  :type '(choice (const "png") (const "svg"))
  :group 'wsd-mode)

(defcustom wsd-style "modern-blue"
  "Output style, default is modern-blue."
  :type '(choice (const "plain-uml") (const "rose") (const "qsd") (const "napkin") (const "vs2010") (const "mscgen") (const "patent") (const "omegapple") (const "modern-blue") (const "earth") (const "roundgreen") (const "magazine"))
  :group 'wsd-mode)

(defcustom wsd-base-url "http://www.websequencediagrams.com/"
  "Default base URL."
  :type 'string
  :group 'wsd-mode)

;; actual code

;; implementation based on documentation as found here:
;; http://www.websequencediagrams.com/embedding.html

(require 'url)
(require 'json)

(defun wsd-get-apikey-section ()
  "Return a key-value pair for the API-key to be user in request data.

Delimiters included.  If no api-key is used, returns nil."
  (if wsd-api-key
      (concat "&apikey=" wsd-api-key)
    ""))

(defun wsd-encode (message)
  "Encodes the provided `MESSAGE' into something which can be transported over HTTP."
  (url-hexify-string message))

(defun wsd-get-request-data (message)
  "Transform `MESSAGE' into request-data for a HTTP post to the wsd.com API."
  (let* ((encoded (wsd-encode message))
         (apikey  (wsd-get-apikey-section)))
    (concat "apiVersion=1"
            "&format=" wsd-format
            "&style=" wsd-style
            "&message=" encoded
            apikey)))

(defun wsd-get-json (message)
  "Send `MESSAGE' to the server and return the server's JSON-response."
  (let* ((url-request-method        "POST")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data          (wsd-get-request-data message))
         (wsd-response              (url-retrieve-synchronously wsd-base-url)))
    (save-excursion
      (switch-to-buffer wsd-response)

      (goto-char (point-min))
      ;; move to beginning of JSON response
      (search-forward "{")
      (backward-char)
      ;; parse and return json at point

      (let* ((json (json-read)))
        (kill-buffer wsd-response)
        json))))

(defun wsd-get-image-url (json)
  "Extract the image-url based on the server's `JSON'-response."
  (let* ((url (concat wsd-base-url
                      (cdr (assoc 'img json)))))
    url))

(defun wsd-get-errors (json)
  "Extract the error-elements based on the server's `JSON'-response."
  (let* ((errors (cdr (assoc 'errors json))))
    (append errors '())))

(defun wsd-parse-error (entry)
  "Parse a single error-response `ENTRY' returned by the WSD server."
  (with-temp-buffer
    (insert entry)
    (goto-char (point-min))
    (search-forward-regexp "Line \\([[:digit:]]+\\): \\(.*\\)")
    (let* ((line-num          (string-to-number (match-string 1)))
           (error-description (match-string 2)))
      (cons line-num error-description))))

(defun wsd-get-error-lines (error-list)
  "Process and parse all the errors in `ERROR-LIST'."
  (mapcar #'wsd-parse-error error-list))

(defun wsd-get-image-extension ()
  "Return the file-name extension to be used based on the current wsd-mode configuration."
  (concat "." wsd-format))

(defun wsd-get-temp-filename ()
  "Return an appropriate corresponding image-filename for a given non-persisted buffer."
  (make-temp-file "wsd-" nil (wsd-get-image-extension)))

(defun wsd-get-image-filename (name)
  "Return an appropriate corresponding image-filename for a buffer `NAME'."
  (if name
      (concat (file-name-sans-extension name) (wsd-get-image-extension))
    nil))

(defun wsd-get-image-buffer-name (buffer-name file-name)
  "Return an appropriate corresponding buffer name to display resulting image in."
  (if (not buffer-name)
      (concat "wsd-temp-buffer." wsd-format)
    file-name))

(defun wsd-display-image-inline (buffer-name file-name)
  "Use `BUFFER-NAME' to display the image in `FILE-NAME'.

Checks weather `BUFFER-NAME' already exists, and if not create as needed."
  (save-excursion
    (switch-to-buffer buffer-name)
    (iimage-mode t)

    (read-only-mode -1)
    (kill-region (point-min) (point-max))
    ;; unless we clear the cache, the same cached image will
    ;; always get redisplayed.
    (clear-image-cache nil)
    (insert "\n")
    (insert-image (create-image file-name))
    (beginning-of-buffer)
    (read-only-mode t)))

(defun wsd-image-format-supported-p ()
  "Helper function to determine if we can display the image we're generating."
  (image-type-available-p (intern wsd-format)))

;; buffer-local state variables
(defvar wsd-last-temp-file nil)

(defvar wsd-mode-processing-complete-hook nil
  "Hook called when wsd-mode has processed the script in the current buffer.
Hook is called with an `ERROR' parameter.")

(defun wsd-show-diagram-inline ()
  "Attempt to show the diagram provided by the current buffer inside Emacs.

If Emacs lacks format for the given graphics-format it will be delegated
to the operating-system to open the local copy."
  (interactive)
  (let* ((orig-buffer (buffer-name))
         (buffer-name (buffer-file-name))
         (temp-name   (wsd-get-temp-filename))
         ;; only required for saved buffers.
         (file-name   (wsd-get-image-filename buffer-name))
         (message (buffer-substring-no-properties (point-min) (point-max)))
         (json    (wsd-get-json message))
         (url     (wsd-get-image-url json))
         (errors  (wsd-get-error-lines (wsd-get-errors json))))
    (save-excursion
      (url-copy-file url temp-name t)

      ;; only copy to file when in a saved buffer
      (when file-name
        (copy-file temp-name file-name t t t)))

    (when wsd-mode-processing-complete-hook
      (run-hook-with-args 'wsd-mode-processing-complete-hook errors))

    (if (display-graphic-p)
        (if (wsd-image-format-supported-p)
            (let* ((image-buffer-name (wsd-get-image-buffer-name buffer-name file-name))
                   (buffer-exists     (get-buffer image-buffer-name)))
              ;; display image from temp-area because of bug in OSX Emacs.
              ;; https://github.com/josteink/wsd-mode/issues/11
              (wsd-display-image-inline image-buffer-name temp-name)
              (switch-to-buffer orig-buffer)
              (when (not buffer-exists)
                (switch-to-buffer-other-window image-buffer-name))

              ;; avoid ending up with a flurry of temp-files.
              (when wsd-last-temp-file
                (delete-file wsd-last-temp-file))

              (set (make-local-variable 'wsd-last-temp-file) temp-name))
          (wsd-browse-url temp-name))
      (message url))))

(defun wsd-browse-url (url)
  "Open `URL' in a browser which can support image-formats not understood by Emacs."

  ;; Some people have eww bound as the default-browser.
  ;;
  ;; If we're being invoked it's because Emacs doesn't support the image-format
  ;; being used. That means eww wont be able to display it either.
  ;; Therefore if eww is available, we force launching in external browser,
  ;; using eww's own implementation.
  ;;
  ;; This way we should be guaranteed to always use an external viewer.
  (if (fboundp 'eww-browse-with-external-browser)
      (eww-browse-with-external-browser url)
    (browse-url url)))

(defun wsd-get-diagram-online-url (message)
  "Return the url to show `MESSAGE' online."
  (let*
      ((encoded      (url-build-query-string
                      `((m ,message))))
       (url          (concat wsd-base-url "?" encoded)))
    url))

(defun wsd-show-diagram-online ()
  "Show the current buffer on www.websequencediagrams.com."
  (interactive)
  (let* ((message      (buffer-substring-no-properties (point-min) (point-max)))
         (url          (wsd-get-diagram-online-url message)))
    (browse-url url)))

(provide 'wsd-core)
;;; wsd-core.el ends here
