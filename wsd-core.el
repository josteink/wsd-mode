
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

(defun wsd-list-intersperse (w-lst w-item)
  "Intersperse sequence W-LST W-ITEM.\n
:EXAMPLE\n\n\(mon-list-intersperse (number-sequence 0 6) '^\)\n
\(mon-mapcar #'\(lambda \(x &rest y\)
                \(mon-list-intersperse `\(,x ,@y\) '_\)\)
            '\(a s d f g\) '\(g f d s a\)
            '\(q w e r t\) '\(t r e w q\)\)\n
:SEE-ALSO `mon-transpose', `mon-map-append', `mon-mapcan', `mon-mapcon'.\n►►►"
  (let ((mil-list* ;; this is cl.el's `list*'
         #'(lambda (mil-L-1-arg &rest mil-L-1-rest)
             (cond ((not mil-L-1-rest) mil-L-1-arg)
                   ((not (cdr mil-L-1-rest))
                    ;; (cons mil-L-1-arg (car mil-L-1-rest))
                    (nconc mil-L-1-arg (car mil-L-1-rest)))
                   (t (let* ((mil-L-1-len (length mil-L-1-rest))
                             (mil-L-1-cpy (copy-sequence mil-L-1-rest))
                             (mil-L-1-tl  (nthcdr (- mil-L-1-len 2) mil-L-1-cpy)))
                        (setcdr mil-L-1-tl (cadr mil-L-1-tl))
                        (cons mil-L-1-arg mil-L-1-cpy)))))))
    (if (null (cdr w-lst))
        w-lst
      ;; If we could use cl.el's `list*', then following would work:
      ;; :WAS (list* (car w-lst) w-item
      ;;          (mon-intersperse-list (cdr w-lst) w-item))
      (apply mil-list*
             (car w-lst)
             (list w-item (wsd-list-intersperse (cdr w-lst) w-item))))))

(defun wsd-string-replace (source match replacement)
  (let* ((parts  (split-string source match))
         (parts2 (wsd-list-intersperse parts replacement))
         (result (apply 'concatenate 'string parts2)))
    result))

(defun wsd-encode (message)
  (let* ((encode1 (wsd-string-replace message "+" "%2B"))
         (encode2 (url-encode-url encode1)))
    encode2))

(defun wsd-get-request-data (message)
  (let* ((encoded (wsd-encode message))
         (apikey  (wsd-get-apikey-section)))
    (concatenate 'string
                 "apiVersion=1"
                 "&format=png" wsd-format
                 "&style=" wsd-style
                 "&message=" encoded
                 apikey)))

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
