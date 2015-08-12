;;; wsd-mode.el --- Emacs major-mode for www.websequencediagrams.com

;; Author     : Jostein Kjønigsen <jostein@gmail.com>
;; Created    : December 2014
;; Modified   : August 2015
;; Version    : 0.4.0
;; Keywords   : wsd diagrams design process modelling uml
;; X-URL      : https://github.com/josteink/wsd-mode
;;

;;; Commentary:
;;
;; This is a major-mode for modelling and editing sequence-diagrams
;; using the syntax employed by the online service
;; www.websequencediagrams.com.
;;
;; The mode supports inline rendering the diagrams through the API
;; provided through the website and persisting these to image-files next
;; to the files used to generate them.
;;
;; It will automatically activate for files with a WSD-extension.
;;
;;
;; Features:
;;
;; - syntax higlighting of reccognized keywords
;; - automatic indentation of block-statements
;; - generating and saving diagrams generated through WSD's online API.
;; - support for WSD premium features (svg-export, etc) if API-key is
;;   provided.
;; - rendering diagrams inline in emacs, or in external OS viewer if image
;;   format is not supported by Emacs.
;;
;;
;; Customization:
;;
;; The mode can be slightly customized. Documenting this fully is on the
;; TODO-list.
;;
;; To create mode-specific emacs-customizations, please use the
;; wsd-mode-hook.
;;
;; A short summary of customizable variables:
;;
;; - wsd-api-key (default blank. required for premium-features.)
;; - wsd-format (default png. svg requires premium, thus api-key.)
;; - wsd-style (default modern-blue)
;; - wsd-indent-offset (default 4)
;;

;;; Versions:
;;
;;    0.1.0 - Initial release.
;;    0.2.0 - Fix file-format bug and temp-buffer reuse.
;;            Improved syntax-highlighting.
;;            Fix OSX rendering issues.
;;    0.3.0 - Fix compatiblity issues with Emacs 24.3.
;;            Improved org-babel support.
;;    0.4.0 - integrate with customize framework

;;; Code:

(require 'wsd-core)

(ignore-errors
  (require 'ob-wsdmode))

;; notes about derived mode here: http://www.emacswiki.org/emacs/DerivedMode

(defun wsd-create-font-lock-list (face list)
  (mapcar (lambda (i)
            (cons i face))
          list))

(defun wsd-add-keywords (face list)
  (let* ((klist (wsd-create-font-lock-list face list)))
    (font-lock-add-keywords nil klist)))

(defcustom wsd-indent-offset 4
  "Indentation offset for `wsd-mode'."
  :type 'integer
  :group 'wsd-mode)

(defun wsd-any (predicate list)
  (let* ((result nil))
    (dolist (item list)
      (setq result (or result
                       (funcall predicate item))))
    result))

(defun wsd-line-starts-with (keywords)
  (beginning-of-line-text)
  (let* ((word       (thing-at-point 'word t)))
    (if (equal nil word)
	nil
      (wsd-any `(lambda (x) (equal ,(downcase word) x)) keywords))))

(defun wsd-indent-line ()
  "Indent current line for `wsd-mode'."
  (interactive)
  (indent-line-to (wsd-get-line-indent)))

(defun wsd-get-string-at-point ()
  (let* ((thing (thing-at-point 'word t)))
    (if (equal nil thing)
        nil
      (downcase thing))))

;; else not included as it doesnt affect the -overall- indentation either way
(defconst wsd-indentation-keywords '("alt" "opt" "loop" "end"))

(defun wsd-is-indentation-keyword (word)
  (and (not (equal nil word))
       (member word wsd-indentation-keywords)))

(defun wsd-get-buffer-indentation-keywords ()
  "Returns the list of indentation-keywords found from the current point in the buffer, back to the start."
  (interactive)

  (save-excursion
    (beginning-of-line)

    (let* ((words '()))
      (while (not (equal (point) (point-min)))
        (beginning-of-line-text)
        (let* ((word (wsd-get-string-at-point)))
          (when (wsd-is-indentation-keyword word)
            (setq words (cons word words))))
        (beginning-of-line)
        (forward-line -1))
      words)))

(defun wsd-get-indentation-from-keywords (keywords)
  "Get the overall indentation from the supplied keywords."
  (let* ((indent-col   0)
	 (indent-plus  '("alt" "opt" "loop"))
	 (indent-minus '("end")))
    (dolist (keyword keywords)
      (when (member keyword indent-plus)
	(setq indent-col (+ indent-col wsd-indent-offset)))
      (when (member keyword indent-minus)
	(setq indent-col (- indent-col wsd-indent-offset)))
      (setq indent-col (max 0 indent-col)))
    indent-col))

(defun wsd-get-adjustment-indent ()
  "Adjust overall document indentation with specific reverse compensation for branch-starting keywords based on the current line."
  (if (wsd-line-starts-with '("alt" "opt" "else"))
      (- 0 wsd-indent-offset)
    0))

(defun wsd-get-line-indent ()
  (let* ((keywords          (wsd-get-buffer-indentation-keywords))
	 (keyword-indent    (wsd-get-indentation-from-keywords keywords))
	 (adjustment-indent (wsd-get-adjustment-indent)))
    (+ keyword-indent adjustment-indent)))

;;;###autoload
(define-derived-mode wsd-mode fundamental-mode "wsd-mode"
  "Major-mode for websequencediagrams.com"
  (let* ((line-starters (mapcar
			 (lambda (kw) (concat "^[[:space:]]*" kw))
			 '("title" "participant" "deactivate" "activate"
			   "alt" "else" "opt" "loop" "end"
			   "note")))
	 (non-starters   '(" over " " right " " left " " of "
			   " as "))
	 (all-keywords    (append non-starters line-starters)))
    (wsd-add-keywords 'font-lock-keyword-face all-keywords))

  ;; lines starting with # are actual commens
  (let* ((operator-list '("-->-" "-->" "->+" "->-" "->" ": " "#"))
         (operators      (wsd-create-font-lock-list 'font-lock-comment-face operator-list)))
    ;;(setq operators (cons (cons (regexp-quote "#") 'font-lock-command-face) operators))
    (font-lock-add-keywords nil operators))

  (make-local-variable 'wsd-indent-offset)
  (set (make-local-variable 'indent-line-function) 'wsd-indent-line))

(define-key wsd-mode-map (kbd "C-c C-c") 'wsd-show-diagram-inline)
(define-key wsd-mode-map (kbd "C-c C-e") 'wsd-show-diagram-online)
(define-key wsd-mode-map (kbd "C-c C-k") 'wsd-strip-errors)


;;; Autoload mode trigger
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wsd$" . wsd-mode))

(provide 'wsd-mode)

;;; wsd-mode.el ends here
