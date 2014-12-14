;;; wsd-mode.el --- Emacs major-mode for www.websequencediagrams.com

;; Author     : Jostein Kjønigsen <jostein@gmail.com>
;; Created    : December 2014
;; Modified   : November 2014
;; Version    : 0.0.1
;; Keywords   : wsd diagrams design process modelling
;; X-URL      : https://github.com/josteink/wsd-mode
;;

(require 'wsd-core)

;; notes about derived mode here: http://www.emacswiki.org/emacs/DerivedMode

(defun wsd-create-font-lock-list (face list)
  (mapcar (lambda (i)
            (cons (regexp-quote i) face))
          list))

(defun wsd-add-keywords (face list)
  (let* ((klist (wsd-create-font-lock-list face list)))
    (font-lock-add-keywords nil klist)))

(defvar wsd-indent-offset 4
  "*Indentation offset for `wsd-mode'.")

(defun wsd-any (predicate list)
  (let* ((result nil))
    (dolist (item list)
      (setq result (or result
                       (funcall predicate item))))
    result))

(defun wsd-line-starts-with (keywords)
  (beginning-of-line-text)
  (let* ((word       (thing-at-point 'word t)))
    (if (equalp nil word)
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
  (wsd-add-keywords 'font-lock-keyword-face
                    '("title"
                      "participant" " as "
                      "deactivate" "activate"
                      "alt" "else" "opt" "loop" "end"
                      "note" "over" "right" "left" "of"))

  ;; lines starting with # are actual commens
  (let* ((operator-list '("-->-" "-->" "->+" "->-" "->" ": " "#"))
         (operators      (wsd-create-font-lock-list 'font-lock-comment-face operator-list)))
    ;;(setq operators (cons (cons (regexp-quote "#") 'font-lock-command-face) operators))
    (font-lock-add-keywords nil operators))

  (local-set-key (kbd "C-c C-c") 'wsd-show-diagram-inline)
  (local-set-key (kbd "C-c C-e") 'wsd-show-diagram-online)
  (local-set-key (kbd "C-c C-k") 'wsd-strip-errors)

  (make-local-variable 'wsd-indent-offset)
  (set (make-local-variable 'indent-line-function) 'wsd-indent-line))

;;; Autoload mode trigger
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wsd$" . wsd-mode))

;;; Autoload to load actual mode.
;;;###autoload
(autoload 'wsd-mode "wsd-mode" "Emacs major-mode for www.websequencediagrams.com." t)

(provide 'wsd-mode)

;;; wsd-mode.el ends here
