;;; wsd-mode.el --- Emacs major-mode for www.websequencediagrams.com

;; Author     : Jostein Kjønigsen <jostein@gmail.com>
;; Created    : December 2014
;; Modified   : November 2014
;; Version    : 0.0.1
;; Keywords   : wsd diagrams design process modelling
;; X-URL      : https://github.com/josteink/wsd-mode
;;

(require 'wsd-core)
(require 'pcase)

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
  (wsd-any 'looking-at-p keywords))

(defun wsd-previous-line-starts-with (keywords)
  (save-excursion
    (previous-line)
    (wsd-line-starts-with keywords)))

(defun wsd-indent-line ()
  "Indent current line for `wsd-mode'."
  (interactive)
  (let ((indent-col 0)
        (indent-mappings '((wsd-previous-line-starts-with ("alt " "opt " "loop" "else") +)
                           (wsd-line-starts-with          ("end" "else") -))))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (while t
            (beginning-of-line-text)
            (dolist (mapping indent-mappings)
              (pcase-let* ((`(,look-up-func ,keywords ,indent-func) mapping))
                (when (funcall look-up-func keywords)
                  (setq indent-col (funcall indent-func indent-col wsd-indent-offset)))))
            (previous-line))
        (error nil)))
    (setq indent-col (max 0 indent-col))
    (indent-line-to indent-col)))

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
