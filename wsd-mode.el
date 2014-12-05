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
            (list (regexp-quote i) 'quote face))
          list))

(defun wsd-add-keywords (face list)
  (let* ((klist (wsd-create-font-lock-list face list)))
    (font-lock-add-keywords nil klist)))

(define-derived-mode wsd-mode fundamental-mode "wsd-mode"
  "Major-mode for websequencediagrams.com"
  (wsd-add-keywords 'font-lock-keyword-face
                    '("title"
                      "participant" " as "

                      "alt" "else" "end"
                      "note" "over" "right" "left" "of"))
  (wsd-add-keywords 'font-lock-comment-face
		    '("-->-" "-->" "->+" "->-" "->" ": "))

  (local-set-key (kbd "C-c C-c") 'wsd-process))

(provide 'wsd-mode)
