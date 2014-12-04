
(require 'wsd-core)

;; notes about derived mode here: http://www.emacswiki.org/emacs/DerivedMode

(defface wsd-face-1 
  '((t (:foreground "#0000FF")))
  "blue")

(setq wsd-keyswords '("title"
		      "participant" "as"
		      "-->-" "-->" "->+" "->-" "->"
		      "alt" "else" "end"
		      "note" "over" "right" "left" "of"))

;; map keywords to font-faces
(setq wsd-keyword-list
      (mapcar (lambda (i)
		(list (regexp-quote i) 'quote 'wsd-face-1))
	      wsd-keyswords))

(define-derived-mode wsd-mode fundamental-mode "wsd-mode"
  "Major-mode for websequencediagrams.com"
  (font-lock-add-keywords nil wsd-keyword-list)

  (local-set-key (kbd "C-c C-c") 'wsd-process))
