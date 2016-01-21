;;; Package --- Summary
;;
;; This package provides company completion for wsd-mode buffers.
;;
;;
;;; Commentary:
;;
;; Activated automatically by wsd-mode itself.
;;
;;; Code:
(require 'cl-lib)
(require 'company)

(defconst wsd-keyword-completions
  '("title" "participant" "as" "over" "right" "left" "of"
    "autonumber" "parallel" "activate" "deactivate"
    "alt" "opt" "end" "loop" "else" "destroy"
    "note" "state" "ref"))

(defun wsd-get-completion-keywords ()
  "Return things which are valid completions for the current buffer."
  
  ;; TODO: enumerate participant-statements (for shorthand)
  ;; TODO: enumerate -> statements
  ;; TODO: enumerate active/deactivate statements? or redundant?
  wsd-keyword-completions)

(defun company-wsd-mode (command &optional arg &rest ignored)
  "Company back-end function for `wsd-mode'.

See https://github.com/company-mode/company-mode/wiki/Writing-backends for
documentation on parameters."
  
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'company-wsd-mode))
    (prefix (and (eq major-mode 'wsd-mode)
                 (company-grab-symbol)))
    (candidates
     (cl-remove-if-not
      (lambda (c) (string-prefix-p arg c))
      (wsd-get-completion-keywords)))))

(add-to-list 'company-backends 'company-wsd-mode)

(provide 'company-wsdmode)
;;; company-wsdmode.el ends here
