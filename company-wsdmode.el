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
(ignore-errors
  ;; required to allow byte-compilation
  (require 'company))

(defconst wsd-keyword-completions
  '("title" "participant" "as" "over" "right" "left" "of"
    "autonumber" "parallel" "activate" "deactivate"
    "alt" "opt" "end" "loop" "else" "destroy"
    "note" "state" "ref"))

(defun wsd-get-participants ()
  "Returns a list of participants found in the document."
  (save-excursion
    (goto-char (point-min))
    (let (res)
      (while (re-search-forward "^[[:blank:]]*participant[[:blank:]]+\\(.+\\)[[:blank:]]+as[[:blank:]]+\\(.+\\)$" nil t nil)
        (add-to-list 'res (match-string-no-properties 1))
        (add-to-list 'res (match-string-no-properties 2)))
      res)))

(defun wsd-get-actors ()
  "Returns a list of actors found in the document."
  (save-excursion
    (goto-char (point-min))
    (let* ((operators '("-->-" "-->" "->+" "->*" "->-" "->"))
           (rx-operators (regexp-opt operators t))
           (rx-actors (wsd-rx-lstart (concat "\\([^\n-]+\\)"
                                             rx-operators
                                             "\\(.+\\)"
                                             ":.*$")))
           res)
      (while (re-search-forward rx-actors nil t nil)
        (add-to-list 'res (match-string-no-properties 1))
        (add-to-list 'res (match-string-no-properties 3)))
      res)))

(defun wsd-get-completion-keywords ()
  "Return things which are valid completions for the current buffer."

  ;; TODO: determine if looking at statement where participant is expected
  ;; in those cases, do NOT return keyword matches!
  (append
   wsd-keyword-completions
   (wsd-get-participants)
   (wsd-get-actors))
  ;; TODO: enumerate active/deactivate statements? or redundant?
  )

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


(when (boundp 'company-backends)
  (add-to-list 'company-backends 'company-wsd-mode))

(provide 'company-wsdmode)
;;; company-wsdmode.el ends here
