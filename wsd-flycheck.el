;;; wsd-flycheck.el --- flycheck-support for wsd-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Jostein Kjønigsen

;; Author: Jostein Kjønigsen <jostein@gmail.com>
;; Keywords: languages, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Quick support drafted based on sample found here:
;; https://github.com/flycheck/flycheck-ocaml/blob/master/flycheck-ocaml.el

;;; Code:

;; assign default-values.
(defvar wsd-flycheck-checker nil)
(defvar wsd-flycheck-callback nil)

(defun wsd-flycheck-parse-errors (checker wsd-errors)
  (mapcar (lambda (wsd-error)
            (let* (;; wsd indexes are 0-based. emacs is 1-based.
                   (line     (+ 1 (car wsd-error)))
                   (message  (cdr wsd-error)))
              (flycheck-error-new-at line 1 'error message
                                     :checker checker
                                     :buffer (current-buffer)
                                     :filename (buffer-file-name))))
          wsd-errors))

(defun wsd-flycheck-start (checker callback)
  "Start a wsd website invocation to get rendered results and errors.."

  ;; store callback for hook.
  (setq-local wsd-flycheck-checker checker)
  (setq-local wsd-flycheck-callback callback)
  ;; updates are done via wsd-mode-processing-complete-hook.
  )

(defun wsd-flycheck-update-errors (wsd-errors)
  (when (and (boundp 'wsd-flycheck-callback)
             wsd-flycheck-callback)
    (condition-case err
        ;; wsd-errors is set by wsd-mode's rendering functions to be picked up here.
        (let ((fc-errors (wsd-flycheck-parse-errors wsd-flycheck-checker wsd-errors)))
          (funcall wsd-flycheck-callback 'finished (delq nil fc-errors)))
      (error (funcall wsd-flycheck-callback 'errored (error-message-string err))))))

(add-hook 'wsd-mode-processing-complete-hook #'wsd-flycheck-update-errors)

;; tentatively load flycheck if installed.
(ignore-errors
  (require 'flycheck))

(when (and (fboundp 'flycheck-define-generic-checker)
           (boundp 'flycheck-checkers))
  (flycheck-define-generic-checker 'wsd-mode-checker
    "A syntax-checker for wsd-mode based on the errors reported from the
wsd-mode website itself."

    :modes 'wsd-mode
    :start #'wsd-flycheck-start)

  (add-to-list 'flycheck-checkers 'wsd-mode-checker))

(provide 'wsd-flycheck)
;;; wsd-flycheck.el ends here
