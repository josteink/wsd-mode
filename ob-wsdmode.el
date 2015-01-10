;;; ob-wsdmode.el --- Org-babel integration for wsd-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Jostein Kjønigsen

;; Author: Jostein Kjønigsen <jostein@office.lan>
;; Keywords: languages

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

;;

;;; Code:

(require 'ob)

(defvar org-babel-default-header-args:wsd
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluating a wsd-mode source block.")

(defun org-babel-expand-body:wsd (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body." body)

(defun org-babel-execute:wsd (body params)
  "Execute a block of wsd-mode code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
         (out-file (cdr (assoc :file params)))
         (img-file (if out-file
                       out-file
                     (wsd-get-temp-filename)))
         (json      (wsd-get-json body))
         (image-url (wsd-get-image-url json))
         (errors    (wsd-get-errors json)))

    (when (not (= 0 (length errors)))
      (error "Errors reported from websequence-diagrams service!"))
    
    (url-copy-file image-url img-file t)
    img-file))

(provide 'ob-wsdmode)
;;; ob-wsdmode.el ends here
