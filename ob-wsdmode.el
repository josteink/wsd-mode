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
(require 'wsd-core)

(defvar org-babel-default-header-args:wsd
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluating a wsd-mode source block.")

(defun org-babel-expand-body:wsd (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body." body)

(defun org-babel-execute:wsd (body params)
  "Execute a block of wsd-mode code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((out-file (cdr (assoc :file params)))
         (img-file (if out-file
                       out-file
                     (wsd-get-temp-filename)))
         ;; override preference based on whatever filename says.
         (wsd-format (file-name-extension img-file))
         (json      (wsd-get-json body))
         (image-url (wsd-get-image-url json))
         (errors    (wsd-get-errors json)))

    (when (not (= 0 (length errors)))
      (error "Errors reported from websequence-diagrams service!"))

    (url-copy-file image-url img-file t)

    ;; org-babel :file is special cased. if provided by the document
    ;; any non-nil data we return will be written to that file.
    ;;
    ;; When not provided org will interpret our-return data as a file-name to
    ;; use.
    ;;
    ;; Therefore only return file-name when we have generated it. Otherwise
    ;, return nil.
    (if out-file
        nil
      img-file)))

(provide 'ob-wsdmode)
;;; ob-wsdmode.el ends here
