;;; wsd-tests --- This file contains automated tests for wsd-mode.el

;;; Commentary:
;; Run tests using (ert-run-tests-interactively t).

;;; Code:

(require 'ert)
(require 'wsd-core)
(require 'wsd-mode)
(require 'ob-wsdmode)
(require 'wsd-flycheck)


;; test-helpers

(when (not (fboundp 'string-suffix-p))
  (defun string-suffix-p (str1 str2 &optional ignore-case)
    (let ((begin2 (- (length str2) (length str1)))
          (end2 (length str2)))
      (when (< begin2 0) (setq begin2 0))
      (eq t (compare-strings str1 nil nil
                             str2 begin2 end2
                             ignore-case)))))

(defun wsd-test-indent-all ()
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(setq file-names '("lalalala" "test.txt" "test.wsd" "test.a.wsd"))

(ert-deftest local-filename-is-never-nil ()
  (dolist (file-name file-names)
    (let* ((image-name (wsd-get-image-filename file-name)))
      (should
       (not (equal nil image-name))))))

(ert-deftest local-filename-always-contains-extension ()
  (dolist (file-name file-names)
    (let* ((image-name (wsd-get-image-filename file-name)))
      (should
       (string-suffix-p wsd-format image-name)))))

(setq wsd-test-img "?png=mscb9q07p")
(setq wsd-test-json-img `((actualWidth . 529.516) (naturalWidth . 529.516) (errors . []) (img . ,wsd-test-img)))

(setq wsd-test-error "Line 20: Deactivate: User was not activated.")
(setq wsd-test-json-error `((actualWidth . 531.016) (naturalWidth . 531.016) (errors . [,wsd-test-error]) (img . "?png=mscpFx2np")))

(ert-deftest image-urls-are-detected ()
  (let* ((image-url (wsd-get-image-url wsd-test-json-img)))
    (should
     (string-suffix-p wsd-test-img image-url))))

(ert-deftest errors-are-detected ()
  (let* ((errors (wsd-get-errors wsd-test-json-error)))
    (should
     (equal (list wsd-test-error) errors))))

(ert-deftest indentation-rules-behaves-like-expected ()
  (dolist (test-run '((("alt") . 4)
                      (("alt" "end") . 0)
                      (("alt" "end" "alt") . 4)
                      (("alt" "end" "end") . 0)
                      (("alt" "end" "end" "alt") . 4)
                      (("alt" "opt" "end" "opt") . 8)
                      (("alt" "state over foo: " "end") . 0)
                      (("alt" "state over bar") . 8)
                      (("alt" "note over foo: " "end") . 0)
                      (("alt" "note over bar") . 8)
                      (("alt" "state" "end state") . 4)
                      (("alt" "state" "end" "end") . 0)))
    (let* ((test-data       (car test-run))
           (expected-result (cdr test-run))
           (actual-result   (wsd-get-indentation-from-lines test-data)))
      (should
       (= expected-result actual-result)))))

(ert-deftest activating-mode-doesnt-cause-failure ()
  (with-temp-buffer
    (wsd-mode)
    (should
     (equal 'wsd-mode major-mode))))

(ert-deftest errors-are-interpeted ()
  (let* ((error-list  (wsd-get-errors wsd-test-json-error))
         (error-items (wsd-get-error-lines error-list))
         (first-error (car error-items)))
    (should (= 1 (length error-items)))

    (should (= 20 (car first-error)))
    (should (equal "Deactivate: User was not activated." (cdr first-error)))))

(ert-deftest line-starters-are-only-fontified-when-actually-starting-a-line ()
  (let* ((buffer (find-file-read-only "test-files/fontification-tests.wsd")))
    ;; double-ensure mode is active
    (wsd-mode)
    ;; required when running in unit-test runner.
    (font-lock-fontify-buffer)

    (dolist (item '("title" "participant" "deactivate" "activate"
                    "alt" "else" "opt" "loop" "end" "note"
                    "end state" "end note" "end ref"
                    "state" "ref" "parallel"))
      (message (concat "Testing fontification of '" item "'."))
      (let* ((buffer1)
             (buffer2))
        (goto-char (point-min))

        ;; get reference string
        (search-forward item)
        (backward-char) ;; we're AFTER the fontified area
        (setq buffer1 (thing-at-point 'word))
        (should (eql
                 'font-lock-keyword-face
                 (face-at-point)))

        ;; get verification string
        (search-forward item)
        (backward-char) ;; we're AFTER the fontified area
        (setq buffer2 (thing-at-point 'word))

        ;; verify string equality
        (should
         (equal buffer1 buffer2))

        ;; verify different fontification
        (should (not (eql
                      'font-lock-keyword-face
                      (face-at-point))))))

    (kill-buffer buffer)))

(ert-deftest positional-actors-are-fontified-as-variables ()
  (let* ((buffer (find-file-read-only "test-files/fontification-actors.wsd")))
    ;; double-ensure mode is active
    (wsd-mode)
    ;; required when running in unit-test runner.
    (font-lock-fontify-buffer)

    (dolist (item '("ref over" "note right of" "state left of"))
      (let* ((buffer1)
             (buffer2))
        (goto-char (point-min))

        ;; get reference string
        (search-forward item)
        (backward-char) ;; we're AFTER the fontified area
        (should (eql
                 'font-lock-keyword-face
                 (face-at-point)))

        ;; get verification string
        (search-forward "moo")
        (backward-char) ;; we're AFTER the fontified area

        ;; verify different fontification
        (should (eql
                 'font-lock-variable-name-face
                 (face-at-point)))))))

(ert-deftest statement-actors-are-fontified-as-variables ()
  (let* ((buffer (find-file-read-only "test-files/fontification-double-colon.wsd")))
    ;; double-ensure mode is active
    (wsd-mode)
    ;; required when running in unit-test runner.
    (font-lock-fontify-buffer)

    (while (search-forward "pc" nil t)
      (backward-char 1)
      (should (eql
               'font-lock-variable-name-face
               (face-at-point))))))

(ert-deftest indentation-reference-document-is-reflowed-correctly ()
  (let* ((buffer (find-file "test-files/indentation-tests.wsd")))
    ;; double ensure mode is active
    (wsd-mode)

    (setq wsd-test-reference (buffer-substring-no-properties (point-min) (point-max)))
    (wsd-test-indent-all)
    (setq wsd-test-reflowed  (buffer-substring-no-properties (point-min) (point-max)))

    (should (equal wsd-test-reference wsd-test-reflowed))

    (kill-buffer buffer)))

(ert-deftest document-contents-are-encoded-when-shown-online ()
  (let* ((test-chars "\"#¤&/()+&<>")
         (url-base   (wsd-get-diagram-online-url ""))
         (url        (wsd-get-diagram-online-url test-chars))
         (url-query  (substring url (length url-base))))
    (dolist (char (string-to-list test-chars))
      ;; (message (concat "Testing " (string char)))
      (should (not (string-match-p
                    (regexp-quote (string char))
                    url-query))))))

;; (ert-deftest flycheck-errors-are-returned ()
;;   (let* ((result (wsd-flycheck-parse-errors
;;                   'wsd-mode-checker
;;                   '((33 . "Error: end without prior group.") (34 . "Error: end without prior group.")))))
;;     (should (not (equal nil result)))))


;;(ert-run-tests-interactively t)

(provide 'wsd-tests)
;;; wsd-tests.el ends here
