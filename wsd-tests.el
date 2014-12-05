(require 'ert)
(require 'wsd-core)

;; test-helpers

(when (not (fboundp 'string-suffix-p))
  (defun string-suffix-p (str1 str2 &optional ignore-case)
    (let ((begin2 (- (length str2) (length str1)))
	  (end2 (length str2)))
      (when (< begin2 0) (setq begin2 0))
      (eq t (compare-strings str1 nil nil
			     str2 begin2 end2
			     ignore-case)))))

(setq file-names '(nil "lalalala" "test.txt" "test.wsd" "test.a.wsd"))

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

(ert-deftest image-urls-are-detected ()
  ;; todo: craft JSON, parse JSON, verify URL
  )

(ert-deftest errors-are-detected ()
  ;; todo: craft JSON, parse JSON, verify errors
  )


;;(ert-run-tests-interactively t)

