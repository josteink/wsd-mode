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

(setq wsd-test-json '((boxes . [((rect . [48.0156 272 132 18]) (line . 13)) ((rect . [48.0156 244 132 18]) (line . 12)) ((rect . [59.0156 198 85 36]) (line . 10)) ((rect . [197.516 152 311 36]) (line . 8)) ((rect . [48.0156 124 132 18]) (line . 6)) ((rect . [48.0156 96 132 18]) (line . 5)) ((rect . [10 10 509.516 22]) (line . 0)) ((rect . [134.797 300 101.438 44]) (line . 3)) ((rect . [134.797 42 101.438 44]) (line . 3)) ((rect . [13.5781 300 66.875 44]) (line . 2)) ((rect . [13.5781 42 66.875 44]) (line . 2))]) (actualWidth . 529.516) (naturalWidth . 529.516) (errors . []) (img . "?png=mscb9q07p")))

(ert-deftest image-urls-are-detected ()
  ;; todo: craft JSON, parse JSON, verify URL
  
  )

(ert-deftest errors-are-detected ()
  ;; todo: craft JSON, parse JSON, verify errors
  )


;;(ert-run-tests-interactively t)

