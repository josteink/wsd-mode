(require 'ert)
(require 'wsd-core)

;; test-helpers

(defun ends-with-p (source haystack)
  (string-suffix-p)
  )

(setq file-names '(nil, "lalalala", "test.txt", "test.wsd", "test.a.wsd"))

(ert-deftest local-filename-is-never-nil ()
  (dolist (file-name file-names)
    (let* ((image-name (wsd-get-image-filename file-name)))
      (assert
       (not (equal (nil image-name)))))))

(ert-deftest local-filename-is-never-nil ()
  (dolist (file-name file-names)
    (let* ((image-name (wsd-get-image-filename file-name)))
      (assert
       (string-suffix-p wsd-format image-name)))))
