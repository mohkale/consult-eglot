;;; consult-eglot-embark-test.el --- Test code actions -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Nicolas Graves

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'ert)
(require 'consult-eglot-embark)

(ert-deftest consult-eglot-embark-code-action-preserves-payload ()
  (let* ((server 'server)
         (action '(:title "Organize imports" :kind "source.organizeImports"
                   :data (:provider "test-server")))
         (candidate
          (consult-eglot-embark--code-action-candidate server action)))
    (should (string-match-p "Organize imports" candidate))
    (should (equal (get-text-property 0 'consult--candidate candidate)
                   (cons server action)))))

(ert-deftest consult-eglot-embark-executes-preserved-action ()
  (let* ((server 'server)
         (action '(:title "Extract function" :kind "refactor.extract"))
         executed)
    (cl-letf (((symbol-function 'eglot-execute)
               (lambda (actual-server actual-action)
                 (setq executed (cons actual-server actual-action)))))
      (consult-eglot-embark-execute-code-action
       (consult-eglot-embark--code-action-candidate server action)))
    (should (equal executed (cons server action)))))

(ert-deftest consult-eglot-embark-mode-registers-code-action-support ()
  (let ((embark-default-action-overrides nil)
        (embark-exporters-alist nil)
        (embark-keymap-alist nil))
    (unwind-protect
        (progn
          (consult-eglot-embark-mode 1)
          (should (eq (alist-get 'eglot-code-action embark-keymap-alist)
                      'consult-eglot-embark-code-action-map))
          (consult-eglot-embark-mode -1)
          (should-not (assq 'eglot-code-action embark-keymap-alist)))
      (consult-eglot-embark-mode -1))))

(provide 'consult-eglot-embark-test)
;;; consult-eglot-embark-test.el ends here
