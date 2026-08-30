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

(ert-deftest consult-eglot-embark-augments-symbol-map-with-code-actions ()
  (with-temp-buffer
    (insert "example")
    (goto-char 4)
    (let ((eglot--managed-mode t)
          (base-map (let ((map (make-sparse-keymap)))
                      (define-key map (kbd "x") #'ignore)
                      map)))
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda () 'server))
                ((symbol-function 'eglot-code-actions)
                 (lambda (&rest _)
                   '((:title "Format buffer" :kind "source.format")
                     (:title "Rename symbol" :kind "refactor.rename")))))
        (let ((map (consult-eglot-embark--augment-action-keymap
                    (lambda (_type _cycle) base-map) 'symbol nil)))
          (should (eq (lookup-key map (kbd "x")) #'ignore))
          (should (commandp (lookup-key map (kbd "f"))))
          (should (commandp (lookup-key map (kbd "r")))))))))

(ert-deftest consult-eglot-embark-generated-action-executes-payload ()
  (let* ((server 'server)
         (action '(:title "Apply fix" :kind "quickfix" :data (:id 7)))
         (map (consult-eglot-embark--action-map
               server (list action) (make-sparse-keymap)))
         executed)
    (cl-letf (((symbol-function 'eglot-execute)
               (lambda (actual-server actual-action)
                 (setq executed (cons actual-server actual-action)))))
      (funcall (lookup-key map (kbd "a")) nil))
    (should (equal executed (cons server action)))))

(ert-deftest consult-eglot-embark-generated-actions-avoid-existing-keys ()
  (let ((base-map (let ((map (make-sparse-keymap)))
                    (define-key map (kbd "f") #'ignore)
                    map)))
    (should (equal
             "b"
             (consult-eglot-embark--action-key
              "Format buffer" nil base-map)))))

(ert-deftest consult-eglot-embark-uses-active-region-for-augmented-actions ()
  (with-temp-buffer
    (insert "some text")
    (goto-char 1)
    (push-mark 5 t t)
    (let ((eglot--managed-mode t)
          (transient-mark-mode t)
          requested-bounds)
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda () 'server))
                ((symbol-function 'eglot-code-actions)
                 (lambda (beg end &rest _)
                   (setq requested-bounds (list beg end))
                   nil)))
        (consult-eglot-embark--augment-action-keymap
         (lambda (_type _cycle) (make-sparse-keymap)) 'region nil)
        (should (equal requested-bounds '(1 5)))))))

(ert-deftest consult-eglot-embark-mode-registers-code-action-support ()
  (let ((embark-default-action-overrides nil)
        (embark-exporters-alist nil)
        (embark-keymap-alist nil))
    (unwind-protect
        (progn
          (consult-eglot-embark-mode 1)
          (should (eq (alist-get 'eglot-code-action embark-keymap-alist)
                      'consult-eglot-embark-code-action-map))
          (should (advice-member-p
                   #'consult-eglot-embark--augment-action-keymap
                   'embark--action-keymap))
          (consult-eglot-embark-mode -1)
          (should-not (assq 'eglot-code-action embark-keymap-alist))
          (should-not (advice-member-p
                       #'consult-eglot-embark--augment-action-keymap
                       'embark--action-keymap)))
      (consult-eglot-embark-mode -1))))

(provide 'consult-eglot-embark-test)
;;; consult-eglot-embark-test.el ends here
