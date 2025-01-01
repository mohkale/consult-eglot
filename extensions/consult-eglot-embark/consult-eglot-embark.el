;;; consult-eglot-embark.el --- Embark integration for `consult-eglot'  -*- lexical-binding: t; -*-

;; Licence: MIT
;; Keywords: tools, completion, lsp
;; Author: mohsin kaleem <mohkale@kisara.moe>
;; Maintainer: Mohsin Kaleem
;; Version: 0.4
;; Package-Requires: ((emacs "27.1") (consult-eglot "0.3") (embark-consult "1.0"))
;; Homepage: https://github.com/mohkale/consult-eglot

;; Copyright (c) 2024 Mohsin Kaleem

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

;;; Commentary:

;; Embark goto symbol and export suppport with `consult-eglot'.

;;; Code:
(require 'consult-eglot)
(require 'embark-consult)

(defgroup consult-eglot-embark nil
  "Emabark compatible actions for `consult-eglot'."
  :group 'consult-eglot)

;;;###autoload
(defun consult-eglot-embark-export-grep (candidates)
  "Exporter for a `consult-eglot' session to a grep buffer.
CANDIDATES is the collection of completion candidates to include int he grep
buffer."
  (setq candidates (mapcar (apply-partially
                            #'get-text-property
                            0 'consult--candidate)
                           candidates))
  (let ((lines nil))
    (dolist (symbol-info candidates)
      (cl-destructuring-bind (file line _)
          (consult-eglot--symbol-information-to-grep-params symbol-info)
        (push (concat file
                      ":" (number-to-string line)
                      ;; ":" (number-to-string column)
                      ": "
                      (eglot--dbind ((SymbolInformation) name) symbol-info
                        name))
              lines)))

    (setq lines (nreverse lines))
    (embark-consult-export-grep lines)))

;;;###autoload
(defun consult-eglot-embark-goto-symbol (candidate)
  "Jump to a `consult-eglot' CANDIDATE through `embark-act'."
  (when-let* ((symbol-info (get-text-property 0 'consult--candidate candidate)))
    (cl-destructuring-bind (file line column)
        (consult-eglot--symbol-information-to-grep-params symbol-info)
      (consult--jump
       (consult--marker-from-line-column
        (find-file-noselect file) line column))
      (pulse-momentary-highlight-one-line (point)))))

;;;###autoload
(define-minor-mode consult-eglot-embark-mode
  "Setup `consult-eglot' actions for embark."
  :global t
  :group 'consult-eglot-embark
  (progn
    (defvar embark-default-action-overrides)
    (defvar embark-exporters-alist)

    (if consult-eglot-embark-mode
        (progn
          (setf (alist-get 'consult-eglot-symbols embark-default-action-overrides)
                #'consult-eglot-embark-goto-symbol)
          (setf (alist-get 'consult-eglot-symbols embark-exporters-alist)
                #'consult-eglot-embark-export-grep))
      (setq embark-default-action-overrides
            (assq-delete-all 'consult-eglot-symbols embark-default-action-overrides)
            embark-exporters-alist
            (assq-delete-all 'consult-eglot-symbols embark-exporters-alist)))))

(provide 'consult-eglot-embark)
;;; consult-eglot-embark.el ends here
