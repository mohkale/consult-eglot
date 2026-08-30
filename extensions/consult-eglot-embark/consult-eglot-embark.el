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
(require 'eglot)
(require 'seq)
(require 'subr-x)

(defgroup consult-eglot-embark nil
  "Embark compatible actions for `consult-eglot'."
  :group 'consult-eglot)

(defvar consult-eglot-embark-code-action-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'consult-eglot-embark-execute-code-action)
    (define-key map (kbd "e") #'consult-eglot-embark-execute-code-action)
    map)
  "Embark actions for Eglot code-action candidates.")

(defun consult-eglot-embark--code-action-bounds ()
  "Return the bounds for a code-action request at point."
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (list (point) (point))))

(defun consult-eglot-embark--code-action-candidate (server action)
  "Return a completion candidate carrying SERVER and ACTION."
  (let ((title (plist-get action :title))
        (kind (plist-get action :kind)))
    (propertize (if (and kind (not (string-empty-p kind)))
                    (format "%s  [%s]" title kind)
                  title)
                'consult--candidate (cons server action))))

;;;###autoload
(defun consult-eglot-embark-code-actions (beg &optional end action-kind)
  "Select and execute an Eglot code action between BEG and END.
When ACTION-KIND is non-nil, only request actions of that LSP kind."
  (interactive
   (append (consult-eglot-embark--code-action-bounds)
           (list (and current-prefix-arg
                      (completing-read
                       "[eglot] Action kind: "
                       '("quickfix" "refactor.extract" "refactor.inline"
                         "refactor.rewrite" "source.organizeImports"))))))
  (let ((server (eglot-current-server)))
    (unless server
      (user-error "Current buffer is not managed by Eglot"))
    (let ((actions (eglot-code-actions beg end action-kind nil)))
      (unless actions
        (user-error "No code actions here"))
      (let* ((candidates
              (mapcar (lambda (action)
                        (consult-eglot-embark--code-action-candidate
                         server action))
                      actions))
             (payload
              (consult--read candidates
                             :prompt "[eglot] Code action: "
                             :category 'eglot-code-action
                             :lookup #'consult--lookup-candidate
                             :require-match t
                             :sort nil)))
        (consult-eglot-embark-execute-code-action payload)))))

(defun consult-eglot-embark-execute-code-action (candidate)
  "Execute the Eglot code action carried by CANDIDATE."
  (if-let* ((payload
             (if (stringp candidate)
                 (get-text-property 0 'consult--candidate candidate)
               candidate)))
      (eglot-execute (car payload) (cdr payload))
    (user-error "Candidate does not carry an Eglot code action")))

(defun consult-eglot-embark--action-command (server action)
  "Return an Embark command which executes ACTION using SERVER."
  (let ((command (make-symbol (plist-get action :title))))
    (fset command
          (lambda (_target)
            (interactive "sTarget: ")
            (eglot-execute server action)))
    command))

(defun consult-eglot-embark--action-key (title used base-map)
  "Choose a mnemonic key from TITLE absent from USED and BASE-MAP."
  (or (seq-find (lambda (key)
                  (and (not (member key used))
                       (not (lookup-key base-map (kbd key)))))
                (append (mapcar (lambda (word) (substring word 0 1))
                                (split-string (downcase title) "[^[:alnum:]]+" t))
                        (mapcar #'char-to-string
                                (string-to-list (downcase title)))
                        (mapcar #'char-to-string
                                (string-to-list "abcdefghijklmnopqrstuvwxyz0123456789"))))
      (user-error "Too many code actions for the Embark keymap")))

(defun consult-eglot-embark--action-map (server actions base-map)
  "Return direct Embark entries for SERVER's ACTIONS alongside BASE-MAP."
  (let ((map (make-sparse-keymap))
        used)
    (dolist (action actions)
      (let* ((title (plist-get action :title))
             (key (consult-eglot-embark--action-key title used base-map))
             (command (consult-eglot-embark--action-command server action)))
        (push key used)
        (define-key map (kbd key) command)))
    map))

(defun consult-eglot-embark-code-actions-at-point (_target)
  "Select an Eglot code action at point for an Embark TARGET."
  (apply #'consult-eglot-embark-code-actions
         (consult-eglot-embark--code-action-bounds)))

(defun consult-eglot-embark--actions-at-point-map (base-map)
  "Return applicable Eglot actions at point as an Embark map.
BASE-MAP is preserved: generated action keys never shadow its bindings."
  (when (and (bound-and-true-p eglot--managed-mode)
             (eglot-current-server)
             (not (minibufferp)))
    (pcase-let* ((server (eglot-current-server))
                 (`(,beg ,end) (consult-eglot-embark--code-action-bounds))
                 (actions (eglot-code-actions beg end nil nil)))
      (when actions
        (consult-eglot-embark--action-map server actions base-map)))))

(defun consult-eglot-embark--augment-action-keymap
    (original type cycle)
  "Add applicable Eglot actions to Embark's ordinary TYPE action map.
ORIGINAL is Embark's internal action-map constructor and CYCLE is forwarded
unchanged.  Keeping this adaptation here makes the dependency on Embark's
internal constructor explicit and easy to update if that API changes."
  (let* ((base-map (funcall original type cycle))
         (action-map
          (condition-case nil
              (consult-eglot-embark--actions-at-point-map base-map)
            (error nil))))
    (if action-map
        (make-composed-keymap action-map base-map)
      base-map)))

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
    (defvar embark-keymap-alist)

    (if consult-eglot-embark-mode
        (progn
          (setf (alist-get 'consult-eglot-symbols embark-default-action-overrides)
                #'consult-eglot-embark-goto-symbol)
          (setf (alist-get 'consult-eglot-symbols embark-exporters-alist)
                #'consult-eglot-embark-export-grep)
          (setf (alist-get 'eglot-code-action embark-keymap-alist)
                'consult-eglot-embark-code-action-map)
          (setf (alist-get 'eglot-code-action embark-default-action-overrides)
                #'consult-eglot-embark-execute-code-action)
          (advice-add 'embark--action-keymap :around
                      #'consult-eglot-embark--augment-action-keymap))
      (setq embark-default-action-overrides
            (assq-delete-all
             'eglot-code-action
             (assq-delete-all 'consult-eglot-symbols
                              embark-default-action-overrides))
            embark-exporters-alist
            (assq-delete-all 'consult-eglot-symbols embark-exporters-alist)
            embark-keymap-alist
            (assq-delete-all 'eglot-code-action embark-keymap-alist))
      (advice-remove 'embark--action-keymap
                     #'consult-eglot-embark--augment-action-keymap))))

(provide 'consult-eglot-embark)
;;; consult-eglot-embark.el ends here
