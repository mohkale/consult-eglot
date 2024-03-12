;;; consult-eglot.el --- A consulting-read interface for eglot  -*- lexical-binding: t; -*-

;; Licence: MIT
;; Keywords: tools, completion, lsp
;; Author: mohsin kaleem <mohkale@kisara.moe>
;; Maintainer: Mohsin Kaleem
;; Version: 0.2
;; Package-Requires: ((emacs "27.1") (eglot "1.7") (consult "0.31") (project "0.3.0"))
;; Homepage: https://github.com/mohkale/consult-eglot

;; Copyright (c) 2021 Mohsin Kaleem

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

;; Query workspace symbol from eglot using consult.
;;
;; This package provides a single command `consult-eglot-symbols' that uses the
;; lsp workspace/symbol procedure to get a list of symbols exposed in the current
;; workspace. This differs from the default document/symbols call, that eglot
;; exposes through imenu, in that it can present symbols from multiple open files
;; or even files not indirectly loaded by an open file but still used by your
;; project.
;;
;; This code was partially adapted from the excellent consult-lsp package.

;;; Code:

(require 'eglot)
(require 'consult)

(defgroup consult-eglot nil
  "Consulting-read for eglot."
  :prefix "consult-eglot"
  :group 'completion
  :group 'eglot
  :group 'consult)

(defcustom consult-eglot-ignore-column nil
  "When true `consult-eglot-symbols' only jumps to start of symbols line.
Otherwise `consult-eglot-symbols' will go to the exact symbol of a matched
candidate."
  :type 'boolean)

(defcustom consult-eglot-narrow
  '(;; Lowercase classes
    (?c . "Class")
    (?f . "Function")
    (?e . "Enum")
    (?i . "Interface")
    (?m . "Module")
    (?n . "Namespace")
    (?p . "Package")
    (?s . "Struct")
    (?t . "Type Parameter")
    (?v . "Variable")

    ;; Uppercase classes
    (?A . "Array")
    (?B . "Boolean")
    (?C . "Constant")
    (?E . "Enum Member")
    (?F . "Field")
    (?M . "Method")
    (?N . "Number")
    (?O . "Object")
    (?P . "Property")
    (?S . "String")

    ;; Other. Any which aren't above are taken from here
    (?o . "Other"))
  "Narrow key configuration used with `consult-eglot-symbols'.
For the format see `consult--read', for the value types see the
values in `eglot--symbol-kind-names'."
  :type '(alist :key-type character :value-type string))

(defcustom consult-eglot-show-kind-name nil
  "When true prefix completion candidates with their type."
  :type 'boolean)

(defun consult-eglot--format-file-line-match (file line &optional match)
  "Format string FILE LINE and MATCH with faces."
  (setq line (number-to-string line)
        match (concat file ":" line (when match ":") match)
        file (length file))
  (put-text-property 0 file 'face 'consult-file match)
  (put-text-property (1+ file) (+ 1 file (length line)) 'face 'consult-line-number match)
  match)

(defun consult-eglot--make-async-source (async server)
  "Search for symbols in a consult ASYNC source.
Pipe a `consult--read' compatible async-source ASYNC to search for
symbols in the workspace tied to SERVER."
  (lambda (action)
    (pcase-exhaustive action
      ((or 'setup (pred stringp))
       (let ((query (if (stringp action) action "")))
         (jsonrpc-async-request
          server :workspace/symbol
          `(:query ,query)
          :success-fn
          (lambda (resp)
            (funcall async 'flush)
            (funcall async (append resp nil)))
          :error-fn
          (eglot--lambda ((ResponseError) code message)
            (message "%s: %s" code message))
          :timeout-fn
          (lambda ()
            (message "error: request timed out")))
         (funcall async action)))
      (_ (funcall async action)))))

(defun consult-eglot--transformer (symbol-info)
  "Default transformer to produce a completion candidate from SYMBOL-INFO.
The produced candidate follows the same form as `consult--grep' however it
contains the SYMBOL-INFO as the second field instead of the file URI."
  (eglot--dbind ((SymbolInformation) name kind location)
      symbol-info
    (eglot--dbind ((Location) uri range) location
      (let* ((line (1+ (plist-get (plist-get range :start) :line)))
             (kind-name (alist-get kind eglot--symbol-kind-names))
             (uri-path (eglot--uri-to-path uri)))
        (propertize
         (concat
          (when consult-eglot-show-kind-name
            (format "%-7s " kind-name))
          name
          " "
          (string-remove-suffix ":"
                                (consult-eglot--format-file-line-match
                                 ;; If the src is relative to our project directory then use
                                 ;; the path from there, otherwise use the absolute file path.
                                 (let ((relative-uri-path (file-relative-name uri-path)))
                                   (if (string-prefix-p ".." relative-uri-path)
                                       (abbreviate-file-name uri-path)
                                     relative-uri-path))
                                 line)))
         'consult--type (or (car (rassoc kind-name consult-eglot-narrow))
                            (car (rassoc "Other" consult-eglot-narrow)))
         'consult--candidate symbol-info)))))

(defun consult-eglot--symbol-information-to-grep-params (symbol-info)
  "Extract grep parameters from SYMBOL-INFO."
  (eglot--dbind ((SymbolInformation) location) symbol-info
    (eglot--dbind ((Location) uri range) location
      (list
       (eglot--uri-to-path uri)                           ; URI
       (1+ (plist-get (plist-get range :start) :line))    ; Line number
       ; Column Number
       (or
        (and (not consult-eglot-ignore-column)
             (plist-get (plist-get range :start) :character))
        0)))))

(defun consult-eglot--state ()
  "State function for `consult-eglot-symbols' to preview candidates.
This is mostly just a copy-paste of `consult--grep-state' except it doesn't
rely on regexp matching to extract the relevent file and column fields."
  (let ((open (consult--temporary-files))
        (jump (consult--jump-state)))
    (lambda (action cand)
      (when (eq action 'exit)
        (funcall open)
        (setq open nil))
      (funcall jump
               action
               (and cand
                    (pcase-let
                        ((`(,file ,line ,col)
                          (consult-eglot--symbol-information-to-grep-params cand)))
                      (consult--marker-from-line-column
                       (funcall (or open #'find-file) file)
                       line col)))))))

(defun consult-eglot--server ()
  "Return server supporting symbol search by buffer and project."
  (cl-find-if
   (lambda (server)
     (and server
          (let (;; Bind for eglot internal server usage in
                ;; `eglot--server-capable'.
                (eglot--cached-server server))
            (eglot--server-capable :workspaceSymbolProvider))))
   (cons (eglot-current-server)
         (and-let* ((project (project-current))
                    (servers (gethash project eglot--servers-by-project)))
           servers))))

(defvar consult-eglot--history nil)

;;;###autoload
(defun consult-eglot-symbols ()
  "Interactively select a symbol from the current workspace."
  (interactive)
  ;; Set `default-directory' here so we can show file names
  ;; relative to the project root.
  (if-let* ((server (consult-eglot--server))
            (default-directory (or (project-root (eglot--project server))
                                   default-directory)))
      (progn
        (consult--read
         (thread-first
           (consult--async-sink)
           (consult--async-refresh-immediate)
           (consult--async-map #'consult-eglot--transformer)
           (consult-eglot--make-async-source server)
           (consult--async-throttle)
           (consult--async-split))
         :require-match t
         :prompt "LSP Symbols: "
         :initial (consult--async-split-initial nil)
         :history '(:input consult-eglot--history)
         :category 'consult-lsp-symbols
         :lookup #'consult--lookup-candidate
         :group (consult--type-group consult-eglot-narrow)
         :narrow (consult--type-narrow consult-eglot-narrow)
         :state (consult-eglot--state))
        (run-hooks 'consult-after-jump-hook))
    (user-error "No server supporting symbol search")))

;;; `consult-eglot-embark'.
;; TODO: Extract into separate external package.

(declare-function embark-consult-export-grep "embark-consult")
(declare-function embark-consult-goto-grep "embark-consult")

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
                      (eglot--dbind ((SymbolInformation) name kind) symbol-info
                        (concat
                         (when consult-eglot-show-kind-name
                           (when-let ((kind-name (alist-get kind eglot--symbol-kind-names)))
                             kind-name))
                         name)))
              lines)))

    (setq lines (nreverse lines))
    (embark-consult-export-grep lines)))

(define-obsolete-function-alias 'consult-eglot-export-grep 'consult-eglot-embark-export-grep "0.2")

(defun consult-eglot-embark-goto-symbol (candidate)
  "Jump to a `consult-eglot' CANDIDATE through `embark-act'."
  (if-let ((symbol-info (get-text-property 0 'consult--candidate candidate)))
      (cl-destructuring-bind (file line column)
          (consult-eglot--symbol-information-to-grep-params symbol-info)
        (consult--jump
         (consult--marker-from-line-column
          (find-file-noselect file) line column))
        (pulse-momentary-highlight-one-line (point)))))

(with-eval-after-load 'embark-consult
  (defvar embark-default-action-overrides)
  (defvar embark-exporters-alist)

  (setf (alist-get 'consult-lsp-symbols embark-default-action-overrides)
        #'consult-eglot-embark-goto-symbol)
  (setf (alist-get 'consult-lsp-symbols embark-exporters-alist)
        #'consult-eglot-embark-export-grep))

(provide 'consult-eglot)
;;; consult-eglot.el ends here
