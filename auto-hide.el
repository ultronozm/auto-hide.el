;;; auto-hide.el --- Automatically hide function bodies  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/auto-hide.el
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functionality to automatically hide function
;; bodies in various programming languages using `tree-sitter', as a
;; light wrapper around the built-in `hideshow' package.  To use it,
;; tweak the options `auto-hide-language-config' and `auto-hide-modes'
;; to your liking, then enable `global-auto-hide-mode', e.g., by
;; putting
;;
;;   (global-auto-hide-mode)
;;
;; in your init file.

;;; Code:

(require 'treesit)
(require 'hideshow)

(defgroup auto-hide nil
  "Automatically hide function bodies using tree-sitter."
  :group 'convenience)

(defcustom auto-hide-language-config
  '((rust-ts-mode . ((function-node . "function_item")
                     (body-field . "body")))
    (c++-ts-mode . ((function-node . "function_definition")
                    (body-field . "body")))
    (c-ts-mode . ((function-node . "function_definition")
		  (body-field . "body")))
    (js-ts-mode . ((function-node . "function_declaration")
                   (body-field . "body")))
    (python-ts-mode . ((function-node . "function_definition")
                       (body-field . "body")))
    (go-ts-mode . ((function-node . ("function_declaration" "method_declaration"))
                      (body-field . "body"))))
  "Configuration for function nodes and body fields in different languages.
Each entry is a cons cell (MAJOR-MODE . CONFIG), where CONFIG is an
alist with keys `function-node' and `body-field'.

The value for `function-node' should be a string or list of strings matching the
tree-sitter node type(s) for function definitions in the given language.
The value for `body-field' should be a string matching the field name
for the function body within the function node."
  :type '(alist :key-type symbol
           :value-type (alist :key-type symbol
                         :value-type (choice string (repeat string)))))

(defvar auto-hide-modes)

(defun auto-hide--update-hooks (enable)
  "Update hooks for all modes in `auto-hide-modes'.
If ENABLE is non-nil, add hooks; otherwise, remove them."
  (dolist (mode auto-hide-modes)
    (let ((hook (intern (concat (symbol-name mode) "-hook"))))
      (if enable
          (add-hook hook #'auto-hide--maybe-enable)
        (remove-hook hook #'auto-hide--maybe-enable)))))

(defcustom auto-hide-modes
  (mapcar #'car auto-hide-language-config)
  "List of major modes where Auto Hide should be active.
Each element should be a symbol representing a major mode.  Defaults to
the major modes in `auto-hide-language-config'.  Each major mode should
have a corresponding entry in `auto-hide-language-config'."
  :type '(repeat symbol)
  :set (lambda (symbol value)
         (when (bound-and-true-p global-auto-hide-mode)
           (auto-hide--update-hooks nil))
         (set-default symbol value)
         (when (bound-and-true-p global-auto-hide-mode)
           (auto-hide--update-hooks t))))

;;; Internal functions

(defun auto-hide--body-of-node (node field-name)
  "Get region for child of NODE with FIELD-NAME."
  (when-let* ((body-node (treesit-node-child-by-field-name node field-name))
              (start (treesit-node-start body-node))
              (end (treesit-node-end body-node)))
    (cons start end)))

(defun auto-hide--bodies ()
  "Collect regions of function bodies in the current buffer.
Return list of cons cells (BEGIN . END) representing function body
regions."
  (when-let* ((lang-config (alist-get major-mode auto-hide-language-config))
              (function-node-types (alist-get 'function-node lang-config))
              (function-node-types (if (listp function-node-types)
                                       function-node-types
                                     (list function-node-types)))
              (regexp (concat "\\`"
                              (regexp-opt function-node-types)
                              "\\'"))
              (body-field (alist-get 'body-field lang-config))
              (root (treesit-buffer-root-node))
              (tree (treesit-induce-sparse-tree root regexp)))
    (let (regions)
      (cl-labels ((traverse-tree (node)
                    (when-let* ((ts-node (car node))
                                (region (auto-hide--body-of-node
                                         ts-node body-field)))
                      (push region regions))
                    (dolist (child (cdr node))
                      (traverse-tree child))))
        (traverse-tree tree))
      (nreverse regions))))

(defun auto-hide--body-at-point ()
  "Get the body for the function at point."
  (when-let* ((lang-config (alist-get major-mode
                                      auto-hide-language-config))
              (function-node-types (alist-get 'function-node lang-config))
              (function-node-types (if (listp function-node-types)
                                       function-node-types
                                     (list function-node-types)))
              (node (treesit-node-at (point)))
              (function-node (treesit-parent-until
                              node (lambda (n)
                                     (member (treesit-node-type n)
                                             function-node-types))))
              (body-field (alist-get 'body-field lang-config)))
    (auto-hide--body-of-node function-node body-field)))

(defun auto-hide--region-operation (region operation)
  "Perform OPERATION on REGION."
  (unless hs-minor-mode
    (hs-minor-mode 1))
  (save-excursion
    (goto-char (car region))
    (funcall operation)))

(defun auto-hide--hide-region (region)
  "Hide the region defined by the cons cell REGION."
  (auto-hide--region-operation
   region (lambda ()
            (unless (hs-already-hidden-p)
              (hs-hide-block)))))

(defun auto-hide--show-region (region)
  "Show the region defined by the cons cell REGION."
  (auto-hide--region-operation region #'hs-show-block))

;;; Interactive functions

(defun auto-hide-hide-all ()
  "Hide all function bodies in the current buffer."
  (interactive)
  (dolist (body (auto-hide--bodies))
    (auto-hide--hide-region body)))

(defun auto-hide-hide-at-point ()
  "Hide the body for the function at point."
  (interactive)
  (when-let* ((body (auto-hide--body-at-point)))
    (auto-hide--hide-region body)))

(defun auto-hide-show-at-point ()
  "Show the body for the function at point."
  (interactive)
  (when-let* ((body (auto-hide--body-at-point)))
    (auto-hide--show-region body)))

(defun auto-hide-toggle-at-point ()
  "Toggle the visibility of the body at point."
  (interactive)
  (when-let ((body (auto-hide--body-at-point)))
    (auto-hide--region-operation
     body (if (hs-already-hidden-p) #'hs-show-block #'hs-hide-block))))

;;; Advice following `xref-find-definitions'

(defcustom auto-hide-xref-integration t
  "Whether to automatically show bodies after `xref-find-definitions'."
  :type 'boolean)

(defun auto-hide-show-after-advice (oldfun &rest args)
  "Advice function to show body after `xref-find-definitions'.
OLDFUN and ARGS are as in the docs of `advice-add'.
If `auto-hide-xref-integration' is non-nil, show the body of the
function at point after calling `xref-find-definitions'.  If called
twice in a row, toggle the visibility of the body instead."
  (let ((result (apply oldfun args)))
    (when (and auto-hide-mode auto-hide-xref-integration)
      (if (eq last-command 'xref-find-definitions)
          (auto-hide-toggle-at-point)
        (auto-hide-show-at-point)))
    result))

;;; Minor mode

(defvar auto-hide-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c @ t") #'auto-hide-hide-all)
    (define-key map (kbd "C-c @ s") #'auto-hide-show-at-point)
    (define-key map (kbd "C-c @ d") #'auto-hide-hide-at-point)
    (define-key map (kbd "C-c @ e") #'auto-hide-toggle-at-point)
    map)
  "Keymap for auto-hide mode.")

(define-minor-mode auto-hide-mode
  "Minor mode for automatically hiding function bodies.
This minor mode serves mainly to provide keybindings for hiding and
showing function bodies.  It is automatically enabled in supported major
modes when `global-auto-hide-mode' is enabled."
  :lighter " AH"
  :keymap auto-hide-mode-map
  (if auto-hide-mode
      (progn
        (unless hs-minor-mode
          (hs-minor-mode 1))
        (auto-hide-hide-all))
    (hs-show-all)))

(defun auto-hide--maybe-enable ()
  "Enable auto-hide-mode if the current major mode is in `auto-hide-modes'."
  (when (and (bound-and-true-p global-auto-hide-mode)
             (memq major-mode auto-hide-modes))
    (auto-hide-mode 1)))

;;;###autoload
(define-minor-mode global-auto-hide-mode
  "Toggle Global Auto Hide mode.
When enabled, automatically hide function bodies in supported major modes."
  :global t
  :group 'auto-hide
  (if global-auto-hide-mode
      (progn
        (auto-hide--update-hooks t)
        (advice-add 'xref-find-definitions :around
                    #'auto-hide-show-after-advice)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (auto-hide--maybe-enable))))
    (auto-hide--update-hooks nil)
    (advice-remove 'xref-find-definitions
                   #'auto-hide-show-after-advice)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when auto-hide-mode
          (auto-hide-mode -1))))))

(provide 'auto-hide)
;;; auto-hide.el ends here
