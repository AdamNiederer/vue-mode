;;; vue-mode.el --- Major mode for vue component based on mmm-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 codefalling, Adam Niederer

;; Author: codefalling <code.falling@gmail.com>
;; Keywords: languages

;; Version: 0.4.0
;; Package-Requires: ((mmm-mode "0.5.5") (vue-html-mode "0.2") (ssass-mode "0.2") (edit-indirect "0.1.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'mmm-mode)
(require 'mmm-cmds)
(require 'vue-html-mode)
(require 'ssass-mode)
(require 'edit-indirect)

(defgroup vue nil
  "Group for vue-mode"
  :prefix "vue-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/CodeFalling/vue-mode")
  :link '(emacs-commentary-link :tag "Commentary" "vue-mode"))

(defcustom vue-modes
  '((:type template :name nil :mode vue-html-mode)
    (:type template :name html :mode vue-html-mode)
    (:type template :name jade :mode jade-mode)
    (:type template :name pug :mode pug-mode)
    (:type template :name slm :mode slim-mode)
    (:type template :name slim :mode slim-mode)
    (:type script :name nil :mode js-mode)
    (:type script :name js :mode js-mode)
    (:type script :name es6 :mode js-mode)
    (:type script :name babel :mode js-mode)
    (:type script :name coffee :mode coffee-mode)
    (:type script :name ts :mode typescript-mode)
    (:type script :name typescript :mode typescript-mode)
    (:type script :name tsx :mode typescript-tsx-mode)
    (:type style :name nil :mode css-mode)
    (:type style :name css :mode css-mode)
    (:type style :name stylus :mode stylus-mode)
    (:type style :name less :mode less-css-mode)
    (:type style :name postcss :mode css-mode)
    (:type style :name scss :mode css-mode)
    (:type style :name sass :mode ssass-mode)
    (:type i18n :name nil :mode json-mode)
    (:type i18n :name json :mode json-mode)
    (:type i18n :name yaml :mode yaml-mode))
  "A list of vue component languages.

A component language consists of a langauge type, name, and
corresponding submode.

The language type is the tag which this languge is valid under -
one of template, script, or style.

The language name is the value of the lang=\"\" element in the
opening tag of the language section. If there is no lang=\"\"
element, the the language name is nil.

The submode to activate is the major mode which should be used
for all text in the language section.

For example, somebody wishing to activate pug-mode in blocks like
<template lang=\"pug\"> </template> would add an entry with the
language type to template, the language name to pug, and the
submode to pug-mode."
  :type '(repeat (list (const :format "Language Type: " :type)
                       (choice (const template)
                               (const script)
                               (const style)
                               (symbol :tag "Custom element type"))
                       (const :format "" :name)
                       (symbol :format "Language Name: %v")

                       (const :format "" :mode)
                       (symbol :format "Submode to activate: %v")))
  :group 'vue)

(defcustom vue-dedicated-modes nil
  "A list of modes to override in dedicated buffers.

For example, if you would like your javascript to display with
`js-mode' in the root window and `js2-mode' in a dedicated buffer,
add an entry with a root mode of `js-mode' and dedicated mode of `js2-mode'"
  :type '(plist :key-type (symbol :format "Root mode: %v")
                :value-type (symbol :format "Dedicated mode: %v"))
  :group 'vue)

(defvar vue-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-l") 'vue-mode-reparse)
    (define-key map (kbd "C-c C-k") 'vue-mode-edit-indirect-at-point)
    map)
  "Keymap for `vue-mode'.")

(defvar vue-initialized nil
  "If false, `vue-mode' still needs to prepare `mmm-mode' before being activated.")

(defconst vue--not-lang-key
  (concat
   "\\(?:"
   "\\w*[^l]\\w\\w\\w=" ; Anything not starting with a lowercase l, or
   "\\|"
   "\\w*[^a]\\w\\w=" ; Anything without a in the second position, or
   "\\|"
   "\\w*[^n]\\w=" ; Anything without n in the third position, or
   "\\|"
   "\\w*[^g]=" ; Anything not ending with g, or
   "\\|"
   "g=" ; Just g, or
   "\\|"
   "\\w\\{5,\\}=" ; A 5+-character word
   "\\)")
  "Matches anything but 'lang'. See `vue--front-tag-regex'.")

(defconst vue--front-tag-lang-regex
  (concat "<%s"                               ; The tag name
          "\\(?:"                             ; Zero of more of...
          "\\(?:\\s-+\\w+=[\"'].*?[\"']\\)"   ; Any optional key-value pairs like type="foo/bar"
          "\\|\\(?:\\s-+scoped\\)"            ; The optional "scoped" attribute
          "\\|\\(?:\\s-+module\\)"            ; The optional "module" attribute
          "\\)*"
          "\\(?:\\s-+lang=[\"']%s[\"']\\)"    ; The language specifier (required)
          "\\(?:"                             ; Zero of more of...
          "\\(?:\\s-+\\w+=[\"'].*?[\"']\\)"   ; Any optional key-value pairs like type="foo/bar"
          "\\|\\(?:\\s-+scoped\\)"            ; The optional "scoped" attribute
          "\\|\\(?:\\s-+module\\)"            ; The optional "module" attribute
          "\\)*"
          " *>\n")                            ; The end of the tag
  "A regular expression for the starting tags of template areas with languages.
To be formatted with the tag name, and the language.")

(defconst vue--front-tag-regex
  (concat "<%s"                        ; The tag name
          "\\(?:"                      ; Zero of more of...
          "\\(?:\\s-+" vue--not-lang-key "[\"'][^\"']*?[\"']\\)" ; Any optional key-value pairs like type="foo/bar".
          ;; ^ Disallow "lang" in k/v pairs to avoid matching regions with non-default languages
          "\\|\\(?:\\s-+scoped\\)"      ; The optional "scoped" attribute
          "\\|\\(?:\\s-+module\\)"      ; The optional "module" attribute
          "\\)*"
          "\\s-*>\n")                     ; The end of the tag
  "A regular expression for the starting tags of template areas.
To be formatted with the tag name.")

(defun vue--setup-mmm ()
  "Add syntax highlighting regions to mmm-mode, according to `vue-modes'."
  (dolist (mode-binding vue-modes)
    (let* ((type (plist-get mode-binding :type))
           (name (plist-get mode-binding :name))
           (mode (plist-get mode-binding :mode))
           (class (make-symbol (format "vue-%s" name)))
           (front (if name (format vue--front-tag-lang-regex type name)
                    (format vue--front-tag-regex type)))
           (back (format "^</%s *>" type)))
      (mmm-add-classes `((,class :submode ,mode :front ,front :back ,back)))
      (mmm-add-mode-ext-class 'vue-mode nil class)))
  (setq vue-initialized t))

(defun vue-mode-reparse ()
  "Reparse the buffer, reapplying all major modes."
  (interactive)
  (mmm-parse-buffer))

(defun vue-mmm-indent-line-narrowed ()
  "An indent function which works on some modes where `mmm-indent-line' doesn't.
Works like `mmm-indent-line', but narrows the buffer before indenting to
appease modes which rely on constructs like (point-min) to indent."
  (interactive)
  (mmm-update-submode-region)
  (if mmm-current-overlay
      (save-restriction
        (mmm-narrow-to-submode-region)
        (funcall (get
                  (if (and mmm-current-overlay
                           (>= (overlay-end mmm-current-overlay) (point)))
                      mmm-current-submode
                    mmm-primary-mode)
                  'mmm-indent-line-function)))
    (mmm-indent-line)))

(defun vue-mmm-indent-region (start end)
  "Indent the region from START to END according to `mmm-indent-line-function'.
Then, indent all submodes overlapping the region according to
`mmm-indent-line-function'"
  (interactive)
  (save-excursion
    (while (< (point) end)
      (indent-according-to-mode)
      (forward-line 1))
    ;; Indent each submode in the region seperately
    (dolist (submode (mmm-overlays-overlapping start end))
      (goto-char (overlay-start submode))
      (while (< (point) (min end (overlay-end submode)))
        (indent-according-to-mode)
        (forward-line 1)))))

(defun vue-mode-edit-indirect-at-point ()
  "Open the section of the template at point with `edit-indirect-mode'."
  (interactive)
  (if mmm-current-overlay
      (let ((indirect-mode (or (plist-get vue-dedicated-modes mmm-current-submode)
                               mmm-current-submode)))
        (setq-local edit-indirect-after-creation-hook (list (lambda () (funcall indirect-mode))))
        (edit-indirect-region (overlay-start mmm-current-overlay)
                              (1- (overlay-end mmm-current-overlay)) ;; Work around edit-indirect-mode bug
                              (current-buffer)))
    (user-error "Not in a template subsection")))

;;;###autoload
(defun vue-mode-edit-all-indirect (&optional keep-windows)
  "Open all subsections with `edit-indirect-mode' in seperate windows.
If KEEP-WINDOWS is set, do not delete other windows and keep the root window
open."
  (interactive "P")
  (when (not keep-windows)
    (delete-other-windows))
  (save-selected-window
    (dolist (ol (mmm-overlays-contained-in (point-min) (point-max)))
      (let* ((window (split-window-below))
             (mode (or (plist-get vue-dedicated-modes (overlay-get ol 'mmm-mode))
                       (overlay-get ol 'mmm-mode)))
             (buffer (edit-indirect-region (overlay-start ol) (overlay-end ol))))
        (maximize-window)
        (with-current-buffer buffer
          (funcall mode))
        (set-window-buffer window buffer)))
    (balance-windows))
  (when (not keep-windows)
    (delete-window)
    (balance-windows)))

;;;###autoload
(define-derived-mode vue-mode html-mode "vue"
  (when (not vue-initialized)
    (vue--setup-mmm))
  (setq-local mmm-indent-line-function #'vue-mmm-indent-line-narrowed)
  (setq-local indent-region-function #'vue-mmm-indent-region))

;;;###autoload
(setq mmm-global-mode 'maybe)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

(provide 'vue-mode)
;;; vue-mode.el ends here
