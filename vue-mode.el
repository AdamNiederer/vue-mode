;;; vue-mode.el --- Major mode for vue component based on web-mode and mmm-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 codefalling, Adam Niederer

;; Author: codefalling <code.falling@gmail.com>
;; Keywords: languages

;; Version: 0.2
;; Package-Requires: ((mmm-mode "0.5.4") (vue-html-mode "0.1") (ssass-mode "0.1"))

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
(require 'vue-html-mode)
(require 'ssass-mode)

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
    (:type script :name nil :mode js-mode)
    (:type script :name js :mode js-mode)
    (:type script :name es6 :mode js-mode)
    (:type script :name babel :mode js-mode)
    (:type script :name coffee :mode coffee-mode)
    (:type script :name typescript :mode typescript-mode)
    (:type style :name nil :mode css-mode)
    (:type style :name css :mode css-mode)
    (:type style :name stylus :mode stylus-mode)
    (:type style :name less :mode less-css-mode)
    (:type style :name scss :mode scss-mode)
    (:type style :name sass :mode ssass-mode))
  "A list of vue component languages, their type, and their corresponding major modes"
  :type '(list (plist :type 'symbol :name 'symbol :mode 'function))
  :group 'vue)

(defvar vue-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-l") 'vue-mode-reparse)
    map)
  "Keymap for vue-mode")

(defvar vue-initialized nil
  "If false, vue-mode still needs to prepare mmm-mode before being activated.")

(defun vue--setup-mmm ()
  (dolist (mode-binding vue-modes)
    (let* ((type (plist-get mode-binding :type))
           (name (plist-get mode-binding :name))
           (mode (plist-get mode-binding :mode))
           (class (make-symbol (format "vue-%s" name)))
           (front (format "<%s\\( +lang=\"%s\"\\)?\\( +scoped\\)? *>\n" type name))
           (back (format "^</%s *>" type)))
      (mmm-add-classes `((,class :submode ,mode :front ,front :back ,back)))
      (mmm-add-mode-ext-class 'vue-mode nil class)))
  (setq vue-initialized t))

(defun vue-mode-reparse ()
  "Reparse the buffer, reapplying all major modes"
  (interactive)
  (mmm-parse-buffer))

;;;###autoload
(define-derived-mode vue-mode html-mode "vue"
  (when (not vue-initialized)
    (vue--setup-mmm)))

;;;###autoload
(setq mmm-global-mode 'maybe)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

(provide 'vue-mode)
;;; vue-mode.el ends here
