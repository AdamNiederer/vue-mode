;;; vue-mode.el --- Major mode for vue component based on web-mode and mmm-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  codefalling

;; Author: codefalling <code.falling@gmail.com>
;; Keywords: languages

;; Version: 0.1
;; Package-Requires: ((mmm-mode "0.5.4"))

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

;;;###autoload
(define-derived-mode vue-mode css-mode "vue")
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

(require 'mmm-mode)
(dolist (langsets '(("template" . ((default . web-mode)
                                   (html . web-mode)
                                   (jade . jade-mode)
                                   ))
                    ("script" . ((default . js-mode)
                                 (js . js-mode)
                                 (es6    . js-mode)
                                 (coffee . coffee-mode)
                                 ))
                    ("style"  . ((default . css-mode)
                                 (css    . css-mode  )
                                 (stylus . stylus-mode)
                                 (less   . less-css-mode)
                                 (scss   . scss-mode)
                                 (sass   . sass-mode)
                                 ))))
  (let ((tag (car langsets)))
    (dolist (pair (cdr langsets))
      (let* ((lang       (car pair))
             (submode    (cdr pair))
             (class-name (make-symbol (format "vueify-%s-%s" tag lang)))
             (front      (if (eq lang 'default)
                             (format "<%s *\\(scoped\\)?>" tag)
                           (format "<%s *lang=\"%s\" *\\(scoped\\)?>" tag lang)))
             (back       (format "</%s>" tag)))
        (mmm-add-classes
         `((,class-name
            :submode ,submode
            :front ,front
            :back ,back)))
        (mmm-add-mode-ext-class 'vue-mode nil class-name)))))

(defun vue-mode-reparse ()
  "Reparse for `lang' changed"
  (interactive)
  (mmm-parse-buffer)
  )

(add-hook 'vue-mode-hook 'mmm-mode-on)

(provide 'vue-mode)
;;; vue-mode.el ends here
