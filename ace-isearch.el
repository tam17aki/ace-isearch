;;; ace-isearch.el --- A seamless bridge between isearch, ace-jump-mode, avy and helm-swoop -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2014, 2015 by Akira TAMAMORI

;; Author: Akira Tamamori
;; URL: https://github.com/tam17aki/ace-isearch
;; Version: 0.1.4
;; Created: Sep 25 2014
;; Package-Requires: ((ace-jump-mode "2.0") (avy "0.3") (helm-swoop "1.4") (emacs "24"))

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `ace-isearch.el' provides a minor mode which combines `isearch',
;; `ace-jump-mode', `avy', and `helm-swoop'.
;;
;; The "default" behavior can be summrized as:
;;
;; L = 1     : `ace-jump-mode' or `avy'
;; 1 < L < 6 : `isearch'
;; L >= 6    : `helm-swoop'
;;
;; where L is the input string length during `isearch'.  When L is 1, after a
;; few seconds specified by `ace-isearch-jump-delay', `ace-jump-mode' or `avy'
;; will be invoked. Of course you can customize the above behaviour.

;;; Installation:
;;
;; To use this package, add following code to your init file.
;;
;;   (require 'ace-isearch)
;;   (global-ace-isearch-mode +1)

;;; Code:

(require 'helm-swoop)
(require 'ace-jump-mode)
(require 'avy)

(defgroup ace-isearch nil
  "Group of ace-isearch."
  :group 'convenience
  :prefix "ace-isearch-")

(defcustom ace-isearch-lighter " AceI"
  "Lighter of ace-isearch-mode."
  :type 'string
  :group 'ace-isearch)

(defcustom ace-isearch-jump-delay 0.3
  "Delay seconds for invoking `ace-jump-mode' or `avy' during isearch."
  :type 'number
  :group 'ace-isearch)

(defcustom ace-isearch-func-delay 0.0
  "Delay seconds for invoking `ace-isearch-function-from-isearch' during isearch."
  :type 'number
  :group 'ace-isearch)

(defcustom ace-isearch-input-length 6
  "Length of input string required for invoking `ace-isearch-function-from-isearch'
during isearch."
  :type 'integer
  :group 'ace-isearch)

(defcustom ace-isearch-function 'ace-jump-word-mode
  "Function name in invoking ace-jump-mode or avy."
  :type '(choice (const :tag "Use ace-jump-word-mode." ace-jump-word-mode)
                 (const :tag "Use ace-jump-char-mode." ace-jump-char-mode)
                 (const :tag "Use avy-goto-word-1." avy-goto-word-1)
                 (const :tag "Use avy-goto-word-1." avy-goto-subword-1)
                 (const :tag "Use avy-goto-word-or-subword-1." avy-goto-word-or-subword-1)
                 (const :tag "Use avy-goto-char." avy-goto-char))
  :group 'ace-isearch)

(defcustom ace-isearch-use-jump t
  "If `nil', `ace-jump-mode' or `avy' is never invoked.

If `t', it is always invoked if the length of `isearch-string' is
equal to 1.

If `printing-char', it is invoked only if you hit a printing
character to search for as a first input.  This prevents it from
being invoked when repeating a one character search, yanking a
character or calling `isearch-delete-char' leaving only one
character."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Only after a printing character is input" printing-char)
                 (const :tag "Never" nil))
  :group 'ace-isearch)

(defcustom ace-isearch-function-from-isearch 'helm-swoop-from-isearch
  "Symbol name of function which is invoked when the length of `isearch-string'
is longer than or equal to `ace-isearch-input-length'."
  :type 'symbol
  :group 'ace-isearch)

(defcustom ace-isearch-use-function-from-isearch t
  "When non-nil, invoke `ace-isearch-function-from-isearch' if the length
of `isearch-string' is longer than or equal to `ace-isearch-input-length'."
  :type 'boolean
  :group 'ace-isearch)

(defcustom ace-isearch-fallback-function 'helm-swoop-from-isearch
  "Symbol name of function which is invoked when isearch fails and
`ace-isearch-use-fallback-function' is non-nil."
  :type 'symbol
  :group 'ace-isearch)

(defcustom ace-isearch-use-fallback-function nil
  "When non-nil, invoke `ace-isearch-fallback-function' when isearch fails."
  :type 'boolean
  :group 'ace-isearch)

(defvar ace-isearch--ace-jump-function-list
  (list "ace-jump-word-mode" "ace-jump-char-mode"))

(defvar ace-isearch--avy-function-list
  (list "avy-goto-word-1" "avy-goto-subword-1"
        "avy-goto-word-or-subword-1" "avy-goto-char"))

(defvar ace-isearch--function-list
  (append ace-isearch--ace-jump-function-list ace-isearch--avy-function-list)
  "List of functions in jumping.")

(defvar ace-isearch--ace-jump-or-avy)

(defun ace-isearch-switch-function ()
  (interactive)
  (let ((func (completing-read
               (format "Function for ace-isearch (current is %s): "
                       ace-isearch-function)
               ace-isearch--function-list nil t)))
    (setq ace-isearch-function (intern-soft func))
    (ace-isearch--make-ace-jump-or-avy)
    (message "Function for ace-isearch is set to %s." func)))

(defun ace-isearch--fboundp (func flag)
  (declare (indent 1))
  (when flag
    (when (eq func nil)
      (error "function name must be specified!"))
    (unless (fboundp func)
      (error (format "function %s is not bounded!" func)))
    t))

(defun ace-isearch--jumper-function ()
  (cond ((and (= (length isearch-string) 1)
              (not (or isearch-regexp
                       isearch-word))
              (ace-isearch--fboundp ace-isearch-function
                (or (eq ace-isearch-use-jump t)
                    (and (eq ace-isearch-use-jump 'printing-char)
                         (eq this-command 'isearch-printing-char))))
              (sit-for ace-isearch-jump-delay))
         (isearch-exit)
         (goto-char isearch-opoint)
         (if (or (< (point) (window-start)) (> (point) (window-end)))
             (message "Notice: Character '%s' could not be found in the \"selected visible window\"." isearch-string))
         (funcall ace-isearch-function (string-to-char isearch-string)))

        ((and (> (length isearch-string) 1)
              (< (length isearch-string) ace-isearch-input-length)
              (not isearch-success)
              (sit-for ace-isearch-jump-delay))
         (if (ace-isearch--fboundp ace-isearch-fallback-function
               ace-isearch-use-fallback-function)
             (funcall ace-isearch-fallback-function)))

        ((and (>= (length isearch-string) ace-isearch-input-length)
              (not isearch-regexp)
              (ace-isearch--fboundp ace-isearch-function-from-isearch
                ace-isearch-use-function-from-isearch)
              (sit-for ace-isearch-func-delay))
         (funcall ace-isearch-function-from-isearch))))

(defun ace-isearch-pop-mark ()
  "Jump back to the last location of `ace-jump-mode' invoked or `avy-push-mark'."
  (interactive)
  (cond ((eq ace-isearch--ace-jump-or-avy 'ace-jump)
         (ace-jump-mode-pop-mark))
        ((eq ace-isearch--ace-jump-or-avy 'avy)
         (avy-pop-mark))))

(defun ace-isearch--make-ace-jump-or-avy ()
  (let ((func-str (format "%s" ace-isearch-function)))
    (cond ((member func-str ace-isearch--function-list)
           (cond ((member func-str ace-isearch--ace-jump-function-list)
                  (setq ace-isearch--ace-jump-or-avy 'ace-jump))
                 ((member func-str ace-isearch--avy-function-list)
                  (setq ace-isearch--ace-jump-or-avy 'avy))))
          (t
           (error (format "Function name %s for ace-isearch is invalid!"
                          ace-isearch-function))))))

;;;###autoload
(defun ace-isearch-jump-during-isearch ()
  "Jump to the one of the current isearch candidates."
  (interactive)
  (if (< (length isearch-string) ace-isearch-input-length)
      (cond ((eq ace-isearch--ace-jump-or-avy 'ace-jump)
             (let ((ace-jump-mode-scope 'window))
               (isearch-exit)
               (ace-jump-do (regexp-quote isearch-string))))
            ((eq ace-isearch--ace-jump-or-avy 'avy)
             (let ((avy-all-windows nil))
               (avy-isearch))))))

;;;###autoload
(define-minor-mode ace-isearch-mode
  "Minor-mode which combines isearch, ace-jump-mode, avy, and helm-swoop seamlessly."
  :group      'ace-isearch
  :init-value nil
  :global     nil
  :lighter    ace-isearch-lighter
  (if ace-isearch-mode
      (progn
        (add-hook 'isearch-update-post-hook 'ace-isearch--jumper-function nil t)
        (ace-isearch--make-ace-jump-or-avy))
    (remove-hook 'isearch-update-post-hook 'ace-isearch--jumper-function t)))

(defun ace-isearch--turn-on ()
  (unless (minibufferp)
    (ace-isearch-mode +1)))

;;;###autoload
(define-globalized-minor-mode global-ace-isearch-mode
  ace-isearch-mode ace-isearch--turn-on
  :group 'ace-isearch)

;; obsolete functions and variables
(define-obsolete-function-alias 'ace-isearch-switch-submode
  'ace-isearch-switch-function "0.1.3")
(define-obsolete-variable-alias 'ace-isearch-submode
  'ace-isearch-function "0.1.3")
(define-obsolete-variable-alias 'ace-isearch-input-idle-jump-delay
  'ace-isearch-jump-delay "0.1.3")
(define-obsolete-variable-alias 'ace-isearch-input-idle-func-delay
  'ace-isearch-func-delay "0.1.3")
(define-obsolete-variable-alias 'ace-isearch-use-ace-jump
  'ace-isearch-use-jump "0.1.3")

(provide 'ace-isearch)
;;; ace-isearch.el ends here
