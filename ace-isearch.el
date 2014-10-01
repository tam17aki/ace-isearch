;;; ace-isearch.el --- A seamless bridge between isearch and ace-jump-mode -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2014 by Akira Tamamori

;; Author: Akira Tamamori
;; URL: https://github.com/tam17aki/ace-isearch-mode
;; Created: Sep 25 2014
;; Package-Requires: ((ace-jump-mode "2.0") (helm "1.4"))

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

;; To use this package, add following code to your init file.
;;
;;   (require 'ace-isearch)
;;   (global-ace-isearch-mode +1)
;;

;;; Code:

(eval-when-compile (defvar migemo-isearch-enable-p))

(require 'helm)
(require 'ace-jump-mode)

(defgroup ace-isearch nil
  "Group of ace-isearch."
  :group 'ace-jump)

(defcustom ace-isearch-lighter " IACE"
  "Lighter of ace-isearch-mode."
  :type 'string
  :group 'ace-isearch)

(defcustom ace-isearch-input-idle-delay 0.4
  "Idle second before invoking `ace-isearch-function-from-iserach'."
  :type 'number
  :group 'ace-isearch)

(defcustom ace-isearch-input-length 6
  "Minimum input length to invoke `ace-isearch-function-from-isearch'."
  :type 'integer
  :group 'ace-isearch)

(defcustom ace-isearch-submode 'ace-jump-word-mode
  "Sub-mode for ace-jump-mode."
  :type '(choice (const :tag "Use ace-jump-word-mode." ace-jump-word-mode)
                 (const :tag "Use ace-jump-char-mode." ace-jump-char-mode))
  :group 'ace-isearch)

(defcustom ace-isearch-funtion-from-isearch 'helm-occur-from-isearch
  "A function which is invoked when the length of `isearch-string'
is longer than or equal to `ace-isearch-input-length'."
  :type 'symbol
  :group 'ace-isearch)

(defcustom ace-isearch-use-function-from-isearch t
  "When non-nil, invoke `ace-isearch-funtion-from-isearch' if the length
of `isearch-string' is longer than or equal to `ace-isearch-input-length'."
  :type 'boolean
  :group 'ace-isearch)

(defvar ace-isearch--submode-list
  (list "ace-jump-word-mode" "ace-jump-char-mode")
  "List of jump type for ace-jump-mode.")

;;;###autoload
(define-minor-mode ace-isearch-mode
  "Minor-mode which connects isearch and ace-jump-mode seamlessly."
  :group      'ace-isearch
  :init-value nil
  :global     nil
  :lighter    ace-isearch-mode-lighter
  (if ace-isearch-mode
      (add-hook 'isearch-update-post-hook 'ace-isearch--jumper-function nil t)
    (remove-hook 'isearch-update-post-hook 'ace-isearch--jumper-function t)))

(defun ace-isearch--turn-on ()
  (unless (minibufferp)
    (ace-isearch-mode +1)))

;;;###autoload
(define-globalized-minor-mode global-ace-isearch-mode
  ace-isearch-mode ace-isearch--turn-on
  :group 'ace-isearch)

;;;###autoload
(defun ace-isearch-switch-submode ()
  (interactive)
  (let ((submode (completing-read
                  (format "Sub-mode (current is %s): " ace-isearch-submode)
                  ace-isearch--submode-list nil t)))
    (setq ace-isearch-submode (intern-soft submode))
    (message "Sub-mode of ace-isearch is set to %s." submode)))

(defun ace-isearch--jumper-function ()
  (cond ((and (= (length isearch-string) 1)
              (sit-for ace-isearch-input-idle-delay))
         (isearch-exit)
         (funcall ace-isearch-submode (string-to-char isearch-string)))
        ((and (>= (length isearch-string) ace-isearch-input-length)
              (sit-for ace-isearch-input-idle-delay))
         (isearch-exit)
         (cond ((and (not (featurep 'migemo))
                     ace-isearch-use-function-from-isearch)
                (funcall ace-isearch-funtion-from-isearch))
               ((and (featurep 'migemo)
                     (not migemo-isearch-enable-p)
                     ace-isearch-use-function-from-isearch)
                (funcall ace-isearch-funtion-from-isearch))))))

(provide 'ace-isearch)
;;; ace-isearch.el ends here
