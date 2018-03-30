;;; repeater.el --- Repeat recent repeated commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Keywords: convenience

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

;; 

;;; Code:

(defgroup repeater nil
  "Repeat recent repeated commands."
  :group 'tools)

(defcustom repeater-min-times 3
  "The minimum number of times of a repeated command to consider."
  :group 'repeater
  :type 'number)

(defcustom repeater-interval 0.1
  "Interval between repeating."
  :group 'repeater
  :type 'number)

(defcustom repeater-ignore-functions '(repeater-default-ignore-function)
  "Functions to call with no arguments to check about repeating.
If any of these functions returns non-nil, repeater will not repeat."
  :group 'repeater
  :type 'hook)

(defcustom repeater-query-function 'repeater-default-query-function
  "Function to call with no arguments to query about repeating.
If any of these functions returns nil, repeater will not repeat."
  :group 'repeater
  :type 'hook)

(defun repeater-equals (first &rest rest)
  "Return t if args are equal."
  (catch 'repeater-equals--break
    (dolist (elt rest)
      (or (equal elt first)
          (throw 'repeater-equals--break nil)))
    t))

(defvar repeater-ring nil
  "List of recent commands.")

(defun repeater-ring-push (elt)
  (let ((repeater-ring-max (1- repeater-min-times)))
    (push elt repeater-ring)
    (when (> (length repeater-ring) repeater-ring-max)
      (setcdr (nthcdr (1- repeater-ring-max) repeater-ring) nil))))

(defun repeater-default-ignore-function ()
  (or (minibufferp)
      ;; Suggested by @casouri at https://emacs-china.org/t/topic/5414/2
      (and (derived-mode-p 'markdown-mode)
           (equal (this-command-keys-vector) [?\`]))
      (and (derived-mode-p 'js-mode)
           (equal (this-command-keys-vector) [?=]))
      (and (eq major-mode 'python-mode)
           (or (equal (this-command-keys-vector) [?\"])
               (equal (this-command-keys-vector) [?\'])))))

(defvar repeater-sit-for .5)
(defvar repeater-confirm-timeout 1)

(defun repeater-default-query-function ()
  (sit-for repeater-sit-for)
  (message "About to repeat %s (Hit any key to stop)"
           (propertize (symbol-name this-command)
                       'face font-lock-function-name-face))
  (sit-for repeater-confirm-timeout))

(defun repeater-post-command ()
  (repeater-ring-push (cons last-repeatable-command last-command-event))
  (let* ((this-command-event
          (let* ((vec (this-command-keys-vector))
                 (len (length vec)))
            (and (> len 0) (aref vec (1- len)))))
         (this (cons this-command this-command-event)))
    (when (apply #'repeater-equals this repeater-ring)
      (let ((message-log-max nil)
            (name (propertize (symbol-name this-command)
                              'face font-lock-function-name-face)))
        (and
         (null (run-hook-with-args-until-success 'repeater-ignore-functions))
         (run-hook-with-args-until-success 'repeater-query-function)
         (condition-case err
             (let ((count 0))
               (while (and (sit-for repeater-interval)
                           (condition-case err
                               (prog1 t (call-interactively this-command))
                             (error
                              (message "%s" (error-message-string err))
                              nil)))
                 (setq count (1+ count))
                 (message "Repeating %s [%d times] (Hit any key to stop)"
                          name count)))
           (setq repeater-ring nil)))))))

;;;###autoload
(define-minor-mode repeater-mode
  "If you run the same command for 3 times, reepat it."
  :global t
  :lighter " Repeater"
  (if repeater-mode
      (add-hook 'post-command-hook #'repeater-post-command)
    (remove-hook 'post-command-hook #'repeater-post-command)))

(provide 'repeater)
;;; repeater.el ends here
