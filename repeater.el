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

(defvar repeater-commands (make-ring 2))
(defvar repeater-sit-for .5)
(defvar repeater-confirm-timeout 1)
(defvar repeater-interval 0.1)

(defun repeater-post-command ()
  (ring-insert repeater-commands (cons last-repeatable-command
                                       last-command-event))
  (when (= (ring-length repeater-commands) 2)
    (let ((last (ring-ref repeater-commands 0))
          (penult (ring-ref repeater-commands 1)))
      (when (equal last penult)
        (let* ((vec (this-command-keys-vector))
               (len (length vec))
               (key (and (> len 0) (aref vec (1- len))))
               (this (cons this-command key)))
          (when (equal this last)
            (let ((message-log-max nil)
                  (name (propertize (symbol-name this-command)
                                    'face font-lock-function-name-face)))
              (and (sit-for repeater-sit-for)
                   (message "About to repeating %s... (Hit any key to quit)" name)
                   (sit-for repeater-confirm-timeout)
                   (condition-case err
                       (let ((count 0))
                         (while (and (sit-for repeater-interval)
                                     (condition-case err
                                         (prog1 t (call-interactively this-command))
                                       (error
                                        (message "%s" (error-message-string err))
                                        nil)))
                           (setq count (1+ count))
                           (message "Repeating %s [%d times] (Hit any key to quit)"
                                    name count)))
                     (dotimes (_ (ring-length repeater-commands))
                       (ring-remove repeater-commands)))))))))))

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
