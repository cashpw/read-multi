;;; read-multi.el --- Read multiple inputs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Cash Prokop-Weaver
;;
;; Author: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Maintainer: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Created: November 16, 2025
;; Modified: November 16, 2025
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/cashpw/read-multi
;; Package-Requires: ((emacs "24.3") (dash "2.20.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Read multiple inputs and return them as a list.
;;
;;; Code:

(require 'dash)

(defgroup read-multi nil
  "Group for read-multi package.")

(defconst read-multi--buffer nil
  "Read-multi buffer.")
(defconst read-multi--buffer-name "*read-multi*"
  "Name for the read-multi buffer.")
(defvar read-multi--state '()
  "Prompt and response state.")

(defun read-multi (prompts)
  "Read multiple inputs from user based on PROMPTS.

PROMPTS is a list of prompt specifiers. Each prompt should be:

1. The prompt as a string
2. The function to call to read input from the user."
  (setq
   read-multi--buffer (get-buffer-create read-multi--buffer-name)
   read-multi--state
   (--map
    `(:prompt
      ,(plist-get it :prompt)
      :read-fn ,(plist-get it :read-fn)
      :stringify-fn ,(plist-get it :stringify-fn)
      :response
      ,(when (plist-member it :default)
         (plist-get it :default))
      :current-p nil)
    prompts))
  (with-current-buffer read-multi--buffer
    (read-multi--render)
    (pop-to-buffer read-multi--buffer)
    (let ((i 0))
      (dolist (s read-multi--state)
        (when (not (plist-get s :response))
          (read-multi--ask i))
        (cl-incf i)))
    (prog1 (pcase (read-multi--confirm-or-change)
             ('confirm (--map (plist-get it :response) read-multi--state))
             ('cancel nil))
      (kill-buffer read-multi--buffer))))

(defun read-multi--render ()
  "Render the prompts and responses."
  (erase-buffer)
  (save-excursion
    (goto-char (point-min))
    (let ((i 1))
      (dolist (s read-multi--state)
        (let* ((response
                (propertize (cond
                             ((stringp (plist-get s :response))
                              (plist-get s :response))
                             ((not (plist-get s :response))
                              "__")
                             ((plist-member s :stringify-fn)
                              (funcall (plist-get s :stringify-fn)
                                       (plist-get s :response)))
                             (t
                              "__"))
                            'face
                            (if (plist-get s :current-p)
                                'highlight
                              nil))))
          (insert
           (format
            "%d. %s
   %s

"
            i (plist-get s :prompt) response)))
        (cl-incf i)))))

(defun read-multi--ask (index)
  "Prompt the user for value at INDEX of state."
  (read-multi--set-current index t)
  (read-multi--render)
  (let ((s (nth index read-multi--state)))
    (read-multi--set-response
     index
     (funcall (plist-get s :read-fn)
              (plist-get s :prompt)
              (plist-get s :response))))
  (read-multi--set-current index nil)
  (read-multi--render))

(defun read-multi--set-current (index current)
  "Set :current-p for INDEX in state to CURRENT."
  (setf (elt read-multi--state index)
        (plist-put (-copy (nth index read-multi--state)) :current-p current)))

(defun read-multi--set-response (index response)
  "Set :response for INDEX in state to RESPONSE."
  (setf (elt read-multi--state index)
        (plist-put (-copy (nth index read-multi--state)) :response response)))

(defun read-multi--confirm-or-change ()
  "Confirm user input. Allow user to change a response."
  (let* ((prompt-count (length read-multi--state))
         (choice
          (read-string
           (format "Submit (or change) your responses [Y/n/%s]? "
                   (string-join (mapcar
                                 #'number-to-string
                                 (number-sequence 1 prompt-count))
                                "/")))))
    (cond
     ((or (string-equal "y" choice)
          (string-equal "Y" choice)
          (string-empty-p choice))
      'confirm)
     ((or (string-equal "n" choice) (string-equal "N" choice))
      'cancel)
     ((let ((choice-number (string-to-number choice)))
        (and (> choice-number 0) (<= choice-number prompt-count)))
      (let ((index (1- (string-to-number choice))))
        (read-multi--set-response index nil)
        (read-multi--ask index)
        (read-multi--confirm-or-change)))
     (t
      (read-multi--confirm-or-change)))))

(provide 'read-multi)
;;; read-multi.el ends here
