;;; info-note.el --- Easy transition between info and org mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Emin Umut Gerçek
;;
;; Author: Emin Umut Gerçek <https://github.com/eugercek>
;; Maintainer: Emin Umut Gerçek <umutgercek1999@gmail.com>
;; Created: July 02, 2021
;; Modified: July 02, 2021
;; Version: 0.0.1
;; Keywords: info-mode org-mode workflow
;; Homepage: https://github.com/eugercek/info-note
;; Package-Requires: ((emacs "27.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;;; Variables

(defvar info-noter/notes-dir
  "Base path for info-noter notes.")

(defvar info-noter/current-note-file
  "Currently note file.")


;;; Interactive functions

(defun info-noter/setup (arg)
  "Start info-noter session.
Choose a file no take notes on.
Give ARG for choose from the directory `info-noter/notes-dir'
If: Only one org buffer is visible choose that.
Else: Choose selectively."

  (interactive "P")
  (info-noter--set-info-file arg))

(defun info-noter/copy-heading (arg)
  "Send current heading to `info-noter/note-file' file.
Choose level of heading with ARG.
nil: One level down.
4: Same level
16: One level up"
  (interactive "P")
  (let ((text (thing-at-point 'line t)))
    (with-current-buffer info-noter/current-note-file
      (goto-char (point-max))
      (insert text)
      (forward-line -1)
      (org-toggle-heading)
      (info-noter--heading-adjustment (car arg)))))

(defun info-noter/copy-text ()
  "Copy text to note file."
  (interactive)
  (let ((text (thing-at-point 'sentence t)))
    (with-current-buffer info-noter/current-note-file
      (goto-char (point-max))
      (insert text)
      (newline))))


;;; Helper Functions

(defun info-noter--set-info-file (arg)
  "Set info file.
TODO ARG"
  (setq info-noter/current-note-file
        (if arg
            (expand-file-name
             (read-file-name
              "Choose note file:"
              info-noter/notes-dir))
          (let ((buffer-list (info-noter--get-visible-note-buffers "org")))
            (cond ((eq (length buffer-list) 1)
                   (car buffer-list))
                  ((eq (length buffer-list) 1)
                   (completing-read "Choose note file:"
                                    buffer-list))))))
  (message "Selected %s" info-noter/current-note-file))

(defun info-noter--get-visible-note-buffers (extension)
  "Get visible buffers that has EXTENSION as extension."
  (seq-filter (lambda (buffer)
                (and
                 (not (eq (current-buffer)
                          buffer))
                 (string= (file-name-extension
                           (buffer-name buffer))
                          extension)))
              (map 'list #'window-buffer (window-list))))

;; TODO Add number for promote
(defun info-noter--heading-adjustment (arg)
  "Adjust heading with respect to ARG.
nil: One level down.
4: Same level
16: One level up."
  (cond ((eq arg 4)
         (progn
           (org-promote)))
        ((eq arg 16)
         (dotimes (var 2)
           (org-promote)))))


(provide 'info-noter)
;;; info-note.el ends here
