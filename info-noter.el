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
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun info-heading->org-heading (arg)
  "Send current heading to an org-mode buffer.
Buffer is chosen from the `info-noter-org-file`.
To change already setted `info-noter-org-file` call function with some
argument like \\[universal-argument].
To set the variable just call the function."
  (interactive "P")
  (let ((text (thing-at-point 'line t))
        (buf (get-info-noter-org-file-name arg)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert text)
      (forward-line -1)
      (org-ctrl-c-star))))

(defun get-info-noter-org-file-name (arg)
  "Gets `info-noter-org-file`.
If it's not sett yet user prompted to choose.
If it's already selected it'll used."
  (defun ask ()
    (setq info-noter-org-file
          (file-name-nondirectory
           (read-file-name
            "Select note file:"))))

  (cond ((not  (eq arg nil)) (ask))
        ((boundp 'info-noter-org-file) info-noter-org-file)
        (t (ask))))

;;; info-note.el ends he
