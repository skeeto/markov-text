;;; lorem-ipsum.el --- Generate lorem ipsum by Markov chain

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; Version: 1.0
;; URL: https://github.com/skeeto/markov-text

;;; Commentary:

;; Generates lorem ipsum using the Markov chain functions in
;; `markov-text'. Use `lorem-ipsum-insert' with a prefix argument to
;; insert filler text into the current buffer.

;;  This package maintains its own Markov chain database in
;; `lorem-ipsum-database'.

;;; Code:

(require 'markov-text)

(defvar lorem-ipsum "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
  "The classic first line to lorem-ipsum.")

(defvar lorem-ipsum-data-file
  (concat (or "" (file-name-directory load-file-name)) "data/lorem-ipsum.txt")
  "File containing Cicero's 'De finibus bonorum et malorum'.")

(defvar lorem-ipsum-database
  (let ((markov-text-database (make-markov-text-database 3)))
    (markov-text-feed-file lorem-ipsum-data-file)
    markov-text-database)
  "Database containing lorem ipsum Markov chain.")

(defun lorem-ipsum-insert (n &optional no-classic)
  "Insert prefix-argument words of lorem ipsum. If NO-CLASSIC is
true, don't start with the classic 'Lorem ipsum ...' line."
  (interactive "p")
  (insert
   (with-temp-buffer
     (let ((markov-text-database lorem-ipsum-database))
       (markov-text-insert n))
     (unless no-classic
       (goto-char (point-min))
       (insert lorem-ipsum " ")
       (fill-paragraph))
     (buffer-string))))

(provide 'lorem-ipsum)

;;; lorem-ipsum.el ends here
