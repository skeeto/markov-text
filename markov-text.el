;;; markov-text.el --- Generate text with Markov chains

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; Version: 1.0
;; URL: https://github.com/skeeto/markov-text

;;; Commentary:

;; Generates text based on sample input text. Sometimes it produces funny
;; nonsense.

;; > He wiped himself again, as if he didn't marry her by hand.

;; By default, the Markov chain is generated from the text samples in
;; `data/`. The chain is stored in `markov-text-database' and text is
;; generated with `markov-text-generate' and `markov-text-insert'
;; (interactive with prefix-argument).

;; Paragraph and sentence structure comes from the states of the
;; Markov chain itself so there's no direct control over the size of
;; paragraphs and such.

;; Chains can be saved and loaded with `markov-text-save' and
;; `markov-text-load', appended with more sample text with
;; `markov-text-feed-file', and adjusted output randomness with
;; `markov-text-state-size' (larger is less random).

;;; Code:

(require 'cl)

(defun make-markov-text-database ()
  (make-hash-table :test 'equal))

(defvar markov-text-split-regex "[ \"]+"
  "Regex used to split words in an input text.")

(defvar markov-text-database (make-markov-text-database)
  "Database of Markov chain states. Meant to be seeded by source text.")

(defvar markov-text-database-file
  (concat (or "" (file-name-directory load-file-name)) "main.db.gz"))

(defvar markov-text-state-size 3
  "Number of words in a state. Smaller values lead to more random output.")

(defvar markov-text-data-root
  (concat (or "" (file-name-directory load-file-name)) "data/")
  "Root for finding provided data files.")

(defun markov-text--get-words (file)
  "Get the words from a file."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) markov-text-split-regex)))

(defun markov-text-feed (words)
  "Feed the word list to the database."
  (while (nth markov-text-state-size words)
    (let* ((state (subseq words 0 markov-text-state-size))
           (next (nth markov-text-state-size words))
           (nexts (gethash state markov-text-database)))
      (puthash state (cons next nexts) markov-text-database))
    (pop words)))

(defun markov-text-feed-file (file)
  "Feel a file to the database."
  (markov-text-feed (markov-text--get-words file)))

(defun markov-text--hash-keys (hash)
  "Return a list of a hash table's keys."
  (let ((keys ()))
    (maphash (lambda (k v) (push k keys)) markov-text-database)
    keys))

(defun markov-text-generate (n &optional no-fill)
  "Generate (roughly) N words of text from the states in the
database."
  (with-temp-buffer
    (let* ((states (markov-text--hash-keys markov-text-database))
           (state (nth (random (length states)) states)))
      (insert (capitalize (car state)))
      (mapc (lambda (s) (insert " " s)) (cdr state))
      (dotimes (i n (buffer-string))
        (let* ((nexts (gethash state markov-text-database))
               (next (nth (random (length nexts)) nexts)))
          (when (not next)
            (insert (markov-text-generate (- n i) no-fill))
            (return (buffer-string)))
          (setq state (append (cdr state) (list next)))
          (unless (looking-at "^") (insert " "))
          (insert next)
          (unless no-fill (fill-paragraph)))))))

(defun markov-text-insert (n)
  "Generate prefix-argument count words and insert them into the
buffer. The database maintains sentence and paragraph structure,
so the output will be formed into sentences and paragraphs,
automatically filled."
  (interactive "p")
  (insert (markov-text-generate n)))

(defun markov-text-reset ()
  "Reset the Markov chain database."
  (clrhash markov-text-database))

(defun markov-text-save (database file)
  "Save a Markov chain to disk."
  (with-temp-buffer
    (print database (current-buffer))
    (write-file file)))

(defun markov-text-load (file)
  "Save a Markov chain to disk."
  (with-current-buffer (find-file-noselect file)
    (prog1 (read (current-buffer))
      (kill-buffer))))

(defun markov-text--load-samples ()
  (let ((samples '("great-expectations.txt" "a-princess-of-mars.txt")))
    (dolist (sample samples)
      (markov-text-feed-file (concat markov-text-data-root sample)))))

;; Prepare sample chain

(eval-when (load eval)
  (if (file-exists-p markov-text-database-file)
      (setq markov-text-database (markov-text-load markov-text-database-file))
    (markov-text-reset)
    (markov-text--load-samples)
    (markov-text-save markov-text-database markov-text-database-file)))

(defun markov-text-to-dot (database file)
  "Dump a database out in DOT format (Graphviz) for visualization."
  (let ((print-escape-newlines t))
    (labels ((cat (list) (mapconcat 'identity list " ")))
      (with-temp-buffer
        (insert "digraph {\n")
        (maphash (lambda (k v)
                   (let ((from (cat k)))
                     (dolist (to (remove-duplicates v :test 'equal))
                       (insert (format "  %S -> %S [label=%S];\n"
                                       from (cat (append (cdr k) (list to)))
                                       (concat " " to))))))
                 database)
        (insert "}\n")
        (write-file file)))))

(provide 'markov-text)

;;; markov-text.el ends here
