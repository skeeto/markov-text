(require 'cl)

(defun make-markov-text-database ()
  (make-hash-table :test 'equal))

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
    (split-string (buffer-string) "[ \"]+")))

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
          (when (not next) (return (buffer-string)))
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

(eval-when (compile)
  (markov-text-reset)
  (markov-text--load-samples)
  (markov-text-save markov-text-database markov-text-database-file))

(eval-when (load)
  (setq markov-text-database (markov-text-load markov-text-database-file)))

(eval-when (eval)
  (markov-text--load-samples))

(provide 'markov-text)
