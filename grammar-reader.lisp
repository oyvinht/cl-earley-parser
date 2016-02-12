(in-package :earley-parser)

;; The context free grammar representation is a hashtable keyed on non-terminal
;; symbols. For each symbol there is a list of productions/rules for that
;; symbol. The productions take the form of a ordered list of symbols.
;;
;; The non-terminal symbols are represented as strings.


;; The lexicon is the representation of all terminal symbols (words in the 
;; language). It is a hashtable keyed on word. For each word there is a list
;; of "terminal" structures that are different semantic mappings of the word. 



;;;; Backus-Naur Form grammar reader functions
;;;;------------------------------------------
(defun load-bnf-grammar (pathname)
  "Reads a grammar on Backus-Naur form into the representation of the context 
   free grammar(CFG)."
  (let ((rules (make-hash-table :test *string-comparer*)))
    ;; For each production in the BNF file, create a list of possible ordered
    ;; symbol sequences (list of strings) that is a legal expression for the
    ;; symbol
    (with-open-file (file pathname :direction :input)
      (loop for production = (read-next-bnf-production file)
	    until (null production)
	    do (labels ((inject-expansions! (symbol-list expansion)
			  (cond
			   ;; No more expansions -> Inject last one
			   ((null symbol-list)
			    (push expansion (gethash (car production) rules)))
			   ;; More expansion -> Inject current and continue
			   ((equalp (first symbol-list) "|")
			    (push expansion (gethash (car production) rules))
			    (inject-expansions! (cdr symbol-list) nil))
		       ;; Expansion not ended -> Collect rest of expansion
			   (t
			    (inject-expansions! 
			     (cdr symbol-list) 
			     (append expansion (list (first symbol-list))))))))
		       (inject-expansions! (cddr production) nil))))
    (make-grammar :rules rules)))

(let ((lexeme-cache))
  (defun read-next-bnf-production (file &optional (keep-newline nil))
    "Reads and returns the next Backus-Naur production from file."
    (let ((production))
      ;; If there is anything in the cache, pop it off and add it to production
      (loop until (null lexeme-cache)
	 do (push (pop lexeme-cache) production))
      (loop for lexeme = (read-next-bnf-lexeme file keep-newline)
	 until (null lexeme)
	 do (if
	     ;; If we just read a "::=" and there already is one in the 
	     ;; production -> Push it and last lexeme onto cahce instead of
	     ;; production
	     (and 
	      (equalp lexeme "::=") (member lexeme production :test *string-comparer*))
	     (progn
	       (push lexeme lexeme-cache)
	       (push (pop production) lexeme-cache)
	       (return))
	     ;; Otherwise -> Keep adding lexemes to the production
	     (push lexeme production)))
      (reverse production))))

(defun read-next-bnf-lexeme (file &optional (keep-newline nil))
  "Reads and returns the next Backus-Naur lexeme from file."
  (let ((whitespace (list #\Space #\Tab))
	(lexeme ""))
    (unless keep-newline (push #\Newline whitespace))
    (loop for char = (read-char file nil nil)
       until (null char)
       do (cond
	    ;; If the char is an '=' and the rest of the lexeme is already
	    ;; "::" -> Return lexeme
	    ((and (char-equal char #\=) (string= lexeme "::"))
	     (setf lexeme (concatenate 'string lexeme (list char)))
	     (return))
	    ;; If the char is an '>' and lexeme starts with an '<' ->
	    ;; Return lexeme
	    ((and (char-equal char #\>) (char-equal (char lexeme 0) #\<))
	     (setf lexeme (concatenate 'string lexeme (list char)))
	     (return))
	    ;; If the char is '|' it is in it self a complete lexeme
	    ((and (char-equal char #\|) (= (length lexeme) 0))
	     (setf lexeme (concatenate 'string lexeme (list char)))
	     (return))
	    ;; Newlines are also (or may be) complete lexemes
	    ((and (char-equal char #\Newline) (= (length lexeme) 0)
		  keep-newline)
	     (setf lexeme (concatenate 'string lexeme (list char)))
	     (return))
	    ;; If the char is whitespace, and lexeme is empty ->
	    ;; Do nothing, just continue with the next char
	    ((and (member char whitespace :test *string-comparer*)
		  (= (length lexeme) 0)))
	    ;; If nothing specific matches -> Add char to lexeme
	    (t
	     (setf lexeme (concatenate 'string lexeme (list char))))))
    (when (> (length lexeme) 0) (string-trim (list #\< #\>) lexeme))))



;;;; Lexicon functions
;;;;------------------
;;;; Reads a dictionary on the form:
;;;;
;;;; <word> :class <class> :gender <gender>
;;;;
;;;; into a hashtable of lists of word instances and 
;;;; a list of word classes (part of speech).

(defun read-lexicon-line (file)
  "Read a line from the given file, and return the corresponding terminal."
  ;; Parse a line from the file
  (let* ((words
	  (remove "" (split-sequence:split-sequence #\Space (read-line file))))
	 (string (first words))
	 (options (mapcar #'(lambda (string) 
			      (if (eq (char string 0) #\:)
				  (read-from-string string)
				  string))
			  (rest words))))
    ;; Create the word object
    (make-terminal :word string 
		   :class (string-trim '(#\< #\>) (getf options :class))
		   :gender (getf options :gender))))

(defun load-lexicon (pathname)
  "Read all words from a dictionary file into a lexicon and a part of speech."
  (let ((lexicon (make-hash-table :test *string-comparer*))
	(part-of-speech nil))
    (with-open-file (file pathname :direction :input)
      (loop 
       while (< (file-position file) (file-length file))
       do (let ((w (read-lexicon-line file)))
	    ;; Add (unless existing) the word class to part of speech
	    (pushnew (terminal-class w) part-of-speech :test *string-comparer*)
	    ;; Add the word definition to the lexicon
	    (push w (gethash (terminal-word w) lexicon)))))
    (make-lexicon :dictionary lexicon :part-of-speech part-of-speech)))
