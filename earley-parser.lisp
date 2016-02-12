(in-package :earley-parser)

(defmethod predictor ((state state)
		      (chart-listing chart-listing) 
		      (grammar grammar))
  "Predict possible successor states based on the grammar. As a side-effect, add
 these states to the chart that this state belong to."
  (let ((B (next-cat state))
        (j (state-dot-index state)))
    (loop for production in (grammar-productions B grammar)
       collect (let ((new-state
		      (make-state :condition B
				  :subtree production
				  :dot 0 
				  :constituent-index j
				  :dot-index j)))
		 (when (> *debug* 2)
		   (format t "  predictor attempting to enqueue")
		   (format t " ~A into chart ~A~%" new-state j))
		 (enqueue new-state (nth j (chart-listing-charts
					    chart-listing)))
		 new-state))))

(defmethod scanner ((state state)
		    (words list) 
		    (chart-listing chart-listing)
		    (lexicon lexicon))
  "Check if the next category for this state is a member of the pos-cathegories
 for the current word. As a side effect, enqueue a new state corresponding to th
is find, into the current chart."
  (let* ((B (next-cat state))
         (j (state-dot-index state))
         (word (nth j words)))
    (when (> *debug* 2)
      (format t "  scanner is considering if ~A is a member of" B)
      (format t " the word-class list for \"~A\" (= ~A)~%"
	      word (lexicon-lookup word lexicon)))
    (when (member B (lexicon-lookup word lexicon)
		  :test #'(lambda (b terminal)
			    (funcall *string-comparer* 
				     b (terminal-class terminal))))
      (let ((new-state (make-state :condition B 
				   :subtree (list word)
				   :dot 1 
				   :constituent-index j
				   :dot-index (+ j 1))))
        (when (> *debug* 2)
          (format t "  scanner attempting to enqueue")
	  (format t " ~A into chart ~A~%"  new-state (+ j 1)))
        (enqueue
	 new-state (nth (+ j 1) (chart-listing-charts chart-listing)))))))

(defun completer (state chart-listing)
  "Find and return a list of the previous states that expect this states cathego
ry at this dot-index with the dot moved one step forward. As a side-effect, enqu
eue the states in the current chart."
  (let ((B (state-condition state))
        (j (state-constituent-index state))
        (k (state-dot-index state)))
    (loop for prev-state 
       in (chart-states (nth j (chart-listing-charts chart-listing)))
       when (let ((A (next-cat prev-state))) 
	      (when (funcall *string-comparer* A B)
		(when (> *debug* 3)
		  (format
		   t "    completer found the state: ~A to match ~A~%" 
		   prev-state state))
		t))
       collect (let ((new-state (make-state
				 :condition (state-condition prev-state)
				 :subtree (state-subtree prev-state)
				 :dot (+ (state-dot prev-state) 1)
				 :constituent-index (state-constituent-index
						     prev-state)
				 :dot-index k
				 :source-states (append (list state)
							(state-source-states
							 prev-state)))))
		 (when (> *debug* 2)
		   (format t "  completer attempting to enqueue")
		   (format t " ~A into chart ~A~%" new-state k))
		 (enqueue 
		  new-state (nth k (chart-listing-charts chart-listing)))
		 new-state))))

(defun earley-parse (sentence grammar lexicon)
  "Convert a string of words into a chart conforming to the grammar."
  (let ((words (remove "" (split-sequence:split-sequence #\Space sentence)))
        (chart-listing (make-chart-listing)))
    ;; Initialize charts, one chart per word in the sentence
    (loop for i from 0 to (length words)
       do (add-chart (make-chart) chart-listing))
    ;; Start off by enqueuing a dummy state in the first chart
    (enqueue (make-state :condition "G"
			 :subtree '("S")
			 :dot-index 0)
             (nth 0 (chart-listing-charts chart-listing)))
    ;; Then for each chart (= one per word)...
    (loop for chart in (chart-listing-charts chart-listing)
       and chart-index from 0
       ;; And for each possible state in that chart
       do (progn
	    (when (> *debug* 0)
	      (format t "~%---- processing chart #~A ----~%" chart-index))
	    (loop for state-index from 0
	       until (>= state-index (length (chart-states chart)))
	       do (let ((state (nth state-index (chart-states chart))))
		    (when (> *debug* 1)
		      (format t "considering state: ~A~%" state)
		      (format t "next cat of this~Astate is ~A~%"
			      (if (incomplete? state) " (incomplete) " " ")
			      (next-cat state)))
		    (cond ((and (incomplete? state) 
				(not (member (next-cat state)
					     (lexicon-part-of-speech lexicon)
					     :test *string-comparer*)))
			   (when (> *debug* 1)
			     (format t "predicting...~%"))
			   (predictor state chart-listing grammar))
			  ((and (incomplete? state) 
				(member (next-cat state)
					(lexicon-part-of-speech lexicon)
					:test *string-comparer*))
			   (unless (eq chart (first
					      (last
					       (chart-listing-charts
						chart-listing))))
			     (when (> *debug* 1)
			       (format t "scanning...~%"))
			     (scanner state words chart-listing lexicon)))
			  (t
			   (when (> *debug* 1)
			     (format t "completing...~%"))
			   (completer state chart-listing)))))
	    (when (> *debug* 0)
	      (format t "~%"))))
    chart-listing))
