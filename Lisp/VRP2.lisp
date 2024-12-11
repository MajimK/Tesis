(in-package :vrp)


(defvar *cvrp-distances* nil)

(defvar *cvrp-demands* nil)

(defvar *cvrp-capacity* 20)

(defvar *cvrp-routes* nil)


; Ya arregle el cvrp-data.lisp
(let ((start-time (get-internal-real-time)))
(defparameter *best-sol1*
  (with-cvrp-problem (p1 :distances *cvrp-distances*
                         :demands *cvrp-demands*
                         :capacity *cvrp-capacity*)



    (with-basic-cvrp-solution (s1 ((34 12 25 2)
(3 32)
(33 8 31)
(16)
(10 18)
(9 1)
(23 27 15)
(17)
(11 19 4)
(30 5 26 14)
(21 35 24)
(6)
(29 22)
(20 7)
(28 13))p1)

      (let* ((wc (basic-working-copy s1))
	     (ops-list nil)
	     (current-delta-cost 0)
	     (action (delta-cvrp-action))
	     (best-delta-cost nil)
	     (best-ops-list nil))
        (prepare-solution-for-neighborhood-exploration wc)
        (initialize-action-for-delta-cost-computation wc p1 action)

        (doselect-route (r1 wc)
	  (doselect-subroute (s1 r1 wc)
	    (doselect-route (r2 wc)
	      (doinsert-subroute (s1 r2 wc)
	        (setf current-delta-cost
                      (delta-cost (reverse ops-list)
				  wc
				  p1
				  action
				  ))

	        (format t "ops-list for current-neighbor: ~a~%~%" ops-list)

	        (format t "reversed neighbor operations:  ~a~%" (reverse ops-list))
	        (format t "DELTA-COST: ~a~%" current-delta-cost)

	        (when (or (not best-delta-cost)
			  (< current-delta-cost best-delta-cost))
		  (setf best-delta-cost current-delta-cost)
		  (setf best-ops-list (reverse ops-list))
		  (format t "Update best-delta-cost: ~a~%" best-delta-cost)
		  (format t "Update best-ops-list: ~a~%" best-ops-list))))))
        (format t "the best neigh delta-cost: ~a~%" best-delta-cost)
        (format t "the best region: ~a~%" best-ops-list)
        ;; let's print the info about the best-solution
        (format t "Info about the best neighbor~%")
        ;; in (first best-ops-list) we have the select-subroute instance
        (format t "Route of the select-subroute operation: ~a~%"
                (route (first best-ops-list)))
        (format t "Position of the select-subroute operation: ~a~%"
                (pos (first best-ops-list)))
        (format t "Size of the select-subroute operation: ~a~%"
                (size (first best-ops-list)))

        (format t "In position size + 1 we have the first insertion~%")

        (format t "Element size + 1 (~a) of the list: ~a~%"
                (+ 1 (size (first best-ops-list)))
                (nth (+ 1 (size (first best-ops-list))) best-ops-list))

        (format t "And from that element we get the insertion position in route and pos:~%")

        (format t "Insertion route: ~a~%Insertion pos: ~a~%"
                (route (nth (+ 1 (size (first best-ops-list))) best-ops-list))
                (pos (nth (+ 1 (size (first best-ops-list))) best-ops-list)))
        
        
        
        
        ;; let's return de best ops-list
        (clone best-ops-list)))))
      

  (let ((end-time (get-internal-real-time)))
    (format t "Tiempo transcurrido: ~a segundos~%"
            (/ (- end-time start-time) ;; Diferencia en ticks
               internal-time-units-per-second))))







