(in-package :vrp)

(format t "~%~% Buscando ~%~%")

(defvar *cvrp-distances* nil)

(defvar *cvrp-demands* nil)

(defvar *cvrp-capacity* 20)

(defvar *cvrp-routes* nil)


; Ya arregle el cvrp-data.lisp

(defparameter *best-sol1*
  (with-cvrp-problem (p1 :distances *cvrp-distances*
                         :demands *cvrp-demands*
                         :capacity *cvrp-capacity*)



    (with-basic-cvrp-solution (s1 ((20 3 2 21 30)
(5 7)
(24 10 25 16)
(28 19 35 12)
(18 32 6)
(29 9 22 33 4)
(26 15)
(14 17 1)
(34 31 13)
(23 27)
(11 8))p1)

      (let* ((wc (basic-working-copy s1))
	     (ops-list nil)
	     (current-delta-cost 0)
	     (action (delta-cvrp-action))
	     (best-delta-cost nil)
	     (best-ops-list nil))
        (prepare-solution-for-neighborhood-exploration wc)
        (initialize-action-for-delta-cost-computation wc p1 action)

	(let ((start-time (get-internal-real-time)))
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

	          (when (or (not best-delta-cost)
			    (< current-delta-cost best-delta-cost))
		    (setf best-delta-cost current-delta-cost)
		    (setf best-ops-list (reverse ops-list)))))))
	  
	    (let ((end-time (get-internal-real-time)))
	      (format t "Tiempo transcurrido exhaustiva: ~a segundos~%"
		      (/ (- end-time start-time) ;; Diferencia en ticks
               (float internal-time-units-per-second)))))
          (format t "the best neigh delta-cost: ~a~%" best-delta-cost)
          (format t "the best region: ~a~%" best-ops-list)
   
          (clone best-ops-list)))))
      









