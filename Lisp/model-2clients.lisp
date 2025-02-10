(in-package :vrp)

(format t "Entrando en model 2clients~%")

(defparameter *best-sol1*
  (with-cvrp-problem (p1 :distances *cvrp-distances*
			 :demands *cvrp-demands*
			 :capacity *cvrp-capacity*)
    (with-basic-cvrp-solution (s1 ((1 26 29 3)
(12 33)
(6 5)
(30 4)
(22 17 25)
(11 2)
(31 20)
(19 32 16 15 27)
(23 7 21 13 24)
(8 10 18 34)
(9 35)
(14 28)
) p1 )
      (let ((wc (basic-working-copy s1))
	    (ops-list nil)
	    (current-delta-cost 0)
	    (action (delta-cvrp-action))
	    (best-delta-cost nil)
	    (best-ops-list nil))
	(prepare-solution-for-neighborhood-exploration wc)
	(initialize-action-for-delta-cost-computation wc p1 action)

	(let ((start-time (get-internal-real-time)))
	  (doselect-route (r1 wc)
	    (doselect-client (c1 r1 wc)
	      (doselect-route (r2 wc)
		(doselect-client (c2 r2 wc)
		  (doselect-route (ri1 wc)
		    (doselect-route (ri2 wc)
		      (doinsert-client (c1 ri1 wc)
			(doinsert-client (c2 ri2 wc)
			  (setf current-delta-cost
				(delta-cost (reverse ops-list)
					    wc
					    p1
					    action))

			  (when (or (not best-delta-cost)
				    (< current-delta-cost best-delta-cost))
			    (setf best-delta-cost current-delta-cost)
			    (setf best-ops-list (reverse ops-list)))))))))))

	  (let ((end-time (get-internal-real-time)))
	    (format t "Tiempo transcurrido exhaustiva 2clientes: ~a segundos~%"
		    (/ (- end-time start-time)
		       (float internal-time-units-per-second)))))
        (format t "the best neigh delta-cost: ~a~%" best-delta-cost)
        (format t "the best region: ~a~%" best-ops-list)
	(clone best-ops-list)))))
