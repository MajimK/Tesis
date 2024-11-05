(in-package :vrp)


(defvar *cvrp-distances* nil)

(defvar *cvrp-demands* nil)

(defvar *cvrp-capacity* 20)

(defvar *cvrp-routes* nil)


; Ya arregle el cvrp-data.lisp

(with-cvrp-problem (p1 :distances *cvrp-distances*
                       :demands *cvrp-demands*
                       :capacity *cvrp-capacity*)



  (with-basic-cvrp-solution (s1 ((1 2)	 ; Ruta 1
                                 (3 4 5) ; Ruta 2
                                 (6)     ; Ruta 3
                                 (7 8)   ; Ruta 4
                                 (9)     ; Ruta 5
                                 (10 11) ; Ruta 6
                                 (12)    ; Ruta 7
                                 (13 14) ; Ruta 8
                                 (15)    ; Ruta 9
                                 (16 17) ; Ruta 10
                                 (18)    ; Ruta 11
                                 (19 20) ; Ruta 12
                                 (21 22) ; Ruta 13
                                 (23)    ; Ruta 14
                                 (24)    ; Ruta 15
                                 (25 26) ; Ruta 16
                                 (27 28) ; Ruta 17
                                 (29)    ; Ruta 18
                                 (30 31) ; Ruta 19
                                 (32)    ; Ruta 20
                                 (33 34) ; Ruta 21
                                 (35)) p1)

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
	      (setf current-delta-cost (delta-cost (reverse ops-list)
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
		(format t "Update best-ops-list: ~a~%" best-ops-list))
	      ))))
      (format t "the best neigh delta-cost: ~a~%" best-delta-cost)
      (format t "the best region: ~a~%" best-ops-list))))
      









