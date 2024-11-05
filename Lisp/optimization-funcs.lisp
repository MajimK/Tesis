(in-package :vrp)
(format t "Loading optimization-funcs.lisp...")



(defvar *routes* '((1 2)	 ; Ruta 1
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
                                 (35)))



(defparameter *c* (make-hash-table :test 'equal))  
(defparameter *s* (make-hash-table :test 'equal)) 
(defparameter *k* (make-hash-table :test 'equal))  
(defparameter *l* (make-hash-table :test 'equal))
(defparameter *p* (make-hash-table :test 'equal))

(defun create-bridges (routes)
  (mapcar (lambda (ruta)
	    (let ((bridges '()))
	      (loop for i from 0 below (length ruta) do
		(push (list (if (= i 0) 0 (nth (- i 1) ruta)) (nth i ruta)) bridges))
	      (push (list (nth (- (length ruta) 1) ruta) 0 ) bridges)
	      (reverse bridges)))
	  routes))

(defvar *bridges* (create-bridges *routes*))

(defun calculate-matrix-c (bridges)
  "Calculate the cost matrix c using the node-to-node tuples."
  (loop for r from 0 below (length bridges) do
       (let ((route (nth r bridges)))
         (loop for i from 0 below (length route) do
              (let ((node1 (nth 0 (nth i route)))
                    (node2 (nth 1 (nth i route))))
                ;; Get the cost between node1 and node2
                (setf (gethash (list r i) *c*) (nth node2 (nth node1 *cvrp-distances*))))))))

(defun imprimir-matriz (matrix)
  "Imprime todos los elementos de la tabla hash *c*."
  (maphash (lambda (key value)
             (format t "~a -> ~a~%" key value))  ;; Imprime la clave y el valor
           matrix))  ;; *c* es la tabla hash de costos

(defun calculate-matrix-s (routes-tuples)
  "Calculate the cost sum matrix S using the node-to-node tuples."
  (loop for r from 0 below (length routes-tuples) do
    (let ((route (nth r routes-tuples)))
      (loop for i from 0 below (length route) do
        (loop for j from (+ i 1) below (length route) do
          (let ((sum 0))
            (loop for k from (+ i 1) below j do
              (let ((node1 (nth 0 (nth k route)))
		    (node2 (nth 1 (nth k route))))
		(let ((cost (nth node2 (nth node1 *cvrp-distances*))))
		  (incf sum cost))))
	    (setf (gethash (list r i j) *s*) sum)))))))

(defun calculate-matrix-k (routes-tuples)
  "Calculate the cost matrix K for connecting nodes from different routes."
  (loop for r1 from 0 below (length routes-tuples) do
       (loop for i from 0 below (length (nth r1 routes-tuples)) do
            (loop for r2 from 0 below (length routes-tuples) do
                 (loop for j from 0 below (length (nth r2 routes-tuples)) do
                      (let ((bridge1 (nth i (nth r1 routes-tuples)))
                            (bridge2 (nth j (nth r2 routes-tuples))))
                        (setf (gethash (list r1 i r2 j) *k*) (nth (nth 0 bridge2) (nth (nth 1 bridge1) *cvrp-distances*)))))))))

(defun calculate-matrix-l (routes-tuples)
  "Calculate the cost matrix L for connecting nodes within the same route."
  (loop for r1 from 0 below (length routes-tuples) do
       (loop for i from 0 below (length (nth r1 routes-tuples)) do
            (loop for r2 from 0 below (length routes-tuples) do
                 (loop for j from 0 below (length (nth r2 routes-tuples)) do
                      (let ((bridge1 (nth i (nth r1 routes-tuples)))
                            (bridge2 (nth j (nth r2 routes-tuples))))
                        (setf (gethash (list r1 i r2 j) *l*) (nth (nth 0 bridge1) (nth (nth 1 bridge2) *cvrp-distances*)))))))))

(defun calculate-p ()
  (let ((cost 0)
	(r 0))
    (maphash (lambda (key value)
	       (if (/= r (nth 0 key))
		   (progn
		       (setf (gethash r *p*) cost)
		       (setf r (nth 0 key))
		      (setf cost value))
		   (incf cost value))) *c*)
    (setf (gethash r *p*) cost)))







(calculate-matrix-c *bridges*)
(calculate-p)
(calculate-matrix-s *bridges*)
(calculate-matrix-k *bridges*)
(calculate-matrix-l *bridges*)



