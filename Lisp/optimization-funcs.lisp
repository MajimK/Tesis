(in-package :vrp)
(format t "Loading optimization-funcs.lisp...~%")



(defparameter *routes* '((68 14)
(5 1 44 23)
(84 77)
(91 48 36)
(90 12 41 94)
(83 56 74 42)
(26 93)
(71 63)
(49 27 76 72)
(79 88 38)
(96 20)
(16 92)
(98 67)
(81 55 15)
(6 10 8 95)
(85 50 59 75)
(51 73 7 54)
(62 97)
(34 52 87 35)
(64 86)
(100 21 53)
(25 60 37 99 70)
(22 31 58 43 78)
(9 33 2 40)
(57 28 24 29 46 30 82)
(3 4 47 19)
(69 13 45)
(32 18)
(66 61 17)
(89 11 39 80 65)))


(defparameter *c* (make-hash-table :test 'equal))
(defparameter *s* (make-hash-table :test 'equal))
(defparameter *d* (make-hash-table :test 'equal)) 
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

   (defparameter *bridges* (create-bridges *routes*))


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

(defun calculate-matrix-d (routes-tuples)
  "Calculate the cost sum matrix S using the node-to-node tuples."
  (loop for r from 0 below (length routes-tuples) do
    (let ((route (nth r routes-tuples)))
      (loop for j1 from 0 below (length route) do
        (loop for j2 from (+ j1 1) below (length route) do
          (let ((sum 0))
            (loop for k from (+ j1 1) below j2 do
              (let ((node1 (nth 0 (nth k route)))
		    (node2 (nth 1 (nth k route))))
		(let ((cost1 (nth (- node1 1) *cvrp-demands*))
		      (cost2 (nth (- node2 1) *cvrp-demands*)))
		  (incf sum (+ cost1 cost2)))))
	    (setf (gethash (list r j1 j2) *d*) sum)))))))


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


(defun imprimir-bridges ()
  "Imprime el contenido de la variable *bridges*."
  (format t "Contenido de *bridges*:~%")
  (loop for ruta in *bridges* do
    (format t "~a~%" ruta)))




(let ((start-time (get-internal-real-time)))
  (defparameter *bridges* (create-bridges *routes*))

(calculate-matrix-c *bridges*)
(calculate-p)
(calculate-matrix-s *bridges*)
(calculate-matrix-k *bridges*)
  (calculate-matrix-l *bridges*)
  (calculate-matrix-d *bridges*)
  
(let ((end-time (get-internal-real-time)))  
  (format t "Tiempo transcurrido en opti-funcs: ~a segundos~%"
          (/ (- end-time start-time) ;; Diferencia en ticks
             (float internal-time-units-per-second)))))
;(imprimir-bridges)
;(imprimir-matriz *d*)


