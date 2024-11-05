
;; le agregué la línea siguiente
(in-package :vrp)
;; sin esa línea, las funciones se crean en otro namespace
;; y se arma tremendo chorizo :-(.

(defun generate-random-matrix (size)
  (let ((matrix (loop for i from 0 below size
                      collect (loop for j from 0 below size
                                    collect (if (= i j) 0
						(+ 1 (random 50)))))))
    matrix))

(defun generate-random-demands (size)
  (let ((values (loop for i from 0 below size
		      collect (+ 1 (random 20)))))
    values))


(defun save-cvrp-data (filename distances demands capacity)
  "Guarda los datos CVRP en un archivo Lisp."
  (with-open-file (stream filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream ";; Datos CVRP~%")
    (format stream "(defparameter *saved-cvrp-distances* '~s)~%~%" distances)
    (format stream "(defparameter *saved-cvrp-demands* '~s)~%~%" demands)
    (format stream "(defparameter *saved-cvrp-capacity* ~s)~%" capacity)))


(save-cvrp-data "cvrp-data.lisp" *cvrp-distances* *cvrp-demands* *cvrp-capacity*)
