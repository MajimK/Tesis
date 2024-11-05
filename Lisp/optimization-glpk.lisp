(in-package :vrp)

(defun create-cvrp-problem ()
  "Create the CVRP optimization problem in GLPK."
  (let ((prob (glp-create-prob)))
    (glp-set-prob-name prob "CVRP")
    (glp-set-obj-dir prob :minimize)
    prob))

(defun define-variables (prob)
  "Define the decision variables X in the GLPK problem."
  (loop for r1 from 0 below (length *routes*) do
       (loop for r2 from 0 below (length *routes*) do
            (loop for i from 0 below (length *bridges*) do
                 (loop for j1 from 0 below (length *bridges*) do
                      (loop for j2 from 0 below (length *bridges*) do
                           (let ((var-name (format nil "X(~a,~a,~a,~a,~a)" r1 j1 j2 r2 i)))
                             (glp-add-cols prob 1)
                             (glp-set-col-name prob (glp-get-num-cols prob) var-name)
                             (glp-set-col-bnds prob (glp-get-num-cols prob) :lo 0 :up 1)
                             (glp-set-col-kind prob (glp-get-num-cols prob) :binary))))))))

(defun define-objective-function (prob)
  "Define the objective function using hash tables *c*, *s*, *k*, *l*, and the pre-calculated values of P."
  (loop for r1 from 0 below (length *routes*) do
       (loop for r2 from 0 below (length *routes*) do
            (loop for i from 0 below (length *bridges*) do
                 (loop for j1 from 0 below (length *bridges*) do
                      (loop for j2 from 0 below (length *bridges*) do
                           (let ((idx (glp-find-col prob (format nil "X(~a,~a,~a,~a,~a)" r1 j1 j2 r2 i))))
                             (let* ((P (gethash r1 *p* 0))                    ;; Route cost
                                    (S (gethash (list r1 j1 j2) *s* 0))       ;; Sum of costs
                                    (c1 (gethash (list r1 j1) *c* 0))         ;; Cost c
                                    (c2 (gethash (list r1 j2) *c* 0))         ;; Another cost c
                                    (K (gethash (list r1 j1 r2 i) *k* 0))     ;; Cost K
                                    (L (gethash (list r1 j2 r2 i) *l* 0)))    ;; Cost L
                               (glp-set-obj-coef prob idx (+ P (- S) (- c1) (- c2) K L))))))))))

(defun define-constraints (prob)
  "Define the constraints for the CVRP in the GLPK problem."
  ;; Visit constraint: each node should be visited exactly once
  (glp-add-rows prob (length *bridges*))
  (loop for i from 0 below (length *bridges*) do
       (glp-set-row-name prob (1+ i) (format nil "Visit_node_~a" i))
       (glp-set-row-bnds prob (1+ i) :fx 1))

  ;; Capacity constraint for each vehicle
  (glp-add-rows prob (length *routes*))
  (loop for r from 0 below (length *routes*) do
       (glp-set-row-name prob (+ (length *bridges*) r) (format nil "Vehicle_capacity_~a" r))
       (glp-set-row-bnds prob (+ (length *bridges*) r) :up *cvrp-capacity*)))

(defun solve-cvrp (prob)
  "Solve the CVRP problem using GLPK."
  (glp-simplex prob nil)
  ;; Print the solution values
  (loop for var from 1 to (glp-get-num-cols prob) do
       (let ((value (glp-get-col-prim prob var)))
         (when (and value (> value 0))
           (format t "~a = ~a~%" (glp-get-col-name prob var) value)))))

(defun run-cvrp ()
  "Main function to execute the full CVRP flow."
  (let ((prob (create-cvrp-problem)))              
    (define-variables prob)              ;; Define decision variables
    (define-objective-function prob)     ;; Define the objective function
    (define-constraints prob)            ;; Define the constraints
    (solve-cvrp prob)                    ;; Solve the problem and print results
    (glp-delete-prob prob)))             ;; Clean up after solving
