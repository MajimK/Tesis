
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :vrp) 

 (format t "Loading")

(defparameter *cvrp-distances* '((0 41 33 24 41 5 43 34 2 42 8 31 21 41 24 20 2 7 46 27 46 16 11 21 1 39 49 5 19 31 25 4 6 24 48 31)
(11 0 35 38 29 21 50 14 41 45 46 8 18 15 23 27 1 50 9 32 12 45 46 44 49 27 4 11 47 43 18 9 14 50 29 21)
(12 30 0 26 29 38 24 18 1 50 31 41 29 26 36 25 3 46 26 24 45 39 14 1 28 35 22 5 48 18 16 6 24 9 22 23)
(14 33 38 0 28 1 24 39 49 15 43 34 9 41 18 48 17 48 50 24 14 9 10 26 38 38 25 37 18 32 11 8 30 49 11 27)
(46 34 2 36 0 12 19 26 18 35 14 12 13 2 31 42 11 39 43 36 7 17 43 32 24 24 47 24 22 28 14 3 47 26 25 47)
(48 49 37 46 20 0 14 9 5 6 41 42 31 14 6 9 2 30 21 20 9 26 2 11 18 1 25 49 44 46 23 5 45 22 9 27)
(7 12 31 15 28 33 0 28 42 38 35 32 18 19 38 28 40 11 27 35 36 25 45 19 32 21 46 21 30 16 30 14 38 39 4 24)
(48 24 23 19 31 27 25 0 46 17 12 37 13 50 1 40 7 44 32 2 9 31 36 13 16 47 43 48 43 49 1 31 17 29 21 25)
(37 9 39 11 9 29 2 20 0 18 32 27 41 26 5 39 29 42 39 48 38 26 36 19 15 6 6 35 45 11 19 36 37 35 42 17)
(36 42 36 45 4 3 3 21 45 0 18 30 16 23 10 17 31 11 9 31 6 33 2 32 2 21 50 39 47 26 43 31 30 34 46 24)
(35 48 39 16 42 41 46 42 33 45 0 12 25 48 19 42 49 19 3 24 35 24 46 31 25 15 24 48 44 25 32 50 36 24 40 30)
(43 31 23 4 47 9 33 46 45 39 38 0 24 12 22 43 30 3 49 13 29 42 27 26 32 3 45 20 5 14 1 29 1 14 36 50)
(24 50 45 19 2 16 29 37 31 44 38 27 0 13 42 21 21 17 22 47 6 33 40 2 41 3 13 2 45 36 30 49 29 46 24 15)
(9 13 38 37 32 39 8 7 3 17 29 40 46 0 13 9 48 3 41 45 6 14 8 15 15 42 41 30 24 1 42 30 21 24 10 14)
(26 20 5 41 45 6 27 29 16 41 8 35 49 50 0 33 19 2 15 1 50 46 37 27 47 19 21 26 49 34 23 43 39 40 14 31)
(5 33 35 1 26 23 24 32 40 13 29 37 23 9 28 0 28 13 30 24 25 38 33 22 49 39 14 23 11 9 8 19 28 40 19 49)
(26 33 17 2 22 1 11 29 18 42 3 43 17 22 24 44 0 35 24 28 17 21 37 18 47 13 50 36 32 7 39 40 48 3 19 6)
(22 50 30 6 30 29 25 9 14 10 3 17 20 39 50 6 12 0 7 44 9 44 15 13 7 6 49 2 5 6 35 35 42 11 2 11)
(37 31 5 28 2 24 43 9 12 49 21 6 27 8 40 33 19 1 0 9 20 45 34 31 34 17 20 31 28 11 43 15 10 11 28 22)
(13 13 17 31 15 44 31 36 44 9 18 48 3 48 22 35 39 25 39 0 46 49 4 18 18 11 40 10 50 9 35 49 27 24 1 5)
(16 47 32 32 18 11 19 30 35 17 3 47 15 24 1 1 40 29 13 12 0 1 49 43 26 48 33 4 47 45 14 42 31 25 14 28)
(7 32 28 23 25 18 46 42 28 26 35 13 7 19 3 1 45 19 26 23 16 0 46 41 11 4 2 47 30 38 43 42 41 45 19 36)
(15 3 44 23 28 2 9 26 11 28 16 33 5 26 32 35 13 6 19 48 39 31 0 9 40 47 34 36 37 46 25 47 36 33 23 27)
(25 22 7 23 23 7 4 18 48 22 26 9 19 17 31 47 45 49 23 31 2 31 13 0 10 2 38 3 9 4 45 19 22 37 15 8)
(24 42 25 30 10 45 9 31 1 7 36 24 28 6 17 5 7 23 50 11 11 6 45 37 0 26 12 26 36 18 41 4 17 39 41 7)
(37 3 32 15 27 18 42 9 1 46 5 34 23 4 11 12 22 3 37 20 45 19 36 12 49 0 11 42 24 16 49 48 11 8 32 21)
(6 1 2 38 34 31 28 42 39 46 16 27 8 27 29 41 3 33 39 35 15 23 28 12 3 42 0 48 48 28 28 21 21 43 2 46)
(26 30 24 31 8 12 43 50 38 12 24 24 30 48 11 35 25 15 38 50 49 47 16 14 17 35 34 0 49 49 47 2 16 11 30 45)
(37 30 24 1 43 32 44 20 8 38 5 28 50 26 6 39 25 43 40 6 44 6 40 39 48 37 11 44 0 27 43 7 4 33 30 48)
(14 15 6 20 34 6 6 17 12 5 33 29 32 32 1 41 35 33 9 43 37 14 33 50 48 8 24 33 11 0 44 31 38 37 14 45)
(9 12 2 45 40 31 27 26 3 16 35 9 46 36 8 33 33 13 11 35 44 17 34 27 17 11 9 35 7 46 0 46 32 9 28 23)
(38 13 22 36 4 48 48 8 7 43 14 19 38 20 12 2 40 13 18 16 15 11 17 15 7 43 24 49 31 26 39 0 46 11 12 38)
(48 11 9 25 37 29 8 18 33 1 4 11 6 30 4 20 3 28 1 22 11 45 48 35 43 13 39 45 39 45 21 38 0 50 40 30)
(37 23 9 43 11 16 39 41 15 47 49 27 2 39 19 19 23 18 45 22 49 23 4 10 10 31 36 20 5 17 1 28 25 0 23 29)
(31 21 9 11 25 48 27 44 9 31 37 20 29 19 32 22 38 11 5 18 3 46 42 27 31 43 10 14 12 36 30 36 43 34 0 31)
(30 16 30 40 35 46 15 36 22 23 31 18 47 2 13 49 28 43 39 50 11 12 31 35 1 24 44 20 13 28 31 13 13 5 45 0)
))

(defparameter *cvrp-demands* '(4 16 8 1 17 7 4 6 12 16 13 1 19 2 17 2 20 4 11 15 20 10 18 20 7 10 13 4 5 8 17 8 6 9 11 ))

(defparameter *cvrp-routes* '((5 4 26 3)
(21 35 34)
(22 9 12 27)
(2 6)
(23 25 28 19)
(33 31 18 7)
(10 32 14 16 30)
(29 24 8)
(11 20)
(17 15)
(13 1)
))

(defparameter *cvrp-capacity* 40)
