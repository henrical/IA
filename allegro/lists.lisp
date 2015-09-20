;; simple list
(list 1 2 3)

;; preperty list (the poor man's hash table)
(list :a 1 :b 2 :c 3)

;; get element of property list
;; returns element after second parameter
(getf(list :a 1 :b 2 :c 3) :a) ;;returns 1

(getf(list :a 1 :b 2 :c 3) :d) ;; returns NIL

;;function that returns a list with parameters
;;as list elements  
(defun make-cd(title artist rating ripped)
(list :title title :artist artist :rating rating :ripped ripped))
;;or 
;; (
;; defun make-cd(title artist rating ripped)
;; 	(list :title title :artist artist :rating rating :ripped ripped)
;; )


;; call function
(make-cd "Ride the Lightning" "Metallica" 8 t)


;; exit clisp
(cl-user::quit)

