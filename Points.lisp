(defun Ptx (pt)
  (car pt))

(defun Pty (pt)
  (cdr pt))

(defun Dist (p1 p2)
  (sqrt (+ (expt (- (Ptx p1) (Ptx p2)) 2) (expt (- (Pty p1) (Pty p2)) 2))))


; Funcao que le um conjunto de pontos de um arquivo e os armazena
; em uma lista, a qual eh retonada pela funcao.
(defun ReadPoints (filename)
  (let ((file (open filename :if-does-not-exist nil))
	(pts '()))
    (when file
      (loop for x = (read file nil)
	    while x do 
	    (let ((y (read file nil)))
	      (push (cons x y) pts)))
      (close file)) pts))


