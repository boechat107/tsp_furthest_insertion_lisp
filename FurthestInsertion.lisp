; Inclui funcoes de leitura dos pontos de um arquivo
;(compile-file "Points.lisp")
(load "Points.lisp")


(defun PtKey (ptQueue)
  (cdr ptQueue))

(defun PtPoint (ptQueue)
  (car ptQueue))


; Funcao que insere ordenado em uma lista de acordo com
; o predicado func passado como parametro
(defun InsertBy (func elem pQueue)
  (if (null pQueue)
    `(,elem)
    (if (funcall func elem (car pQueue))
      (cons elem pQueue)
      (cons (car pQueue) (InsertBy func elem (cdr pQueue))))))


; Recebe dois pontos com distancias e retorna true, caso
; a distancia de ptD1 seja maior, ou falso, caso contrario.
(defun EhMaior (ptD1 ptD2)
  (> (cdr ptD1) (cdr ptD2)))


; Insere pontos com distancia em ordem decrescente em ma lista.
(defmacro Sort-insert (ptD pQueue)
  `(setf ,pQueue (InsertBy #'EhMaior ,ptD ,pQueue)))


; Cria uma lista ordenada de elementos do tipo (Ponto . Distancia)
(defun MkListDist (ptRef pts)
  (let ((pQueue '()))
    (dolist (aux pts)
      (Sort-insert (cons aux (Dist aux ptRef)) pQueue))
    pQueue))


; Encontra em "pts" o ponto de menor distancia em relacao
; a "ptRef"
(defun NearestPoint (ptRef pts)
  (let ((minDist (cons (car pts) (Dist ptRef (car pts))))
	(newDist))
    (dolist (aux (cdr pts))
      (setf newDist (Dist ptRef aux))
      (if (< newDist (cdr minDist))
	(setf minDist (cons aux newDist))))
    minDist))


; Atualiza a lista de prioridade e retorna o ponto mais
; distante "(PtPoint fst)" e a lista atualizada "pQueue"
(defun FurthestPoint (pQueue tour)
  (let ((fst nil) (newDist nil))
    (loop
      (setf fst (pop pQueue))
      (setf newDist (cdr (NearestPoint (PtPoint fst) tour)))
      (when (= newDist (PtKey fst))
	(return))
      (Sort-insert (cons (PtPoint fst) newDist) pQueue))
    (cons (PtPoint fst) pQueue)))


; Define a melhor posicao para inserir um bovo ponto no tour
(defun BestPosition (pt tour)
  (let* ((newValue)
	(m1 0)
	(m2 1)
	(ptLast (car (last tour)))
	(minValue (-
		    (+ (Dist pt (car tour)) (Dist pt ptLast))
		    (Dist (car tour) ptLast))))
    (dolist (aux1 (cdr tour))
      (let ((aux2 (elt tour (- m2 1)))) 
	(setf newValue (-
			 (+ (Dist pt aux2) (Dist pt aux1))
			 (Dist aux2 aux1))))
      (when (< newValue minValue)
	 (setf minValue newValue)
	 (setf m1 m2))
      (incf m2))
    m1))
	 

; Funcao que insere o novo ponto na posicao correta no tour
(defun InsertTour (pt tour pos mark)
  (if (= mark pos)
    (cons pt tour)
    (cons (car tour) (InsertTour pt (cdr tour) pos (+ mark 1)))))

(defmacro InsertInTour (pt tour pos)
  `(setf ,tour (InsertTour ,pt ,tour ,pos 0)))


; Loop principal de busca e insercao do ponto mais distante
(defun FurthestInsertion (pQueue tour)
  (let (newPt pos aux)
    (loop
      (if (not pQueue)
	(return))
      (setf aux (FurthestPoint pQueue tour))
      (setf pQueue (cdr aux))
      (setf newPt (car aux))
      (setf pos (BestPosition newPt tour)) 
      (InsertInTour newPt tour pos))
    tour))


; Imprime cada elemento de uma lista por linha
(defun PrintLine (x)
  (dolist (aux x)
    (format t "~a~%" aux)))


; Faz as chamadas de funcoes de leitura do arquivo e geracao do
; tour
(defun StartFurthestInsertion ()
  (format t "Forneca o nome, entre aspas, do input: ~%")
  (let* ((pts (reverse (ReadPoints (read))))
	(pQueue (MkListDist (car pts) (cdr pts)))
	(tour (cons (car pts) nil)))
    (setf tour (FurthestInsertion pQueue tour))
    (PrintLine tour)))


