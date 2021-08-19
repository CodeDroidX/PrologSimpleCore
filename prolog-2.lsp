;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Проверка является ли конструкция (_ _) или (_ _ _ ...) переменной
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun is-var (x)
  (and (listp x) (eq '? (car x))))
   
(defun vals (x env)
  (if (is-var x)
      ;; Если x - переменная
      (let ((bind (assoc x env))) ;; пробуем получить значение из окружения
           ;; Если значения нет - вернем x  
           (if (null bind) x (vals (cadr bind) env))) 
      ;; Иначе возвращаем x     
      x))       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Унификация
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun unify (x y &optional (env nil))
  (let ((x (vals x env))
        (y (vals y env)))
   (cond ((is-var x) (cons (list x y) env))
         ((is-var y) (cons (list y x) env))
         ((or (atom x) (atom y)) (and (eq x y) env))
         (t (let ((new-env (unify (car x) (car y) env)))
              (and new-env (unify (cdr x) (cdr y) new-env)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Старт Пролога
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prolog (programm)
  (loop
     (printsline "Запрос:")
	 (setq *flgStop* nil)
     (let ((predicates (read)))
           (when (eq predicates 'конец) (return 'конец))
           (prove (list (change-names predicates '(0))) '(nil) programm 1))))
           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Замена имен term - предложение; level - уровень в виде списка (lv)           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;           
(defun change-names (term level)
  (cond ((is-var term) (append term level))
        ((atom term) term)
        (t (cons (change-names (car term) level) 
                 (change-names (cdr term) level)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Показать связи переменных
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;           
;;(defun show-binds (binds)
;;  (let ((r nil))
;;    (iter (for a in binds) 
;;      (when (null a) (return r))
;;      (unless (is-var (cadr a))
;;         (collecting (list (cadr (car a)) (cadr a)) into r)))))

(defun show-binds (binds)
  (let ((r nil))
    (iter (for pair in binds) 
      (when (null pair) (return r))
      (when (= 0 (nth 2 (car pair))) 
            (collecting (list (list '? (cadr (car pair))) (vals (car pair) binds)) into r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Головная функция вычисления
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;           
(defun prove (predicates binds programm level)
  (cond (*flgStop* nil)
        ((null predicates)
              (let ((curr-binds (show-binds binds)))
                   (when curr-binds (printline (show-binds binds))) (printline 'YES))
              (printsline "Еще? (д/н)")
              (if (eq (read) 'н) (setq *flgStop* t) nil))
        (t (prove-every programm programm (cdr predicates) (car predicates) binds level))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Обход всех вариантов
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;           
(defun prove-every (rest-prg prg rest-prev prev binds level)
 (if *flgStop* nil  
  (if (null rest-prg)  nil  
      (let* ((theorem (change-names (car rest-prg) (list level)))
             (new-binds (unify prev (car theorem) binds))) 
             (cond ((null new-binds) (prove-every (cdr rest-prg) prg rest-prev prev binds level))
                   ((prove (append (cdr theorem) rest-prev) new-binds prg (+ level 1)) nil)
                   (t (prove-every (cdr rest-prg) prg rest-prev prev binds level)))))))

;;
;; Правила и факты
;;

(setq *prg-0* 
      '(((cat Lion))
	    ((cat Tiger))
		((twin_cats (? X) (? Y)) (cat(? X)) (cat (? Y)))))

(setq *prg-1* 
       '(((father Michail  Vladimir))
         ((father Olga Vladimir))
         ((father Nikolay Semen))
         ((father Valentina Semen))
         ((father Boris Nikolay))
         ((father Alexey Nikolay))
         ((father Nina Nikolay))
		 ((mother Boris Olga))
         ((mother Alexey Olga))
         ((mother Nina Olga))
         ((mother Michail Anna))
         ((mother Olga Anna))
         ((mother Nikolay Ekaterina))
         ((mother Valentina Ekaterina))
		 ((grandfather(? X) (? Y)) (father (? X) (? Z)) (father (? Z) (? Y)))
		 ((grandfather(? X) (? Y)) (mother (? X) (? Z)) (father (? Z) (? Y)))))
  
(setq *prg-2*
      '(((diff red green))
        ((diff red blue))
        ((diff green red))
        ((diff green blue))          
        ((diff blue red))   
        ((diff blue green))
        ((coloring (? Alabama) (? Missisippi) (? Georgia) (? Tennessee) (? Florida))
         (diff (? Missisippi) (? Alabama))
         (diff (? Missisippi) (? Tennessee))
         (diff (? Alabama) (? Tennessee)) 
         (diff (? Alabama) (? Missisippi))
         (diff (? Alabama) (? Georgia))
         (diff (? Alabama) (? Florida))
         (diff (? Georgia) (? Florida))
         (diff (? Georgia) (? Tennessee)))))

(prolog *prg-2*)
 