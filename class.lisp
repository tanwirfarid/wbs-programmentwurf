(load "versionspace_basic.lisp")

(setq  *path-to-training-data* "Wohnungskartei_D2.lisp")
(setq *path-to-testdata* "Wohnungskartei_TestD2.lisp")
(setq input (get-examplelist (load-exampleset *path-to-training-data*)))


(defun split-lists (inputlist)
    (let ((bplus NIL) (bminus NIL)))
    (aqsplit inputlist bplus bminus)
    (print (cadar inputlist))
)

(defun aqsplit (inputlist bplus bminus)
  (COND ((NULL inputlist) NIL) 
	((EQUAL "ja" (CADAR inputlist))
		(APPEND bplus (CAR inputlist)) 	
	)
	(T (APPEND bminus (CAR inputlist))
    )
  )
  (aqsplit (CDR inputlist) bplus bminus)
)
