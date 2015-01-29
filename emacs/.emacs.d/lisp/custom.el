(provide 'custom)

;; get the value of :directory from a project variable
(defun my-get-project-directory (project)
  (oref project :directory)
  )

;; grab the value of :include-path from a project variable and return it
;; as a list of strings all prepended with the project variable's :directory
(defun my-get-project-include-list (project)
  (let ((path
	 (my-get-project-directory project)
	 )
	)
    (mapcar (lambda (x)
	      (concat path
		      x)
	      )
	    (oref project
		  :include-path)
	    )
    )
  )

;; setup a dir-local class variable for an ede project to add
;; the project's :include-path to flycheck's include-path
(defun my-set-dir-local-ede-variable (class-var project)
  (dir-locals-set-class-variables
   'class-var
   `((c++-mode . (flycheck-clang-include-path . ,(my-get-project-include-list
						  project)
					      )
	       )
     )
   )
  )

(defun my-setup-flycheck-project-deps (project deps)
  (and (listp deps)
       (let ((include-list '())
	     (project-symbol (make-symbol
			      (concat
			       (symbol-name project)
			       "-dir-class"
			       )
			      )
			     ))
	 (progn
	   (mapcar (lambda (x)
		     (setq include-list (append include-list
					       (my-get-project-include-list (eval x))
					       )
			  )
		     )
		   (append deps `(,project)))
	   (dir-locals-set-class-variables project-symbol
	    `((c++-mode . ((flycheck-clang-include-path . ,include-list
							)
			   )
			)
	      )
	    )
	   (dir-locals-set-directory-class (my-get-project-directory (eval project))
					   project-symbol
					   )
	   )
	 )
       )
  )
						       

	       
	   
