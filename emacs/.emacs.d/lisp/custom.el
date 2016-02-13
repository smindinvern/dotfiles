(provide 'custom)
(require 'cl-lib)

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

(defun my-get-project-definitions (project)
  (let ((defs (oref project :spp-table))
	(process-def (lambda (list)
		       (let ((head (car list))
			     (tail (cdr list)))
			 (if (string= tail "") head
			   (concat head "=" tail))
			 )
		       )
		     ))
    (mapcar process-def defs)
    )
  )


;; setup a dir-local class variable for an ede project to add
;; the project's :include-path and :spp-table to flycheck's
;; checker configuration
(defun my-set-dir-local-project-variables (class-var include-list spp-table)
  (dir-locals-set-class-variables
   class-var
   `((c++-mode . ((flycheck-clang-include-path . ,include-list)
		  (flycheck-clang-definitions ,@spp-table))
	       )
     )
   )
  )

(defun my-setup-flycheck-project-deps (project deps)
  (and (listp deps)
       (let ((include-list (cl-reduce #'append (mapcar (lambda (x) (my-get-project-include-list (eval x)))
						       (append deps `(,project))
						       )
				      )
			   )
	     (defs (my-get-project-definitions (eval project)))
	     (project-symbol (make-symbol
			      (concat
			       (symbol-name project)
			       "-dir-class"
			       )
			      )
			     ))
	 (progn
	     (my-set-dir-local-project-variables project-symbol include-list defs)
	     (dir-locals-set-directory-class (my-get-project-directory (eval project))
					     project-symbol
					     )
	     )
	 )
       )
  )
						       

	       
	   
