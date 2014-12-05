(server-start)
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(require 'semantic)
(require 'semantic/ia)
(require 'semantic/bovine/c)
(require 'ecb)

(setq semantic-default-submodes '(global-semanticdb-minor-mode
				  global-semantic-idle-scheduler-mode
				  global-semantic-idle-completions-mode
				  global-semantic-idle-summary-mode
				  global-semantic-decoration-mode)
      )
(setq semantic-idle-scheduler-idle-time 0.5)
(setq semantic-idle-work-parse-neighboring-files-flag t)
(setq semantic-complete-inline-analyzer-idle-displayor-class 'semantic-displayor-ghost)

(defun my-semantic-hook ()
  (setq semantic-idle-scheduler-idle-time 0.5)
  (setq semantic-idle-work-parse-neighboring-files-flag t)
  (setq semantic-complete-inline-analyzer-idle-displayor-class 'semantic-displayor-ghost)
  (setq semantic-imenu-auto-rebuild-directory-indexes t)
  (setq semantic-imenu-index-directory t)
  (setq semantic-imenu-sort-bucket-function 'semantic-sort-tags-by-name-increasing-ci)
  (setq semantic-imenu-summary-function 'semantic-format-tag-uml-prototype)
  (setq semanticdb-find-default-throttle '(project system recursive local))
;  (semanticdb-global-mode t)
  )

(add-hook 'semantic-init-hooks 'my-semantic-hook)

(defun my-c-mode-hook-common ()
  (setq c-default-style "linux"
	c-basic-offset 8
	tab-width 8
	indent-tabs-mode t)
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
  (semantic-mode t)
  (global-set-key (kbd "C-c , d") 'semantic-ia-fast-jump)
  (c-set-offset 'substatement-open '0)
  (c-set-offset 'block-open '+)
  (c-set-offset 'brace-list-open '+)
  (c-set-offset 'case-label '0)
  (c-set-offset 'label '0)
  (c-set-offset 'cpp-macro '[0])
  (c-set-offset 'func-decl-cont '0)
  (c-set-offset 'block-open '0)
  (column-number-mode t)
  (global-semanticdb-minor-mode t)
  )

(add-hook 'c-mode-hook (defun my-c-mode-hook ()
			 (my-c-mode-hook-common)
			 (c-set-offset 'inline-open '+)
			 )
	  )

(add-hook 'c++-mode-hook (defun my-c++-mode-hook ()
			   (my-c-mode-hook-common)
			   (c-set-offset 'inline-open '0)
			   )
	  )

(require 'compile)
(setq compilation-disable-input nil)
(setq compilation-scroll-output t)

;; (autoload 'smart-tabs-mode "smart-tabs-mode"
;; "Intelligently indent with tabs, align with spaces!")
;; (autload 'smart-tabs-mode-enable "smart-tabs-mode")
;; (autoload 'smart-tabs-advice "smart-tabs-mode")
;; (autoload 'smart-tabs-insinuate "smart-tabs-mode")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(create-lockfiles nil)
 '(ecb-compile-window-temporally-enlarge (quote both))
 '(ecb-enlarged-compilation-window-max-height 0.3)
 '(ecb-layout-name "left-methods-analyze")
 '(ecb-layout-window-sizes nil)
 '(ecb-options-version "2.40")
 '(ecb-scroll-other-window-scrolls-compile-window nil)
 '(ecb-windows-width 0.25)
 '(fci-rule-column 80)
 '(global-linum-mode t)
 '(inhibit-startup-screen t)
 '(standard-indent 8))

(setq-default fill-column 80)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
