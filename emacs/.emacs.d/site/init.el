(require 'package)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load-library "custom")
(load-library "ede-projects")

(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(require 'use-package)

;; setup ede projects
(use-package ede-projects
  :commands (c++-mode c-mode)
  :config
  ;; remove ede keybinding (C-c .) that interferes with ecb keybindings
  (assq-delete-all 'ede-minor-mode minor-mode-map-alist))

;; emacs code browser
(use-package ecb
  :commands (c++-mode c-mode)
  :config
  (setq-default ecb-compile-window-temporally-enlarge (quote both))
  (setq-default ecb-enlarged-compilation-window-max-height 0.3)
  (setq-default ecb-layout-name "left-methods-analyze")
  (setq-default ecb-layout-window-sizes nil)
  (setq-default ecb-options-version "2.40")
  (setq-default ecb-scroll-other-window-scrolls-compile-window nil)
  (setq-default ecb-windows-width 0.25))

;; structured-haskell-mode
(use-package shm
  :commands (haskell-mode))
(add-hook 'haskell-mode-hook 'structured-haskell-mode)

;; semantic bovinator
(defun my-semantic-hook ()
  (setq semantic-idle-scheduler-idle-time 0.5)
  (setq semantic-idle-work-parse-neighboring-files-flag t)
  (setq semantic-complete-inline-analyzer-idle-displayor-class 'semantic-displayor-ghost)
  (setq semantic-imenu-auto-rebuild-directory-indexes t)
  (setq semantic-imenu-index-directory t)
  (setq semantic-imenu-sort-bucket-function 'semantic-sort-tags-by-name-increasing-ci)
  (setq semantic-imenu-summary-function 'semantic-format-tag-uml-prototype)
  (setq semanticdb-find-default-throttle '(project system recursive local))
;;  (semanticdb-global-mode t)
  )

(use-package semantic
  :commands (c++-mode c-mode)
  :config
  (use-package semantic/ia)
  (use-package semantic/bovine/c)
  (use-package compile)
  ;; why are these here and not in `my-semantic-hook'?
  (setq semantic-default-submodes '(global-semanticdb-minor-mode
				    global-seantic-idle-scheduler-mode
				    global-semantic-idle-completions-mode
				    global-semantic-idle-summary-mode
				    global-semantic-stickyfunc-mode
				    ;; global-semantic-decoration-mode
				    ))
  (setq semantic-idle-scheduler-idle-time 0.5)
  (setq semantic-idle-work-parse-neighboring-files-flag t)
  (setq semantic-complete-inline-analyzer-idle-displayor-class 'semantic-displayor-ghost)
  (add-hook 'semantic-init-hooks 'my-semantic-hook)
  )

(defun my-flycheck-lang-standard ()
  (if (string= (symbol-name major-mode) "c++-mode") "c++14" "c11"))

;; flycheck
(use-package flycheck
  :commands (flycheck-mode)
  :init
  (defun my-lang-standard-hook ()
    (setq-local flycheck-clang-language-standard (my-flycheck-lang-standard))
    (setq-local flycheck-gcc-language-standard (my-flycheck-lang-standard))
    )
  (add-hook 'c-mode-hook 'my-lang-standard-hook)
  (add-hook 'c++-mode-hook 'my-lang-standard-hook)
  :config
  (flycheck-select-checker 'c/c++-clang)
  (setq-default flycheck-clang-args '("-pedantic" "-fstrict-aliasing"))
  (setq-default flycheck-clang-standard-library nil)
  (setq-default flycheck-gcc-args '("-fstrict-aliasing"))
  )

;; C and C++ modes
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
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)

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
 '(display-time-format "%FT%T%z")
 '(enable-dir-local-variables t t)
 '(fci-rule-character-color "#192028")
 '(fci-rule-column 80)
 '(fringe-mode 6 nil (fringe))
 '(global-linum-mode t)
 '(hl-sexp-background-color "#1c1f26")
 '(inhibit-startup-screen t)
 '(linum-format "%3i")
 '(safe-local-variable-values
   (quote
    ((flycheck-gcc-include-path
      (my-get-include-list test-genie-project))
     (flycheck-clang-include-path
      (my-get-include-list test-genie-project)))))
 '(scroll-error-top-bottom t)
 '(standard-indent 8)
)

(setq-default fill-column 80)

