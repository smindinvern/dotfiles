(require 'package)
(require 'use-package)

(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))


(use-package omnisharp
  :commands (csharp-mode)
  :config
  (define-key omnisharp-mode-map (kbd "C-c TAB") 'omnisharp-auto-complete)
  (define-key omnisharp-mode-map (kbd "C-c .") 'omnisharp-add-dot-and-auto-complete)
  (define-key omnisharp-mode-map (kbd "<f12>") 'omnisharp-go-to-definition))

(add-hook 'csharp-mode-hook 'omnisharp-mode)

;; C and C++ modes
(defun my-c-mode-hook-common ()
  (setq c-default-style "linux"
	c-basic-offset 4
	tab-width 4)
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
  (c-set-offset 'substatement-open '0)
  (c-set-offset 'block-open '+)
  (c-set-offset 'brace-list-open '+)
  (c-set-offset 'case-label '0)
  (c-set-offset 'label '0)
  (c-set-offset 'cpp-macro '[0])
  (c-set-offset 'func-decl-cont '0)
  (c-set-offset 'block-open '0)
  (column-number-mode t)
  )

(add-hook 'c-mode-hook (defun my-c-mode-hook ()
			 (my-c-mode-hook-common)
			 (c-set-offset 'inline-open '+)))
(add-hook 'c++-mode-hook (defun my-c++-mode-hook ()
			   (my-c-mode-hook-common)
			   (c-set-offset 'inline-open '0)))

(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)

(setq-default fill-column 80)
(electric-pair-mode)

;; Taken from https://emacs-doctor.com/emacs-strip-tease.html
(blink-cursor-mode 0)
(setq initial-scratch-message "")
(scroll-bar-mode 0)

;; Take from https://www.masteringemacs.org/article/introduction-to-ido-mode
(setq ido-enable-flex-matching t)
(ido-mode 1)
