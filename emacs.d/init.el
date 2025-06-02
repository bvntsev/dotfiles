;; ===== Optimization =====
(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)
(setq native-comp-async-report-warnings-errors 'silent)  ; можно изменить на nil для дебага
(setq warning-minimum-level :error)
(setq gc-cons-threshold (* 50 1000 1000))  ; 50 MB
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 2 1000 1000))))  ; 2 MB после запуска
(setq read-process-output-max (* 1024 1024))  ; 1 MB для асинхронных процессов
(prefer-coding-system 'utf-8)

;; ===== Startup Cleanup =====
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; ===== Font =====
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 130)

;; Win+x как Meta+x
(when (eq system-type 'windows-nt)
  (define-key key-translation-map (kbd "s-x") (kbd "M-x")))

;; ===== Package Manager =====
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; ===== Interface =====
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq visible-bell t)

;; Линии номеров
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Темы и иконки
(use-package doom-themes
  :config 
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config))  ; визуальный звонок в стиле doom

(use-package all-the-icons
  :if (display-graphic-p))

;; Моделайн
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon t
        doom-modeline-major-mode-color-icon t))

(use-package which-key
  :init (which-key-mode))

;; Сохранение позиций в файлах
(save-place-mode 1)
(setq save-place-file (expand-file-name "places" user-emacs-directory))

;; ===== Completion & Navigation =====
(use-package vertico
  :init (vertico-mode))

(use-package consult)

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        orderless-component-separator #'orderless-escapable-split-on-space))

(use-package marginalia
  :init (marginalia-mode))

;; ===== Evil Mode =====
(use-package evil
  :init
  (setq evil-want-keybinding nil)  ; для корректной работы с другими пакетами
  :config
  (evil-mode 1)
  (dolist (mode '(magit-status-mode magit-popup-mode magit-revision-mode
                magit-diff-mode magit-log-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; Навигация по окнам в evil
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; ===== LSP =====
(use-package eglot
  :hook ((python-mode c-mode c++-mode rust-mode go-mode) . eglot-ensure)
  :config
  (setq eglot-autoshutdown t
        eglot-ignored-server-capabilities '(:signatureHelpProvider))
  (add-to-list 'eglot-server-programs
               '(makefile-mode . ("bash-language-server" "start")))

(use-package dap-mode
  :after eglot
  :config
  (dap-auto-configure-mode))

;; ===== Autocompletion =====
(use-package company
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.3)  ; более плавное автодополнение
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-y") #'company-complete-selection))

;; ===== Git =====
(use-package magit
  :bind ("C-x g" . magit-status))

;; ===== Tree View =====
(use-package treemacs
  :bind ("C-x t" . treemacs)
  :config 
  (treemacs-git-mode 'extended))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-evil :after (treemacs evil))

;; ===== Treesitter =====
(use-package tree-sitter
  :config 
  (global-tree-sitter-mode)
  (tree-sitter-require 'tsx)  ; пример для конкретного языка
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-mode . tsx)))

(use-package tree-sitter-langs
  :after tree-sitter)

;; ===== Terminal =====
(use-package vterm
  :if (not (eq system-type 'windows-nt)))  ; на Windows может работать плохо

;; ===== Bufferline =====
(use-package centaur-tabs
  :init (centaur-tabs-mode 1)
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-set-icons t
        centaur-tabs-show-new-tab-button nil)
  (centaur-tabs-headline-match))

;; ===== Makefile support =====
(defun my-make-compile ()
  (interactive)
  (compile "make -k"))
(global-set-key (kbd "<f5>") 'my-make-compile)

;; ===== Tabs & Indentation =====
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default electric-indent-inhibit nil)
(electric-pair-mode 1)  ; авто-закрытие скобок

(add-hook 'prog-mode-hook
          (lambda ()
            (setq tab-width 4)))

;; ===== Дополнительные улучшения =====
;; Автосохранение
(use-package super-save
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t
        super-save-idle-duration 5))

;; Проверка орфографии
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))
