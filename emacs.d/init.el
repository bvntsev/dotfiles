;;; init.el --- minimal vanilla Emacs config -*- lexical-binding: t; -*-

;; Keep built-in package initialization off; we do it ourselves.
(setq package-enable-at-startup nil)
(set-face-attribute 'default nil
                    :family "SF Mono"
                    :height 100)
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 40))
;; (set-face-attribute 'fixed-pitch nil :family "Hack")
;; (set-face-attribute 'variable-pitch nil :family "Cantarell")
(load-theme 'gruber-darker t)

(setq-default fill-column 80)
(global-display-fill-column-indicator-mode 1)
(set-face-foreground 'fill-column-indicator "#444444") ;; grey

(defun my-scroll-half-page-down ()
  (interactive)
  (scroll-up-command (/ (window-body-height) 2)))

(defun my-scroll-half-page-up ()
  (interactive)
  (scroll-down-command (/ (window-body-height) 2)))

(global-set-key (kbd "C-v") #'my-scroll-half-page-down)
(global-set-key (kbd "M-v") #'my-scroll-half-page-up)

(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (condition-case err
      (package-refresh-contents)
    (error
     (message "Package refresh failed: %S" err))))

(unless (package-installed-p 'use-package)
  (condition-case err
      (progn
        (unless package-archive-contents
          (package-refresh-contents))
        (package-install 'use-package))
    (error
     (message "Failed to install use-package: %S" err))))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t
      use-package-expand-minimally t)

;;; Basic editor behavior

(setq inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function #'ignore
      use-dialog-box nil
      visible-bell nil
      frame-title-format '("%b"))

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(column-number-mode 1)
(global-auto-revert-mode 1)
(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)

(setq recentf-max-saved-items 200
      recentf-max-menu-items 25)

;; Keep backups and auto-saves away from project trees.
(defvar my/emacs-backup-dir (expand-file-name "backups/" user-emacs-directory))
(defvar my/emacs-autosave-dir (expand-file-name "auto-save/" user-emacs-directory))

(make-directory my/emacs-backup-dir t)
(make-directory my/emacs-autosave-dir t)

(setq backup-directory-alist `(("." . ,my/emacs-backup-dir))
      auto-save-file-name-transforms `((".*" ,my/emacs-autosave-dir t))
      create-lockfiles nil)

;; Useful in programming buffers, but not forced everywhere.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;; Completion stack

(use-package vertico
  :init
  (vertico-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package consult
  :custom
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  (completion-in-region-function #'consult-completion-in-region))

;; (use-package corfu
;;   :init
;;   (global-corfu-mode 1)
;;   :custom
;;   (corfu-auto t)
;;   (corfu-auto-prefix 2)
;;   (corfu-cycle t)
;;   (corfu-preview-current nil)
;;   (corfu-on-exact-match nil))
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-show-numbers t)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-auto-commit nil
        company-auto-complete nil))
  
(use-package cape
  :init
  ;; Backends can be added later, per mode, after you approve the setup.
  nil)

;;; Languages / LSP

(use-package eglot
  :hook ((c-mode
          c++-mode
          python-mode
          sh-mode
          cmake-mode) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-report-progress nil))
(setq eglot-ignored-server-capabilities
      '(:inlayHintProvider
        :documentFormattingProvider
        :documentRangeFormattingProvider))
(defun my-insert-tab ()
  (interactive)
  (insert "\t"))


(add-hook 'c-mode-common-hook
          (lambda ()
            (setq-local c-syntactic-indentation nil)
            (setq-local electric-indent-inhibit t)
            (local-set-key (kbd "TAB") #'my-insert-tab)))
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (remove-hook 'post-self-insert-hook
                         #'eglot--post-self-insert-hook
                         t)))
(electric-indent-mode 1)

(setq whitespace-style '(face tabs tab-mark))
(global-whitespace-mode 1)
(setq backward-delete-char-untabify-method nil)

;; CMake editing support.
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(setq-default tab-width 4)
(setq-default indent-tabs-mode t)
(setq c-default-style "linux"
      c-basic-offset 4)

(add-hook 'c-mode-common-hook
          (lambda ()
            (setq indent-tabs-mode t
                  tab-width 4)))

;;; Org

(use-package org
  :ensure nil
  :custom
  (org-hide-emphasis-markers t)
  (org-startup-indented t)
  (org-return-follows-link t))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/sync/org/roam"))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode))

;; ===== Org-mode: custom TODO status =====
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)" "REVIEW(r)" "|" "DONE(d)" "CANCELED(c)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "green" :weight bold))
        ("IN-PROGRESS" . (:foreground "orange" :weight bold))
        ("WAITING" . (:foreground "yellow" :weight bold))
        ("REVIEW" . (:foreground "cyan" :weight bold))
        ("DONE" . (:foreground "gray" :weight bold))
        ("CANCELED" . (:foreground "red" :weight bold))))

(setq org-highest-priority ?A)
(setq org-lowest-priority ?E)
(setq org-default-priority ?C)

(defun my/org-skip-old-entries ()
  (let* ((scheduled (org-get-scheduled-time (point)))
         (cutoff (time-subtract
                  (encode-time 0 0 0  ; 00:00:00 current day
                               (nth 3 (decode-time))  ; day
                               (nth 4 (decode-time))  ; month
                               (nth 5 (decode-time))) ; year
                  (* 3 86400))))      ; 3 days ago
    (when (and scheduled (time-less-p scheduled cutoff))
      (org-end-of-subtree t))))

(setq org-agenda-custom-commands
      '(("c" "My Clean agenda"
         agenda ""
         ((org-agenda-skip-function #'my/org-skip-old-entries)
          (org-agenda-span 7)))))

(setq org-agenda-files
      '("~/sync/org/roam/daily"))


;;; Git

(use-package magit
  :commands (magit-status
             magit-dispatch
             magit-file-dispatch))

;;; Compilation / build output

(setq compilation-scroll-output t)

;;; Keybindings

;; Intentionally empty for now.
;; All custom bindings stay pending your approval.

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'scroll-left 'disabled nil)

;; For pdf-tools building
;; sudo dnf install \
;;    gcc gcc-c++ make automake \
;;    libpng-devel zlib-devel \
;;    poppler-devel poppler-glib-devel \
;;    ImageMagick
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install))

(defun trans ()
  (interactive)
  (let* ((origin (selected-window))
         (direction (completing-read
                     "Direction: "
                     '("en:ru" "ru:en")
                     nil t))
         (text (read-string "Text: "))
         (buffer (get-buffer-create "*trans*")))
    (with-current-buffer buffer
      (erase-buffer)
      (call-process "trans" nil buffer nil
                    direction text)
	  (goto-char (point-min)))
    (display-buffer-in-side-window
     buffer
     '((side . bottom)
       (window-height . 0.4)))
    (select-window origin)))
