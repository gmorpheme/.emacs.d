;;; init.el -- summary

;; -*- lexical-binding: t -*-

;;
;; use-package available by deafult since 29 and archives initialised
;; in early-init.el
;;
(require 'use-package)

;;
;; Ensure other files get loaded if present, including customisation
;; (kept out of this file), encrypted secrets and a file specific to
;; the host we're running on.
;;
(setq custom-file "~/.emacs.d/.emacs-custom"
      gh/secrets-file "~/.emacs.d/.emacs-secrets"
      gh/system-config (concat user-emacs-directory system-name ".el"))

(defun gh/eval-extra-init-files ()
  (load custom-file 'no-error)
  (load gh/secrets-file 'no-error)
  (load gh/system-config 'no-error))

(add-hook 'after-init-hook 'gh/eval-extra-init-files)

;;
;; Basic packages
;;
(use-package paradox :ensure t :defer t)
(use-package better-defaults :ensure t :defer t)
(use-package idle-highlight-mode :ensure t :defer t)
(use-package rainbow-delimiters :ensure t :defer t)
(use-package ess :ensure t :defer t)
(use-package solarized-theme :ensure t :defer t)
(use-package tao-theme :ensure t :defer t)
(use-package material-theme :ensure t :defer t)
(use-package color-theme-sanityinc-tomorrow :ensure t :defer t)
(use-package nimbus-theme :ensure t :defer t)
(use-package zenburn-theme :ensure t :defer t)
(use-package twilight-bright-theme :ensure t :defer t)
(use-package base16-theme :ensure t :defer t)
(use-package plan9-theme :ensure t :defer t)
(use-package bind-key :ensure t)

;;;
;;; Small and miscellaneous customisations
;;;
(use-package emacs
  :init

  (setq inhibit-splash-screen t)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  (pixel-scroll-precision-mode 1)
  (blink-cursor-mode 0)
  (auto-compression-mode 1)
  (setq visible-bell nil)
  (setq ring-bell-function 'ignore)
  (delete-selection-mode 1)
  (setq sentence-end-double-space nil)
  (setq help-window-select t)
  (set-default-coding-systems 'utf-8)
  (set-default 'indicate-empty-lines t)

  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'text-mode-hook 'turn-on-flyspell)

  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq confirm-kill-processes nil)

  (defalias 'auto-tail-revert-mode 'tail-mode)

  (define-key global-map (kbd "C-+") 'global-text-scale-adjust)
  (define-key global-map (kbd "C--") 'global-text-scale-adjust)

  (setq disabled-command-function nil)
  (setq require-final-newline t)

  ;; ;; prefer side-by-side window splits if the window is wide
  ;; (setq split-height-threshold nil)
  ;; (setq split-width-threshold 160)
  ;;
  ;; backups already in .emacs.d/backups -
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
  (setq version-control t)
  (setq delete-old-versions t)

  (setq next-error-message-highlight t)

  ;; Window manager configuration

  (setq window-sides-slots '(0 0 1 1))

  (setq display-buffer-alist
	'(
	  ;; - compilation in right hand side bar
	  ("\\*Compilation\\*\\|\\*rustic-compilation\\*"
	   display-buffer-in-side-window
	   (side . right)
	   (slot . 0)
	   (window-width . 80)
	   (window-parameters
	    (no-delete-other-windows . t)))
	  ;; - various help windows in right hand side bar
	  ("\\*info\\*\\|\\*Help\\*\\|\\*Shortdoc.*\\*"
	   display-buffer-in-side-window
	   (side . right)
	   (slot . 0)
	   (window-width . 80)
	   (window-parameters
	    (no-delete-other-windows . t)))
	  ;; - shells below
	  ("\\*e?shell\\*" display-buffer-in-direction
	   (direction . bottom)
	   (window . root)
	   (window-height . 0.3)))))

;;
;; MacOS specifics
;;
(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil)
  ;; I resolve the UK MBP `#` character problem by putting it on cmd-3
  ;; so alt-3 is still available for command prefix numerics
  (bind-key "s-3" (kbd "#") 'key-translation-map))

;;
;; Developer fonts & modeline
;;
;; Call (all-the-icons-install-fonts) if fonts aren't installed
(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :ensure all-the-icons
  :hook (after-init . doom-modeline-mode))

;;
;; Beacon mode for keeping track of cursor
;;
(use-package beacon
  :ensure t
  :init (beacon-mode 1))

;;
;; which-key
;;
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;
;; Whitespace cleanup mode
;;
(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode))

;;
;; Markdown
;;
(use-package markdown-mode
  :ensure t
  :mode (("\\.md$" . gfm-mode)
         "\\.apib$"))

;;; Vertico / Consult

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-count 16))

(use-package recentf
  :ensure t
  :defer t
  :init
  (recentf-mode 1))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :ensure consult-projectile
  :bind (("M-y" . consult-yank-pop)))

;;
;; Window configuration
;;
(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

;;
;; undo for window config C-c <- and C-c ->
;;
(use-package winner
  :defer t
  :init (winner-mode 1))

;; quick access to occur from interactive search
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; company mode wherever - unless and until it gets slow
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :defer 10
  :diminish company-mode
  :init
  (setq
   company-tooltip-align-annotations t
   company-idle-delay 0.05)
  :config (global-company-mode))

;; snippets
(use-package yasnippet
  :ensure t
  :defer 30
  :diminish yas-minor-mode
  :commands yas-hippie-try-expand
  :config
  (setq yas-wrap-around-region t)
  (yas-global-mode 1))

;; projectile mode everywhere
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :config

  (setq projectile-completion-system 'default)
  (setq projectile-switch-project-action #'projectile-commander)
  (setq projectile-create-missing-test-files t)

  ;; rejig to allow projects in monorepos to take preference over
  ;; outer .git
  (setq projectile-project-root-functions '(projectile-root-local
					    projectile-root-marked
					    projectile-root-top-down
					    projectile-root-bottom-up
					    projectile-root-top-down-recurring))

  (def-projectile-commander-method ?S
    "Open a *shell* buffer for the project."
    (projectile-run-shell))

  (def-projectile-commander-method ?F
    "Refresh: fetch from git and go to magit status"
    (call-interactively #'magit-fetch-from-upstream)
    (projectile-vc))

  (projectile-global-mode))

(defun gh/kill-current-buffer ()
  "(just to pop this on a keychord later "
  (interactive)
  (kill-buffer nil))

;;; Make it easy to maintain desktop set-ups in projects

(defun toggle-window-dedicated ()
  "Set window as dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: dedicated window"
     "%s: undedicated window")
   (current-buffer)))


;;
;; Dired
;;
(use-package dired
  :init
  (setq dired-listing-switches "-alh")
  (setq dired-kill-when-opening-new-dired-buffer t))

;;
;; Git
;;
(use-package magit
  :ensure t
  :ensure git-timemachine
  :bind (("C-c g" . magit-status)
         ("C-c b" . magit-blame)
         ("C-c t" . git-timemachine))
  :init
  (setq magit-restore-window-configuration t
	magit-log-auto-more t
	magit-status-buffer-switch-function 'switch-to-buffer))

;;
;; Auto commit for directories with dir local vars to specify it
;;
(use-package git-auto-commit-mode :ensure t)

;;;
;;; General Programming Stuff
;;;
(defun gh/recompile-comint ()
  "Rerun a compilation in comint-mode for interaction"
  (interactive)
  (setf (elt compilation-arguments 1) t)
  (recompile))

(use-package comint
  :bind ("<f5>" . gh/recompile-comint)
  :init
  (setq compilation-ask-about-save nil
	compilation-scroll-output 'next-error
	compilation-skip-threshold 2))

(use-package edit-indirect :ensure t :defer t)

;;
;; lambdas and todos
;;
(defun gh/local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun gh/local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode nil))

(defun gh/set-comment-columns ()
  (set (make-local-variable 'comment-fill-column) 80))

(defun gh/turn-on-hl-line-mode ()
  (when (> (display-color-cells) 8)
    (hl-line-mode t)))

(defun gh/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|QQ\\)"
          1 font-lock-warning-face t))))

(defun gh/truncate-lines ()
  (setq truncate-lines t))

(add-hook 'prog-mode-hook 'gh/local-column-number-mode)
(add-hook 'prog-mode-hook 'gh/local-comment-auto-fill)
(add-hook 'prog-mode-hook 'gh/set-comment-columns)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'prog-mode-hook 'gh/add-watchwords)
(add-hook 'prog-mode-hook 'idle-highlight-mode)
(add-hook 'prog-mode-hook 'gh/truncate-lines)

(defun gh/prog-mode-hook ()
  (run-hooks 'prog-mode-hook))

;;
;; Whitespace butler
;;
(use-package ws-butler
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'ws-butler-mode))

;;
;; Smartparens
;;
(use-package smartparens
  :ensure t
  :init
  (progn
    (require 'smartparens-config)
    (require 'smartparens-rust)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1))
  :config
  (sp-use-smartparens-bindings))

;;
;; iedit
;;
(use-package iedit :defer t)

;;
;; Language server protocol
;;
(use-package eglot
  :ensure t
  :init
  ;; don't lose flymake errors in the minibuffer behind eldoc
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose))

;;
;; Rust (using LSP)
;;
(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("C-c C-c r" . eglot-rename))
  :commands rustic
  :config
  (setq rustic-format-on-save t
	rustic-lsp-client 'eglot)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))


;; Kotlin
(use-package kotlin-mode
  :ensure t)

;; Lispy
(use-package lispy
  :ensure t
  :init (setq lispy-compat '(edebug cider)))

;; Fennel
(use-package fennel-mode
  :ensure t)

;;
;; Lisp modes
;;
(dolist (mode '(scheme emacs-lisp lisp clojure racket fennel))
  (let ((hook (intern (concat (symbol-name mode) "-mode-hook"))))
    (add-hook hook 'gh/prog-mode-hook)
    (add-hook hook (lambda () (lispy-mode 1)))
    (add-hook hook 'rainbow-delimiters-mode)))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;;
;; Haskell mode
;;
(use-package haskell-mode
  :ensure t
  :ensure hindent
  :mode (("\\.hs" . haskell-mode)
         ("\\.fr" . haskell-mode))
  :init
  (setq haskell-compile-cabal-build-command "stack build --test --fast --file-watch --copy-bins --exec 'hlint .'")
  (add-hook 'haskell-mode-hook 'gh/prog-mode-hook)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'hindent-mode))

;;;
;;; Elm mode
;;;
(use-package elm-mode
  :ensure t
  :mode "\\.elm")

;;
;; OCaml
;;
(use-package tuareg
  :ensure t
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode)))

;;
;; Javascript
;;
;; TODO: steal from https://github.com/CSRaghunandan/.emacs.d/blob/master/setup-files/setup-js.el
(use-package js2-mode
  :ensure t
  :ensure json-mode
  :ensure js-comint
  :ensure js2-refactor
  :ensure xref-js2
  :mode "\\.js$"
  :init
  (progn
    (add-hook 'inferior-js-mode-hook 'ansi-color-for-comint-mode-on)
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (js2r-add-keybindings-with-prefix "C-c C-r")
    (setenv "NODE_NO_READLINE" "1")
    (setq tab-width 2)
    (setq js-indent-level 2)
    (setq js2-highlight-level 3)
    (setq inferior-js-program-command "node --interactive"))
  :config
  (progn
    (font-lock-add-keywords
     'js-mode `(("\\(function *\\)("
                 (0 (progn (compose-region (match-beginning 1)
                                           (match-end 1) "\u0192")
                           nil)))))))

(use-package web-mode
  :ensure t
  :mode (("\\.jsx$" . web-mode)))

;;
;; TypeScript mode
;;
(use-package typescript-mode
  :ensure t
  :mode "\\.ts$")

;;
;; Python
;;
(use-package python
  :ensure t
  :commands (run-python)
  :mode (("\\.py$" . python-mode))
  :init (progn
	  (setq python-shell-interpreter "python3")
	  (add-hook 'python-mode-hook 'gh/prog-mode-hook)))

;;
;; Swift
;;
(use-package swift-mode
  :ensure t
  :mode (("\\.swift$" . swift-mode)))

;;
;; CSS mode
;;
(use-package css-mode
  :ensure rainbow-mode
  :mode "\\.css"
  :init
  (progn
    (add-hook 'css-mode-hook 'gh/prog-mode-hook)
    (add-hook 'css-mode-hook 'rainbow-mode)))


;;
;; Org Mode
;;

;; HACK: `org-directory', `org-default-notes-file', `gh/refile-file' are
;; different on my different systems - ensure they're set in
;; `gh/system-config'
(use-package org
  :init
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  (add-hook 'org-mode-hook 'turn-on-visual-line-mode)

  (setq org-adapt-indentation nil)
  (setq org-list-description-max-indent 5)
  (setq org-hide-emphasis-markers t)

  (setq org-startup-indented t)
  (setq org-cycle-separator-lines 0)
  
  (setq org-insert-heading-respect-content t)
  (setq org-return-follows-link t)

  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-use-speed-commands t)

  (setq org-todo-keywords
        (quote ((sequence "NEXT(n)" "TODO(t)" "PROJECT(p)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "SOMEDAY(s@/!)" "DEFERRED(f@/!)" "|" "CANCELLED(c@/!)"))))

  (setq org-catch-invisible-edits 'show-and-error)
  ;; Clock settings

  (setq org-clock-into-drawer t)
  (setq org-clock-continuously t)
  (setq org-clock-in-resume t)
  (setq org-clock-persist t)
  (setq org-clock-persist-query-resume nil)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-report-include-clocking-task t)
  (setq org-clock-auto-clock-resolution 'when-no-clock-is-running)
  
  (setq org-drawers '("PROPERTIES" "LOGBOOK" "CLOCK" "RESULTS"))

  ;; exclude certain tags from inheritance
  (setq org-tags-exclude-from-inheritance '("PROJECT"))
  (setq org-stuck-projects '("+TODO=\"PROJECT\"" ("NEXT") nil "\\<IGNORE\\>"))
  
  ;; Agenda settings

  (setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                   ;; Indent todo items by level to show nesting
                                   (todo . " %i %-12:c%l")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c")))

  ;; Just one day in the agenda please
  (setq org-agenda-span 'day)

  ;; Modal effect, full window but restore windows on q
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-restore-windows-after-quit t)

  (setq org-agenda-compact-blocks t)
  (setq org-agenda-custom-commands
        '(("-" "Grand Unified Agenda"
           ((agenda ""
                    ((org-agenda-ndays 1)
                     (org-agenda-overriding-header "Today")))
            (tags "FLAGGED"
                  ((org-agenda-overriding-header "Flagged")
                   (org-show-context-detail 'lineage)))
            (tags "refile|tidy"
                  ((org-agenda-overriding-header "To Refile / Tidy")))
            (tags-todo "PRIORITY=\"A\"-SCHEDULED={.+}"
                       ((org-agenda-overriding-header "Unscheduled High Priority")))
	    (org-agenda-list-stuck-projects)))))

  ;; Capture and refile settings
  (setq org-capture-templates
        '(("s" "start day" entry (function org-journal-find-location)
	   (file "templates/template-day.org")
	   :clock-in t
	   :unnarrowed t)

	  ("t" "todo" entry (file gh/refile-file)
	   "* TODO %?\n")

	  ("r" "respond" entry (file gh/refile-file)
	   "* NEXT Respond to %? \nSCHEDULED: %t\n%U\n%a\n"
	   :clock-in t
	   :clock-resume t
	   :immediate-finish t)

	  ("n" "note" entry (file gh/refile-file)
	   "* %? :NOTE:\n%U\n\n"
	   :clock-in t
	   :clock-resume t)

	  ("i" "interruption" entry (file gh/refile-file)
	   "* %? :INTERRUPTION:\n\n\n"
	   :clock-resume t
	   :clock-in t)

	  ("m" "meeting" entry (file gh/refile-file)
	   "* MEETING %? :MEETING:\n%U"
	   :clock-in t
	   :clock-resume t)))

  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-allow-creating-parent-nodes (quote confirm))

  ;; Archival
  (setq org-archive-location "%s_archive::datetree/")

  ;; Org babel
  (org-babel-do-load-languages
   (quote org-babel-load-languages)
   (quote ((emacs-lisp . t)
           (js . t)
           (clojure . t)
           (ruby . t)
           (python . t)
           (shell . t)
           (dot . t))))


  :config
  ;; by this point host specific .el should have run
  (require 'org-crypt)
  (require 'epa-file)
  (epa-file-enable)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq epa-pinentry-mode 'loopback))

(use-package ob-restclient
  :ensure t)

(use-package org-babel
  :no-require
  :after ob-restclient
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)
     (clojure . t)
     (ruby . t)
     (python . t)
     (shell . t)
     (dot . t)
     (restclient . t)))
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
	org-edit-src-content-indentation 0
	org-babel-clojure-backend 'cider))

;; Global key bindings for org stuff
(bind-key "\C-cl" 'org-store-link)
(bind-key "\C-ca" 'org-agenda)
(bind-key "<f12>" 'org-agenda)
(bind-key "\C-cc" 'org-capture)

(defun journal-file-insert (time)
  "Insert new journal file contents for a journal file for TIME."
  (with-temp-buffer
    (insert "#+TITLE: ")
    (insert (format-time-string "%A %x" time))
    (insert "\n#+STARTUP: showall\n* ")
    (insert (format-time-string "%A %d" time))
    (insert " Journal")
    (buffer-string)))

(defun org-journal-find-location ()
  "Function to locate today's journal for capture template."
  (org-journal-new-entry t))

;;
;; org-journal to manage daily org files
;;
(use-package org-journal
  :ensure t
  :init
  (setq org-journal-dir (concat org-directory "/journal/"))
  (setq org-journal-file-format "%Y%m%d.org")
  (setq org-journal-date-prefix " ")
  (setq org-journal-date-format 'journal-file-insert)
  (setq org-journal-enable-agenda-integration t)
  (setq org-journal-carryover-items ""))


(defun get-journal-file-today ()
  "Return filename for today's journal entry."
  (let ((daily-name (format-time-string "%Y%m%d")))
    (expand-file-name (concat org-journal-dir daily-name))))

(defun journal-file-today ()
  "Create and load a journal file based on today's date."
  (interactive)
  (find-file (get-journal-file-today)))

(global-set-key (kbd "C-c f j") 'journal-file-today)

;;
;; Check for modifications to open files.
;;
(require 'autorevert)
(global-auto-revert-mode t)

;;
;; Horizontal line hightlighting in list modes
;;
(dolist (mode '(dired ibuffer package-menu process-menu org-agenda))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
            'gh/turn-on-hl-line-mode))

;;
;; windows reg files are UTF-16 little endian - please read them properly.
;;
(modify-coding-system-alist 'file "\\.reg\\'" 'utf-16-le)

;;
;; Set favourite of what fonts are available
;;
(require 'dash)
(defun set-font-from (fonts)
  (when window-system
      (when-let (font (-first (lambda (x) (not (eq (font-info x) nil))) fonts))
	(set-frame-font font))))

(set-font-from '("Hack" "Hack Nerd Font Mono" "FiraCode Nerd Font Mono" "Consolas"))

;;
;; Theme
;;
(defun gh/clear-themes ()
  (interactive)
  (mapcar 'disable-theme custom-enabled-themes))

(defun gh/enable-theme (theme)
  (progn
    (gh/clear-themes)
    (message (format "Switched to theme: %s" theme))
    (load-theme theme t)))

(setq gh/dark-themes '(zenburn
		       nimbus
		       tao-yin
		       base16-kimber
                       base16-monokai
                       sanityinc-tomorrow-bright
                       sanityinc-tomorrow-eighties
                       sanityinc-tomorrow-blue
		       base16-greenscreen
		       material))

(setq gh/light-themes '(tao-yang
			leuven
			material-light
			twilight-bright))

(defun gh/cycle-themes (themes)
  (let* ((current-theme (car custom-enabled-themes))
         (tail (member current-theme themes))
         (next-theme (if (or (null tail)
                             (null (cdr tail)))
                         (car themes)
                       (cadr tail))))
    (gh/enable-theme next-theme)))

(defun gh/cycle-dark-themes ()
  (interactive)
  (gh/cycle-themes gh/dark-themes))

(defun gh/cycle-light-themes ()
  (interactive)
  (gh/cycle-themes gh/light-themes))

(bind-key [(f6)] 'gh/cycle-dark-themes)
(bind-key [(shift f6)] 'gh/cycle-light-themes)

(gh/enable-theme 'nimbus)

;;
;; CIDER / Clojure / ClojureScript / clj-refactor
;;
(use-package cider
  :ensure t
  :ensure seq
  :ensure clojure-mode
  :ensure clj-refactor
  :mode (("\\.clj\\[scx\\]?$" . clojure-mode)
         ("\\.boot$" . clojure-mode))
  :init
  (progn
    (setq cider-auto-select-error-buffer t
          cider-repl-use-clojure-font-lock t
          nrepl-hide-special-buffers t
	        cljr-favor-prefix-notation t
	        cider-print-quota (* 1024 10)
	        lispy-thread-last-macro "->>")
    (add-hook 'clojure-mode-hook
              (lambda ()
                (define-clojure-indent
                  (ANY 2)
                  (DELETE 2)
                  (GET 2)
                  (HEAD 2)
                  (POST 2)
                  (PUT 2)
                  (with 2)
                  (match 1)
                  (domonad 1)
                  (context 2)
                  (for-all 1)
                  (defui '(1 nil nil (1)))
                  (defroutes 'defun))))
    (add-hook 'clojure-mode-hook (lambda ()
                                   (eldoc-mode 1)
                                   (clj-refactor-mode 1)
                                   (cljr-add-keybindings-with-prefix "C-c r")))
    (add-hook 'cider-repl-mode-hook #'subword-mode)
    (add-hook 'cider-mode-hook
          (lambda ()
            (local-set-key (kbd "C-S-x") #'rebl-eval-defun-at-point)
            (local-set-key (kbd "C-x C-r") #'rebl-eval-last-sexp))))
  ;; :config
  ;; (use-package eval-sexp-fu :ensure t)
  ;; (use-package cider-eval-sexp-fu :ensure t)
  )

(use-package lisp-mode
  :config
  (add-hook 'lisp-mode-hook
            (lambda ()
              (setq tab-width 2
	                  indent-tabs-mode nil)
              (put '$define! 'lisp-indent-function 1)
              (put '$vau 'lisp-indent-function 'defun)
              (put '$lambda 'lisp-indent-function 'defun)
              (put '$provide! 'lisp-indent-function 1)
              (put '$let 'lisp-indent-function 1)
              (put '$let* 'lisp-indent-function 1)
              (put '$letrec 'lisp-indent-function 1)
              (put '$letrec* 'lisp-indent-function 1)
              (put '$let-redirect 'lisp-indent-function 1)
              (put '$let-safe 'lisp-indent-function 1))))

(defun clerk-show ()
  (interactive)
  (save-buffer)
  (let
      ((filename
        (buffer-file-name)))
    (when filename
      (cider-interactive-eval
       (concat "(nextjournal.clerk/show! \"" filename "\")")))))

;;
;; REST client
;;
(use-package restclient
  :ensure t
  :commands (restclient-mode)
  :mode "\\.http")

;;
;; Ruby
;;
(use-package enh-ruby-mode
  :ensure t
  :ensure rvm
  :ensure robe
  :mode ("\\.rb$"
         "\\.rake$"
         "\\.gemspec$"
         "\\.ru$"
         "\\.thor$"
         "Rakefile$"
         "Thorfile$"
         "Gemfile$"
         "Gapfile$"
         "Vagrantfile$")
  :interpreter "ruby"
  :init
  (progn
    (defadvice inf-ruby-console-auto
        (before activate-rvm-for-robe activate)
      (rvm-activate-corresponding-ruby))
    (add-hook 'enh-ruby-mode-hook 'robe-mode))
  :config
  (setq enh-ruby-bounce-deep-indent t)
  (setq enh-ruby-hanging-brace-indent-level 2))

;;
;; Java mode
;;
(use-package java-mode
  :mode "\\.java$"
  :init
  (add-hook 'java-mode-hook 'gh/prog-mode-hook)
  (add-hook 'java-mode-hook (lambda ()
                              (setq tab-width 2)
                              (subword-mode t))))

(use-package log4j-mode
  :ensure t
  :mode "\\.log$"
  :init
  (add-hook 'log4j-mode-hook 'gh/prog-mode-hook)
  (add-hook 'log4j-mode-hook 'gh/turn-on-hl-line-mode)
  (add-hook 'log4j-mode-hook (lambda ()
                               (read-only-mode t)
                               (view-mode t))))

;;;
;;; C++
;;;
(use-package cmake-mode
  :ensure t
  :mode "CMakeLists.txt")

;;
;; Erlang
;;
(use-package erlang
  :ensure t
  :mode "\\.erl$")

;;
;; Elixir
;;
(use-package elixir-mode
  :ensure t
  :defer t)

(use-package alchemist
  :ensure t
  :defer t)

;;
;; Golang
;;
(use-package go-mode
  :ensure t
  :ensure go-autocomplete
  :ensure go-projectile
  :mode "\\.go"
  :init
  (add-hook 'before-save-hook 'gofmt-before-save))

;;
;; Racket
;;
(use-package racket-mode
  :ensure t
  :defer t)

;;
;; Terraform
;;
(use-package terraform-mode
  :ensure t
  :defer t)

;;
;; Docker
;;
(use-package docker
  :ensure t
  :ensure dockerfile-mode
  :defer t)

;;
;; EShell
;;
(use-package eshell
  :commands (eshell))

(defun eshell/x ()
  (delete-window))

(defalias 'd 'dired)
(defalias 'ff 'find-file)
(defalias 'g 'magit-status)

;;
;; Remote shells
;;
(use-package tramp
  :init
  (set-default 'tramp-auto-save-directory (expand-file-name "~/temp")))

;;
;; vterm
;;
(use-package vterm :defer t)

;;
;; Octave
;;
;; for some reason this is in octave-mod on my mac
(autoload 'octave-mode "octave-mod" nil t)
(use-package octave-mode
  :mode ("\\.m$" . octave-mode))

;;
;; YAML mode
(use-package yaml-mode
  :ensure t
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)))

;;
;; Julia
;;
(use-package julia-mode
  :ensure t
  :mode "\\.jl$")

;;;
;;; Scala mode
;;;
(use-package scala-mode
  :ensure t
  :mode "\\.scala$")

;;;
;;; Jsonnet mode
;;;
(use-package jsonnet-mode
  :ensure t
  :mode "\\..*jsonnet$")

;;
;; My bindings
;;
(bind-keys
 ("\C-o" . query-replace)
 ([(control next)] . scroll-other-window)
 ([(control prior)] . scroll-other-window-down)
 ([(f8)] . toggle-truncate-lines)
 ([(shift f8)] . display-line-numbers-mode))

;;
;; All kinds of shells on f9...
;;
(use-package eshell-toggle
  :ensure t
  :bind
  ("<f9>" . eshell-toggle))

(use-package shell-toggle
  :ensure t
  :config
  (setq shell-toggle-launch-shell 'shell)
  :bind
  ([(shift f9)] . shell-toggle-cd))

(bind-key [(ctrl f9)] 'run-python)

;;
;; keychords to combat emacs-pinky
;;
(use-package transpose-frame
  :ensure t
  :commands (transpose-frame))

(use-package key-chord
  :ensure t
  :config
  (progn
    (key-chord-define-global "jj" 'ibuffer)
    (key-chord-define-global "j0" 'delete-window)
    (key-chord-define-global "j1" 'delete-other-windows)
    (key-chord-define-global "JJ" 'magit-status)
    (key-chord-define-global ",," 'consult-buffer)
    (key-chord-define-global "hh" 'consult-buffer-other-window)
    (key-chord-define-global "jk" 'transpose-frame)
    (key-chord-define-global "jf" 'consult-recent-file)
    (key-chord-define-global "jp" 'consult-projectile)
    (key-chord-define-global "jw" 'ace-window)
    (key-chord-define-global "kk" 'gh/kill-current-buffer)
    (key-chord-define-global "YY" 'consult-yank-pop)
    (key-chord-mode 1)))

;;
;; Narrow or widen DWIM
;;
(defun gh/narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed else narrow to appropriate region."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
	((region-active-p) (narrow-to-region (region-beginning) (region-end)))
	((derived-mode-p 'org-mode)

	 (cond ((ignore-errors (org-edit-src-code) t) (delete-other-windows))
	       ((ignore-errors (org-narrow-to-block) t))
	       (t (org-narrow-to-subtree))))
	(t (narrow-to-defun))))

(global-set-key (kbd "<f7>") #'gh/narrow-or-widen-dwim)

;;
;; Toggle transparency
;;
(defvar gh/transparent nil)

(defun gh/set-alpha (alpha-value)
  (set-frame-parameter (selected-frame) 'alpha alpha-value)
  (add-to-list 'default-frame-alist (list 'alpha alpha-value)))

(defun gh/toggle-transparency ()
  (interactive)
  (if gh/transparent
      (progn
	(setq gh/transparent nil)
	(gh/set-alpha 100))
    (progn
      (setq gh/transparent t)
      (gh/set-alpha '(82 . 72)))))

;; iterm2 uses command-U for this
(bind-key "s-u" 'gh/toggle-transparency)

;;
;; Enable server / emacsclient
;;
(server-start)

(require 'epg)
(setq epg-pinentry-mode 'loopback)

;;
;; Experimental dispatch functions for access by an experimental
;; Alfred workflow I'm working on
;;

(defun alfred/repl (text)
  "Send TEXT to a current CIDER repl."
  (cider--switch-to-repl-buffer (cadar (cider-sessions)))
  (cider-insert-in-repl text t))
