; init.el
;;
;; Much of this is stolen from elsehwere, in particular Phil
;; Hagelberg's emacs-starter-kit, while it existed.
;;

(require 'package)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(use-package
                      better-defaults
                      exec-path-from-shell
                      idle-highlight-mode
                      paredit
                      rainbow-delimiters
                      ess
                      soft-stone-theme
                      soft-morning-theme
                      color-theme-solarized
                      color-theme-sanityinc-tomorrow
                      zenburn-theme
                      base16-theme
                      melpa)
  "A list of packages to ensure are installed at launch.")

(setq use-package-verbose t)

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (condition-case err
        (package-install p)
      (error
       (message "%s" (error-message-string err))))))

(defun gh/package-list-unaccounted-packages ()
  "Like `package-list-packages', but shows only the packages that
   are installed and are not in `my-packages'.  useful for
   cleaning out unwanted packages."
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x my-packages))
                                   (not (package-built-in-p x))
                                   (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))

(defun gh/package-list-installed-packages ()

  (interactive)
  (package-show-package-list
   (remove-if-not
    'package-installed-p
    (mapcar 'car package-archive-contents))))

;;
;; Keep customize-based settings separate
;;
(setq custom-file "~/.emacs.d/.emacs-custom")
(load custom-file 'no-error)

;;;
;;; Load a secrets file if present too
;;;

(load "~/.emacs.secrets" t)

(require 'bind-key)

;;;
;;; mac specifics
;;;

(defmacro gcr/on-osx (statement &rest statements)
  "Evaluate the enclosed body only when run on OSX."
  `(when (eq system-type 'darwin)
     ,statement
     ,@statements))

(gcr/on-osx
 ;; try and get appropriate path by looking at what shell does
 (exec-path-from-shell-initialize)
 ;; and make sure we have a few other auth settings
 (exec-path-from-shell-copy-envs '("AWS_ACCESS_KEY_ID" "AWS_SECRET_ACCESS_KEY" "GOPATH"))
 ;; typing hash on a UK mac in emacs is tricky
 (bind-key "s-3" '(lambda () (interactive) (insert "#")))
 ;; work around "empty or unsupported pasteboard type" bug
 ;; should be fixed in 24.4 so can remove at that point
 (setq save-interprogram-paste-before-kill nil))

;;
;; Load other files
;;
(setq gh/system-config (concat user-emacs-directory system-name ".el")
      gh/user-config (concat user-emacs-directory user-login-name ".el")
      gh/user-dir (concat user-emacs-directory user-login-name))


(defun gh/eval-after-init (form)
    "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
    (let ((func (list 'lambda nil form)))
      (add-hook 'after-init-hook func)
      (when after-init-time
        (eval form))))

(gh/eval-after-init
 '(progn
    (when (file-exists-p gh/system-config) (load gh/system-config))
    (when (file-exists-p gh/user-config) (load gh/user-config))
    (when (file-exists-p gh/user-dir)
      (mapc 'load (directory-files gh/user-dir t "^[^#].*el$")))))


;;;
;;; Other small customisations
;;;
(setq sentence-end-double-space nil)

;; use package
(require 'use-package)

;;;
;;; Whitespace cleanup mode -
;;;
(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode))

;;
;; Markdown
;;
(use-package markdown-mode
  :ensure t
  :mode ("\\.md$"
         "\\.apib$"))

;;
;; ivy
;;

(use-package ivy
  :ensure t
  :ensure flx
  :ensure smex
  :ensure ivy-hydra
  :ensure counsel
  :ensure counsel-projectile
  :diminish (ivy-mode . "")
  :bind (("C-'" . ivy-avy)
         ("M-x" . counsel-M-x)
         ("C-x f" . counsel-find-file)
         ("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-count-format ""
        ivy-display-style nil
        ivy-minibuffer-faces nil
        ivy-use-virtual-buffers t))

(use-package ido
  :ensure t
  :ensure ido-ubiquitous
  :init
  ;; ido with flex-matching already turned on in better-defaults.el
  (setq ido-enable-prefix nil
        ido-auto-merge-work-directories-length nil
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t
        ido-handle-duplicate-virtual-buffers 2
        ido-max-prospects 10
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window)
  :config
  (ido-ubiquitous-mode t))

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

; quick access to occur from interactive search
(define-key isearch-mode-map (kbd "C-o")
    (lambda () (interactive)
      (let ((case-fold-search isearch-case-fold-search))
        (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; company mode wherever - unless and until it gets slow
(use-package company
  :ensure t
  :defer 10
  :diminish company-mode
  :init
  (setq
   company-idle-delay 0.05
   company-dabbrev-downcase nil)
  :config (global-company-mode))

;; snippets
(use-package yasnippet
  :ensure t
  :defer 30
  :diminish yas-minor-mode
  :commands yas/hippie-try-expand
  :init (progn
          (add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand)
          (yas-global-mode 1)))

;; projectile mode everywhere
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :defer 5
  :config
  (progn

    (setq projectile-completion-system 'ivy)
    (setq projectile-switch-project-action #'projectile-commander)

    (def-projectile-commander-method ?S
      "Open a *shell* buffer for the project."
      (projectile-run-shell))

    (def-projectile-commander-method ?F
      "Refres: fetch from git and go to magit status"
      (call-interactively #'magit-fetch-from-upstream)
      (projectile-vc))

    (projectile-global-mode))
  )

(defun gh/kill-current-buffer ()
  (interactive)
  (kill-buffer nil))

;; undo for window config C-c <- and C-c ->
(use-package winner
  :defer t
  :init (winner-mode 1))

;;
;; basic defaults
;;
(prefer-coding-system 'utf-8)
(set-default 'indicate-empty-lines t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)

(setq require-final-newline t)
(setq inhibit-splash-screen t)
;; why are these no longer coming from better-defaults?
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(blink-cursor-mode 0)
(auto-compression-mode 1)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(delete-selection-mode 1)

;; make mouse and gesture scroll work sanely
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

;; prefer side-by-side window splits if the window is wide
(setq split-height-threshold nil)
(setq split-width-threshold 160)

;;
;; backups already in .emacs.d/backups -
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; keep several old versions but delete excess without prompting
(setq version-control t)
(setq delete-old-versions t)

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
  (progn
    (setq magit-restore-window-configuration t)
    (setq magit-log-auto-more t)
    (setq magit-status-buffer-switch-function 'switch-to-buffer)))

;; (use-package magithub
;;   :ensure t
;;   :after magit
;;   :config (magithub-feature-autoinject t))

;;;
;;; General Programming Stuff
;;;
(defun gh/recompile-comint ()
  "Rerun a compilation in comint-mode for interaction"
  (interactive)
  (setf (elt compilation-arguments 1) t)
  (recompile))

(bind-key "<f5>" 'gh/recompile-comint)

(setq compilation-ask-about-save nil)
(setq compilation-scroll-output 'next-error)
(setq compilation-skip-threshold 2)

;;
;; lambdas and todos
;;
(defun gh/local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun gh/local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun gh/set-comment-columns ()
  (set (make-local-variable 'comment-column) 68)
  (set (make-local-variable 'comment-fill-column) 80))

(defun gh/turn-on-hl-line-mode ()
  (when (> (display-color-cells) 8)
    (hl-line-mode t)))

(defun gh/add-prettify-symbols ()
  (push '("fn" . "\u0192") prettify-symbols-alist))

(defun gh/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|QQ\\)"
          1 font-lock-warning-face t))))

(defun gh/truncate-lines ()
  (setq truncate-lines t))

(add-hook 'prog-mode-hook 'gh/local-column-number-mode)
(add-hook 'prog-mode-hook 'gh/local-comment-auto-fill)
(add-hook 'prog-mode-hook 'gh/add-prettify-symbols)
(add-hook 'prog-mode-hook 'gh/set-comment-columns)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'prog-mode-hook 'gh/add-watchwords)
(add-hook 'prog-mode-hook 'idle-highlight-mode)
(add-hook 'prog-mode-hook 'gh/truncate-lines)
(add-hook 'prog-mode-hook 'electric-indent-mode)

(defun gh/prog-mode-hook ()
  (run-hooks 'prog-mode-hook))

;;
;; An actually usable imenu thing
;;
(set-default 'imenu-auto-rescan t)
(use-package popup-imenu
  :ensure t
  :commands popup-imenu
  :bind ("M-i" . popup-imenu))

;;
;; Lisp modes
;;
(dolist (mode '(scheme emacs-lisp lisp clojure racket))
  (let ((hook (intern (concat (symbol-name mode) "-mode-hook"))))
    (add-hook hook 'gh/prog-mode-hook)
    (add-hook hook 'paredit-mode)
    (add-hook hook 'rainbow-delimiters-mode)))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;;
;; Haskell mode
;;
(use-package haskell-mode
  :ensure t
  :ensure intero
  :mode (("\\.hs" . haskell-mode)
         ("\\.fr" . haskell-mode))
  :init
  (setq haskell-compile-cabal-build-command "stack build")
  (add-hook 'haskell-mode-hook 'gh/prog-mode-hook)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'intero-mode))

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
(use-package js
  :ensure js2-mode
  :ensure json-mode
  :ensure ac-js2
  :ensure js-comint
  :mode "\\.js$"
  :init
  (progn
    (add-hook 'js-mode-hook 'js2-minor-mode)
    (add-hook 'js-mode-hook 'skewer-mode)
    (add-hook 'js2-mode-hook 'ac-js2-mode)
    (add-hook 'inferior-js-mode-hook 'ansi-color-for-comint-mode-on)
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
                           nil)))))
    (bind-key "{" 'paredit-open-curly js-mode-map)
    (bind-key "}" 'paredit-close-curly js-mode-map)))

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
;; CSS mode
;;
(use-package css-mode
  :ensure rainbow-mode
  :mode "\\.css"
  :init
  (progn
    (add-hook 'css-mode-hook 'gh/prog-mode-hook)
    (add-hook 'css-mode-hook 'rainbow-mode)
    (add-hook 'css-mode-hook 'skewer-css-mode)))

;;
;; Ensure that lisp-interaction can still evaluate on ctrl-j...
;;
(defadvice paredit-newline (around eval-print-last-sexp activate)
  (if (eq major-mode 'lisp-interaction-mode)
      (eval-print-last-sexp)
    (paredit-newline)))

;;
;; Org Mode
;;

(defvar gh/org-mobile-sync-timer nil)

(defun gh/org-mobile-sync ()
  (message "Syncing org-mobile...")
  (org-mobile-pull)
  (org-mobile-push))

(defun gh/org-mobile-start-sync ()
  "Start automated `org-mobile-push'"
  (interactive)
  (setq gh/org-mobile-sync-timer
        (run-with-idle-timer (* 60 20) t 'gh/org-mobile-sync)))

(defun gh/org-mobile-stop-sync ()
  "Stop automated `org-mobile-push'"
  (interactive)
  (cancel-timer gh/org-mobile-sync-timer))

(use-package org
  :init
  (setq default-major-mode 'org-mode)
  (setq org-directory "~/Dropbox/notes")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq gh/refile-file (concat org-directory "/refile.org"))
  (setq org-adapt-indentation nil)
  (setq org-list-description-max-indent 5)
  (setq org-html-head-extra "<link rel=\"stylesheet\" href=\"http://www.gmorpheme.net/theme/css/main.css\">
<script type=\"text/javascript\">
WebFontConfig = { fontdeck: { id: '35882' } }; (function() {
  var wf = document.createElement('script');
  wf.src = ('https:' == document.location.protocol ? 'https' : 'http') +
  '://ajax.googleapis.com/ajax/libs/webfont/1/webfont.js';
  wf.type = 'text/javascript';
  wf.async = 'true';
  var s = document.getElementsByTagName('script')[0];
  s.parentNode.insertBefore(wf, s);
})();</script>")

  (setq org-html-head-include-default-style nil)

  ;; Mobile settings
  (setq org-mobile-directory "~/Dropbox/MobileOrg")
  (setq org-mobile-inbox-for-pull "~/Dropbox/from-mobile.org")

  (gh/org-mobile-start-sync)

  (setq org-startup-indented t)
  (setq org-insert-heading-respect-content nil)
  (setq org-return-follows-link t)

  (setq org-use-speed-commands t)
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

  ;; Clock settings

  (setq org-clock-into-drawer t)
  (setq org-clock-out-remove-zero-time-clocks t) ; Remove zero second clocks
  (setq org-clock-report-include-clocking-task t) ; Include current clocking task in clock reports
  (setq org-clock-in-resume t)
  (setq org-clock-persist t)
  (setq org-clock-auto-clock-resolution 'when-no-clock-is-running)
  (setq org-clock-persist-query-resume nil) ; Do not prompt to resume an active clock

  (setq org-drawers '("PROPERTIES" "LOGBOOK" "CLOCK" "RESULTS"))

  ;; exclude certain tags from inheritance
  (setq org-tags-exclude-from-inheritance '("PROJECT"))

  ;; Agenda settings

  ;; Modal effect, full window but restore windows on q
  (setq org-agenda-window-setup 'reorganize-frame)
  (setq org-agenda-restore-windows-after-quit t)

  (setq org-agenda-compact-blocks t)
  (setq org-agenda-custom-commands
        '((" " "Grand Unified Agenda"
           ((agenda ""
                    ((org-agenda-ndays 1)
                     (org-agenda-overriding-header "=== Today")))
            (tags "FLAGGED"
                  ((org-agenda-overriding-header "=== Flagged")
                   (org-show-context-detail 'lineage)))
            (tags "REFILE|TIDY"
                  ((org-agenda-overriding-header "=== To Refile / Tidy")))
            (tags-todo "PRIORITY=\"A\"-SCHEDULED={.+}"
                       ((org-agenda-overriding-header "=== Unscheduled High Priority")))))))

  ;; Capture and refile settings
  (setq org-capture-templates
        '(("s" "start day" entry (file+datetree org-default-notes-file)
           (file "template-day.org")
           :clock-in t)

          ("w" "weekly review" entry (file+datetree org-default-notes-file)
           (file "template-week.org")
           :clock-in t)

          ("M" "monthly review" entry (file+datetree org-default-notes-file)
           (file "template-month.org") :clock-in t)

          ("t" "todo" entry (file gh/refile-file)
           "* TODO %?\n  %i\n  %a")

          ("r" "respond" entry (file gh/refile-file)
           "* NEXT Respond to %? \nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)

          ("n" "note" entry (file gh/refile-file)
           "* %? :NOTE:\n%U\n\n" :clock-in t :clock-resume t)

          ("i" "interruption" entry (file gh/refile-file)
           "* %? :INTERRUPTION:\n\n\n" :clock-resume t :clock-in t)

          ("m" "meeting" entry (file gh/refile-file)
           "* MEETING %? :MEETING:\n%U" :clock-in t :clock-resume t)

          ("p" "phone call" entry (file gh/refile-file)
           "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)))

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
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t))

(defun gh/clock-in-from-history ()
  (interactive)
  (org-clock-in '(4)))

;; Global key bindings for org stuff
(bind-key "\C-cl" 'org-store-link)
(bind-key "\C-ca" 'org-agenda)
(bind-key "<f12>" 'org-agenda)
(bind-key "\C-cc" 'org-capture)

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
;; Python
;;
(use-package python
  :ensure t
  :commands (run-python)
  :mode "\\.py$"
  :init
  (add-hook 'python-mode-hook 'gh/prog-mode-hook))

;;
;; Windows specifics
;;
(if (eq system-type 'windows-nt)
    (progn
      (set-default-font "-outline-Consolas-normal-r-normal-normal-12-97-96-96-c-*-iso8859-1")
                                        ; stop hangs?
      (setq w32-get-true-file-attributes nil)
      (remove-hook 'text-mode-hook 'turn-on-flyspell)))

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
    (load-theme theme)))

(setq gh/dark-themes '(zenburn
                       sanityinc-tomorrow-bright
                       sanityinc-tomorrow-eighties
                       sanityinc-tomorrow-blue
                       base16-monokai))

(setq gh/light-themes '(soft-morning
                        soft-stone
                        solarized))

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

(gh/enable-theme 'sanityinc-tomorrow-bright)

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
    (setq cider-repl-popup-stacktraces t
          cider-auto-select-error-buffer t
          cider-repl-print-length 200
          cider-prompt-save-file-on-load nil
          cider-repl-use-clojure-font-lock t
          nrepl-hide-special-buffers t)
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
    (add-hook 'cider-repl-mode-hook #'paredit-mode))
  ;; :config
  ;; (use-package eval-sexp-fu :ensure t)
  ;; (use-package cider-eval-sexp-fu :ensure t)
  )

;;
;; REST client
;;
(use-package restclient
  :ensure t
  :commands (restclient-mode)
  :mode "\\.http"
  :init (add-hook 'restclient-mode-hook
                  (lambda () (setq tab-width 2))))

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

;; From howardism.org
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

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

;;
;; My bindings
;;
(define-key shell-mode-map (kbd "SPC") 'comint-magic-space)

(bind-key "\C-o" 'query-replace)
(bind-key [(control next)] 'scroll-other-window)
(bind-key [(control prior)] 'scroll-other-window-down)
(bind-key [(f8)] 'toggle-truncate-lines)
(bind-key [(shift f8)] 'linum-mode)
;; put all kinds of shells on f9...
(bind-key [(ctrl f9)] 'run-python)
(bind-key [(shift f9)] 'shell)
(bind-key [(f9)] 'eshell-here)

;;
;; keychords and hydras to combat emacs-pinky
;;

(use-package hydra
  :ensure t)

(use-package transpose-frame
  :ensure t
  :commands (transpose-frame))

(use-package key-chord
  :ensure t
  :config
  (progn
    (key-chord-define-global "jj" 'ibuffer)
    (key-chord-define-global "j1" 'delete-other-windows)
    (key-chord-define-global "JJ" 'magit-status)
    (key-chord-define-global "jz" 'magit-dispatch-popup)
    (key-chord-define-global ",," 'ivy-switch-buffer)
    (key-chord-define-global "hh" 'ivy-switch-buffer-other-window)
    (key-chord-define-global "jk" 'transpose-frame)
    (key-chord-define-global "jf" 'counsel-find-file)
    (key-chord-define-global "jg" 'org-agenda)
    (key-chord-define-global "jp" 'projectile-find-file)
    (key-chord-define-global "jP" 'projectile-switch-project)
    (key-chord-define-global "jt" 'ace-jump-mode)
    (key-chord-define-global "jw" 'ace-window)
    (key-chord-define-global "kk" 'gh/kill-current-buffer)
    (key-chord-define-global "YY" 'counsel-yank-pop)
    (key-chord-mode 1)))

(bind-key
 "C-M-o"
 (defhydra hydra-window (:color amaranth)
   "window"
   ("h" windmove-left)
   ("j" windmove-down)
   ("k" windmove-up)
   ("l" windmove-right)
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
        "vert")
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
        "horz")
   ("t" transpose-frame "'" :color blue)
   ("o" delete-other-windows "one" :color blue)
   ("a" ace-window "ace")
   ("s" ace-swap-window "swap")
   ("d" ace-delete-window "del")
   ("i" ace-maximize-window "ace-one" :color blue)
   ("b" ido-switch-buffer "buf")
   ("q" nil "cancel")))
(put 'set-goal-column 'disabled nil)
