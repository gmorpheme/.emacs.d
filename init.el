;;=============================================================================
;; package.el / starter kit based configuration
;;=============================================================================
(require 'package)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit 
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-ruby
                      starter-kit-js
                      clojure-mode
                      clojure-snippets
                      rainbow-delimiters
                      python-mode
                      groovy-mode
                      powershell
                      yasnippet
                      zenburn-theme
                      cider
                      midje-mode
                      emacs-eclim
                      melpa)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (condition-case err
        (package-install p)
      (error
       (message "%s" (error-message-string err))))))

(defun package-list-unaccounted-packages ()
  "Like `package-list-packages', but shows only the packages that
   are installed and are not in `my-packages'.  Useful for
   cleaning out unwanted packages."
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x my-packages))
                                   (not (package-built-in-p x))
                                   (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))


(yas-global-mode 1)
(add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand)

;;=============================================================================
;; Ensure that lisp-interaction can still evaluate on ctrl-j...
;;=============================================================================
(defadvice paredit-newline (around eval-print-last-sexp activate)
  (if (eq major-mode 'lisp-interaction-mode)
      (eval-print-last-sexp)
    (paredit-newline)))


;;=============================================================================
;; Keep customize-based settings separate
;;=============================================================================
(setq custom-file "~/.emacs.d/.emacs-custom")
(load custom-file 'no-error)

;;=============================================================================
;; Org Mode
;;=============================================================================
(setq org-directory "~/dropbox/notes")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-mobile-directory "~/dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/dropbox/from-mobile.org")
(setq org-ditaa-jar-path "~/.emacs.d/deps/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/.emacs.d/deps/plantuml.jar")

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

(gh/org-mobile-start-sync)

(setq org-use-speed-commands t)

(setq org-clock-into-drawer t)
;; Remove zero second clocks
(setq org-clock-out-remove-zero-time-clocks t)
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)

(setq org-drawers '("PROPERTIES" "LOGBOOK" "CLOCK" "RESULTS" "FEEDSTATUS-RTM"))

(setq org-feed-alist
      '(("RTM"
         "http://www.rememberthemilk.com/atom/gmorpheme/25471103/?tok=eJwFwYENQjEIBcCJmvBaKO04UEBN-NGo*8c7rUnHvOIAAUI3zvAB1k7cZS4uzygsPim7k-lJoiplYsGgdrten-c9r2zPx-fXurACNFrqIUyR6bBTGjQs54jp6dpzbOe0CAKOl4nSlhELDNnman39ARvxK1Y"
         "~/dropbox/notes/notes.org"
         "Inbox"
         :drawer "FEEDSTATUS-RTM"
         :parse-feed org-feed-parse-atom-feed
         :parse-entry org-feed-parse-atom-entry
         :template "
 * TODO %h :RTM:
   %U
   %a
 ")))

(setq org-agenda-window-setup 'current-window)

(setq org-agenda-compact-blocks t)
(setq org-agenda-custom-commands
      '(("y" "Grand Unified Agenda"
         ((agenda ""
                  ((org-agenda-ndays 1)
                   (org-agenda-overriding-header "=== Today")))
          (tags "REFILE"
                ((org-agenda-overriding-header "=== To Refile")))
          (tags-todo "ORGANISE"
                     ((org-agenda-overriding-header "=== Daily Admin")))
          (todo "NEXT"
                ((org-agenda-overriding-header "=== NEXT")))
          (tags-todo "PRIORITY=\"A\"-SCHEDULED={.+}"
                     ((org-agenda-overriding-header "=== Unscheduled High Priority")))))
        ("3" tags-todo "T3")
        ("e" tags-todo "ERRAND")))

(setq org-capture-templates
      '(("s" "Start of day" entry (file org-default-notes-file)
         "* Day %u :ORGANISE: \n %[template-day.org]" :clock-in t)
        ("t" "Todo" entry (file+headline org-default-notes-file "Inbox")
         "* TODO %?\n  %i\n  %a")
        ("d" "Distraction / called away" entry (file+headline org-default-notes-file "Inbox")
         "* %?\n %i\n %a\n" :clock-resume t :clock-in t)))

(setq org-refile-targets '((org-agenda-files :maxlevel . 9)))

                                        ; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(global-set-key "\C-cc" 'org-capture)
(global-set-key [f12] 'org-capture)

;; Distraction on f12
(define-key global-map [f12]
  (lambda () (interactive) (org-capture nil "d")))

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (plantuml . t))))
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
(setq org-confirm-babel-evaluate nil)

;;=============================================================================
;; Enable disabled features
;;=============================================================================
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)

;;==============================================================================
;; Check for modifications to open files.
;;==============================================================================
(require 'autorevert)
(global-auto-revert-mode t)

;;==============================================================================
;; Visual settings
;;==============================================================================
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(auto-compression-mode 1)
(setq visible-bell t)
(setq default-major-mode 'org-mode)
(delete-selection-mode 1)

;;==============================================================================
;; Ido settings
;;==============================================================================
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
(setq ido-auto-merge-work-directories-length -1)

;;==============================================================================
;; Horizontal line hightlighting in dired, not programming modes.
;;==============================================================================
(add-hook 'dired-mode-hook 'esk-turn-on-hl-line-mode)
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;;=============================================================================
;; windows reg files are UTF-16 little endian - please read them properly.
;;=============================================================================
(modify-coding-system-alist 'file "\\.reg\\'" 'utf-16-le)

;;=============================================================================
;; modeline stuff
;;=============================================================================
(display-time)

;;=============================================================================
;; Python
;;=============================================================================
(require 'python-mode)

;;=============================================================================
;; My bindings
;;=============================================================================
(global-set-key "\C-o" 'replace-string)
(global-set-key [(control next)] 'scroll-other-window)
(global-set-key [(control prior)] 'scroll-other-window-down)
(global-set-key [(f8)] 'toggle-truncate-lines)
;; put all kinds of shells on f9...
(global-set-key [(f9)] 'py-shell)
(global-set-key [(shift f9)] 'shell) 
(global-set-key [(ctrl f9)] 'eshell)

;;=============================================================================
;; Windows specifics
;;=============================================================================
(if (eq system-type 'windows-nt)
    (progn
      (set-default-font "-outline-Consolas-normal-r-normal-normal-12-97-96-96-c-*-iso8859-1")
                                        ; stop hangs?
      (setq w32-get-true-file-attributes nil)
      (remove-hook 'text-mode-hook 'turn-on-flyspell)))

;;=============================================================================
;; Theme
;;=============================================================================
(load-theme 'zenburn)
(enable-theme 'zenburn)

;; todo (if / when needed): yaml, n3, psvn?, rst?

;;=============================================================================
;; Clojure indentation
;;=============================================================================
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
              (domonad 1)
              (context 2)
              (defroutes 'defun))))
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(require 'midje-mode)
(require 'clojure-jump-to-file)

;;=============================================================================
;; Eclipse interaction
;;=============================================================================
(require 'eclim)
(global-eclim-mode)

(require 'tramp)
(set-default 'tramp-auto-save-directory (expand-file-name "~/temp"))
(set-default 'tramp-default-method "plinkx")

;;=============================================================================
;; Miscellaneous
;;=============================================================================
(setq gh/scales '(A Bb B C C^ D Eb E F F^ G Ab
                    A-hm Bb-hm B-hm C-hm C^-hm D-hm Eb-hm E-hm F-hm F^-hm G-hm Ab-hm
                    A-mm Bb-mm B-mm C-mm C^-mm D-mm Eb-mm E-mm F-mm F^-mm G-mm Ab-mm))


(defun gh/random-scales (n)
  "Generate n random scales to practice"
  (let ((s (number-sequence 0 (- n 1))))
    (mapcar  
     (lambda (n) (elt gh/scales (random 36)))
     s)))
