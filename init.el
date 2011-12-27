;;=============================================================================
;; package.el / starter kit based configuration
;;=============================================================================
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit 
                      starter-kit-lisp
                      starter-kit-bindings
                      clojure-mode
                      clojure-test-mode
                      rainbow-delimiters
                      zenburn-theme
                      python-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;=============================================================================
;; Keep customize-based settings separate
;;=============================================================================
(setq custom-file "~/.emacs.d/.emacs-custom")
(load custom-file 'no-error)

;;=============================================================================
;; Enable disabled features
;;=============================================================================
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)

;==============================================================================
;; Check for modifications to open files.
;==============================================================================
(require 'autorevert)
(global-auto-revert-mode t)

;==============================================================================
;; Visual settings
;==============================================================================
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(auto-compression-mode 1)
(setq visible-bell t)
(setq default-major-mode 'text-mode)
(delete-selection-mode)

;==============================================================================
;; Ido settings
;==============================================================================
(setq ido-auto-merge-work-directories-length -1)

;;==============================================================================
;; Horizontal line hightlighting in dired = not programming modes.
;;==============================================================================
(add-hook 'dired-mode-hook 'esk-turn-on-hl-line-mode)
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

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
(global-set-key "\C-x\C-r" 'revert-buffer)
(global-set-key [?\C->] 'tags-apropos)
(global-set-key "\C-o" 'replace-string)
(global-set-key "\C-x\C-n" 'other-window)
(global-set-key "\C-x\C-p" 'other-window-backward)
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
    (set-default-font "-outline-Consolas-normal-r-normal-normal-12-97-96-96-c-*-iso8859-1")))

;;=============================================================================
;; Theme
;;=============================================================================
(load-theme 'zenburn)
(enable-theme 'zenburn)

;;=============================================================================
;; Todo: powershell, yaml, js, python
;;=============================================================================

