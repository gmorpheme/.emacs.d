;;; early.init.el
;;
;; -*- lexical-binding: t -*-

;; Configure package archives prior to normal init and let emacs do
;; its thing automatically (since 28)
(with-eval-after-load 'package
  (add-to-list 'package-archives
	       (cons "melpa" "https://melpa.org/packages/")
	       t))
