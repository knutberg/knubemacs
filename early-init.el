;;; early-init.el --- -*- no-byte-compile: t-*-
(defvar startup-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)                   ; reset later

(setq gc-cons-threshold         most-positive-fixnum ; set to 32MB later
      gc-cons-percentage        0.6                  ; set to 0.1 later
      debug-on-error            t                    ; reset to nil later
      site-run-file             nil                  ; disable site-start.el
      package-enable-at-startup nil)                 ; we use straight.el

(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)

(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq inhibit-startup-message t
      inhibit-scratch-message t
      load-prefer-newer       t)

(when (file-exists-p "~/.emacs.d/straight/repos/auto-compile/")
  (progn
    (add-to-list 'load-path "~/.emacs.d/straight/repos/auto-compile/")
    (add-to-list 'load-path "~/.emacs.d/straight/repos/packed/")
    (require 'auto-compile)
    (auto-compile-on-load-mode +1)
    (auto-compile-on-save-mode +1)))

(when (file-exists-p "~/.emacs.d/straight/repos/no-littering/")
  (progn
    (add-to-list  'load-path "~/.emacs.d/straight/repos/no-littering/")
    (require 'no-littering)
    (setq auto-save-file-name-transforms
          `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
    (require 'recentf)
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

(setq-default evil-want-keybinding nil)
