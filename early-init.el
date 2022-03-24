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
