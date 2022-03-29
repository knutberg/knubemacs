;;; init.el --- -*- lexical-binding: t -*-
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(setq user-info-file (concat user-emacs-directory "user-info.el"))
(when (file-exists-p user-info-file)
  (load user-info-file 'noerror))

(setq user-init-file     (concat user-emacs-directory "init.el"))
(setq user-init-org-file (concat user-emacs-directory "init.org"))

(defun knube/reload-init ()
  "Reload ~/.emacs.d/init.el."
  (interactive)
  (org-babel-tangle-file user-init-org-file)
  (load-file user-init-file))

(defun knube/open-init ()
  "Open ~/.emacs.d/init.org."
  (interactive)
  (find-file user-init-org-file))

(defconst *IS-MAC*    (eq system-type 'darwin))
(defconst *IS-WIN*    (eq system-type 'windows-nt))
(defconst *IS-WIN-WSL (and (eq system-type 'windows-nt) (getenv "WSLENV")))
(defconst *IS-LINUX*  (eq system-type 'gnu/linux))

(when *IS-MAC*
  (setq mac-command-modifier      'meta
        mac-option-modifier       nil
        mac-right-option-modifier nil
        mac-function-modifier     'super))

;; todo: do this for win and linux

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq knube/packages '(ace-window
                       all-the-icons
                       auctex
                       auctex-latexmk
                       auto-compile
                       avy
                       cdlatex
                       citar
                       circadian
                       company
                       company-prescient
                       consult
                       crux
                       embark
                       embark-consult
                       exec-path-from-shell
                       logos
                       marginalia
                       minions
                       modus-themes
                       no-littering
                       org
                       org-contrib
                       org-download
                       pulsar
                       rainbow-delimiters
                       restart-emacs
                       selectrum
                       selectrum-prescient
                       smartparens
                       telephone-line
                       undo-fu
                       which-key
                       writeroom-mode
                       yasnippet))
(dolist (p knube/packages)
  (straight-use-package p))

(exec-path-from-shell-initialize)

(auto-compile-on-load-mode +1)
(auto-compile-on-save-mode +1)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq no-littering-etc-directory
      (expand-file-name "config/" user-emacs-directory))
(setq no-littering-var-directory
      (expand-file-name "data/" user-emacs-directory))
(require 'no-littering)
(require 'recentf)
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)

;; Increase this if stuttering occurs. Decrease if freezes occurs.
(defvar knube-gc-cons-threshold (* 64 1024 1024))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold       knube-gc-cons-threshold
                  gc-cons-percentage      0.1
                  debug-on-error          nil
                  file-name-handler-alist startup-file-name-handler-alist)
            (makunbound 'startup-file-name-handler-alist)))

;; Do gc when out of focus. Avoid gc when using minibuffer.
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* knube-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold knube-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))

(setq utf-translate-cjk-mode nil     ; disable CJK coding/encoding
      locale-coding-system   'utf-8)
(set-language-environment    'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

(setq scroll-step                     1
      scroll-conservatively           101
      scroll-preserve-screen-position 'always
      next-screen-context-lines       5
      debugger-stack-frame-as-list    t
      mouse-wheel-follow-mouse        t
      mouse-wheel-scroll-amount       '(1 ((shift) . 1))
      mouse-wheel-progressive-speed   nil
      mouse-yank-at-point             t)

(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

(require 'smartparens-config)
(smartparens-global-mode +1)

(add-hook 'prog-mode-hook   'subword-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook
 (lambda ()
   (when buffer-file-name
     (let ((dir (file-name-directory buffer-file-name)))
       (when (and (not (file-exists-p dir))
                  (y-or-n-p
                   (format "Directory %s does not exist. Create it?" dir)))
         (make-directory dir t))))))

(blink-cursor-mode       0)
(delete-selection-mode   1)
(transient-mark-mode     1) ; https://www.emacswiki.org/emacs/TransientMarkMode
(save-place-mode         1) ; https://www.emacswiki.org/emacs/SavePlace
(show-paren-mode         1) ; Indicate matching pairs of parentheses
(column-number-mode      1)
(global-font-lock-mode   t) ; is this really a good idea?
(global-auto-revert-mode t) ; refresh buffer on file change

(setq-default cursor-type            'bar
              indent-tabs-mode       nil  ; indent with space
              fill-column            80   ; always break at 80
              abbrev-mode            t
              dired-listing-switches "-alh")

(require 'uniquify)
(setq uniquify-buffer-name-style          'forward ; unique buffer names
      show-paren-delay                    0.0
      tab-width                           2
      delete-selection-mode               t
      sentence-end-double-space           nil
      vc-follow-symlinks                  t
      default-directory                   "~/"
      confirm-kill-emacs                  'y-or-n-p
      require-final-newline               t
      visible-bell                        t
      save-interprogram-paste-before-kill t
      apropos-do-all                      t
      save-abbrevs                        'silently
      large-file-warning-threshold        (* 15 1024 1024)
      global-mark-ring-max                500  ; we have buttloads of
      mark-ring-max                       500  ; memory, might as well
      kill-ring-max                       500) ; use it

(fset 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))



(setq org-list-allow-alphabetical      t
      org-fontify-whole-heading-line   t
      org-startup-indented             t     ; indent sections
      org-indent-indentation-per-level 2
      org-adapt-indentation            nil
      org-src-tab-acts-natively        t     ; tab works as in any major mode
      org-src-preserve-indentation     t
      org-log-into-drawer              t     ; wtf is this?
      org-src-fontify-natively         t     ; highlight code
      org-log-done                     'time ; add dates on completion of TODOs
      org-support-shift-select         t     ; select holding down shift
      org-startup-truncated            nil
      org-directory                    "~/Dropbox/org"
      org-agenda-files                 '("~/Dropbox/org/agenda/")
      org-ellipsis                     " ⤵"
      org-src-window-setup             'current-window
      org-latex-pdf-process            (list "latexmk -xelatex -f %f"))

(add-hook 'org-mode-hook (lambda ()
                           (add-to-list 'org-structure-template-alist
                                        '("se" . "src emacs-lisp"))))

(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (latex      . t)))

(setq-default org-download-image-dir "~/bilder/")
(add-hook 'dired-mode-hook 'org-download-enable)
(with-eval-after-load 'org
    (org-download-enable))

(add-hook 'LaTeX-mode-hook 'reftex-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

(setq-default TeX-master nil
              TeX-engine 'xetex)

(setq TeX-source-correlate-method 'synctex
      TeX-source-correlate        t
      TeX-PDF-mode                t
      TeX-auto-save               t
      TeX-save-query              nil
      TeX-parse-self              t
      reftex-plug-into-AUCTeX     t
      TeX-view-program-list       '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -g %n %o %b"))
      TeX-view-program-selection  '((output-pdf "Skim"))
      TeX-clean-confirm           nil)

;; make sure everything works fine with latexmk
(setq auctex-latexmk-inherit-TeX-PDF-mode t)

(auctex-latexmk-setup)

(add-hook 'org-mode-hook   'turn-on-org-cdlatex)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)

(setq cdlatex-env-alist
      '(("equation*" "\\begin{equation*}\n?\n\\end{equation*}\n" nil)))

(global-set-key (kbd "s-w") 'ace-window)
(global-set-key [remap other-window] 'ace-window)

(set-face-attribute 'default nil
                    :family "Iosevka Fixed Extended"
                    :height 170
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :family "Iosevka Fixed Extended"
                    :height 170
                    :weight 'medium)
(set-face-attribute 'variable-pitch nil
                    :family "Iosevka Fixed Extended"
                    :height 170
                    :weight 'medium)

(defun knube/fix-org-blocks ()
  "Extend org-block-line"
  (interactive)
  (eval-after-load 'org
    (lambda ()
      (set-face-attribute
       'org-block nil :extend t)
      (set-face-attribute 'org-block-begin-line nil :extend t
                          :underline nil :overline nil
                          :slant 'italic)
      (set-face-attribute 'org-block-end-line nil :extend t
                          :underline nil :overline nil
                          :slant 'italic))))

(setq modus-themes-org-blocks 'gray-background)

(modus-themes-load-themes)
;(modus-themes-load-operandi)

(setq knube/dark-theme-enabled-p nil)

(setq circadian-themes '((:sunrise . modus-operandi)
                         (:sunset  . modus-vivendi)))
(circadian-setup)

(knube/fix-org-blocks)

(defun knube/toggle-themes ()
  "Toggle light/dark theme."
  (interactive)
  (modus-themes-toggle)
  (setq knube/dark-theme-enabled-p (not knube/dark-theme-enabled-p))
  (knube/fix-org-blocks))

(require 'pulsar)

(setq pulsar-pulse-functions
      '(isearch-repeat-forward
        isearch-repeat-backward
        recenter-top-bottom
        move-to-window-line-top-bottom
        reposition-window
        bookmark-jump
        other-window
        delete-window
        delete-other-windows
        forward-page
        backward-page
        scroll-up-command
        scroll-down-command
        windmove-right
        windmove-left
        windmove-up
        windmove-down
        windmove-swap-states-right
        windmove-swap-states-left
        windmove-swap-states-up
        windmove-swap-states-down
        tab-new
        tab-close
        tab-next
        org-next-visible-heading
        org-previous-visible-heading
        org-forward-heading-same-level
        org-backward-heading-same-level
        outline-backward-same-level
        outline-forward-same-level
        outline-next-visible-heading
        outline-previous-visible-heading
        outline-up-heading))

(setq pulsar-pulse t)
(setq pulsar-delay 0.055)
(setq pulsar-iterations 10)
(setq pulsar-face 'pulsar-magenta)
(setq pulsar-highlight-face 'pulsar-yellow)

(pulsar-global-mode 1)

;; OR use the local mode for select mode hooks

(dolist (hook '(org-mode-hook emacs-lisp-mode-hook))
  (add-hook hook #'pulsar-mode))

;; pulsar does not define any key bindings.  This is just a sample that
;; respects the key binding conventions.  Evaluate:
;;
;;     (info "(elisp) Key Binding Conventions")
;;
;; The author uses C-x l for `pulsar-pulse-line' and C-x L for
;; `pulsar-highlight-line'.
(let ((map global-map))
  (define-key map (kbd "C-c h p") #'pulsar-pulse-line)
  (define-key map (kbd "C-c h h") #'pulsar-highlight-line))

;;If you want to use outlines instead of page breaks (the ^L)
(with-eval-after-load 'org
  (setq logos-outlines-are-pages t)
  (setq logos-outline-regexp-alist
        `((emacs-lisp-mode . "^;;;+ ")
          (org-mode . "^\\*+ +")
          (t . ,(or outline-regexp logos--page-delimiter)))))

;; These apply when `logos-focus-mode' is enabled.  Their value is
;; buffer-local.
(setq-default logos-hide-mode-line nil
              logos-scroll-lock nil
              logos-variable-pitch nil
              logos-indicate-buffer-boundaries nil
              logos-buffer-read-only nil
              logos-olivetti nil)

(let ((map global-map))
  (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
  (define-key map [remap forward-page]     #'logos-forward-page-dwim)
  (define-key map [remap backward-page]    #'logos-backward-page-dwim))

(setq minions-mode-line-lighter    "☰"
      minions-mode-line-delimiters '("" . ""))

(minions-mode +1)

(setq telephone-line-lhs
      '((evil   . (telephone-line-evil-tag-segment
                   telephone-line-airline-position-segment))
        (accent . (telephone-line-buffer-name-segment))
        (nil    . (telephone-line-buffer-modified-segment)))

      telephone-line-rhs
      '((nil    . (telephone-line-minions-mode-segment))
        (accent . (telephone-line-vc-segment))
        (nil    . (telephone-line-misc-info-segment))))

(setq display-time-24hr-format            t
      display-time-day-and-date           t
      display-time-default-load-average   nil
      display-time-load-average           nil
      display-time-load-average-threshold nil)

(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode +1))

(display-time-mode +1)
(telephone-line-mode +1)

(add-hook 'writeroom-mode-enable-hook #'(lambda () (text-scale-adjust 2)))
(add-hook 'writeroom-mode-disable-hook #'(lambda () (text-scale-adjust 0)))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(selectrum-mode +1)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)

;; C-c bindings (mode-specific-map)
(global-set-key (kbd "C-c h") 'consult-history)
(global-set-key (kbd "C-c m") 'consult-mode-command)
(global-set-key (kbd "C-c k") 'consult-kmacro)

;; C-x bindings (ctl-x-map)
(global-set-key (kbd "C-x M-:") 'consult-complex-command)     ;; orig. repeat-complex-command
(global-set-key (kbd "C-x b")   'consult-buffer)              ;; orig. switch-to-buffer
(global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
(global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
(global-set-key (kbd "C-x r b") 'consult-bookmark)            ;; orig. bookmark-jump
(global-set-key (kbd "C-x p b") 'consult-project-buffer)      ;; orig. project-switch-to-buffer

;; Custom M-# bindings for fast register access
(global-set-key (kbd "M-#")   'consult-register-load)
(global-set-key (kbd "M-'")   'consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
(global-set-key (kbd "C-M-#") 'consult-register)

;; Other custom bindings
(global-set-key (kbd "M-y")      'consult-yank-pop) ;; orig. yank-pop
(global-set-key (kbd "<help> a") 'consult-apropos)  ;; orig. apropos-command

;; M-g bindings (goto-map)
(global-set-key (kbd "M-g e")   'consult-compile-error)
;;(global-set-key (kbd "M-g f")   'consult-flymake)   ;; Alternative: consult-flycheck
(global-set-key (kbd "M-g g")   'consult-goto-line) ;; orig. goto-line
(global-set-key (kbd "M-g M-g") 'consult-goto-line) ;; orig. goto-line
(global-set-key (kbd "M-g o")   'consult-outline)   ;; Alternative: consult-org-heading
(global-set-key (kbd "M-g m")   'consult-mark)
(global-set-key (kbd "M-g k")   'consult-global-mark)
(global-set-key (kbd "M-g i")   'consult-imenu)
(global-set-key (kbd "M-g I")   'consult-imenu-multi)

;; M-s bindings (search-map)
(global-set-key (kbd "M-s d") 'consult-find)
(global-set-key (kbd "M-s D") 'consult-locate)
(global-set-key (kbd "M-s g") 'consult-grep)
(global-set-key (kbd "M-s G") 'consult-git-grep)
(global-set-key (kbd "M-s r") 'consult-ripgrep)
(global-set-key (kbd "M-s l") 'consult-line)
(global-set-key (kbd "M-s L") 'consult-line-multi)
(global-set-key (kbd "M-s m") 'consult-multi-occur)
(global-set-key (kbd "M-s k") 'consult-keep-lines)
(global-set-key (kbd "M-s u") 'consult-focus-lines)

;; Isearch integration
(global-set-key (kbd "M-s e") 'consult-isearch-history)
(define-key isearch-mode-map (kbd "M-e")   'consult-isearch-history) ;; orig. isearch-edit-string
(define-key isearch-mode-map (kbd "M-s e") 'consult-isearch-history) ;; orig. isearch-edit-string
(define-key isearch-mode-map (kbd "M-s l") 'consult-line)            ;; needed by consult-line to detect isearch
(define-key isearch-mode-map (kbd "M-s L") 'consult-line-multi)      ;; needed by consult-line to detect isearch

;; Minibuffer history
(define-key minibuffer-local-map (kbd "M-s") 'consult-history) ;; orig. next-matching-history-element
(define-key minibuffer-local-map (kbd "M-r") 'consult-history) ;; orig. previous-matching-history-element

;; Enable automatic preview at point in the *Completions* buffer. This is
;; relevant when you use the default completion UI.
(add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode)

;; Optionally configure the register formatting. This improves the register
;; preview for `consult-register', `consult-register-load',
;; `consult-register-store' and the Emacs built-ins.
(setq register-preview-delay    0.5
      register-preview-function #'consult-register-format)

;; Optionally tweak the register preview window.
;; This adds thin lines, sorting and hides the mode line of the window.
(advice-add #'register-preview :override #'consult-register-window)

;; Optionally replace `completing-read-multiple' with an enhanced version.
(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function       #'consult-xref
      xref-show-definitions-function #'consult-xref)



;; Optionally configure preview. The default value
;; is 'any, such that any key triggers the preview.
;; (setq consult-preview-key 'any)
;; (setq consult-preview-key (kbd "M-."))
;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
;; For some commands and buffer sources it is useful to configure the
;; :preview-key on a per-command basis using the `consult-customize' macro.
(with-eval-after-load 'consult
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<")) ;; (kbd "C-+")

(global-set-key (kbd "M-A") 'marginalia-cycle)
(define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)

(marginalia-mode +1)

(global-set-key (kbd "C-.")   'embark-act)      ;; pick some comfortable binding
(global-set-key (kbd "C-,")   'embark-dwim)     ;; good alternative: M-.
(global-set-key (kbd "C-h B") 'embark-bindings) ;; alternative for `describe-bindings'

;; Optionally replace the key help with a completing-read interface
(setq prefix-help-command #'embark-prefix-help-command)

(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)

(setq company-idle-delay                0.5
      company-show-numbers              t
      company-tooltip-limit             10
      company-minimum-prefix-length     2
      company-tooltip-align-annotations t
      ;; invert the navigation direction if the the completion
      ;; popup-isearch-match is displayed on top (happens near the bottom of
      ;; windows)
      company-tooltip-flip-when-above   t)

(global-company-mode +1)
(company-prescient-mode +1)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(yas-global-mode +1)

(setq citar-bibliography '("~/Dropbox/org/bibs/references.bib"))

(global-set-key (kbd "C-c b") 'citar-insert-citation)
(define-key minibuffer-local-map (kbd "M-b") 'citar-insert-preset)

;; use consult-completing-read for enhanced interface
(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

(global-set-key (kbd "C-;")   'avy-goto-char)
(global-set-key (kbd "C-:")   'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)

(setq which-key-idle-delay    0.5
      which-key-separator     " "
      which-key-sort-order    'which-key-description-order
      which-key-prefix-prefix "+")
(which-key-mode +1)

(global-set-key (kbd "C-c o") 'crux-open-with)

(global-set-key [remap kill-line]       #'crux-smart-kill-line)
(global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
(global-set-key (kbd "C-S-k")           #'crux-kill-line-backwards)
(global-set-key (kbd "s-k")             #'crux-kill-and-join-forward)

(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)

(global-set-key [(control shift return)] 'crux-smart-open-line-above)
(global-set-key [(shift return)]         'crux-smart-open-line)

(global-set-key (kbd "C-c n") 'crux-cleanup-buffer-or-region)
(global-set-key (kbd "C-c f") 'crux-recentf-find-file)
(global-set-key (kbd "C-c F") 'crux-recentf-find-directory)
(global-set-key (kbd "C-c u") 'crux-view-url)
(global-set-key (kbd "C-c e") 'crux-eval-and-replace)
(global-set-key (kbd "C-c D") 'crux-delete-file-and-buffer)
(global-set-key (kbd "C-c c") 'crux-copy-file-preserve-attributes)
(global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c r") 'crux-rename-file-and-buffer)
(global-set-key (kbd "C-c t") 'crux-visit-term-buffer)
(global-set-key (kbd "C-c k") 'crux-kill-other-buffers)


(global-set-key (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
(global-set-key (kbd "C-c z")   'crux-indent-defun)
(global-set-key (kbd "C-c TAB") 'crux-indent-rigidly-and-copy-to-clipboard)

(global-set-key (kbd "C-x 4 t") 'crux-transpose-windows)

(global-set-key (kbd "C-x C-u") 'crux-upcase-region)
(global-set-key (kbd "C-x C-l") 'crux-downcase-region)
(global-set-key (kbd "C-x M-c") 'crux-capitalize-region)

(global-unset-key (kbd "C-_"))
(global-set-key [remap undo]  'undo-fu-only-undo)
(global-set-key (kbd "C-?")   'undo-fu-only-redo)
(global-set-key (kbd "C-x U") 'undo-fu-only-redo)
