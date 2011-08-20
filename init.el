;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Viktor "vila" Larsson's Emacs config ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc settings

;; Extra load paths
(add-to-list 'load-path "~/.emacs.d") 
(add-to-list 'load-path "~/.emacs.d/extras/")


;; Disable backup files
(setq make-backup-files nil)

;; Line numbers everywhere
(global-linum-mode 1)
(line-number-mode 1)

;; Show Column numbers
(column-number-mode 1)

;; Fancy ansi colors in shell, yes please
(add-hook 'shell-mode-hook
          'ansi-color-for-comint-mode-on)

;; Visible bell
(setq visible-bell t)

;; PC Selection mode (S-left to extend regsion)
(pc-selection-mode)

;; Disable right mouse button
(global-unset-key (kbd "<mouse-3>"))

;; C-x, C-v, C-c for cut, paste and copy
(cua-mode t)

;; Keep selection after copying
(setq cua-keep-region-after-copy t)

;; No region when it is not highlighted
(transient-mark-mode 1)

;; Scroll line by line instead of recentering on a new page
(setq scroll-step            1
      scroll-conservatively  10000)

;; Mouse wheel scroll settings
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3)))
;; don't accelerate scrolling 
(setq mouse-wheel-progressive-speed nil)
;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)


;; Turn off local CVS copies of files
(setq vc-cvs-stay-local nil)

;; Tabs are the devil!
(setq-default indent-tabs-mode nil)

;; No toolbar
(tool-bar-mode 0)
(menu-bar-mode 0)

;; Scrollbars on the right side of the screen
(set-scroll-bar-mode 'right)

;; Disable Emacs splash screen
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ settings                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use K&R for indentation
(setq c-default-style "k&r")
(setq c-basic-offset 4)


;; Highlight TODO and similar in comments
(defun highlight-todo ()
  (font-lock-add-keywords nil
    '(("\\(REVIEW\\|FIXME\\|TODO\\|BUG\\)" 1 font-lock-warning-face t))))
(add-hook 'c-mode-common-hook 'highlight-todo)
(add-hook 'c++-mode-common-hook 'highlight-todo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Libs/Extras                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Color theme - zenburn
(require 'color-theme)
(require 'color-theme-zenburn)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-zenburn)

;; highlight the current line; set a custom face, so we can
;; recognize from the normal marking (selection)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#4f4f4f")  ;; Emacs 22 Only


;; Uniquify - for unique buffer names
(require 'uniquify)
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

;; ido - for awesome buffer and file selection
(require 'ido) 
(ido-mode 'both) ;; for buffers and files
(setq
  ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
  ido-case-fold  t                 ; be case-insensitive
  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-confirm-unique-completion t  ; wait for RET, even with unique completion
  confirm-nonexistent-file-or-buffer nil) ; makes it so you dont have to double enter for new file

;; ido support for imenu
(load "~/.emacs.d/extras/ido-imenu")


;; Emacs in fullscreen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))

;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Key bindings                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Expand on M-<
(global-set-key (kbd "M-<") 'dabbrev-expand)


;; Indent whole buffer on C-c b
(defun indent-buffer ()
  "Indent every line in the buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))
(global-set-key (kbd "C-c b") 'indent-buffer)

;; Next/Prev line on M-n and M-p, same as C-n and C-p
(define-key global-map (kbd "M-n") 'next-line)
(define-key global-map (kbd "M-p") 'previous-line)


;; Duplicate current line with C-c C-c
;; TODO if current line is blank, copy previous line instead
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))
;; TODO change keybinding
(global-set-key (kbd "C-c C-c") 'duplicate-line)

;; Use the ido version of imenu with C-x j
(global-set-key (kbd "C-x j") 'ido-goto-symbol)

;; Toggle fullscreen emacs
(global-set-key (kbd "<f12>") 'toggle-fullscreen)



;; Load CEDET if it exists
(if (file-exists-p "~/.emacs.d/site-lisp/cedet-1.0/common/cedet.el")
    (load-file "~/.emacs.d/extras/cedet-config.el"))