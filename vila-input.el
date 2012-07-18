;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings

;; iedit search and replace
(define-key global-map (kbd "C-;") 'iedit-mode)

;; Expand on M-<
(global-set-key (kbd "M-<") 'dabbrev-expand)

;; Compilation
(global-set-key (kbd "<f1>") 'recompile)
(global-set-key (kbd "<C-f1>") 'compile)

;; Indent whole buffer
(global-set-key (kbd "C-c b") 'indent-buffer)

;; Next/Prev line on M-n and M-p, same as C-n and C-p
(define-key global-map (kbd "M-n") 'next-line)
(define-key global-map (kbd "M-p") 'previous-line)

;; Duplicate line C-c d
(global-set-key (kbd "C-c d") 'duplicate-line)

;; Use the ido version of imenu with C-x j
(global-set-key (kbd "C-x j") 'ido-goto-symbol)

;; GTAGS keybindings
(global-set-key (kbd "C-<return>") 'gtags-find-tag-from-here)
(global-set-key (kbd "C-'") 'gtags-pop-stack)
(global-set-key (kbd "C-ä") 'gtags-find-rtag)


;; Toggle fullscreen emacs
(global-set-key (kbd "<f12>") 'toggle-fullscreen)
