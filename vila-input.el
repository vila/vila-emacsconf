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
(global-set-key (kbd "C-c j") 'ido-goto-symbol)

;; Toggle fullscreen emacs
(global-set-key (kbd "<f12>") 'toggle-fullscreen)

(defun vila-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  (local-set-key (kbd "<C-SPC>") 'semantic-complete-analyze-inline)
  ;;(local-set-key "\C-c=" 'semantic-decoration-include-visit)

  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  ;;  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  ;;  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)

  ;;(local-set-key "." 'semantic-complete-self-insert)
  ;;(local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref))

(when 'vila-cedet-loaded
  (add-hook 'c-mode-common-hook 'vila-cedet-hook))