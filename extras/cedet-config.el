;; CEDET configuration

(load-file "~/.emacs.d/site-lisp/cedet-1.0/common/cedet.el")

(require 'semantic-ia)
(require 'semanticdb)
(require 'semanticdb-file)
(require 'semantic-gcc)

;; dont load project management
(global-ede-mode 0)  
;; Enable prototype help and smart completion                     
(semantic-load-enable-excessive-code-helpers)

;; Enable template insertion menu TODO
; (global-srecode-minor-mode 1)


(defun vila-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))

(add-hook 'semantic-init-hooks 'my-semantic-hook)


(defun vila-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  (local-set-key (kbd "<C-SPC>") 'semantic-ia-complete-symbol-menu)
  ;;'semantic-complete-analyze-inline)
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

(add-hook 'c-mode-common-hook 'vila-cedet-hook)
;(add-hook 'c++-mode-common-hook 'vila-cedet-hook)

(custom-set-variables
 '(semantic-idle-scheduler-idle-time 3)
 '(semantic-self-insert-show-completion-function (lambda nil (semantic-ia-complete-symbol-menu (point))))
 '(global-semantic-tag-folding-mode t nil (semantic-util-modes)))
(global-semantic-folding-mode 1)