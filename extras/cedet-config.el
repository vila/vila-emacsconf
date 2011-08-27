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

(add-hook 'semantic-init-hooks 'vila-semantic-hook)

(defvar vila-cedet-loaded t)

(custom-set-variables
 '(semantic-idle-scheduler-idle-time 3)
 '(semantic-self-insert-show-completion-function (lambda nil (semantic-ia-complete-symbol-menu (point))))
 '(global-semantic-tag-folding-mode t nil (semantic-util-modes)))
