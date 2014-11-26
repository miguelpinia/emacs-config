;; Cambia todas las preguntas yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No crear archivos ~ mientras editas.
(setq create-lockfiles nil)

(setq inhibit-startup-message t)

;; Permite usar acentos en emacs áéíóúüä
(require 'iso-transl)
