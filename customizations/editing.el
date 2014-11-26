;; Personalización relacionada a la edición de buffers

;; Combinación de teclas para usar "hippie expand" para autocompletado
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Resaltado de matching de parentesis
(show-paren-mode 1)

;; Resaltado de la línea actual.
(global-hl-line-mode 1)

;; Keybindings para búsqueda interactiva. Por default, C-s ejecuta
;; isearch-forward.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; No usar tabs duros
(setq-default indent-tabs-mode nil)

;; Cuando tu abres un archivo, te lleva al último lugar donde estuviste editándolo.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; mantiene la lista de lugares en ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs puede crear respaldos de archivos automáticamente. Esto le dice a emacs
;; que los debe almacenar en  ~/.emacs.d/backups. Más información:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)


;; Comentarios
(defun toggle-comment-on-line ()
  "Comenta o descomenta la línea actual"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; yay arcoiris!
(global-rainbow-delimiters-mode t)

;; usa 2 espacios para los tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(setq electric-indent-mode nil)

;; Elimina espacios en blanco al guardar
(add-hook 'before-save-hook 'delete-trailing-whitespace)
