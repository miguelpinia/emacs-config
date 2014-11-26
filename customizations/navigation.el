;; Estás personalización hacen fácil el navegar entre archivos, intercambiar entre buffers,
;; y elegir opciones desde el minibuffer.

;; "Cuando varios buffers visitan archivos llamados de manera igual,
;; Emacs les da a los buffers nombres distintos. El método usual
;; Para hacer nombres únicos, es añadir '<2>', '<3>', etc. al final
;; del nombre del buffer.
;; El siguiente método de nombrado incluye parte del directorio de
;; archivos al principio del nombre del buffer.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.htm
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Activa el modo de archivos recientes para que puedas cambiar más facilmente
;; a los archivos recientemente editados al moemnto de iniciar emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)


;; El modo ido, permite navegar más facilmente entre opciones. Por ejemplo,
;; cuando tu quieres cambiarte entre buffers, ido presenta una lista de
;; de buffers en el mini-buffer.
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(ido-mode t)

;; Esto permite búsquedas parciales, ej. "tl" hace match con "Tyrion Lannister"
(setq ido-enable-flex-matching t)

;; Apago este comportamiento por que es molesto
(setq ido-use-filename-at-point nil)

;; No intenta hacer match con todos los archivos a través de los directorios de trabajo activos, sólo lo hace
;; con el directorio actual mostrado en el minibuffer.
(setq ido-auto-merge-work-directories-length -1)

;; Incluye en el buffer los nombres de los archivos abiertos recientemente,
;; incluso si no están abiertos.
(setq ido-use-virtual-buffers t)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(ido-ubiquitous-mode 1)

;; Muestra la lista de buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Realza M-x para permitir ejecución sencilla de comandos
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; projectile everywhere!
(projectile-global-mode)
