;; Estás personalizaciones hace que emacs se vea diferente y
;; habilita/deshabilita algunas interfaces de usuario.

;; Inhabilita la barra de menus
(menu-bar-mode -1)

;; Inhabilita la barra de herramientas
(when (fboundp 'scroll-bar-mode)
  (tool-bar-mode -1))


;; Muestra números de líneas.
(global-linum-mode)

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Temas para emacs.
;; Leer http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; para tener una mejor explicación de los temas de emacs.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; Para una explicación más técnica.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'tomorrow-night-bright t)

;; Incrementa el tamaño de la fuente para una mejor lectura
(set-face-attribute 'default nil :height 145)

;; Descomente las siguientes para establecer el ancho (número de carácteres)
;; y el alto (número de lineas).
;; (setq initial-frame-alist '((top . 0) (left . 0)
;;                             (width . 177) (height . 53)))

;; Estás configuración permiten a emacs interactuar con el sistema operativo
(setq
 ;; Permite que killing/yanking interactuen con el clipboard
 x-select-enable-clipboard t

 ;; No estoy seguro que hace esto, pero es recomendable?
 x-select-enable-primary t

 ;; Save clipboard strings into kill ring before replacing them.
 ;; When one selects something in another program to paste it into Emacs,
 ;; but kills something in Emacs before actually pasting it,
 ;; this selection is gone unless this variable is non-nil
 save-interprogram-paste-before-kill t

 ;; Muestra todas las opciones cuando se ejecuta apropos. Para mayor información.
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
 apropos-do-all t

 ;; Mouse yank commands yank at point instead of at click.
 mouse-yank-at-point t)

;; Evita que el cursor parpadee.
(blink-cursor-mode 0)

;; Path completo en la barra de títulos.
(setq-default frame-title-format "%b (%f)")

;; No muestra el menu de fuentes.
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; No campanea.
(setq ring-bell-function 'ignore)

;; (require 'show-wspace)
;; (show-ws-toggle-show-trailing-whitespace)
