;;;;
;; Paquetes
;;;;

;; Define los repositorios para obtener paquetes para Emacs
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa-stable.milkbox.net/packages/")))


;; Carga y activa paquetes de emacs. Hace que primero los
;; paquetes sean cargados antes de que tu inicies a modificarlos.
;; Esto también establece el path de carga.
(package-initialize)

;; Descarga la descripción ELPA del archivo si es necesario.
;; Esto informa a Emacs sobre la última versión de todos los paquetes
;; y hace que estén disponibles para descarga.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Los paquetes que quieres sean instalados. También puedes instalar
;; paquetes con M-x package-install.
;; Añade tantos como tu quieras.
(defvar my-packages
  '(;; Permite hacer expresiones lisp de manera mucho más fácil.
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings y colorización para Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; Resaltado extra para clojure
    clojure-mode-extra-font-locking

    ;; Integración con Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; Permite el uso de IDO en tantos contextos como sea posible.
    ;; Ver la línea 47 de customizations/better-defaults.el
    ;; para tener una descripción de IDO.
    ido-ubiquitous

    ;; Mejora M-x para permitir una fácil ejecución de comandos.
    ;; Provee una lista filtrada de posibles comandos en el minibuffer.
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; On OS X, an Emacs instance started from the graphical user
    ;; interface will have a different environment than a shell in a
    ;; terminal window, because OS X does not run a shell during the
    ;; login. Obviously this will lead to unexpected results when
    ;; calling external utilities like make from Emacs.
    ;; This library works around this problem by copying important
    ;; environment variables from the user's shell.
    ;; https://github.com/purcell/exec-path-from-shell
    exec-path-from-shell

    ;; Navegación de proyectos
    projectile

    ;; Matching de parentesis con colores.
    rainbow-delimiters

    ;; Edita etiquetas html como expresiones simbólicas.
    tagedit

    ;; Integración con GIT
    magit)

  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customización
;;;;

;; Añade un directorio para añadir nuestro path de carga para cuando quieras "cargar" cosas
;; después, Emacs sabrá donde buscar por el archivo correspondiente.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Configura el exec-path-from-shell para que emacs pueda usar las
;; variables de entorno correctas
(load "shell-integration.el")

;; Estás personalización hacen fácil el navegar entre archivos, intercambiar entre buffers,
;; y elegir opciones desde el minibuffer.
(load "navigation.el")

;; Estas configuraciones cambian la forma en que emacs se ve. Además
;; de habilitar y deshabilitar algunos elementos de la interfaz de usuario.
(load "ui.el")

;; Está personalización hace que la edición sea un poco más bonita.
(load "editing.el")

;; Personalizaciones dificiles de categorizar.
(load "misc.el")

;; Para editar lisps.
(load "elisp-editing.el")

;; Language-specific
(load "setup-clojure.el")
(load "setup-js.el")

(custom-set-variables
 '(coffee-tab-width 2))

;; Usa WhiteSpace
;;(require 'whitespace)
