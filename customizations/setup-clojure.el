;;;;
;; Clojures
;;;;

;; Establece paredit para Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; Esto es de ayuda cuando se manejan cadenas camel-case como los nombres en java
;; (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; Un poco más de resaltado de sintaxis.
(require 'clojure-mode-extra-font-locking)

;; Resaltado de sintaxis para midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))))

;;;;
;; Cider
;;;;

;; Provee documentación en el minibuffer para código que estás escribiendo en el repl
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Va directo al buffer REPL cuando se ha terminado de conectar
(setq cider-repl-pop-to-buffer-on-connect t)

;; Cuando hay un error en cider, muestra su buffer y cambiate a él.
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Donde se almacena la historia de cider.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; .
(setq cider-repl-wrap-history t)

;; Activa el paredit en REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Usa clojure para otras extensiones.
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))


;; key bindings
;; Esto ayuda cuando se desarrollan web apps
(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))


(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))
