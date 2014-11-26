;; Estabkece el path de ejecución desde el shell.
;; Esto es para cuando se utiliza en mac.
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(exec-path-from-shell-copy-envs
 '("PATH"))
