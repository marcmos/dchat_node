;;; Main node supervisor
(defmodule dchat_node_sup
  (export (start_link 1)
          (init 1)))

(defun start_link (listen-socket)
  (lfe_io:format "Starting supervision tree...~n" ())
  (supervisor:start_link (tuple 'local (MODULE)) (MODULE) (list listen-socket)))

(defun init
  (((list listen-socket))
   (let ((sup-flags (map))
         (child-specs (list
                       ;; conn_pool_sup
                       )))
     (tuple 'ok (tuple sup-flags child-specs)))))