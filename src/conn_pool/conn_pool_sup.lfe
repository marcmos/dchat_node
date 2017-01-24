(defmodule conn_pool_sup
  (export all))

;;; API
(defun start_link (server-name port handlers)
  (supervisor:start_link (MODULE) (list server-name port handlers)))

(defun start_link (port handlers)
   (supervisor:start_link (MODULE) (list port handlers)))

;;; supervisor callbacks
(defun init (args)
  (let ((sup-flags (map 'strategy 'one_for_all))
        (serv-spec (map 'id 'conn_pool_serv
                        'start `#(conn_pool_serv start_link ,args))))
    (tuple 'ok (tuple sup-flags (list serv-spec)))))
