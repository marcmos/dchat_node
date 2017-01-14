(defmodule conn_pool_sup
  (export all))

;;; API
(defun start_link (port handlers)
  (supervisor:start_link (MODULE) (tuple port handlers)))

;;; supervisor callbacks
(defun init
  (((tuple port handlers))
   (let ((sup-flags (map 'strategy 'one_for_all))
         (conn-pool-serv-spec
          (map 'id 'conn_pool_serv
               'start `#(conn_pool_serv start_link (,port ,handlers)))))
     (tuple 'ok (tuple sup-flags (list conn-pool-serv-spec))))))
