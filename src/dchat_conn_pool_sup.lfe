(defmodule dchat_conn_pool_sup
  (export (start_link 1)
          (init 1)))

;;; API
(defun start_link (port)
  (supervisor:start_link (MODULE) (tuple port)))

;;; supervisor callbacks
(defun init
  (((tuple port))
   (let ((sup-flags (map 'strategy 'one_for_all))
         (conn-pool-serv-spec
          (map 'id 'conn_pool_serv
               'start `#(dchat_conn_pool_serv start_link (,port)))))
     (tuple 'ok (tuple sup-flags (list conn-pool-serv-spec))))))