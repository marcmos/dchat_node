(defmodule dchat_conn_pool_sup
  (export (start_link 2)
          (init 1)))

;;; API
(defun start_link (port handler)
  (supervisor:start_link (MODULE) (tuple port handler)))

;;; supervisor callbacks
(defun init
  (((tuple port handler))
   (let ((sup-flags (map 'strategy 'one_for_all))
         (conn-pool-serv-spec
          (map 'id 'conn_pool_serv
               'start `#(dchat_conn_pool_serv start_link (,port ,handler)))))
     (tuple 'ok (tuple sup-flags (list conn-pool-serv-spec))))))