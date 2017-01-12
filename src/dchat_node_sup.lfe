(defmodule dchat_node_sup
  (export (start_link 1)
          (init 1)))

;;; API
(defun start_link (client-port)
  (lfe_io:format "Starting supervision tree...~n" ())
  (supervisor:start_link (MODULE)
                         (tuple client-port)))

;;; supervisor callbacks
(defun init
  (((tuple client-port))
   (let ((sup-flags (map))
         (user-serv-spec (map 'id 'user_serv
                              'start `#(dchat_user_serv start_link ())))
         (echo-spec (map 'id 'echo
                         'start `#(dchat_echo start_link ())))
         (conn-pool-sup-spec (map 'id 'conn_pool_sup
                                  'start `#(dchat_conn_pool_sup
                                            start_link
                                            (,client-port dchat_echo))
                                  'type 'supervisor)))
     (tuple 'ok (tuple sup-flags (list user-serv-spec
                                       echo-spec
                                       conn-pool-sup-spec))))))