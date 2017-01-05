;;; Main node supervisor
(defmodule dchat_node_sup
  (export (start_link 1)
          (init 1)))

(defun start_link (client-port)
  (lfe_io:format "Starting supervision tree...~n" ())
  (supervisor:start_link (MODULE)
                         (tuple client-port)))

(defun init
  (((tuple client-port))
   (let ((sup-flags (map))
         (echo-spec (map 'id 'echo
                         'start `#(dchat_echo start_link ())))
         (conn-pool-sup-spec (map 'id 'conn_pool_sup
                                  'start `#(dchat_conn_pool_sup
                                            start_link
                                            (,client-port dchat_echo))
                                  'type 'supervisor)))
     (tuple 'ok (tuple sup-flags (list echo-spec
                                       conn-pool-sup-spec))))))