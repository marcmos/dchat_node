(defmodule dchat_node_sup
  (export (start_link 1)
          (init 1)))

;;; API
(defun start_link (listen-port)
  (lfe_io:format "Starting supervision tree...~n" ())
  (supervisor:start_link (MODULE)
                         (tuple listen-port)))

;;; supervisor callbacks
(defun init
  (((tuple listen-port))
   (let ((sup-flags (map))
         (event-spec (map 'id 'event
                          'start #(dchat_node_event start_link ())))
         (user-serv-spec (map 'id 'user_serv
                              'start #(dchat_user_serv start_link ())))
         (user-sup-spec (map 'id 'user_sup
                             'start #(dchat_user_sup start_link ())
                             'type 'supervisor))
         (conn-pool-sup-spec (map 'id 'conn_pool_sup
                                  'start `#(conn_pool_sup
                                            start_link
                                            (#(local dchat_conn_pool_serv)
                                             ,listen-port
                                             (dchat_sockserv_event)))))
         (directory-serv-spec (map 'id 'directory_serv
                                   'start #(dchat_directory_serv
                                            start_link
                                            ())
                                   'type 'supervisor)))
     (tuple 'ok (tuple sup-flags (list event-spec
                                       user-serv-spec
                                       user-sup-spec
                                       conn-pool-sup-spec
                                       directory-serv-spec))))))