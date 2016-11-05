(defmodule dchat_acceptor_sup
  (export (start_link 2)
          (init 1)))

(defun start_link (node-socket client-socket)
  (supervisor:start_link (tuple 'local (MODULE)) (MODULE) (list node-socket client-socket)))

(defun init
  (((list node-socket client-socket))
   (let ((sup-flags (map))
         (child-specs (list (map 'id 'dchat_node_acceptor
                                 'start `#(dchat_node_acceptor start_link (,node-socket)))
                            (map 'id 'dchat_client_acceptor
                                 'start `#(dchat_client_acceptor start_link (,client-socket))))))
     (tuple 'ok (tuple sup-flags child-specs)))))