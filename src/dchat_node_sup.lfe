; Main node supervisor
(defmodule dchat_node_sup
  (export (start_link 2)
          (init 1)))

(defun start_link (node-socket client-socket)
  (lfe_io:format "Starting supervision tree...~n" ())
  (supervisor:start_link (tuple 'local (MODULE)) (MODULE) (list node-socket client-socket)))

(defun init
  (((list node-socket client-socket))
   (let ((sup-flags (map))
         (child-specs (list (map 'id 'dchat_acceptor_sup
                                 'start `#(dchat_acceptor_sup start_link (,node-socket ,client-socket))))))
     (tuple 'ok (tuple sup-flags child-specs)))))