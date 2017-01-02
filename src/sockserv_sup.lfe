(defmodule sockserv_sup
  (export (start_link 1)
          (init 1)
          (accept 0)))

(defun start_link (listen-socket)
  (supervisor:start_link (tuple 'local (MODULE)) (MODULE) listen-socket))

(defun init (listen-socket)
  (let ((sup-flags (map 'strategy 'simple_one_for_one))
        (socket-server-spec (map 'id 'socket_server
                                 'start `#(sockserv_serv
                                           start_link
                                           (,listen-socket)))))
    (tuple 'ok (tuple sup-flags (list socket-server-spec)))))

(defun accept ()
  (supervisor:start_child (MODULE) ()))