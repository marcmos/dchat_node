(defmodule dchat_sockserv_sup
  (export (start_link 1)
          (init 1)
          (accept 2)))

;;; API
(defun start_link (handler)
  (supervisor:start_link (MODULE) (list handler)))

(defun accept (sup listen-socket)
  (supervisor:start_child sup (list listen-socket)))

;;; supervisor callbacks
(defun init (args)
  (let ((sup-flags (map 'strategy 'simple_one_for_one))
        (socket-server-spec (map 'id 'socket_server
                                 'start `#(dchat_sockserv_serv start_link ,args)
                                 'restart 'temporary)))
    (tuple 'ok (tuple sup-flags (list socket-server-spec)))))
