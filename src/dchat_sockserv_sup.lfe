(defmodule dchat_sockserv_sup
  (export (start_link 0)
          (accept 3)
          (init 1)))

;;; API
(defun start_link ()
  (supervisor:start_link (MODULE)
                         ()))

(defun accept (sup listen-socket accept-callback)
  (supervisor:start_child sup (list listen-socket accept-callback)))

;;; supervisor callbacks
(defun init (args)
  (let ((sup-flags (map 'strategy 'simple_one_for_one))
        (socket-server-spec (map 'id 'socket_server
                                 'start `#(dchat_sockserv_serv start_link ,args)
                                 'restart 'temporary)))
    (tuple 'ok (tuple sup-flags (list socket-server-spec)))))
