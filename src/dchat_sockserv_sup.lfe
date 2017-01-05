(defmodule dchat_sockserv_sup
  (export (start_link 3)
          (accept 1)
          (init 1)))

;;; API
(defun start_link (listen-socket accept-callback msg-callback)
  (supervisor:start_link (MODULE)
                         (list listen-socket accept-callback msg-callback)))

(defun accept (sup)
  (supervisor:start_child sup ()))

;;; supervisor callbacks
(defun init (args)
  (let ((sup-flags (map 'strategy 'simple_one_for_one))
        (socket-server-spec (map 'id 'socket_server
                                 'start `#(dchat_sockserv_serv start_link ,args)
                                 'restart 'temporary)))
    (tuple 'ok (tuple sup-flags (list socket-server-spec)))))
