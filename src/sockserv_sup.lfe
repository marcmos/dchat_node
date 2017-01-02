(defmodule sockserv_sup
  (export (start_link 2)
          (accept 0)
          (init 1)))

;;; API
(defun start_link (listen-socket handler)
  (supervisor:start_link (tuple 'local (MODULE))
                         (MODULE)
                         (tuple listen-socket handler)))

;; Starts new acceptor
(defun accept ()
  (supervisor:start_child (MODULE) ()))

;;; supervisor callbacks
(defun init
  (((tuple listen-socket handler))
   (let ((sup-flags (map 'strategy 'simple_one_for_one))
         (socket-server-spec (map 'id 'socket_server
                                  'start `#(sockserv_serv
                                            start_link
                                            (,listen-socket ,handler)))))
     (tuple 'ok (tuple sup-flags (list socket-server-spec))))))
