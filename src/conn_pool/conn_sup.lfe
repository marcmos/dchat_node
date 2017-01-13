(defmodule conn_sup
  (export all))

(defun start_link ()
  (supervisor:start_link (MODULE) ()))

(defun accept (ref listen-socket handlers)
  (supervisor:start_child ref (list listen-socket handlers)))

(defun init (args)
  (let ((sup-flags (map 'strategy 'simple_one_for_one))
        (sockserv-sup-spec (map 'id 'sockserv_sup
                                'start `#(sockserv_sup start_link (,(self)))
                                'restart 'temporary
                                'type 'supervisor)))
    (tuple 'ok (tuple sup-flags (list sockserv-sup-spec)))))