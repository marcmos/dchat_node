(defmodule dchat_user_sup
  (export all)) ;; FIXME add exports

;;; API
(defun start_link ()
  (supervisor:start_link (tuple 'local (MODULE)) (MODULE) ()))

(defun start_user (sockserv)
  (supervisor:start_child (MODULE) (list sockserv)))

;;; supervisor callbacks
(defun init (args)
  (let ((sup-flags (map 'strategy 'simple_one_for_one))
        (user-spec (map 'id 'user_fsm
                            'start `#(dchat_user start_link ,args)
                            'restart 'temporary)))
    (tuple 'ok (tuple sup-flags (list user-spec)))))