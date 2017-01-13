(defmodule sockserv_sup
  (export all))

;;; API
(defun start_link (listen-socket handlers)
  (supervisor:start_link (MODULE) (list listen-socket handlers)))

;;; supervisor callbacks
(defun init (args)
  (let ((sup-flags (map 'strategy 'one_for_one))
        (serv-spec (map 'id 'serv
                        'start (tuple 'sockserv_serv 'start_link args)
                        'restart 'temporary)))
    (tuple 'ok (tuple sup-flags (list serv-spec)))))
