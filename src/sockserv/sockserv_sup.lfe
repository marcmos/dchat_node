(defmodule sockserv_sup
  (export all))

;;; API
(defun start_link (conn-sup listen-socket handlers)
  (supervisor:start_link (MODULE) (list conn-sup listen-socket handlers)))

;;; supervisor callbacks
(defun init (args)
  (let ((sup-flags (map 'strategy 'one_for_all))
        (serv-spec (map 'id 'serv
                        'start (tuple 'sockserv_serv 'start_link args)
                        'restart 'temporary)))
    (tuple 'ok (tuple sup-flags (list serv-spec)))))
