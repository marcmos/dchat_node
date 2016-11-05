(defmodule dchat_client_acceptor
  (export (start_link 1)
          (accept 1)))

(defun start_link (socket)
  (let ((pid (spawn_link (MODULE) 'accept (list socket))))
    (register (MODULE) pid)
    (tuple 'ok pid)))

(defun accept (socket)
  (lfe_io:format "Accepting client connections...~n" ())
  (gen_tcp:accept socket))
