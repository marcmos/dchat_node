(defmodule dchat_socket
  (export (listen 1)))

(defun listen (port)
  (lfe_io:format "Calling listen on port ~p...~n" (list port))
  (gen_tcp:listen port ()))
