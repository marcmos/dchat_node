(defmodule socket_acceptor
  (export (start_link 1)))

(defun start_link (port)
  (tuple 'ok (spawn_link 'acceptor 'start-server (list port))))

(defun acceptor (socket)
  (lfe_io:format "New acceptor: ~p on socket ~p~n" (list (self) socket))
  (gen_tcp:accept socket)
  (receive ((tuple 'tcp _ msg) (lfe_io:format "Got message: ~p~n" (list msg))))
  (lfe_io:format "PID ~p dying... (closing socket)~n" (list (self)))
  (gen_tcp:close socket))