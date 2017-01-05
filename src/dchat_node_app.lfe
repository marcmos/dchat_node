(defmodule dchat_node_app
  (export (start 2)
          (stop 1)))

;;; application callbacks
(defun start (start-type start-args)
  (lfe_io:format "Starting dchat node app...~n" ())
  (let (((tuple 'ok client-port) (application:get_env 'dchat_node 'client_port)))
    (print-env client-port)
    (dchat_node_sup:start_link client-port)))

(defun stop (state)
  (lfe_io:format "dchat node app stopped~n" ())
  'ok)

;;; Internal functions
(defun print-env (client-port)
  (lfe_io:format "~nEnvironment settings:~n" ())
  (lfe_io:format "* client listen port: ~p~n~n" (list client-port)))
