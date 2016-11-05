(defmodule dchat_node_app
  (export (start 2)
          (stop 1)))

(defun start (start-type start-args)
  (lfe_io:format "Starting dchat node app...~n" ())
  (let (((tuple 'ok node-port) (application:get_env 'dchat_node 'node_port))
        ((tuple 'ok client-port) (application:get_env 'dchat_node 'client_port)))
    (print-env node-port client-port)
    (let (((tuple 'ok node-socket) (dchat_socket:listen node-port))
          ((tuple 'ok client-socket) (dchat_socket:listen client-port)))
      (dchat_node_sup:start_link node-socket client-socket))))

(defun stop (state)
  (lfe_io:format "dchat node app stopped~n" ())
  'ok)

(defun print-env (node-port client-port)
  (lfe_io:format "~nEnvironment settings:~n" ())
  (lfe_io:format "* node listen port: ~p~n" (list node-port))
  (lfe_io:format "* client listen port: ~p~n~n" (list client-port)))
