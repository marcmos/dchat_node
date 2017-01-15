(defmodule dchat_node
  (export (start 2)
          (stop 1)
          (send 3)))

;;; application callbacks
(defun start (start-type start-args)
  (lfe_io:format "Starting dchat node app...~n" ())
  (let (((tuple 'ok listen-port) (application:get_env 'listen_port))
        ((tuple 'ok nodes) (application:get_env 'nodes)))
    (mnesia:change_config 'extra_db_nodes nodes)
    (print-env listen-port)
    (dchat_node_sup:start_link listen-port)))

(defun stop (state)
  (lfe_io:format "dchat node app stopped~n" ())
  'ok)

;;; Internal functions
(defun print-env (listen-port)
  (lfe_io:format "~nEnvironment settings:~n" ())
  (lfe_io:format "* client listen port: ~p~n~n" (list listen-port)))

(defun send (from to msg)
  (case (dchat_user_serv:lookup to)
    ('not_exists
     (tuple 'error 'user_not_exists))
    (recipient ;; TODO add is_pid guard
     (dchat_user:send recipient from msg))))