(defmodule dchat_user_serv
    (export all)) ;; FIXME add exports/missing callbacks

;;; API
(defun start_link ()
  (gen_server:start_link (tuple 'local (MODULE)) (MODULE) () ()))

(defun register (nick)
  (gen_server:call (MODULE) (tuple 'register nick (self))))

(defun lookup (nick)
  (gen_server:call (MODULE) (tuple 'lookup nick)))

;;; gen_server callbacks
(defun init (args)
  (let* (((tuple 'ok nodes) (application:get_env 'nodes))
         (create-result (mnesia:create_table (table_name)
                                             (list (tuple 'ram_copies nodes)))))
    (case create-result
      (#(atomic ok)
       (error_logger:info_msg "~p created new user table~n" (list (MODULE))))
      (`#(aborted #(already_exists ,_))
       (error_logger:warning_msg "~p skipped user table creation~n" (list (MODULE)))))
    (tuple 'ok ())))

(defun handle_call
    (((tuple 'register nick pid) from state)
     (handle_register nick pid state))
    (((tuple 'lookup nick) from state)
     (handle_lookup nick state)))

;;; Internal functions
(defun table_name ()
  'users)

(defun handle_register (nick pid state)
  (case (mnesia:activity 'transaction
                         (lambda () (mnesia:write (tuple (table_name) nick pid))))
    ('ok (tuple 'reply 'ok state))))

(defun handle_lookup (nick state)
  (case (mnesia:activity 'transaction
                         (lambda () (mnesia:read (table_name) nick)))
    (() (tuple 'reply 'not_exists state))
    ((list (tuple _ nick pid)) (tuple 'reply pid state))))