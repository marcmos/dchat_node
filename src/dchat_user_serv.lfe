(defmodule dchat_user_serv
    (export all)) ;; FIXME add exports/missing callbacks

;;; API
(defun start_link ()
  (gen_server:start_link (tuple 'local (MODULE)) (MODULE) () ()))

(defun register (nick)
  (gen_server:call (MODULE) (tuple 'register nick (self))))

(defun unregister (nick)
  (gen_server:call (MODULE) (tuple 'unregister nick)))

(defun unregister_node (node)
  (gen_server:call (MODULE) (tuple 'unregister_node node)))

(defun lookup (nick)
  (gen_server:call (MODULE) (tuple 'lookup nick)))

(defun list ()
  (gen_server:call (MODULE) 'list))

;;; gen_server callbacks
(defun init (args)
  (case (init_table)
    ('ok (tuple 'ok ()))))

(defun handle_call
  (((tuple 'register nick pid) from state)
   (handle_register nick pid state))
  (((tuple 'unregister nick) from state)
   (handle_unregister nick state))
  (((tuple 'unregister_node node) from state)
   (handle_unregister_node node state))
  (((tuple 'lookup nick) from state)
   (handle_lookup nick state))
  (('list from state)
   (handle_list state)))

;;; Internal functions
(defun table_name () 'users)

(defun init_table ()
  (let* (((tuple 'ok nodes) (application:get_env 'nodes))
         (create-result (mnesia:create_table (table_name)
                                             (list (tuple 'ram_copies nodes)))))
    (case create-result
      (#(atomic ok)
       (error_logger:info_msg "~p created new user table~n" (list (MODULE))))
      (`#(aborted #(already_exists ,_))
       (error_logger:warning_msg "~p skipped user table creation~n" (list (MODULE))))))
  'ok)

(defun handle_register (nick pid state)
  (case (mnesia:activity 'transaction
                         (lambda ()
                           (case (mnesia:read (tuple (table_name) nick))
                             (() (mnesia:write (tuple (table_name) nick pid)))
                             ((_) 'nick_taken))))

    ;; FIXME temporarily do not check nick ownership
    ;; (lambda () (mnesia:write (tuple (table_name) nick pid))))
    ('ok (tuple 'reply 'ok state))
    ('nick_taken (tuple 'reply 'nick_taken state))))

;; FIXME abstract out mnesia actions out of cast responses
;; (see dchat_directory_serv)
(defun handle_unregister (nick state)
  (case (mnesia:activity 'transaction
                         (lambda () (mnesia:delete (tuple (table_name) nick))))
    ('ok (tuple 'reply 'ok state))))

;; FIXME rewrite it (select should suffice)
(defun handle_unregister_node (node state)
  (mnesia:activity 'transaction
                   (lambda ()
                     (let ((users (mnesia:foldl
                                   (lambda (user user-list)
                                     (cons user user-list))
                                   ()
                                   (table_name))))
                       (lists:map
                        (lambda (user)
                          ;; FIXME create record
                          (if (=:= node (node (element 3 user)))
                            (mnesia:delete (tuple (table_name)
                                                  (element 2 user)))))
                        users)
                       (tuple 'reply users state)))))

(defun handle_lookup (nick state)
  (case (mnesia:activity 'transaction
                         (lambda () (mnesia:read (table_name) nick)))
    (() (tuple 'reply 'not_exists state))
    ((list (tuple _ nick pid)) (tuple 'reply pid state))))

(defun handle_list (state)
  (case (mnesia:activity 'transaction
                    (lambda ()
                      (mnesia:foldl (lambda (user user-list)
                                      (cons user user-list))
                                    ()
                                    (table_name))))
    (users (tuple 'reply users state))))