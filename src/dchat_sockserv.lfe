;; Provides high-level functions over pure sockserv.
;; Translation to external protocol is described here.
(defmodule dchat_sockserv
  (export all))

(defun logged_in (sockserv)
  (sockserv_serv:send sockserv (tuple 'logged_in ())))

(defun message (sockserv from body)
  (sockserv_serv:send sockserv (tuple 'message (list from body))))

(defun nick_taken (sockserv nick)
  (sockserv_serv:send sockserv (tuple 'nick_taken (list nick))))

(defun add_server (sockserv addr port)
  (sockserv_serv:send sockserv (tuple 'add_server (list (tuple addr port)))))

(defun remove_server (sockserv addr port)
  (sockserv_serv:send sockserv (tuple 'remove_server (list (tuple addr port)))))