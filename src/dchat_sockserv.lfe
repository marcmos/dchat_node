;; Provides high-level functions over pure sockserv.
;; Translation to external protocol is described here.
(defmodule dchat_sockserv
  (export all))

(defun logged_in (sockserv)
  (sockserv_serv:send sockserv (tuple 'logged_in ())))

(defun message (sockserv from body)
  (sockserv_serv:send sockserv (tuple 'message (list from body))))