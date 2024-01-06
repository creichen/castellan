;;; todo-test.el --- tests for castellan's todo.el -*- lexical-binding: t -*-

;; Copyright (C) 2024 Christoph Reichenbach

;; Author: Christoph Reichenbach <creichen@creichen.net>
;; Maintainer: Christoph Reichenbach <creichen@creichen.net>
;; Created: 05 Jan 2024

;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (dash "2.0") (compat "29.1"))

;; Keywords: mail
;; URL: https://github.com/creichen/mu4e-tagging

;; This file is not part of GNU Emacs.

 ;; mu4e-tagging is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; mu4e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mu4e.  If not, see <http://www.gnu.org/licenses/>.

(require 'package)
(package-initialize)

(load-file "todo.el")
(require 'el-mock)
(require 'dash)

;; Set up default key bindings

(mu4e-tagging-setup-default-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test harness

(setq cas-agenda-todos (list "DUMMY"))

(defun harness--parse-agenda-todos-recursively (nodelist)
  "Helper function for harnes--parse-agenda"
  (mapcar (lambda (elt)
	    (when (and (listp elt)
		       (eq (car elt) 'headline))
	      (-let* (((&plist :raw-value name :todo-keyword todo-kw) (cadr elt))
		      (head (append
			     (when todo-kw
			       (list (intern todo-kw)))
			     (list (intern name)))))
		(append head (harness--parse-agenda-todos-recursively (cddr elt))))
	    ))
	  nodelist)
  )

(defun harness--parse-agenda (buffers-alist)
  "Parse all (BUF-ID . BUFFER) pairs in BUFFERS-ALIST"
  (let (parsed-todos)
    (dolist (buf-pair buffers-alist)
      (let* ((buf (cdr buf-pair))
	     (buf-id (car buf-pair)))
	(with-current-buffer buf
	  ;;(message "Getting %s" (cddr (org-element-parse-buffer)))
	  (let ((buf-body (harness--parse-agenda-todos-recursively (cddr (org-element-parse-buffer)))))
	    (push (cons buf-id buf-body)
		  parsed-todos)))))
    (nreverse parsed-todos)))

(defun harness--unparse-agenda (s-expr &optional level)
  "Unparse an S-expression S-EXPR into org-mode content in the current buffer."
  (unless level (setq level 1))
  (insert (format "%s" (make-string level ?*)))
  (dolist (item s-expr)
    (if (symbolp item)
	(insert (format " %s" item))))
  (insert "\n")
  (dolist (item s-expr)
    (if (listp item)
        (harness--unparse-agenda item (1+ level)))))

(defun harness--buffer-name (buf-sym)
  "Map a symbol to the corresponding test buffer name"
  (concat "*castellan-test-harness-todo-" (symbol-name buf-sym) "*"))

(defmacro harness-run-with-buffers (buffers &rest commands)
  "Run COMMANDS after setting up BUFFERS.

BUFFERS are an alist of the form ((ID BODY) ... (ID BODY)), where
ID is a symbol that identifies the buffer and BODY is an
S-expression that defines the buffer contents, such
as (headline (TODO task-item) (DONE done-item) (subheading))."
  `(progn
     (setq harness--test-buffers-alist nil)
     (setq castellan-agenda-todos nil)
     (dolist (buffer-spec ,buffers)
       (-let* (((buf-name-spec buf-body-spec) buffer-spec)
	       (buf-name (harness--buffer-name buf-name-spec))
	       (buffer (get-buffer-create buf-name)))
	 (with-current-buffer buffer
	   (erase-buffer)
	   (org-mode)
	   (harness--unparse-agenda buf-body-spec))
	 (push buffer
	       castellan-agenda-todos)
	 (push (cons buf-name-spec buffer)
	       harness--test-buffers-alist)))
     (setq castellan-agenda-todos (nreverse castellan-agenda-todos))
     (setq harness--test-buffers-alist (nreverse harness--test-buffers-alist))
     ,@commands
     ;; on success only:
     (dolist (buf castellan-agenda-todos)
       (kill-buffer buf))))

(defun harness-get-buffer (buffer-id)
  "Retrieves the buffer for a given BUFFER-ID symbol."
  (alist-get buffer-id harness--test-buffers-alist))

(defun harness--get-org-node-recursively (node ref)
  "Recursive helper for `harness-get-org-node'"
  (if (null ref)
    node
    (-let ((name (symbol-name (car ref)))
	   (result nil))
      (dolist (headline (cddr node))
	(when (string= name (plist-get (cadr headline) :raw-value))
	  (setq result (harness--get-org-node-recursively headline (cdr ref)))))
      (should result)
      result
      )))

(defun harness-get-org-node (org-node-ref)
  "Retrieves the org-node for a reference.

References are lists of the form (BUFFER-ID HEADING SUBHEADING ...)."
  (with-current-buffer (alist-get (car org-node-ref) harness--test-buffers-alist)
    (harness--get-org-node-recursively (org-element-parse-buffer) (cdr org-node-ref))))


(defun harness-parsed-buffers (&optional buffers)
  "Parses active test buffers into the buffer spec format used by `harness-run-with-buffers'.

Optionally, BUFFERS can be a list of BUF-ID symbols to parse,
otherwise all buffers are parsed again."
  (let ((proper-buffers
	 (if buffers
	     (mapcar (lambda (bufid) (cons bufid (harness-get-buffer bufid)))
		     buffers)
	   ;; else
	   harness--test-buffers-alist)))
    (harness--parse-agenda proper-buffers)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Tests for test harness

(ert-deftest test-harness ()
  "Validate test harness"
  (harness-run-with-buffers
   '((A (foo (TODO bar) (TODO alphabeta)))
     (B (quux)))
   (should (harness-get-buffer 'A))
   (should (harness-get-buffer 'B))
   (should (not (harness-get-buffer 'C)))
   ;; next one should fail
   (should (equal "bar"
		  (plist-get (cadr (harness-get-org-node '(A foo bar)))
			     :raw-value
			     )))
	    '((A (foo (TODO bar) (TODO alphabeta)))
	      (B (quux))))
   (should (equal
	    '((A (foo (TODO bar) (TODO alphabeta)))
	      (B (quux)))
	    (harness-parsed-buffers)))
   )

(ert-deftest test-parse-weekspec ()
  "Validate week and weekday parsing"
  (let (calendar-week-start-day 1)
    (should (equal '(nil nil 7 0)
		   (castellan-todo--parse-week "#Sun")))
    (should (equal '(nil 33 nil nil)
		   (castellan-todo--parse-week "#W33")))
    (should (equal '(1978 nil nil nil)
		   (castellan-todo--parse-week "#Y1978")))
    (should (equal '(1978 nil 1 1)
		   (castellan-todo--parse-week "#Y1978" '(nil nil 1 1))))
    (should (equal '(1978 2 nil nil)
		   (castellan-todo--parse-week "#Y1978" '(nil 2 nil nil))))
    (should (equal '(2000 nil 2 2)
		   (castellan-todo--parse-week "#Tue" '(2000 nil nil nil))))
  ))

(ert-deftest test-weekspec-to-datetime ()
  "Validate weekspec to datetime conversion"
  (should (equal '(nil nil nil 19 1 2000 3 nil nil)
		 (castellan--weekspec-to-datetime '(2000 3 3 3))))
  ;; corner case:
  (should (equal '(nil nil nil 1 1 2023 0 nil nil)
		 (castellan--weekspec-to-datetime '(2022 52 7 0)))))


(ert-deftest test-weekspec-format ()
  "Weekspec stringification"
  (let castellan-time-locale "C"
       (should (equal "2000 W03: Wed"
		      (castellan--weekspec-format '(2000 3 3 3))))
       (should (equal "2022 W52: Sun"
		      (castellan--weekspec-format '(2022 52 7 0))))
       (should (equal "2022 W52"
		      (castellan--weekspec-format '(2022 52 nil nil))))
       (should (equal "2023 52"
		      (castellan--weekspec-format '(2022 52 7 0) "%Y %V")))))
