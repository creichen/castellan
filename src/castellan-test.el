;;; castellan-test.el --- tests for castellan -*- lexical-binding: t -*-

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

(require 'el-mock)
(require 'dash)

;; Set up default key bindings

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
  (-let [at-root (null level)]
    (when at-root (setq level 1))
    (if (and at-root
	     (stringp s-expr))
	;; top-level string?  Print directly and finish
	(insert s-expr)
      ;; else
      (progn
	(insert (format "%s" (make-string level ?*)))
	(dolist (item s-expr)
	  (if (symbolp item)
	      (insert (format " %s" item))))
	(insert "\n")
	(dolist (item s-expr)
	  (if (listp item)
              (harness--unparse-agenda item (1+ level))))))))

(defun harness--buffer-name (buf-sym)
  "Map a symbol to the corresponding test buffer name"
  (concat "*castellan-test-harness-todo-" (symbol-name buf-sym) "*"))

(defmacro harness-run-with-buffers (&rest spec)
  "Run commands after setting up buffers.

SPEC takes a number of keyword parameters:

- :run COMMANDS is a list of command to run
- :todos BUFFERS is a list of buffer specs for agenda todo buffers

BUFFERS are an alist of the form ((ID BODY) ... (ID BODY)), where
ID is a symbol that identifies the buffer and BODY is either
a string containing verbatim buffer contents or an S-expression that
defines the buffer contents, such as
(headline (TODO task-item) (DONE done-item) (subheading))."
  (-let [(&plist :run commands :todos todo-buffers) spec]
    `(progn
       (setq harness--test-buffers-alist nil)
       (setq castellan-agenda-inbox nil)
       (setq castellan-agenda-todos nil)
       (setq castellan-agenda-calendars nil)
       (dolist (buffer-spec ,todo-buffers)
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
	 (kill-buffer buf)))))

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

(defun harness-filter-items (items props &rest predicate)
  "Given list of plists ITEMS, compute list of lists of PROPS for each item.

If PREDICATE is non-nil, only return elements E for which (PREDICATE E)
returns non-nil."
  (-let [result nil]
    (dolist (item items)
      (when (or (null predicate)
		(apply (car predicate) (list (cdr item))))
	(push (mapcar (lambda (p) (plist-get (cdr item) p)) props)
	      result)))
    (reverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for test harness

(ert-deftest test-harness ()
  "Validate test harness"
  (harness-run-with-buffers
   :todos '((A (foo (TODO bar) (TODO alphabeta)))
	      (B (quux)))
   :run (
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
   ))

(ert-deftest test-harness-filter-items ()
  "Validate harness-filter-items"
  (should (equal '((1 2)
		   (nil "foo"))
		 (harness-filter-items '((ITEM :c 1 :b 2 :a 1)
					 (ITEM :b "foo"))
				       '(:a :b))))
  (should (equal '((3 2)
		   (nil "bar"))
		 (harness-filter-items '((ITEM :c 1 :b 2 :a 3)
					 (ITEM :c 2 :b "bar")
					 (ITEM :b "foo"))
				       '(:a :b)
				       (lambda (e) (plist-get e :c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for datetime processing

(ert-deftest test-parse-weekspec ()
  "Validate week and weekday parsing"
  (let ((calendar-week-start-day 1))
    (should (equal '(nil nil 7 0)
		   (castellan--parse-week "## Sun")))
    (should (equal '(nil 33 nil nil)
		   (castellan--parse-week "# W33")))
    (should (equal '(1978 nil nil nil)
		   (castellan--parse-week "#1978")))
    (should (equal '(1978 nil 1 1)
		   (castellan--parse-week "#1978" '(nil nil 1 1))))
    (should (equal '(1978 2 nil nil)
		   (castellan--parse-week "#1978" '(nil 2 nil nil))))
    (should (equal '(2000 nil 2 2)
		   (castellan--parse-week "## Tue" '(2000 nil nil nil))))
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
  (let ((castellan-time-locale "C"))
    (should (equal "2000 W03: Wed"
		   (castellan--weekspec-format '(2000 3 3 3))))
    (should (equal "2022 W52: Sun"
		   (castellan--weekspec-format '(2022 52 7 0))))
    (should (equal "2022 W52"
		   (castellan--weekspec-format '(2022 52 nil nil))))
    (should (equal "2023 52"
		   (castellan--weekspec-format '(2022 52 7 0) "%Y %V")))))

(ert-deftest test-headline-split ()
  "tests castellan--headline-split"
  (should (equal '(:activity nil :time nil :duration nil :headline "foo bar")
		 (castellan--headline-split "foo bar")))
  (should (equal '(:activity nil :time "11:00" :duration nil :headline "foo")
		 (castellan--headline-split "=11:00 | foo")))
  (should (equal '(:activity "topic" :time nil :duration nil :headline "foo")
		 (castellan--headline-split "topic | foo")))
  (should (equal '(:activity "topic" :time nil :duration nil :headline "foo")
		 (castellan--headline-split "topic|foo")))
  (should (equal '(:activity "topic" :time nil :duration nil :headline "foo")
		 (castellan--headline-split " topic |        foo")))
  (should (equal '(:activity "topic" :time "13:00" :duration "1m" :headline "foo")
		 (castellan--headline-split "=13:00+1m| topic |        foo"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for specification parsing

(ert-deftest test-parse-activity-label ()
  "Basic activity label parsing"
  (harness-run-with-buffers
   :todos '((A "
* title-0
** TODO todo-0
* TODO top-0
* ACT1 | title-1
** TODO todo-ACT1-foo
*** TODO ACT3 | todo-ACT3-subact
** TODO todo-ACT1-bar
** TODO todo-ACT1-quux
* TODO top-2
* title-2
** TODO todo-2
* TODO ACT2 | top-3"))
   :run (
	 (should (equal '(
			  ;; debug: who sets activity away from nil?
			  ;; idea 1: remember level at which activity was introduced, if current activity from same level, null first
			  ;;  (except we're already nulling first?)
			  (2 "todo-0" todo nil)
			  (1 "top-0" todo nil)
			  (2 "todo-ACT1-foo" todo "ACT1")
			  (3 "todo-ACT3-subact" todo "ACT3")
			  (2 "todo-ACT1-bar" todo "ACT1")
			  (2 "todo-ACT1-quux" todo "ACT1")
			  (1 "top-2" todo nil)
			  (2 "todo-2" todo nil)
			  (1 "top-3" todo "ACT2")
			  )
			(harness-filter-items (castellan--activity-items)
					      '(:level :headline :todo-type :activity))
			)))))


(ert-deftest test-parse-inferred-weekspec ()
  "Basic activity label parsing"
  (harness-run-with-buffers
   :todos '((A "
* #2024
** # W01
*** ## Mon
**** TODO =10:00 | start
**** TODO =11:00+5m | foo
*** ## Tue
**** TODO =12:00 | bar
"))
   :run (
	 (should (equal '(
			  ;; debug: who sets activity away from nil?
			  ;; idea 1: remember level at which activity was introduced, if current activity from same level, null first
			  ;;  (except we're already nulling first?)
			  ("start"
			   ((0 0 10 nil nil nil nil -1 nil)
			    (2024 1 1 1)))
			  ("foo"
			   ((0 0 11 nil nil nil nil -1 nil)
			    (2024 1 1 1)))
			  ("bar"
			   ((0 0 12 nil nil nil nil -1 nil)
			    (2024 1 2 2)))
			  )
			(harness-filter-items (castellan--activity-items)
					      '(:headline :scheduled))
			)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for basic operations

(ert-deftest test-todo-to-done ()
  "Marking TODO as DONE should work"
  (harness-run-with-buffers
   :todos '((A (TODO bar)))
   :run (
	 (should (harness-get-org-node '(A bar)))
	 (with-current-buffer (castellan--activity-refresh)
	   (goto-line 2)
	   (castellan-item-done))
	 (should (equal
		  '((A (DONE bar)))
		  (harness-parsed-buffers)))
   )))

