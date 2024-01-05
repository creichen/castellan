;;; todo.el --- Castellan activity tracker: TODO management -*- lexical-binding: t -*-

;; Copyright (C) 2023 Christoph Reichenbach

;; Author: Christoph Reichenbach <creichen@creichen.net>
;; Maintainer: Christoph Reichenbach <creichen@creichen.net>
;; Created: 02 Jan 2024

;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (dash "2.0") (compat "29.1"))

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

;;; Commentary:

;; TBD

;; Requires `dash' and `compat-29'.

;; Terminology:
;; - tags: all the things managed by mu4e-tagging-mode, comprising
;;   - flags: toggles that are independent of each other
;;   - categories: selections that are mutually exclusive.
;;
;; Mails may violate the "categories" restriction and/or include
;; unknown tags; we handle these cases gracefully.
;;
;; - A mail is:
;;   - cagetorised ("categorized" in function/var definitions) iff it
;;     has at least one category
;;   - uncategorised ("uncategorized" in code) otherwise

;;; Code:

(require 'compat-29)
(require 'dash)
(require 'org-ql)

;(require 'benchmark)
;(benchmark-elapse (org-ql-select (org-agenda-files)

;; default for new TODO items
(setq castellan-agenda-inbox creichen/org-agenda-inbox-file)
;; TODO buffers
(setq castellan-agenda-todos creichen/org-agenda-todo-files)
;; calendar buffers (only ASSIGNATIONS, i.e., elements that must not be auto-scheduled)
(setq castellan-agenda-calendars creichen/org-agenda-todo-files)

(defun castellan--get-all-org-items ()
  (org-ql-select (cons castellan-agenda-inbox castellan-agenda-todos)
    t
    :action (lambda ()
              ;; At each heading, collect the necessary information
              (let* ((headline-raw (org-get-heading))
		     (headline (org-element-headline-parser headline-raw))
		     (level (org-current-level))
                     (buffer (current-buffer))
                     (file-path (buffer-file-name))
                     (position (point))
		     (properties (org-entry-properties)))
                (list :headline-props (cadr headline)
                      :buffer buffer
                      :file file-path
                      :position position
		      :properties properties)
                ))
    ))

(defun castellan--headline-get-activity (headline)
  (if (string-prefix-p "=" headline) ;; Old-style duration annotations: ignore (temp workaround)
      nil
    (let ((split (string-split headline "|")))
      (when (cdr split)
	(string-clean-whitespace (car split))))))

(defun castellan--headline-get (headline)
  (string-clean-whitespace
   (let ((split (string-split headline "|")))
     (if (cdr split)
	 ;; there was an activity?  Strip it out
	 (substring headline (+ 1 (length (car split))))
       ;; Keep unchanged
       headline))))

(defun castellan--org-file-strip (filename)
  (file-name-base filename))

(defun castellan--aggregate-org-items (items)
  (let ((last-file nil)
	(last-level 0)
	(activity-stack nil)
	(activity nil)
	(prefixes nil)
	(activities nil)
	(last-headline nil)
	(last-pos nil)
	(last-was-todo nil)
	(results nil))
    (dolist (item items)
      (-let* (((&plist :headline-props headline-props
		       :buffer buffer
		       :file file
		       :position position
		       :properties properties) item)
	      ((&plist :level level :priority priority :todo-keyword todo-status :raw-value unsplit-headline) headline-props)
	      (headline (castellan--headline-get unsplit-headline))
	      (new-activity (castellan--headline-get-activity unsplit-headline))
	      )
	(when (not (string= file last-file))
	  (setq last-file file)
	  (setq last-level 0)
	  (setq activity-stack nil)
	  (setq activity nil)
	  (setq prefixes nil)
	  (setq last-headline (castellan--org-file-strip file)))
	(while (> last-level level)
	  ;; level down
	  (setq last-level (- last-level 1))
	  (setq activity (pop activities))
	  (pop prefixes)
	  )
	(when new-activity
	  (setq activity new-activity))
	(while (< last-level level)
	  ;; level up
	  (setq last-level (+ last-level 1))
	  (push activity activities)
	  (push last-headline prefixes)
	  (setq last-headline nil)
	  )
	(setq last-headline headline)
	;; produce output:
	(let ((castellan-id (cons headline prefixes)))
	  (if todo-status
	      (progn
		(when (not last-was-todo)
		  (push (cons 'MARKER (list :castellan-id prefixes :activity activity :level level :pos last-pos :path prefixes))
			results)
		  )
		(push (cons 'ITEM (list :castellan-id castellan-id :headline headline :todo-status todo-status :priority priority :activity activity :level level :pos (list buffer file position) :properties properties :path prefixes))
		      results)
		(setq last-was-todo t))
	  ;; not a TODO item (incl, DONE items)?
	    (setq last-was-todo nil)))
	  ;; Finally, retain the previous position
	  (setq last-pos (list buffer file position))
	)
      )
    (reverse results)))

(defun castellan--propertize-todo-keyword (keyword)
  (-let [props (alist-get keyword org-todo-keyword-faces nil nil #'string=)]
    (if props
	(propertize keyword 'face props)
      ;; fall back to unpropertized
      keyword
      ))
  )

(defun castellan--propertize-priority (priority)
  (cond
   ((not priority)
    " ")
   ((eq ?A priority)
    (propertize "A" 'face 'castellan-todo-priority-A))
   ((eq ?A priority)
    (propertize "B" 'face 'castellan-todo-priority-B))
   (t (propertize
       (format "%c" priority) 'face 'castellan-todo-priority-C))
  ))



(defun castellan--insert-aggregate-items (items)
  (dolist (item items)
    (-let* (((type . item-properties) item)
	    ((&plist :castellan-id castellan-id :level level :pos pos :path path :activity activity-raw) item-properties)
	    (activity (if activity-raw
			  activity-raw
			""))
	    )
      (-let [next-line
	     (if (eq 'MARKER type)
		 ;; marker/separator
		 (format "%-8s %s\n" activity (propertize (mapconcat 'identity (reverse path) "/")
								 'face 'castellan-context-info))
	       ;; item
	       (-let* (((&plist :todo-status todo-status :priority priority :properties properties :headline headline) item-properties)
		       (prio (if priority
				 (concat (castellan--propertize-priority priority) ":")
			       ""))
		       (indentation (make-string level ?>)))
		 (format "%-8s  %9s %2s%s %s\n"
			 activity
			 (castellan--propertize-todo-keyword todo-status)
			 prio
			 (propertize indentation 'face 'castellan-todo-indentation)
			 (propertize headline 'face 'castellan-todo-headline)))
	       )]
	(insert (propertize (propertize next-line
					'castellan item)
			    'castellan-id castellan-id)))
      )))

(defgroup castellan nil
  "Customization group for the 'castellan' package."
  :prefix "castellan-"
  :group 'applications)


(defun castellan-todo--debug ()
  (interactive)
  (message (format "%s" ;(castellan-todo--info-at-point)
		   (get-text-property (point) 'castellan-id)
)))



(defun castellan-todo--info-at-point (&rest required-type)
  "Returns the plist that describes the item or marker at point.

If REQUIRED-TYPE is not nil, it should be 'ITEM or 'MARKER.
In that case, if the entry at point is not of type
REQUIRED-TYPE, this function returns nil."
  (-let [(type . prop)  (get-text-property (point) 'castellan)]
    (when (or (not required-type)
	      (eq (car required-type) type))
      prop)))


(defun castellan-todo-jump-to-item ()
  "Jump to position of the TODO item at point."
  (interactive)
  (-let [(&plist :pos (buffer filename position)) (castellan-todo--info-at-point)]
    (when filename
      (find-file filename)
      (when position
	(goto-char position)))
    ))

(defmacro castellan-todo--in-org-buffer (pos &rest body)
  "Execute BODY in buffer POS"
  `(-let [(buffer filename position) pos]
    (with-current-buffer (find-file filename)
      (goto-char position)
      ,@body
      )
    ))

(defmacro castellan-todo--update (&rest body)
  "Execute body and then update the TODO buffer"
  `(-let [(&plist :castellan-id castellan-id) (castellan-todo--info-at-point)]
     ,@body
     (switch-to-buffer (castellan-todo--refresh))
     (castellan-todo--goto-id castellan-id)
     ))

(defun castellan-todo--goto-id (id)
  (interactive)
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))  ; While not found and not at end of buffer
      (if (equal id
		 (get-text-property (point) 'castellan-id))
          (progn
	    (setq found t)
	    (message "found at: %s" (point)))
	(forward-line 1))
      )
    (unless found
      (message "Item has disappeared"))))

(defun castellan-todo--item-update (mode)
  (interactive)
  (castellan-todo--update
   (-let* ((info (castellan-todo--info-at-point 'ITEM))
	   ((&plist :pos pos) info))
     (castellan-todo--in-org-buffer pos
				    ;; disable notes
				    (let ((current-prefix-arg 0)) (org-todo mode))
				    ))
   ))

(defun castellan-todo-item-done ()
  (interactive)
  (castellan-todo--item-update "DONE"))

(defun castellan-todo-item-todo ()
  (interactive)
  (castellan-todo--item-update "TODO"))

(defun castellan-todo-item-cancel ()
  (interactive)
  (castellan-todo--item-update "CANCELLED"))

(defvar-keymap castellan-todo-mode-map
  :doc "Keymap for castellan TODO management."
  "RET"           #'castellan-todo-jump-to-item
  "?"             #'castellan-todo--debug
  "+"             #'castellan-todo-item-done
  "-"             #'castellan-todo-item-todo
  "<backspace>"   #'castellan-todo-item-cancel
  )

(define-derived-mode castellan-todo--mode fundamental-mode "castellan:Todo"
  "Castellan activity manager: Integrated TODO mode.

\\{castellan-todo-mode-map}"
  (castellan-todo--mode-setup)
  :mode-class 'special
  :group 'castellan
  :keymap castellan-todo-mode-map
  )

(defun castellan-todo--mode-setup ()
  (hl-line-mode 1)    ; Enable line highlighting
  (use-local-map castellan-todo-mode-map)  ; Activate our keymap
  )

(defun castellan-todo--buffer ()
  (get-buffer-create "*Castellan TODO*"))

(defun castellan-todo--refresh ()
 (let ((buffer (castellan-todo--buffer)))
   (with-current-buffer buffer
     (read-only-mode)
     (let ((inhibit-read-only t))
       (erase-buffer)
       (castellan-todo--mode)
       (castellan--insert-aggregate-items
	(castellan--aggregate-org-items
	 (castellan--get-all-org-items)))
       ))
   buffer))


;; NASTY BUT APPARENTLY NECESSARY
(defun org-add-log-note ())


; --------------------------------------------------------------------------------

(defface castellan-todo-headline
  '((t :inherit org-level-3))
  "Face for TODO item headlines in castellan."
  :group 'castellan)

(defface castellan-todo-indentation
  '((t :inherit org-indent))
  "Face for TODO item indentation markers in castellan."
  :group 'castellan)

(defface castellan-todo-effort
  '((t :inherit org-duration))
  "Face for TODO item effort/estimated duration in castellan."
  :group 'castellan)

(defface castellan-todo-priority-A
  '((t :inherit org-upcoming-deadline))
  "Face for TODO item headlines in castellan."
  :group 'castellan)

(defface castellan-todo-priority-B
  '((t :inherit org-distant-deadline))
  "Face for TODO item headlines in castellan."
  :group 'castellan)

(defface castellan-todo-priority-C
  '((t :inherit org-priority))
  "Face for TODO item headlines in castellan."
  :group 'castellan)

(defface castellan-todo-scheduled-time
  '((t :inherit org-scheduled))
  "Face for TODO item scheduled start time in castellan."
  :group 'castellan)

(defface castellan-todo-delegated
  '((t :inherit org-tag))
  "Face for delegated TODO items in castellan."
  :group 'castellan)

(defface castellan-todo-delegatee-name
  '((t :inherit font-lock-function-name-face))
  "Face for the name of the person a task is delegated to in castellan."
  :group 'castellan)

(defface castellan-todo-waiting
  '((t :inherit org-warning))
  "Face for TODO items waiting for someone in castellan."
  :group 'castellan)

(defface castellan-todo-waiting-name
  '((t :inherit font-lock-variable-name-face))
  "Face for the name of the person a task is waiting for in castellan."
  :group 'castellan)

(defface castellan-context-info
  '((t :inherit org-document-info))
  "Face for context information lines in castellan."
  :group 'castellan)


;; --------------------------------------------------------------------------------

(when nil
  (let ((buffer (castellan-todo--refresh)))
    (pop-to-buffer buffer))
)
