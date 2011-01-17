;;; minutes-mode.el --- 

;; Copyright (C) 2008-2011  Shelagh Manton <shelagh.manton@gmail.com>

;; Author: Shelagh Manton <shelagh.manton@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Commentary: This is only an early work in progress. Or not progress exactly as I have
;;; not done anything on this for ages.  Intended to be a quick way for entering draft
;;; minutes. Not intended for publication from emacs, though that is not out of the
;;; question in the future. Set up so it is set out as org-mode syntax, this way I get
;;; publication for free and can use the org-struct minor mode for easy outlining.  ring
;;; motions, easy set-up through abbrevs for participants, apologies. etc leverages emacs
;;; builtin dabbrevs and abbrevs along with skeletons.  Todo: indexing functions for
;;; minutes. (lgrep or traverse-lisp.el Easily find topics discussed in meetings later.
;;; Set the file name to an iso date plus project or dir name perhaps a minor mode
;;; restricted to the minutes mode -possible?  macro for setting up the headers for the
;;; particular club, company etc. a la smirc in myerc.el?  setup a special directory for
;;; particular sets of minutes. makes for easier searching of topics. ala eproject.el use
;;; defstruct to create a new structure minutes that has its own set of abbrevs and
;;; inherits the various slots or functions. and its own directory. so that each minute
;;; derivative can reside easily in its own directory.  I favour a style of coding that
;;; encompasses great confusion and lots of circular thinking. I write copious comments as I try
;;; to work out what could work and what may not.

;;; Code:

;;; global variables
;;(add-to-list 'committee-list committee-name)

(defvar committee-list nil
"A list of committee names created with make-committee.  It will
somehow be used to separate the various committee things and yet
be loaded when that committee is loaded.")

; "
;;; Set up the committee structure.
(defstruct (committee (:constructor create-committee)
		      ;(:constructor create-sub-committee) ;have a look at the example
		     (:type list)
		      :named)
  (name :read-only t)
  (header :read-only t)
  members ;make a function which takes this list and adds it in an abbrev table such that
	;the Initials plus say - will be the abbrev for the name. ? two names with same
	;initials? can you make an abbrev table on the fly for when a mode is loaded?
  (committee-dir :read-only t)
  default-location ;string to say where the room is for the meeting can be nil if there is no default location.
  default-chair ;String for the name of the default chair of committee
  )
;;; functions to open files and set directories.


;;; let's just get on with some simple things.
(define-skeleton minutes-apologies
"Insert a list of names of people who didn't turn up for the meeting."
nil
"* Apologies:     " ("Who didn't come? " str ", " ) 
)

(define-skeleton minutes-attended
"Insert a list of people who attended meeting. Use abbrev list for this."
nil
"* Attended by:   " ("Who came? " str", ")
)

(define-skeleton minutes-sub-header
"Insert preamble to minutes."
nil
"* Date:          " (format-time-string "%e %b %Y") \n
"* Time:          " (format-time-string "%R") \n
(attended) \n
(apologies) \n
"* Location:      " (skeleton-read "Where? " )\| (insert default-location) \n ;variable
"* Chair:         " (skeleton-read "Chair? ")\| (insert default-chair) \n
)

(define-skeleton minutes-topic
"Insert a topic for discussion."
nil
"* Topic          " (skeleton-read "Topic? ") \n
_
)

(define-skeleton minutes-motion
""
nil
(skeleton-read "Who? ")" moved that "_
)

(define-skeleton minutes-vote
"Insert a voting table."
nil
>"|----------------+----------+--------------|" \n
>"|Vote:           |For: " (setq v1 (skeleton-read "How many for? "))"    |Against: " (setq v2 (skeleton-read "How many against? "))"    |" \n
>"|----------------+----------+--------------|" \n
"|" (if (< (string-to-number v1)
	   (string-to-number v2))
	(insert "Not Carried                               |")
      (insert "Carried                                   |")) \n 
>"|------------------------------------------|" \n
)


(defvar minutes-mode-abbrev-table nil
  "Abbrev table used in minutes-mode.")
(if minutes-mode-abbrev-table ()
(let ((ac abbrevs-changed))
  (define-abbrev-table 'minutes-mode-abbrev-table ())
  (define-abbrev minutes-mode-abbrev-table "ma" "" 'minutes-apologies)
  (define-abbrev minutes-mode-abbrev-table "mbc" "" 'minutes-block-comment)
  (define-abbrev minutes-mode-abbrev-table "mbs" "" 'minutes-book-skeleton)
  (define-abbrev minutes-mode-abbrev-table "mmo" "" 'minutes-motion)
  (define-abbrev minutes-mode-abbrev-table "msh" "" 'minutes-sub-header)
  (define-abbrev minutes-mode-abbrev-table "msn" "" 'minutes-snippet)
  (define-abbrev minutes-mode-abbrev-table "mt" "" 'minutes-attended)
  (define-abbrev minutes-mode-abbrev-table "mt" "" 'minutes-topic)
  (define-abbrev minutes-mode-abbrev-table "mv" "" 'minutes-votes)
  (setq abbrevs-changed ac)))

;;; defcustoms
(defgroup minutes nil
  "Customisation group for minutes-mode."
  :group 'convenience
  :prefix 'minutes-)

(defcustom minutes-dir nil
  "Directory where the set of minutes can be found."
  :type 'string
  :group 'minutes);this might be too inflexible. Maybe someone would want the committee
		  ;files to be attached to the other company docs not just the minutes.

;;; TODOS
;; need a function to create a minutes file that puts it in the right directory, inserts
;; the correct header for the committee, and sets up the proper abbrevs-table which will
;; inherit from the main minutes abbrev table.  look at dir-locals and lgrep to set
;; project-level variables and to find relevant text in certain projects. 

;;;###autoload
(define-derived-mode minutes-mode text-mode "mins"
  "Major mode for quickly entering notes for minutes.

It sets up a \"committee\" structure with members and company
headers to easily add this information to minutes.
\\{minutes-mode-map}."
  :abbrevs-table minutes-mode-abbrevs-table
;  (set (make-local-variable 'font-lock-defaults) '(minutes-mode-font-lock-keywords))
  (set (make-local-variable 'skeleton-end-hook) 'nil)
  (set (make-local-variable 'abbrev-mode) 't)
  (use-local-map minutes-mode-map))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.minutes\\'" . minutes-mode))

(provide 'minutes-mode)
;;; minutes-mode.el ends here

;;; Commentary:
;;
;;   I think you'll agree this macro is handy. 
;;
;;   Call it with arguments of your choosing to define interactive 
;;  commands that either provide instant access to existing files in 
;;   a specified directory, or return named buffers which will be 
;;   written to files (with the same name) within the same specified 
;;   directory.
;;
;;   The first argument to the macro call is the name of interactive 
;;  command you want to able to issue. 
;;
;;   The second argument is the string to be displayed when prompted 
;;  for a file name.
;;
;;   The third argument is the name of a default file which will be 
;;  visited if you don't provide a file name when prompted, .i.e., 
;;   you just hit RET.
;;
;;   Unless the fourth argument to the macro call is 'stack (a symbol) 
;;  point will move to the end of the buffer immediately after the 
;;   file is visited.
;;
;;   If the fifth argument (the last) to the macro call is non-nil, 
;;   a pretty date-time stamp is inserted at point's final position. 
;;
;;   Finally, if a file named '.template' is found in the specified 
;;  directory, its contents are automatically inserted when a new 
;;   file is created and any file local variables it may conatin are 
;;   evaluated.

;;; Installation:
;;
;;   Put this file somewhere in your load-path and add the line: ;;
;;    (require 'commmandir)
;;
;;   to your ~/.emacs.   Then add calls to commandir, e.g., ;;
;;    (commandir memo "Memo subject: " "~/memo" "misc" 'stack) ;;
;;  When Emacs has loaded you can then type 'M-x memo RET' to compose a 
;; new memo or edit an old one.


;;; commandir.el begins here
;; I would like to base my opening command on this macro
;; but with less options as I don't think I need it.
;; think about this, carefully. circular logic is nearby!

;; (defvar commandir-date-format "%a %d %b %Y %l:%M %p%n%n") 
;; (defvar commandir-date-prefix "* ") ; useful for outline-mode e.t.c.

;; (defmacro commandir (call pmt dir def &optional type date)
;;   `(defun ,call ()
;;      (interactive)
;;      (let ((dir/ (file-name-as-directory ,dir)) ; ensure final '/'
;; 	   filename insert-default-directory) ; nil
;;        (setq filename (read-file-name ,pmt dir/))
;;        (when (equal filename "") (setq filename ,def))
;;        ;; find-file automatically selects buffer for editing 
;;        (find-file (concat dir/ filename) nil)
;;        (when (eq (buffer-size) 0) ; buffer empty?
;;          ;; insert template file and evaluate local-variables 
;; 	 ;; (if (file-readable-p (concat dir/ ".template"))
;;          ;;     (progn (insert-file (concat dir/ ".template"))
;; 	 ;; insert the main skeleton header and local variables.
;; 	 (insert header)
;; 	 (minutes-sub-header)
;; 	 (hack-local-variables))
;;            (minutes-mode)))
;;        ;; position point
;;        (unless (equal ,type 'stack)
;;          (goto-char (point-max)))
;;        ;; display
;;        (switch-to-buffer (buffer-name)))

;; (provide 'commandir)

;;; commandir.el ends here

;;; TODO
;; so I can use define-abbrev and use sys to put abbrevs from a list of strings
;; - take first letter of each name and make the abbrev name
;; (define-abbrev table name expansion &optional hook &rest props)


;; So we have dir-local-variables in emacs 23
;; we can set variables for a set of files in a folder. this accords well with the minutes mode model.
;; one set of files for a committee in one folder
;; So we can set up functions to create .dir-locals.el for the folder that we are aiming at.
;; we can name new variables based on name of directory, which presumably reflects the committee name.
;; variable such as cgf-minutes-abbrev-mode. can we add-to-list with abbrevs?
;; 1. make directory, make .dir-local.el with defstruct or class with variables.
;; 2. a function to take the names in the list of committee members and add to abbrev list.
;; 3. and make the files in that list be in minutes mode.


;;; minutes-mode.el ends here.
