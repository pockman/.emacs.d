;;; freepascal.el --- major mode for editing freepascal source in Emacs
;; Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.

;; Author: Espen Skoglund (espensk@stud.cs.uit.no)
;; Keywords: languages

;; Contributors:  Christian Sperr
;; 			Phil Gold


;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; USAGE
;;; =====

;;; Emacs should enter Free Pascal mode when you find a Free Pascal source file.
;;; When you have entered Free Pascal mode, you may get more info by pressing
;;; C-h m. You may also get online help describing various functions by:
;;; C-h f <Name of function you want described>

;;; If you want to customize Free Pascal mode to fit you better, you may add
;;; these lines (the values of the variables presented here are the defaults):
;;;
;;; ;; User customization for Free Pascal mode
;;; (setq freepascal-indent-level       3
;;;       freepascal-case-indent        2
;;;       freepascal-auto-newline       nil
;;;       freepascal-tab-always-indent  t
;;;       freepascal-auto-endcomments   t
;;;       freepascal-auto-lineup        '(all)
;;;       freepascal-toggle-completions nil
;;;       freepascal-type-keywords      '("array" "file" "packed" "char" 
;;; 				      "integer" "real" "string" "record")
;;;       freepascal-start-keywords     '("begin" "end" "function" "procedure"
;;; 				      "repeat" "until" "while" "read" "readln"
;;; 				      "reset" "rewrite" "write" "writeln")
;;;       freepascal-separator-keywords '("downto" "else" "mod" "div" "then"))

;;; KNOWN BUGS / BUGREPORTS
;;; =======================
;;; As far as I know, there are no bugs in the current version of this
;;; package.  This may not be true however, since I never use this mode
;;; myself and therefore would never notice them anyway.   If you do
;;; find any bugs, you may submit them to: espensk@stud.cs.uit.no
;;; as well as to bug-gnu-emacs@prep.ai.mit.edu.

;; So that dynamic Abbrev ignores qualification prefixes 
(setq dabbrev-abbrev-skip-leading-regexp "\\(\\sw+\\.\\)+" )


(defvar freepascal-mode-abbrev-table nil
"Abbrev table in use in Free Pascal-mode buffers.")

(define-abbrev-table 'freepascal-mode-abbrev-table ())

(defgroup pascal nil
  "Major mode for editing Free Pascal source in Emacs"
  :group 'language)


(defvar freepascal-mode-map ()
  "Keymap used in Free Pascal mode.")
(if freepascal-mode-map
    ()
  (setq freepascal-mode-map (make-sparse-keymap))
  (define-key freepascal-mode-map ";"        'electric-freepascal-semi-or-dot)
  (define-key freepascal-mode-map "."        'electric-freepascal-semi-or-dot)
  (define-key freepascal-mode-map ":"        'electric-freepascal-colon)
  (define-key freepascal-mode-map "="        'electric-freepascal-equal)
					;  (define-key freepascal-mode-map "#"        'electric-freepascal-hash)
  (define-key freepascal-mode-map "\r"       'electric-freepascal-terminate-line)
  (define-key freepascal-mode-map "\t"       'electric-freepascal-tab)
  (define-key freepascal-mode-map "\M-\t"    'freepascal-complete-word)
  (define-key freepascal-mode-map "\M-?"     'freepascal-show-completions)
					;  (define-key freepascal-mode-map "\177"     'backward-delete-char-untabify)
  (define-key freepascal-mode-map "\M-\C-h"  'freepascal-mark-defun)
  (define-key freepascal-mode-map "\C-c\C-b" 'freepascal-insert-block)
  (define-key freepascal-mode-map "\C-c\C-e" 'freepascal-insert-try-except)
  (define-key freepascal-mode-map "\C-c\C-f" 'freepascal-insert-try-finally)
  (define-key freepascal-mode-map "\M-*"     'freepascal-star-comment)
  (define-key freepascal-mode-map "\C-c\C-c" 'freepascal-comment-area)
  (define-key freepascal-mode-map "\C-c\C-u" 'freepascal-uncomment-area)
  (define-key freepascal-mode-map "\M-\C-a"  'freepascal-beg-of-defun)
  (define-key freepascal-mode-map "\M-\C-e"  'freepascal-end-of-defun)
  (define-key freepascal-mode-map "\C-c\C-d" 'freepascal-goto-defun)
  (define-key freepascal-mode-map "\C-c\C-o" 'freepascal-outline)
;;; A command to change the whole buffer won't be used terribly
;;; often, so no need for a key binding.
					;  (define-key freepascal-mode-map "\C-cd"    'freepascal-downcase-keywords)
					;  (define-key freepascal-mode-map "\C-cu"    'freepascal-upcase-keywords)
					;  (define-key freepascal-mode-map "\C-cc"    'freepascal-capitalize-keywords)
  )

(defvar freepascal-imenu-generic-expression
  '((nil "^\\s-*\\(constructor\\|destructor\\|function\\|procedure\\)\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\(\\.[a-zA-Z_][a-zA-Z0-9_]*\\)*\\)" 2))
  "Imenu expression for Free Pascal-mode.  See `imenu-generic-expression'.")

(defvar freepascal-keywords
  '("and" "array" "as" "asm" "begin" "case" "class" "const" "constructor" 
    "destructor" "div" "do" "downto" "else" "end" "except" "exports" 
    "file" "finalization" "finally" "for" "function" "goto" "if" 
    "implementation" "in" "inherited" "initialization" "inline" "interface"
    "is" "label" "library" "mod" "nil" "not" "object" "of" "or" "packed" 
    "procedure" "program" "property" "raise" "record" "repeat" "set" 
    "shl" "shr" "string" "then" "threadvar" "to" "try" "type" "unit" "until"
    "uses" "var" "while" "with" "xor" 
    ;; standard directives
    "absolute" "abstract" "assembler" "automated" "cdecl" "default" "dispid"
    "dynamic" "export" "external" "far" "forward" "index" "message" "name"
    "near" "nodefault" "override" "freepascal" "private" "protected" "public"
    "published" "read" "register" "resident" "stdcall" "stored" "strict"
    "virtual" "write" ))

;;;
;;; Regular expressions used to calculate indent, etc.
;;;
(defconst freepascal-symbol-re      "\\<[a-zA-Z_][a-zA-Z_0-9.]*\\>")
(defconst freepascal-beg-block-re   "\\<\\(begin\\|case\\|class\\|interface\\|object\\|record\\|repeat\\|try\\)\\>")
(defconst freepascal-no-block-re    "\\(\\<class *\\(function\\|procedure\\|(.*)\\)?\\|\\(\\<procedure\\|\\<function\\) +of object\\);")
(defconst freepascal-end-block-re   "\\<\\(end\\.?\\|until\\)\\>")
(defconst freepascal-declaration-re "\\<\\(const\\|label\\|type\\|uses\\|var\\|out\\)\\>")
(defconst freepascal-defun-re       "\\<\\(function\\|procedure\\|program\\|unit\\|library\\|constructor\\|destructor\\|property\\)\\>")
(defconst freepascal-sub-block-re   "\\<\\(if\\|else\\|except\\|for\\|on\\|while\\|with\\)\\>")
(defconst freepascal-noindent-re    "\\<\\(begin\\|end\\|until\\|else\\|except\\|finally\\|initialization\\|finalization\\|interface\\|implementation\\|program\\|library\\)\\>")
(defconst freepascal-class-prot-re  "\\<\\(strict \\)?\\(private\\|protected\\|public\\|published\\)\\>")
(defconst freepascal-nosemi-re      "\\<\\(begin\\|repeat\\|then\\|do\\|else\\)\\>")
(defconst freepascal-autoindent-lines-re
  "\\<\\(label\\|var\\|type\\|const\\|until\\|end\\|except\\|finally\\|begin\\|repeat\\|else\\|interface\\|implementation\\|initialization\\|finalization\\|uses\\private\\|public\\|published\\|protected\\|strict private\\| strict protected\\)\\>")
(defconst freepascal-sections-re
  (concat "\\<\\(interface\\|implementation\\|initialization\\|finalization\\|"
	  "end\\.\\)\\>"))

(defconst freepascal-directives-re
  (concat "\\<\\(virtual\\|dynamic\\|message\\|register\\|freepascal\\|cdecl\\|"
	  "stdcall\\|abstract\\|override\\)\\>"))

(defconst freepascal-caselabel-re
  "^[ \t]*[^ \t,\\.:]+[ \t]*\\(,[ \t]*[^ \t,:]+[ \t]*\\)*:")

;;; Noodling, mostly.
(defconst freepascal-identifier-re "[a-z_][a-z0-9_]+")
(defconst freepascal-qualified-identifier-re
  (concat freepascal-identifier-re "\\(?:\\." freepascal-identifier-re "\\)*"))
(defconst freepascal-numeral-re
  "\\(?:[+-]?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:e[+-]?[0-9]+\\)?\\|\\$[0-9a-f]+\\)")
(defconst freepascal-label-re
  (concat "\\(?:" freepascal-identifier-re
	  "\\|[0-9]\\(?:[0-9]\\(?:[0-9]\\(?:[0-9]\\)?\\)?\\)?\\)"))
(defconst freepascal-control-string-re "#\\(?:[0-9]+\\|\\$[0-9a-f]+\\)")
(defconst freepascal-character-string-re
  (concat "'\\(?:[^']\\|''\\|'\\(?:" freepascal-control-string-re
	  "\\)+'\\)*'"))

(defconst freepascal-directives
  '("absolute" "abstract" "assembler" "automated"
    "cdecl" "contains"
    "default" "dispid" "dynamic"
    "export" "external"
    "far" "forward"
    "implements" "index"
    "message"
    "name" "near" "nodefault"
    "overload" "override"
    "package" "freepascal" "private" "protected" "public" "published"
    "read" "readonly" "register" "reintroduce" "requires" "resident"
    "safecall" "stdcall" "stored" "strict"
    "virtual"
    "write" "writeonly")
  "Object Free Pascal directives")

(defconst freepascal-reserved-words
  '("and" "array" "as" "asm"
    "begin"
    "case" "class" "const" "constructor"
    "destructor" "dispinterface" "div" "do" "downto"
    "else" "end" "end." "except" "exports"
    "file" "finalization" "finally" "for" "function"
    "goto"
    "if" "implementation" "in" "inherited" "initialization" "inline"
    "interface" "is"
    "label" "library"
    "mod"
    "nil" "not"
    "object" "of" "or" "out"
    "packed" "procedure" "program" "property"
    "raise" "record" "repeat" "resourcestring"
    "set" "shl" "shr" "string"
    "then" "threadvar" "to" "try" "type"
    "unit" "until" "uses"
    "var"
    "while" "with"
    "xor")
  "Object Free Pascal reserved words")

(defconst freepascal-types
  '(;; Integer types
    "integer" "cardinal"
    "shortint" "smallint" "longint" "int64" "byte" "word" "longword"
    ;; Character types
    "char"
    "ansichar" "widechar"
    ;; Boolean types
    "boolean"
    "bytebool" "wordbool" "longbool"
    ;; Real types
    "real"
    "real48" "single" "double" "extended" "comp" "currency"
    ;; String types
    "ansistring" "shortstring" "widestring"
    ;; Pointer types
    "pointer"
    "pchar" "pansichar" "pwidechar"
    "pansistring" "pstring" "pbytearray" "pcurrency" "pextended"
    "polevariant" "pshortstring" "ptextbuf" "pvarrec" "pvariant"
    "pwidestring" "pwordarray"
    ;; Variant types
    "variant" "olevariant")
  "Object Free Pascal built-in types")


;;; Strings used to mark beginning and end of excluded text
(defconst freepascal-exclude-str-start "{-----\\/----- EXCLUDED -----\\/-----")
(defconst freepascal-exclude-str-end " -----/\\----- EXCLUDED -----/\\-----}")

(defvar freepascal-mode-syntax-table nil
  "Syntax table in use in Free Pascal-mode buffers.")

(if freepascal-mode-syntax-table
    ()
  (setq freepascal-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "."   freepascal-mode-syntax-table)
  (modify-syntax-entry ?$ "."    freepascal-mode-syntax-table)
  (modify-syntax-entry ?( "()1"  freepascal-mode-syntax-table)  
		       (modify-syntax-entry ?) ")(4"  freepascal-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" freepascal-mode-syntax-table)
  (modify-syntax-entry ?{ "<"    freepascal-mode-syntax-table)
  (modify-syntax-entry ?} ">"    freepascal-mode-syntax-table)
  (modify-syntax-entry ?+ "."    freepascal-mode-syntax-table)
  (modify-syntax-entry ?- "."    freepascal-mode-syntax-table)
  (modify-syntax-entry ?= "."    freepascal-mode-syntax-table)
  (modify-syntax-entry ?% "."    freepascal-mode-syntax-table)
  (modify-syntax-entry ?< "."    freepascal-mode-syntax-table)
  (modify-syntax-entry ?> "."    freepascal-mode-syntax-table)
  (modify-syntax-entry ?& "."    freepascal-mode-syntax-table)
  (modify-syntax-entry ?| "."    freepascal-mode-syntax-table)
  (modify-syntax-entry ?_ "w"    freepascal-mode-syntax-table)
  (modify-syntax-entry ?. "_"    freepascal-mode-syntax-table)
  (modify-syntax-entry ?\' "\""  freepascal-mode-syntax-table)
  (modify-syntax-entry ?/' ". 12b" freepascal-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" freepascal-mode-syntax-table)
  (modify-syntax-entry ?\" "."  freepascal-mode-syntax-table)
  )

(defconst freepascal-font-lock-keywords
  (list
   '("^\\s-*\\(\\(class\\)?\\s-*\\(\\(con\\|de\\)structor\\|function\\|pro\\(cedure\\|gram\\)\\|unit\\)\\>\\)[ \t]*\\([a-z_][0-9a-z_]*\\(\\.[a-z_][0-9a-z_]*\\)*\\)?"
     (1 font-lock-keyword-face) (6 font-lock-function-name-face nil t))
   (cons (concat "\\<"
		 (regexp-opt (union freepascal-types
				    '("type" "var" "const" "string" "packed"
				      "array" "set" "record" "file" "class"
				      "out")
				    :test 'string=)
			     t)
		 "\\>")
	 'font-lock-type-face)
   (concat "\\<"
	   (regexp-opt (set-difference freepascal-reserved-words
				       '("array" "class" "const" "file" "goto"
					 "nil" "packed" "record" "set" "string"
					 "type" "var" "out")
				       :test 'string=)
		       t)
	   "\\>")
   (concat "\\<" (regexp-opt freepascal-directives t) "\\>")
   ;; special
   "\\<\\(at\\|on\\)\\>"
   ;; labels
					;'("^\\s-*\\([0-9a-z_]+\\):" (1 font-lock-reference-face))
   '("\\<\\(goto\\)\\(\\s-+\\([0-9a-z_]+\\)\\)?\\>"
     (1 font-lock-keyword-face) (3 font-lock-reference-face nil t))
   ;; builtins
   (cons (concat "\\<"
		 (regexp-opt '("nil" "true" "false" "unassigned") t)
		 "\\>")
	 'font-lock-builtin-face)
   ;; control strings
   (cons freepascal-control-string-re 'font-lock-string-face)
   (cons (concat "\\<" freepascal-numeral-re "\\>") 'font-lock-constant-face)
   )
  "Additional expressions to highlight in Free Pascal mode.")

(defcustom freepascal-indent-level 4
  "*Indentation of Free Pascal statements with respect to containing block."
  :type 'integer
  :group 'pascal
  )

(defcustom freepascal-case-indent 4
  "*Indentation for case statements."
  :type 'integer
  :group 'pascal
  )

(defcustom freepascal-auto-newline nil
  "*Non-nil means automatically newline after semicolons and the punctation mark
after an end."
  :type 'boolean
  :group 'pascal
  )

(defcustom freepascal-tab-always-indent t
  "*Non-nil means TAB in Free Pascal mode should always reindent the current line, regardless of where in the line point is when the TAB command is used."
  :type 'boolean
  :group 'pascal
  )

(defcustom freepascal-auto-endcomments t
  "*Non-nil means a comment { ... } is set after the ends which ends cases and
functions. The name of the function or case will be set between the braces."
  :type 'boolean
  :group 'pascal
  )

(defcustom freepascal-indent-nested-functions t
  "Non-nil means nested functions are indented."
  :type 'boolean
  :group 'pascal)

(defcustom freepascal-auto-lineup '(all)
  "*List of contexts where auto lineup of :'s or ='s should be done.
Elements can be of type: 'paramlist', 'declaration' or 'case', which will
do auto lineup in parameterlist, declarations or case-statements
respectively. The word 'all' will do all lineups. '(case paramlist) for
instance will do lineup in case-statements and parameterlist, while '(all)
will do all lineups."
  :type '(set :extra-offset 8
	      (const :tag "Everything" all)
	      (const :tag "Parameter lists" paramlist)
	      (const :tag "Decalrations" declaration)
	      (const :tag "Case statements" case))
  :group 'pascal)




(defcustom freepascal-toggle-completions nil
  "*Non-nil means \\<freepascal-mode-map>\\[freepascal-complete-word] should try all possible completions one by one.
Repeated use of \\[freepascal-complete-word] will show you all of them.
Normally, when there is more than one possible completion,
it displays a list of all possible completions."
  :type 'boolean
  :group 'pascal
  )

(defcustom freepascal-type-keywords
  '("array" "file" "packed" "char" "integer" "real" "string" "record")
  "*Keywords for types used when completing a word in a declaration or parmlist.
\(eg. integer, real, char.)  The types defined within the Free Pascal program
will be completed runtime, and should not be added to this list."
  :type '(repeat (string :tag "Keyword"))
  :group 'pascal)

(defcustom freepascal-start-keywords
  '("begin" "end" "function" "procedure" "repeat" "until" "while"
    "read" "readln" "reset" "rewrite" "write" "writeln")
  "*Keywords to complete when standing at the first word of a statement.
\(eg. begin, repeat, until, readln.)
The procedures and variables defined within the Free Pascal program
will be completed runtime and should not be added to this list."
  :type '(repeat (string :tag "Keyword"))
  :group 'pascal)

(defcustom freepascal-separator-keywords
  '("downto" "else" "mod" "div" "then")
  "*Keywords to complete when NOT standing at the first word of a statement.
\(eg. downto, else, mod, then.) 
Variables and function names defined within the
Free Pascal program are completed runtime and should not be added to this list."
  :type '(repeat (string :tag "Keyword"))
  :group 'pascal)

;;;
;;;  Macros
;;;

(defsubst freepascal-get-beg-of-line (&optional arg)
  (save-excursion
    (beginning-of-line arg)
    (point)))

(defsubst freepascal-get-end-of-line (&optional arg)
  (save-excursion
    (end-of-line arg)
    (point)))

(defsubst freepascal-within-string ()
  (save-excursion
    (nth 3 (parse-partial-sexp (freepascal-get-beg-of-line) (point)))))

(defsubst freepascal-within-comment (startpos)
  (save-excursion
    (nth 4 (parse-partial-sexp startpos (point)))))

					; sets the point to the end of declaration -- not quite true for delphi
(defun freepascal-declaration-end (&optional arg)
  (let* ((inclass (if arg arg 0))
	 (nest inclass)
	 (startpoint (point))
	 (reg (concat 
	       "\\<\\(procedure\\|function\\|constructor\\|destructor\\)\\>\\|"
	       "\\<\\(begin\\|implementation\\)\\>\\|"
	       "\\<\\(property\\|published\\|public\\|protected\\|private\\|automated\\|strict private\\|strict protected\\)\\>\\|"
	       "[:=]\\|"
	       "\\<\\(record\\|class\\|interface\\|object\\)\\>\\|"
	       "\\<\\(end\\)\\>\\|"
	       "\\<\\(var\\|const\\|out\\)\\>\\|"
	       "\\<\\(type\\)\\>\\|"
	       "\\()\\)\\|"
	       "\\((\\)" ))
	 (found nil)
	 (paren (save-excursion
		  (nth 0 (parse-partial-sexp (point-min) (point)))))
	 (parenstart (> paren 0)))
    (while (and 
	    (not found)
	    (re-search-forward 
	     reg
					;(save-excursion (end-of-line 5) (point))
	     (point-max) t))
      (if (not (freepascal-within-comment startpoint))
	  (cond ((match-beginning 1) 
					;procedure, function,constructor or 
					;destructor
		 (if parenstart (setq found t)
		   (setq found 
			 (and (= nest 0)
			      (save-excursion
				(forward-sexp -2)
				(forward-sexp 1)
				(not (looking-at "[ \t]*="))))
			 )))
		((match-beginning 2) 
					;begin, implementation
		 (setq found t))
		((match-beginning 3) 
					;property, published, public, 
					;protected, private, automated
		 (if parenstart (setq found t))
		 (if (= 0 inclass) (setq inclass (setq nest (1+ nest)))))
		((match-beginning 4) 
					;record, class, object, interface
		 (save-excursion
		   (forward-word -1)
		   (if (looking-at "\\(class\\|interface\\)\\((.*)\\)?[^;]")
		       (setq inclass 1))
		   (if (looking-at "class;")
		       (setq nest (1- nest)))
		   (setq nest (1+ nest))))
		((match-beginning 5) 
					;end
		 (setq nest (1- nest))
		 (if (< nest inclass) (setq inclass nest)))
		((match-beginning 6) 
					;var, const, out
		 (if (= 0 paren) (setq found t)))
		((match-beginning 7)
					;type
		 (if (<= 0 paren) (setq found t)))
		((match-beginning 8) 
					; )
		 (setq paren (1- paren)) 
		 (if (and parenstart (= 0 paren)) (setq found t)))
		((match-beginning 9)
					; (
		 (setq paren (1+ paren))))))
    (if (or (match-beginning 1) (match-beginning 2) (match-beginning 3)
	    (and (not parenstart) found)) (forward-word -1))
    (skip-chars-backward " \t\n")
    ;;    (if (looking-at "\\<function\\|procedure\\|constructor\\|destructor\\>")
    ;;	(forward-line -1))
    ))


					;should set point to the beginning of a declaration
(defun freepascal-declaration-beg ()
  (let ((nest 0)
	(found nil)
	(paren (save-excursion
		 (nth 0 (parse-partial-sexp (point-min) (point))))))
    (while (and (not found)
		(re-search-backward 
		 (concat "[:=]\\|"
			 "\\<\\(type\\|label\\)\\>\\|"
			 "\\(\\<record\\>\\|\\<class\\>\\|\\<interface\\>"
			 "\\|\\<object\\>\\)\\|"
			 "\\(\\<end\\>\\)\\|"
			 "\\<\\(var\\|const\\|out\\)\\>\\|\\()\\)\\|\\((\\)" )
		 (freepascal-get-beg-of-line -5) t))
      (cond ((match-beginning 1) (setq found t))
	    ((match-beginning 2) (setq nest (1- nest)))
	    ((match-beginning 3) (setq nest (1+ nest)))
	    ((match-beginning 4) (if (= 0 paren) (setq found t)) )
	    ((match-beginning 5) (setq paren (1+ paren)))
	    ((match-beginning 6) (setq paren (1- paren)))))

    (= 0 paren)))


;; returns true if the point is in a section that can contain implementations
;; of functions and procedures

(defun freepascal-in-implementation ()

  (save-excursion
    (let ((found nil)) 
      (while (and (not (bobp)) (not found)) 
	(if (re-search-backward "\\<\\(implementation\\|program\\|library\\)\\>" 
				(point-min) 'move )
	    (setq found 
		  (not (let ((state (save-excursion (parse-partial-sexp 
						     (point-min) (point)))))
			 (or (nth 3 state) (nth 4 state)))))))
      found)))


(defun freepascal-defun-valid ()
;;; returns t if the subroutine at point really starts the definition of
;;; a subroutine or a program or library
  (if (looking-at "program\\|library") t
    (if (freepascal-in-implementation)
	(save-excursion
	  (skip-chars-backward " \t\n")
	  (if (looking-at "=") nil
	    (let ((nest 1) (found nil))
	      (while (and (> nest 0) (not (bobp)))
		(re-search-backward "\\(=[ \t\n]*\\(class\\|object\\interface\\)\\)\\|\\(\\<record\\>\\)\\|\\(\\<end\\>\\)" (point-min) 'move)
		(cond
		 ((bobp))
		 ((match-beginning 4) (setq nest (1+ nest)))
		 ((match-beginning 3) (setq nest (1- nest)))
		 ((and (match-beginning 1) (>= 1 nest)) 
		  (setq nest 0) (setq found t))))
	      (not found))))
      nil)))


(defun freepascal-hs-forward-sexp (&optional arg)
  "Function for use with hide-show-minor-mode for skipping blocks"
  (let ((nest 0) (spos (point)))
    (if (and arg (< arg 0))
	(progn
	  (save-excursion 
	    (forward-word -1)
	    (if (looking-at freepascal-end-block-re)
		(setq nest -1)))
	  (if (= nest 0)
	      (forward-sexp -1)
	    (while (and (< nest 0) (not (bobp)))
	      (re-search-backward (concat freepascal-beg-block-re "\\|"
					  freepascal-end-block-re) 
				  (point-min) 'move)
	      (cond
	       ((bobp))
	       ((and (match-beginning 1) 
		     (not (freepascal-within-comment spos))
		     (not 
		      (looking-at freepascal-no-block-re)
		      )) (setq nest (1+ nest)))
	       ((and (match-beginning 2) (not (freepascal-within-comment spos)))
		(setq nest (1- nest)))))
	    (if (= nest 0)
		(goto-char (match-beginning 1)))))
      ;; Positives Argument
      (if (not (looking-at freepascal-beg-block-re))
	  (forward-sexp 1)
	(forward-word 1)
	(setq nest 1)
	(while (and (> nest 0) (not (eobp)))
	  (re-search-forward (concat freepascal-beg-block-re "\\|"
				     freepascal-end-block-re) 
			     (point-max) 'move)
	  (cond
	   ((eobp))
	   ((and (match-beginning 1) 
		 (not (freepascal-within-comment spos))
		 (save-excursion (goto-char (match-beginning 1))
				 (not (looking-at freepascal-no-block-re)))
		 )
	    (setq nest (1+ nest)))
	   ((and (match-beginning 2) (not (freepascal-within-comment spos)))
	    (setq nest (1- nest)))))
	(if (= nest 0)
	    (goto-char (match-end 2)))))))


;;;###autoload
(defun freepascal-mode ()
  "Major mode for editing Free Pascal code. \\<freepascal-mode-map>
TAB indents for Free Pascal code.  Delete converts tabs to spaces as it moves back.

\\[freepascal-complete-word] completes the word around current point with respect \
to position in code
\\[freepascal-show-completions] shows all possible completions at this point.

Other useful functions are:

\\[freepascal-mark-defun]\t- Mark function.
\\[freepascal-insert-block]\t- insert begin ... end;
\\[freepascal-star-comment]\t- insert (* ... *)
\\[freepascal-comment-area]\t- Put marked area in a comment, fixing nested comments.
\\[freepascal-uncomment-area]\t- Uncomment an area commented with \
\\[freepascal-comment-area].
\\[freepascal-beg-of-defun]\t- Move to beginning of current function.
\\[freepascal-end-of-defun]\t- Move to end of current function.
\\[freepascal-goto-defun]\t- Goto function prompted for in the minibuffer.
\\[freepascal-outline]\t- Enter freepascal-outline-mode (see also freepascal-outline).

Variables controlling indentation/edit style:

freepascal-indent-level      (default 4)
  Indentation of Free Pascal statements with respect to containing block.
freepascal-case-indent       (default 4)
  Indentation for case statements.
freepascal-auto-newline      (default nil)
  Non-nil means automatically newline after simcolons and the punctation mark
  after an end.
freepascal-tab-always-indent (default t)
  Non-nil means TAB in Free Pascal mode should always reindent the current line,
  regardless of where in the line point is when the TAB command is used.
freepascal-auto-endcomments  (default t)
  Non-nil means a comment { ... } is set after the ends which ends cases and
  functions. The name of the function or case will be set between the braces.
freepascal-auto-lineup       (default t)
  List of contexts where auto lineup of :'s or ='s hould be done.

See also the user variables freepascal-type-keywords, freepascal-start-keywords and
freepascal-separator-keywords.

Turning on Free Pascal mode calls the value of the variable freepascal-mode-hook with
no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map freepascal-mode-map)
  (setq major-mode 'freepascal-mode)
  (setq mode-name "Free Pascal")
  (setq local-abbrev-table freepascal-mode-abbrev-table)
  (set-syntax-table freepascal-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'freepascal-indent-line)
  (setq comment-indent-function 'freepascal-indent-comment)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'case-fold-search)
  (setq case-fold-search t)
  (make-local-variable 'comment-start)
  (setq comment-start "{")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+ *\\|{+ *\\|// *")
  (make-local-variable 'comment-end)
  (setq comment-end "}")
  ;; Font lock support
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(freepascal-font-lock-keywords nil t))
  ;; Imenu support
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression freepascal-imenu-generic-expression)
  (tempo-use-tag-list 'freepascal-tempo-tags)
  (run-hooks 'freepascal-mode-hook))



;;;
;;;  Electric functions
;;;
(defun electric-freepascal-terminate-line ()
  "Terminate line and indent next line."
  (interactive)
  ;; First, check if current line should be indented
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (looking-at freepascal-autoindent-lines-re)
	(freepascal-indent-line)))
  (delete-horizontal-space) ; Removes trailing whitespaces
  (newline)
  ;; Indent next line
  (freepascal-indent-line)
  ;; Maybe we should set some endcomments
  (if freepascal-auto-endcomments
      (freepascal-set-auto-comments))
  ;; Check if we shall indent inside comment
  (if (save-excursion
	(not (nth 4 (parse-partial-sexp (point-min) (point)))))
      ()
    (let ((setstar nil))
      (save-excursion
	(forward-line -1)
	(skip-chars-forward " \t")
	(cond ((looking-at "\\*[ \t]+)")
	       ;; Delete region between `*' and `)' if there is 
	       ;; only whitespaces.
	       (forward-char 1)
	       (delete-horizontal-space))
	      ((and (looking-at "(\\*\\|\\*[^)]")
		    (not (save-excursion
			   (search-forward "*)" 
					   (freepascal-get-end-of-line) t))))
	       (setq setstar t))))
      ;; If last line was a star comment line then this one shall be too.
      (if (null setstar)	
	  (freepascal-indent-line)
	(insert "*  ")))))

(defun freepascal-reindent-line ()
  "Indent line even if point is not at the beginning"
  (save-excursion
    (beginning-of-line)
    (freepascal-indent-line)
    nil))

(defun electric-freepascal-semi-or-dot ()
  "Insert `;' or `.' character and reindent the line."
  (interactive)
  (insert last-command-event)
  (save-excursion
    (beginning-of-line)
    (freepascal-indent-line))
  (if freepascal-auto-newline
      (electric-freepascal-terminate-line)))

(defun electric-freepascal-colon ()
  "Insert `:' and do all indentions except line indent on this line."
  (interactive)
  (insert last-command-event)
  ;; Do nothing if within string.
  (if (freepascal-within-string)
      ()
    (save-excursion
      (beginning-of-line)
      (freepascal-indent-line))
    (let ((freepascal-tab-always-indent nil))
      (freepascal-indent-command))))

(defun electric-freepascal-equal ()
  "Insert `=', and do indention if within type declaration."
  (interactive)
  (insert last-command-event)
  (if (eq (car (freepascal-calculate-indent)) 'declaration)
      (let ((freepascal-tab-always-indent nil))
	(freepascal-indent-command))))

(defun electric-freepascal-hash ()
  "Insert `#', and indent to coulmn 0 if this is a CPP directive."
  (interactive)
  (insert last-command-event)
					; in borland pascal # is used for character constants
					;  (if (save-excursion (beginning-of-line) (looking-at "^[ \t]*#"))
					;      (save-excursion (beginning-of-line)
					;		      (delete-horizontal-space)))
  )

(defun electric-freepascal-tab ()
  "Function called when TAB is pressed in Free Pascal mode."
  (interactive)
  ;; Do nothing if within a string or in a CPP directive.
  (if (or (freepascal-within-string)
	  ;;(and (not (bolp))
	  ;;     (save-excursion (beginning-of-line) (eq (following-char) ?#)))
	  )
      (insert "\t")
    ;; If freepascal-tab-always-indent, indent the beginning of the line.
    (if freepascal-tab-always-indent
	(save-excursion
	  (beginning-of-line)
	  (freepascal-indent-line))
      (insert "\t"))
    (freepascal-indent-command)))



;;;
;;; Interactive functions
;;;
(defun freepascal-insert-block ()
  "Insert Free Pascal begin ... end; block in the code with right indentation."
  (interactive)
  (freepascal-indent-line)
  (insert "begin")
  (electric-freepascal-terminate-line)
  (freepascal-indent-line)
  (save-excursion
    (electric-freepascal-terminate-line)
    (insert "end;")
    (beginning-of-line)
    (freepascal-indent-line)
    (end-of-line)))
;; Wieso steht man nicht auf der Leerzeile ?

(defun freepascal-insert-try (except-or-finally)
  (freepascal-indent-line)
  (insert "try")
  (electric-freepascal-terminate-line)
  (freepascal-indent-line)
  (save-excursion
    (electric-freepascal-terminate-line)
    (insert except-or-finally)
    (beginning-of-line)
    (freepascal-indent-line)
    (end-of-line)
    (electric-freepascal-terminate-line)
    (insert "end;")
    (beginning-of-line)
    (freepascal-indent-line)))

(defun freepascal-insert-try-except ()
  "Insert Free Pascal try ... except; block in the code with right indentation."
  (interactive)
  (freepascal-insert-try "except"))

(defun freepascal-insert-try-finally ()
  "Insert Free Pascal try ... finally; block in the code with right indentation."
  (interactive)
  (freepascal-insert-try "finally"))

(defun freepascal-star-comment ()
  "Insert Free Pascal star comment at point."
  (interactive)
  (freepascal-indent-line)
  (insert "(*")
  (electric-freepascal-terminate-line)
  (save-excursion
    (electric-freepascal-terminate-line)
    (delete-horizontal-space)
    (insert ")"))
  (insert "  "))

(defun freepascal-mark-defun ()
  "Mark the current freepascal function (or procedure).
This puts the mark at the end, and point at the beginning."
  (interactive)
  (push-mark (point))
  (freepascal-end-of-defun)
  (push-mark (point))
  (freepascal-beg-of-defun)
  (if (fboundp 'zmacs-activate-region)
      (zmacs-activate-region)))

(defun freepascal-comment-area (start end)
  "Put the region into a Free Pascal comment.
The comments that are in this area are \"deformed\":
`*)' becomes `!(*' and `}' becomes `!{'.
These deformed comments are returned to normal if you use
\\[freepascal-uncomment-area] to undo the commenting.

The commented area starts with `freepascal-exclude-str-start', and ends with
`freepascal-include-str-end'.  But if you change these variables,
\\[freepascal-uncomment-area] won't recognize the comments."
  (interactive "r")
  (save-excursion
    ;; Insert start and endcomments
    (goto-char end)
    (if (and (save-excursion (skip-chars-forward " \t") (eolp))
	     (not (save-excursion (skip-chars-backward " \t") (bolp))))
	(forward-line 1)
      (beginning-of-line))
    (insert freepascal-exclude-str-end)
    (setq end (point))
    (newline)
    (goto-char start)
    (beginning-of-line)
    (insert freepascal-exclude-str-start)
    (newline)
    ;; Replace end-comments within commented area
    (goto-char end)
    (save-excursion
      (while (re-search-backward "\\*)" start t)
	(replace-match "!(*" t t)))
    (save-excursion
      (while (re-search-backward "}" start t)
	(replace-match "!{" t t)))))

(defun freepascal-uncomment-area ()
  "Uncomment a commented area; change deformed comments back to normal.
This command does nothing if the pointer is not in a commented
area.  See also `freepascal-comment-area'."
  (interactive)
  (save-excursion
    (let ((start (point))
	  (end (point)))
      ;; Find the boundaries of the comment
      (save-excursion
	(setq start (progn (search-backward freepascal-exclude-str-start nil t)
			   (point)))
	(setq end (progn (search-forward freepascal-exclude-str-end nil t)
			 (point))))
      ;; Check if we're really inside a comment
      (if (or (equal start (point)) (<= end (point)))
	  (message "Not standing within commented area.")
	(progn
	  ;; Remove endcomment
	  (goto-char end)
	  (beginning-of-line)
	  (let ((pos (point)))
	    (end-of-line)
	    (delete-region pos (1+ (point))))
	  ;; Change comments back to normal
	  (save-excursion
	    (while (re-search-backward "!{" start t)
	      (replace-match "}" t t)))
	  (save-excursion
	    (while (re-search-backward "!(\\*" start t)
	      (replace-match "*)" t t)))
	  ;; Remove startcomment
	  (goto-char start)
	  (beginning-of-line)
	  (let ((pos (point)))
	    (end-of-line)
	    (delete-region pos (1+ (point)))))))))

(defun freepascal-beg-of-defun (&optional arg)
  "Move backward to the beginning of the current function or procedure.
if arg is set then stop when indentation of a function or procedure is
less or eqal to freepascal-indent-level."
  (interactive "P")
  (catch 'found
					;(if (not (looking-at (concat "\\s \\|\\s)\\|" freepascal-defun-re)))
					;	(forward-sexp 1))
    (let ((nest 0) (endlist ()) p
	  (reg (concat freepascal-beg-block-re "\\|" 
		       freepascal-end-block-re "\\|"
		       freepascal-defun-re "\\|" 
		       "\\(\\<external\\>\\)" )))
      (while (re-search-backward reg nil 'move)
	(cond ((let ((state (save-excursion
			      (parse-partial-sexp (point-min) (point)))))
		 (or (nth 3 state) (nth 4 state))) ; Inside string or comment
	       ())
	      ((match-end 1)                       ; begin|case|record|repeat
	       (if (and (looking-at "\\<case\\>") 
			(save-excursion
			  (forward-sexp 1)
			  (freepascal-check-case)))
		   ()
		 (setq p endlist)
		 (while p 
		   (setcar p (1+ (car p)))
		   (setq p (cdr p)))
		 (if (looking-at "\\<record\\>")
		     (setq endlist (cdr endlist))))
	       )
	      ((match-end 2)                       ; end|until
	       (setq p endlist)
	       (while p 
		 (setcar p (1- (car p)))
		 (setq p (cdr p))
		 )
	       (setq endlist (cons -1 endlist)))
	      ((and (match-end 3) (freepascal-defun-valid))   ; function|procedure
	       (if (or (not (member 0 endlist)) (null endlist)) 
		   (throw 'found t)
		 (setq p endlist)
		 (if (= (car p) 0)
		     (setq endlist (cdr endlist))
		   (while (not (= (car (cdr p)) 0)) (setq p (cdr p)))
		   (setcdr p (cdr (cdr p)))
		   ))
	       (if arg (if (> (current-column) freepascal-indent-level) ()
			 (goto-char (point-min))
			 (throw 'found t)))
	       )
	      ((match-end 4)     ; external
	       (setq endlist (cons 0 endlist))
	       ))))
    nil)
  ;; check for class modifier
  (let ((curpos (point)))
    (forward-sexp -1)
    (if (looking-at "class\\>")
	nil
      (if curpos
	  (goto-char curpos))))
  )

(defun freepascal-end-of-defun ()
  "Move forward to the end of the current function or procedure."
  (interactive)
  (if (looking-at "\\s ")
      (forward-sexp 1))
  (if (not (looking-at freepascal-defun-re))
      (freepascal-beg-of-defun))
  (forward-char 1)
  (let ((nest 0) (func 1)
	(reg (concat freepascal-beg-block-re "\\|" 
		     freepascal-end-block-re "\\|"
		     freepascal-defun-re)))
    (while (and (/= func 0)
		(re-search-forward reg nil 'move))
      (cond ((let ((state (save-excursion
			    (parse-partial-sexp (point-min) (point)))))
	       (or (nth 3 state) (nth 4 state))) ; Inside string or comment
	     ())
	    ((match-end 1)
	     (setq nest (1+ nest))
	     (if (save-excursion
		   (goto-char (match-beginning 0))
		   (looking-at "\\<record\\>"))
		 (setq func (1+ func))))
	    ((match-end 2)
	     (setq nest (1- nest))
	     (if (= nest 0)
		 (setq func (1- func))))
	    ((match-end 3)
	     (setq func (1+ func))))))
  (forward-line 1))

(defun freepascal-beg-of-case ()
  "Move backward to the beginning of the current case-statement."
  (interactive)
  (catch 'found
    (if (not (looking-at (concat "\\s \\|\\s)\\|" "case")))
	(forward-sexp 1))
    (let ((nest 0) (max -1) (func 0)
	  (reg (concat freepascal-beg-block-re "\\|" 
		       freepascal-end-block-re )))
      (while (re-search-backward reg nil 'move)
	(cond ((let ((state (save-excursion
			      (parse-partial-sexp (point-min) (point)))))
		 (or (nth 3 state) (nth 4 state))) ; Inside string or comment
	       ())
	      ((match-end 1)                       ; begin|case|record|repeat
	       (if (and (looking-at "\\<case\\>") (>= nest max))
		   (throw 'found t)
		 (setq nest (1+ nest)
		       max (max max nest))))
	      ((match-end 2)                       ; end|until
	       (setq nest (1- nest)))
	      )))
    nil))

(defun freepascal-next-case-label (&optional limit)
  "Move forward to the next label of the current case statement"
  (interactive)
  (let ((nest (if (looking-at "\\<case\\>") -1 0)) (found nil)
	(reg (concat freepascal-beg-block-re "\\|" 
		     freepascal-end-block-re "\\|"
		     "\\(" freepascal-caselabel-re "\\)" )))
    (while (and (not found)
		(re-search-forward reg limit 'move))
      (cond ((let ((state (save-excursion
			    (parse-partial-sexp (point-min) (point)))))
	       (or (nth 3 state) (nth 4 state))) ; Inside string or comment
	     ())
	    ((match-end 1)
	     (setq nest (1+ nest)))
	    ((match-end 2)
	     (setq nest (1- nest)))
	    ((match-end 3)
	     (if (and (<= nest 0) (/= (following-char) ?=))
		 (setq found t)))))
    found))


(defun freepascal-end-of-statement ()
  "Move forward to end of current statement."
  (interactive)
  (let ((nest 0) pos
	(regexp (concat "\\(" freepascal-beg-block-re "\\)\\|\\("
			freepascal-end-block-re "\\)")))
    (if (not (looking-at "[ \t\n]")) (forward-sexp -1))
    (or (looking-at freepascal-beg-block-re)
	;; Skip to end of statement
	(setq pos (catch 'found
		    (while t
		      (forward-sexp 1)
		      (cond ((looking-at "[ \t]*;")
			     (skip-chars-forward "^;")
			     (forward-char 1)
			     (throw 'found (point)))
			    ((save-excursion
			       (forward-sexp -1)
			       (looking-at freepascal-beg-block-re))
			     (goto-char (match-beginning 0))
			     (throw 'found nil))
			    ((eobp)
			     (throw 'found (point))))))))
    (if (not pos)
	;; Skip a whole block
	(catch 'found
	  (while t
	    (re-search-forward regexp nil 'move)
	    (setq nest (if (match-end 1) 
			   (1+ nest)
			 (1- nest)))
	    (cond ((eobp)
		   (throw 'found (point)))
		  ((= 0 nest)
		   (throw 'found (freepascal-end-of-statement))))))
      pos)))




(defun freepascal-downcase-keywords ()
  "Downcase all Free Pascal keywords in the buffer."
  (interactive)
  (freepascal-change-keywords 'downcase-word))

(defun freepascal-upcase-keywords ()
  "Upcase all Free Pascal keywords in the buffer."
  (interactive)
  (freepascal-change-keywords 'upcase-word))

(defun freepascal-capitalize-keywords ()
  "Capitalize all Free Pascal keywords in the buffer."
  (interactive)
  (freepascal-change-keywords 'capitalize-word))

;; Change the keywords according to argument.
(defun freepascal-change-keywords (change-word)
  (save-excursion
    (let ((keyword-re (concat "\\<\\("
			      (regexp-opt freepascal-keywords)
			      "\\)\\>")))
      (goto-char (point-min))
      (while (re-search-forward keyword-re nil t)
	(funcall change-word -1)))))



;;;
;;; Other functions
;;;
(defun freepascal-set-auto-comments ()
  "Insert `{ case }' or `{ NAME }' on this line if appropriate.
Insert `{ case }' if there is an `end' on the line which
ends a case block.  Insert `{ NAME }' if there is an `end'
on the line which ends a function or procedure named NAME."
  (save-excursion
    (forward-line -1)
    (skip-chars-forward " \t")
    (if (and (looking-at "\\<end;") (freepascal-in-implementation)
	     (not (save-excursion
		    (end-of-line)
		    (search-backward "{" (freepascal-get-beg-of-line) t))))
	(let ((type (car (freepascal-calculate-indent))))
	  (if (eq type 'declaration)
	      ()
	    (if (eq type 'case)
		;; This is a case block
		(progn
		  (end-of-line)
		  (delete-horizontal-space)
		  (insert " { case }"))
	      (let ((nest 1))
		;; Check if this is the end of a function
		(save-excursion
		  (while (not (or (looking-at freepascal-defun-re) (bobp)))
		    (backward-sexp 1)
		    (cond ((and (looking-at freepascal-beg-block-re)
				(not (looking-at "class[ \t]function"))
				(not (looking-at "class[ \t]procedure")))
			   (setq nest (1- nest)))
			  ((looking-at freepascal-end-block-re)
			   (setq nest (1+ nest)))))
		  (if (bobp)
		      (setq nest 1)))
		(if (zerop nest)
		    (progn
		      (end-of-line)
		      (delete-horizontal-space)
		      (insert " { ")
		      (let (b e)
			(save-excursion
			  (setq b (progn (beginning-of-line)
					 (freepascal-beg-of-defun)
					 (if (not (looking-at "class")) nil
					   (skip-chars-forward "^ \t")
					   (skip-chars-forward " \t"))
					 (skip-chars-forward "^ \t")
					 (skip-chars-forward " \t")
					 (point))
				e (progn (skip-chars-forward "a-zA-Z0-9_.")
					 (point))))
			(insert-buffer-substring (current-buffer) b e))
		      (insert " }"))))))))))

(defun freepascal-interface-is-section ()
  "Returns t if the interface keyword we are looking at is the start of
   the interface section of the module"
  (if (looking-at "interface\\>")
      (save-excursion
	(forward-sexp -1)
	(forward-sexp 1)
	(not (looking-at "[ \t]*=")))
    t))




;;;
;;; Indentation
;;;
(defconst freepascal-indent-alist
  '((block . (+ ind freepascal-indent-level))
    (case . (+ ind freepascal-case-indent))
    (caseblock . ind) (cpp . 0)
    (declaration . (+ ind freepascal-indent-level))
    (paramlist . (freepascal-indent-paramlist t))
    (comment . (freepascal-indent-comment t))
    (defun . (freepascal-get-defun-indent)) 
    (contexp . ind) (section . 2)
    (uncomplete-assignement . (freepascal-get-assignement-indent))
    (uncomplete-property . (+ ind freepascal-indent-level))
    (directives . (+ (* 2 freepascal-indent-level) ind))
    (unknown . ind) (string . 0)))

(defun freepascal-indent-command ()
  "Indent for special part of code."
  (let* ((indent-str (freepascal-calculate-indent))
	 (type (car indent-str))
	 (ind (car (cdr indent-str))))
    (cond ((and (eq type 'paramlist)
		(or (memq 'all freepascal-auto-lineup)
		    (memq 'paramlist freepascal-auto-lineup)))
	   (freepascal-indent-paramlist)
	   (freepascal-indent-paramlist))
	  ((and (eq type 'declaration) (save-excursion 
					 (beginning-of-line)
					 (not (looking-at "[ \t]*$")))
		(or (memq 'all freepascal-auto-lineup)
		    (memq 'declaration  freepascal-auto-lineup)))
	   (freepascal-indent-declaration))
	  ((and (eq type 'case) (not (looking-at "^[ \t]*$"))
		(or (memq 'all freepascal-auto-lineup)
		    (memq 'case freepascal-auto-lineup)))
	   (freepascal-indent-case)))
    (if (looking-at "[ \t]+$")
	(skip-chars-forward " \t"))))

(defun freepascal-indent-line ()
  "Indent current line as a Free Pascal statement."
  (let* ((indent-str (freepascal-calculate-indent))
	 (type (car indent-str))
	 (ind (car (cdr indent-str))))
    (if (looking-at "^[0-9a-zA-Z]+[ \t]*:[^=]")
	(search-forward ":" nil t))
    (delete-horizontal-space)
    ;; Some things should not be indented
    (if (or (and (eq type 'declaration) (looking-at freepascal-declaration-re))
	    (eq type 'cpp)
	    (and (looking-at freepascal-sections-re) (freepascal-interface-is-section)))
	()
      ;; Some things only a bit 
      (if (looking-at freepascal-class-prot-re)
	  (indent-to (+ (1- ind) freepascal-indent-level))
	;; Other things should have no extra indent
	(if (looking-at freepascal-noindent-re)
	    (cond ((or (eq type 'defun)
		       (and (looking-at "\\<else\\>") (eq type 'case)))
		   (indent-to (eval (cdr (assoc type freepascal-indent-alist)))))
		  ((and (eq type 'case) (looking-at "\\<end\\>"))
		   (let ((new-ind (freepascal-check-case)))
		     (if new-ind 
			 (indent-to new-ind)
		       (indent-to ind))))
		  (t
		   (indent-to ind)))
	  ;; But most lines are treated this way:
	  (indent-to (eval (cdr (assoc type freepascal-indent-alist))))
	  )))))


;; (defun freepascal-indent-line ()
;;   "Indent current line as a Freepascal statement."
;;   (let* ((indent-str (freepascal-calculate-indent))
;; 	 (type (car indent-str))
;; 	 (freepascal--extra-indent (car (cdr indent-str))))
;;     ;; Labels should not be indented.
;;     (if (and (looking-at "^[0-9a-zA-Z]+[ \t]*:[^=]")
;; 	     (not (eq type 'declaration)))
;; 	(search-forward ":" nil t))
;;     (delete-horizontal-space)
;;     (cond (; Some things should not be indented
;; 	   (or (and (eq type 'declaration) (looking-at freepascal-declaration-re))
;; 	       (eq type 'cpp))
;; 	   ())
;; 	  (; Other things should have no extra indent
;; 	   (looking-at freepascal-noindent-re)
;; 	   (indent-to freepascal--extra-indent))
;; 	  (; Nested functions should be indented
;; 	   (looking-at freepascal-defun-re)
;; 	   (if (and freepascal-indent-nested-functions
;; 		    (eq type 'defun))
;; 	       (indent-to (+ freepascal--extra-indent freepascal-indent-level))
;; 	     (indent-to freepascal--extra-indent)))
;; 	  (; But most lines are treated this way
;; 	   (indent-to (eval (cdr (assoc type freepascal-indent-alist))))
;; 	   ))))


(defun freepascal-calculate-indent ()
  "Calculate the indent of the current Free Pascal line.
Return a list of two elements: (INDENT-TYPE INDENT-LEVEL)."
  (save-excursion
    (let* ((oldpos (point)) (parse-sexp-ignore-comments t)
	   (state (save-excursion (parse-partial-sexp (point-min) (point))))
	   (nest 0) (par 0) (complete (looking-at "[ \t]*end\\>"))
	   (elsed (looking-at "[ \t]*else\\>"))
	   (ended (looking-at "[ \t]*end\\>"))
	   (tod   (looking-at "[ \t]*\\(down\\)?to\\>"))
	   (elsecount 0)
	   (directives (looking-at (concat "[ \t]*" freepascal-directives-re)))
	   (type (catch 'nesting
		   ;; Check if inside a string, comment or parenthesis
		   (cond ((nth 3 state) (throw 'nesting 'string))
			 ((nth 4 state) (throw 'nesting 'comment))
			 ((> (car state) 0)
			  (save-excursion
			    (goto-char (scan-lists (point) -1 1))
			    (if (and (looking-at ".*[[(]$") (> (car state) 0))
				(progn
					;				  (goto-char (scan-lists (point) -1 1))
				  (forward-sexp -1)
				  (setq par (+ 2 (current-column))))
			      (setq par (1+ (current-column)))))
			  (goto-char (scan-lists (point) -1 (car state))))
			 ;;((save-excursion (beginning-of-line)
			 ;;		  (eq (following-char) ?#))
			 ;; (throw 'nesting 'cpp))
			 )
		   ;; Loop until correct indent is found
		   (while t
		     (backward-sexp 1)
		     (cond (;--Escape from case statements
			    (and (looking-at "[^ \t,:(]+[ \t]*\\(,[ \t]*[^ \t,:]+[ \t]*\\)*:[^=]")
					;(looking-at "[A-Za-z0-9]+[ \t]*:[^=]")
				 (not complete)
				 (save-excursion (skip-chars-backward " \t")
						 (bolp))
					;(= (save-excursion
					;     (end-of-line) (backward-sexp) (point))
					;   (point))
				 (> (save-excursion (goto-char oldpos)
						    (beginning-of-line)
						    (point))
				    (point)))
			    (throw 'nesting 'caseblock))
			   (;--Nest block outwards
			    (and (looking-at freepascal-beg-block-re)
				 (not (looking-at "class\\((.*)\\)?;"))
				 (or (freepascal-in-implementation)
				     (not (looking-at "class[ \t]+function")))
				 (or (freepascal-in-implementation)
				     (not (looking-at "class[ \t]+procedure")))
				 (not (looking-at "object;")))
			    (if (= nest 0)
				(cond ((looking-at "case\\>")
				       (throw 'nesting 'case))
				      ((looking-at "record\\>")
				       (throw 'nesting 'declaration))
				      (directives
				       (throw 'nesting 'directives))
				      (t (throw 'nesting 'block)))
			      (if (and (looking-at "case\\>") 
				       (save-excursion 
					 (forward-sexp 1)
					 (freepascal-check-case)))
				  () 
				(setq nest (1- nest)))))
			   (;--Nest block inwards
			    (looking-at freepascal-end-block-re)
					;(if (and (looking-at "end\\s ")
					;     elsed (not complete))
					;(throw 'nesting 'block))
			    (setq complete t
				  nest (1+ nest)))
			   (;--Defun (or parameter list)
			    (and (looking-at freepascal-defun-re)
				 (or (> par 0) (freepascal-defun-valid)))
			    (if (= 0 par)
				(throw 'nesting 'defun)
			      (setq par 0)
			      (let ((n 0))
				(while (re-search-forward
					"\\(\\<record\\>\\)\\|\\<end\\>"
					oldpos t)
				  (if (match-end 1)
				      (setq n (1+ n)) (setq n (1- n))))
				(if (> n 0)
				    (throw 'nesting 'declaration)
				  (throw 'nesting 'paramlist)))))
			   (;--Declaration part
			    (looking-at freepascal-declaration-re)
			    (if (or (save-excursion
				      (forward-word 1)
				      (freepascal-declaration-end)
				      (forward-line 1)
				      (>= oldpos (point)))
				    (looking-at freepascal-defun-re)) 
				(throw 'nesting 'defun)
			      (throw 'nesting 'declaration)))
			   (;--If, else or while statement 
			    (and (or (not complete)) (not elsed)
				 (looking-at freepascal-sub-block-re))
			    (throw 'nesting 'block))
			   (;--another else
			    (and elsed (= nest 0) (looking-at "else\\>"))
			    (setq elsecount (1+ elsecount))) 
			   (;--If Statement with else
			    (and elsed (= 0 nest) 
				 (looking-at "\\<if\\>"))
			    (if (= elsecount 0) 
				(throw 'nesting 'block)
			      (setq elsecount (1- elsecount))))
			   (;--- looking at a to
			    (looking-at "\\<\\(down\\)?to\\>")
			    (setq tod t))
			   (;--Uncomplete property definition
			    (and (not complete)
				 (looking-at "property\\>")
				 (not (looking-at 
				       "property\\>.*;\([ \t]*\{.*\}\)*$")))
			    (throw 'nesting 'uncomplete-property))
			   (;--Uncomplete assignement statement
			    (and (not complete)
				 (looking-at "[^;\n]*:=")
				 (not (or elsed ended tod 
					  (looking-at ".*:=.*;"))))
			    (throw 'nesting 'uncomplete-assignement))
			   (;--Found complete statement
			    (save-excursion (forward-sexp 1)
					    (= (following-char) ?\;))
			    (setq complete t))
			   (;-- a new section of code
			    (looking-at freepascal-sections-re)
			    (throw 'nesting 'section))
			   (;--No known statements
			    (bobp)
			    (throw 'nesting 'unknown))
			   )))))

      ;; Return type of block and indent level.
      (if (> par 0)                               ; Unclosed Parenthesis 
	  (list 'contexp par)
	(list type (freepascal-indent-level))))))


(defun freepascal-indent-level ()
  "Return the indent-level the current statement has.
Do not count labels, case-statements or records."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[ \t]*[^ \t,:]+[ \t]*\\(,[ \t]*[^ \t,:]+[ \t]*\\)*:[^=]")
					;(looking-at "[ \t]*[0-9a-zA-Z]+[ \t]*:[^=]")
	(if (looking-at ".*:[ \t]*$")
	    (progn (re-search-forward "[ \t]*" nil t)
		   (move-to-column (+ (current-column) 2) t))
	  (search-forward ":" nil t)
	  (skip-chars-forward " \t"))
      (if (looking-at ".*=[ \t]*\\(packed[ \t]+\\)?record\\>")
	  (search-forward "=" nil t))
      (skip-chars-forward " \t"))
    (current-column)))

(defun freepascal-indent-comment (&optional arg)
  "Indent current line as comment.
If optional arg is non-nil, just return the
column number the line should be indented to."
  (let* ((stcol (save-excursion
		  (re-search-backward "(\\*\\|{" nil t)
		  (+ 2 (current-column)))))
    (if arg stcol
      (delete-horizontal-space)
      (indent-to stcol))))

(defun freepascal-indent-case ()
  "Indent within case statements."
  (skip-chars-forward ": \t")
  (let ((end (progn
	       (end-of-line)
	       (point-marker)))
	(beg (progn 
	       (freepascal-beg-of-case)
	       (point))) 
	oldpos
	(ind 0))
    ;; Get right indent
    (while (< (point) (marker-position end))
      (if (not (freepascal-next-case-label
		(marker-position end))) ()
	(forward-char -1)
	(delete-horizontal-space)
	(if (> (current-column) ind)
	    (setq ind (current-column))))
      )
    (goto-char beg)
    (setq oldpos (marker-position end))
    ;; Indent all case statements
    (while (< (point) (marker-position end))
      (if (freepascal-next-case-label (marker-position end))
	  (forward-char -1))
      (indent-to (1+ ind))
      (if (/= (following-char) ?:)
	  ()
	(forward-char 1)
	(if (= (following-char) ?=)
	    ()
	  (delete-horizontal-space)
	  (insert " "))
	(if (looking-at ".*:")
	    (progn (re-search-forward
		    ".*:" (marker-position end) 'move)
		   )
	  ))
      (setq oldpos (point))
      )
    (goto-char oldpos)))

(defun freepascal-indent-paramlist (&optional arg)
  "Indent current line in parameterlist.
If optional arg is non-nil, just return the
indent of the current line in parameterlist."
  (save-excursion
    (let* ((oldpos (point-marker)) 
	   newvar newconst
	   (stpos (progn (goto-char (1+ (scan-lists (point) -1 1))) (point)))
	   (stcol (if (not (looking-at "[ \t]*$"))
		      (current-column)
		    (save-excursion (forward-char -1)
				    (forward-sexp -1)
				    (+ 2 (current-column)))))
	   
	   (edpos (progn (freepascal-declaration-end) 
			 (search-backward ")" (freepascal-get-beg-of-line) t)
			 (if (> (point) stpos) (point-marker)
			   (forward-line 1) (point-marker))))
	   (usevar (re-search-backward "\\<\\(var\\|out\\)\\>" stpos t))
	   (useconst (progn 
		       (goto-char (marker-position edpos))
		       (re-search-backward "\\(^\\|\\S_\\s-\\)\\s-*\\<const\\>" stpos t)))
	   (inpos (progn
		    (goto-char oldpos)
		    (beginning-of-line)
		    (if (< (point) stpos) (goto-char stpos)) 
		    (if useconst
			(if (not usevar) 
			    (if (looking-at "[ \t]*const\\>")
				(prog1 stcol (setq newconst t))
			      (+ 6 stcol))
			  (if (looking-at "[ \t]*const\\>")
			      (prog1 stcol (setq newconst t))
			    (if (looking-at "[ \t]*\\(var\\|out\\)\\>")
				(prog1 (+ 2 stcol) (setq newvar t))
			      (+ 6 stcol))))
		      (if (and usevar (not (looking-at "[ \t]*\\(var\\|out\\)\\>")))
			  (+ 4 stcol) 
			(prog1 stcol 
			  (setq newconst (looking-at "[ \t]*const\\>"))
			  (setq newvar (looking-at "[ \t]*\\(var\\|out\\)\\>"))))
		      ))))
      (if arg inpos
	(if (not (or newconst newvar))
	    ()
	  (goto-char stpos)
	  (while (< (point) (marker-position oldpos))
	    (delete-horizontal-space)
	    (cond 
	     ((and newvar (not useconst)) 
	      (if (looking-at "[ \t]*\\(var\\|out\\)\\>")
		  (indent-to stcol)
		(indent-to (+ 4 stcol))))
	     ((or newconst useconst) 
	      (if (looking-at "[ \t]*\\(var\\|out\\)\\>")
		  (indent-to (+ 2 stcol))
		(if (looking-at "[ \t]*const\\>")
		    (indent-to stcol)
		  (indent-to (+ 6 stcol))))))
	    (forward-line 1))
	  )    
	(goto-char (marker-position oldpos))
					;(forward-char 1)
	(delete-horizontal-space)
	(indent-to inpos)
	(freepascal-indent-declaration nil stpos (marker-position edpos)))

      )))

(defun freepascal-indent-declaration (&optional arg start end)
  "Indent current lines as declaration, lining up the `:'s and, or `='s."
  (let ((pos (point-marker)) status)
    (if (and (not (or arg start)) (not (freepascal-declaration-beg)))
	()
      (let* ((lineup 
	      (if (and (not (looking-at "\\<\\(class\\|object\\)\\((.*)\\)?;"))
		       (or (looking-at "\\<var\\>\\|\\<record\\>\\|\\<class\\>\\|\\<object\\>") 
			   arg start)) 
		  ":" "="))
	     (lineup-both (looking-at "\\<const\\>" ))
	     (class (looking-at "\\<\\(class\\|object\\)\\>"))
					;	    (stpos (if start start
					;		       (forward-word 2) (backward-word 1) (point)))
	     (stpos (if start start
		      (forward-line 1)
		      (skip-chars-forward " \t")
		      (point)))
	     (edpos (set-marker (make-marker)
				(if end end
				  (max (progn (freepascal-declaration-end 
					       (if class 1 nil))
					      (point))
				       pos))))
	     ind
	     ind-second )

	(goto-char stpos)
	;; Indent lines in record block
	(if arg
	    (while (<= (point) (marker-position edpos))
	      (beginning-of-line)
	      (delete-horizontal-space)
	      (save-excursion
		(setq status (parse-partial-sexp stpos (point))))
	      (if (looking-at "\\<end\\>")
		  (progn (indent-to arg)
			 (set-marker edpos (1- (point))))
		(if (or (nth 4 status) (looking-at "{\\|(\\*\\|\\\\"))
		    (freepascal-indent-line)
		  (if (looking-at freepascal-class-prot-re)
		      (indent-to (+ 1 arg))
		    (if (and (looking-at "read\\>\\|write\\>\\|stored\\>")
			     (eq 'uncomplete-property 
				 (car (freepascal-calculate-indent))))
			(indent-to (+ arg (+ freepascal-indent-level 
					     freepascal-indent-level)))
		      (indent-to (+ arg freepascal-indent-level))
		      (if (= 0 (car status))
			  ()
			(delete-horizontal-space)
			(freepascal-indent-command)))))
		(forward-line 1))))

	;; Do lineup
	(if (not lineup-both) () 
	  (setq ind-second (freepascal-get-lineup-indent stpos edpos ":" nil))
	  (goto-char stpos)
	  (while (<= (point) (marker-position edpos))
	    (if (search-forward ":" (freepascal-get-end-of-line) 'move)
		(forward-char -1))
	    (delete-horizontal-space)
	    (indent-to ind-second)
	    (if (not (looking-at ":"))
		(forward-line 1) ; No more indent if there is no : or =
	      (forward-char 1)
	      (delete-horizontal-space)
	      (insert " ")
	      (forward-line 1))))

	(setq ind (freepascal-get-lineup-indent stpos edpos lineup class))
	(goto-char stpos)
	(while (<= (point) (marker-position edpos))
	  (if (search-forward lineup (freepascal-get-end-of-line) 'move)
	      (forward-char -1))
	  (if (and class 
		   (or (save-excursion 
			 (beginning-of-line)
			 (looking-at "[ \t]*\\<\\(constructor\\|destructor\\|function\\|procedure\\|property\\)\\>"))
		       (< 0 (car (parse-partial-sexp stpos (point))))))
	      (forward-line 1)
	    (delete-horizontal-space)
	    (indent-to ind)
	    (if (not (looking-at lineup))
		(forward-line 1) ; No more indent if there is no : or =
	      (forward-char 1)
	      (delete-horizontal-space)
	      (insert " ")
	      ;; Indent record block
	      (if (and (not (looking-at "\\<class\\((.*)\\)?;"))
		       (looking-at "\\(packed[ \t]+\\)?record\\>\\|class\\>\\|object\\>"))
		  (if (looking-at "\\(packed[ \t]+\\)?record\\>")
		      (freepascal-indent-declaration (current-column))
		    (freepascal-indent-declaration 
		     (save-excursion
		       (beginning-of-line)
		       (skip-chars-forward " \t")
		       (current-column)))))
	      (forward-line 1))))))

    ;; If arg - move point
    (if arg (forward-line -1)
      (goto-char (marker-position pos)))))

					;  "Return the indent level that will line up several lines within the region
					;from b to e nicely. The lineup string is str."
(defun freepascal-get-lineup-indent (b e str class)
  (save-excursion
    (let ((ind 0)
	  (reg (concat str "\\|\\(\\<record\\>\\|\\<class\\>[^;]\\|\\<object\\>[^;]\\)\\|\\(\\<end\\>\\)"))
	  (nest 0))
      (goto-char b)
      ;; Get rightmost position
      (while (< (point) e)
	(setq nest 0)
	(if (re-search-forward reg (min e (freepascal-get-end-of-line 2)) 'move)
	    (progn
	      ;; Skip record blocks
	      (cond ((match-beginning 1)
		     (setq nest (1+ nest))
		     (while (and (< (point) e) (> nest 0))
		       (re-search-forward "\\(\\<record\\>\\<class\\>\\|\\<object\\>\\)\\|\\(\\<end\\>\\)"
					  e 'move)
		       (cond ((match-beginning 1)
			      (setq nest (1+ nest)))
			     ((match-beginning 2)
			      (setq nest (1- nest)))))
		     )
		    ((match-beginning 2)
		     (goto-char e))
		    (t
		     (goto-char (match-beginning 0))
		     (skip-chars-backward " \t")
		     (if (and (> (current-column) ind)
			      (or (not class)
				  (and (save-excursion
					 (beginning-of-line)
					 (not (or (looking-at "[ \t]*\\<\\(constructor\\|destructor\\|property\\|function\\|procedure\\)\\>")
						  (looking-at "[ \t]*)[ \t]*:"))))
				       (= 0 (car (parse-partial-sexp b (point)))))))
			 (setq ind (current-column)))
		     (goto-char (match-end 0))
		     (if (not (looking-at ".*\\(\\<record\\>\\|\\<class\\>\\(([^)]*)\\)?[^;]\\|\\<object\\>[^;]\\|\\<end\\>\\)" ))
			 (end-of-line))))))) ;;end-of-line added 
      ;;search only first linup char in line
      ;; In case no lineup was found
      (if (> ind 0)
	  (1+ ind)
	;; No lineup-string found
	(goto-char b)
	(end-of-line)
	(skip-chars-backward " \t")
	(1+ (current-column))))))


(defun freepascal-get-assignement-indent ()
  (save-excursion
    (re-search-backward ":=" nil 'move)
    (beginning-of-line)
    (if (looking-at ".*:=$")
	(progn (skip-chars-forward " \t")
	       (+ (current-column) freepascal-indent-level))
      (re-search-forward ":=[ \t]*" nil 'move)
      (current-column))))

(defun freepascal-get-defun-indent ()
  (let ((ind (save-excursion
	       (freepascal-beg-of-defun t)
	       (current-column)))) 
    (if (or (not (looking-at  freepascal-beg-block-re))
	    (looking-at "class[ \t]+function")
	    (looking-at "class[ \t]+procedure"))
	(+ ind freepascal-indent-level)
      ind
      )))


(defun freepascal-check-case ()
  "Returns nil if the in ordinary Case statement, the indentation of of the
record block if point is in the case of a variant record definition"
  (let (indent)
    (save-excursion
      (freepascal-beg-of-case)
      (setq indent (freepascal-calculate-indent))
      (if (not (eq (car indent) 'declaration))
	  nil
	(car (cdr indent))))))




;;;
;;; Completion
;;;
(defvar freepascal-str nil)
(defvar freepascal-all nil)
(defvar freepascal-pred nil)
(defvar freepascal-buffer-to-use nil)
(defvar freepascal-flag nil)

(defun freepascal-string-diff (str1 str2)
  "Return index of first letter where STR1 and STR2 differs."
  (catch 'done
    (let ((diff 0))
      (while t
	(if (or (> (1+ diff) (length str1))
		(> (1+ diff) (length str2)))
	    (throw 'done diff))
	(or (equal (aref str1 diff) (aref str2 diff))
	    (throw 'done diff))
	(setq diff (1+ diff))))))

;; Calculate all possible completions for functions if argument is `function',
;; completions for procedures if argument is `procedure' or both functions and
;; procedures otherwise.

(defun freepascal-func-completion (type)
  ;; Build regular expression for function/procedure names
  (if (string= freepascal-str "")
      (setq freepascal-str "[a-zA-Z_]"))
  (let ((freepascal-str (concat (cond
				 ((eq type 'procedure) "\\<\\(procedure\\)\\s +")
				 ((eq type 'function) "\\<\\(function\\)\\s +")
				 ((eq type 'constructor) 
				  "\\<\\(constructor\\)\\s +")
				 ((eq type 'destructor) "\\<\\(destructor\\)\\s +")
				 (t "\\<\\(function\\|procedure\\|constructor\\|destructor\\)\\s +"))
				"\\<\\(" freepascal-str "[a-zA-Z0-9_.]*\\)\\>"))
	match)
    
    (if (not (looking-at 
	      "\\<\\(function\\|procedure\\|constructor\\|destructor\\)\\>"))
	(re-search-backward 
	 "\\<\\(function\\|procedure\\|constructor\\|destructor\\)\\>" nil t))
    (forward-char 1)

    ;; Search through all reachable functions
    (while (freepascal-beg-of-defun)
      (if (re-search-forward freepascal-str (freepascal-get-end-of-line) t)
	  (progn (setq match (buffer-substring (match-beginning 2)
					       (match-end 2)))
		 (if (or (null freepascal-pred)
			 (funcall freepascal-pred match))
		     (setq freepascal-all (cons match freepascal-all)))))
      (goto-char (match-beginning 0)))))

(defun freepascal-get-completion-decl ()
  ;; Macro for searching through current declaration (var, type or const)
  ;; for matches of `str' and adding the occurence tp `all'
  (let ((end (save-excursion (freepascal-declaration-end)
			     (point)))
	match)
    ;; Traverse lines
    (while (< (point) end)
      (if (re-search-forward "[:=]" (freepascal-get-end-of-line) t)
	  ;; Traverse current line
	  (while (and (re-search-backward 
		       (concat "\\((\\|\\<\\(var\\|type\\|const\\)\\>\\)\\|" 
			       freepascal-symbol-re)
		       (freepascal-get-beg-of-line) t)
		      (not (match-end 1)))
	    (setq match (buffer-substring (match-beginning 0) (match-end 0)))
	    (if (string-match (concat "\\<" freepascal-str) match)
		(if (or (null freepascal-pred)
			(funcall freepascal-pred match))
		    (setq freepascal-all (cons match freepascal-all))))))
      (if (re-search-forward "\\<record\\>" (freepascal-get-end-of-line) t)
	  (freepascal-declaration-end)
	(forward-line 1)))))

(defun freepascal-type-completion ()
  "Calculate all possible completions for types."
  (let ((start (point))
	goon)
    ;; Search for all reachable type declarations
    (while (or (freepascal-beg-of-defun)
	       (setq goon (not goon)))
      (save-excursion
	(if (and (< start (prog1 (save-excursion (freepascal-end-of-defun)
						 (point))
			    (forward-char 1)))
		 (re-search-forward
		  "\\<type\\>\\|\\<\\(begin\\|function\\|procedure\\)\\>"
		  start t)
		 (not (match-end 1)))
	    ;; Check current type declaration
	    (freepascal-get-completion-decl))))))

(defun freepascal-var-completion ()
  "Calculate all possible completions for variables (or constants)."
  (let ((start (point))
	goon twice)
    ;; Search for all reachable var declarations
    (while (or (freepascal-beg-of-defun)
	       (setq goon (not goon)))
      (save-excursion
	(if (> start (prog1 (save-excursion (freepascal-end-of-defun)
					    (point))))
	    () ; Declarations not reacable
	  (if (search-forward "(" (freepascal-get-end-of-line) t)
	      ;; Check parameterlist
	      (freepascal-get-completion-decl))
	  (setq twice 2)
	  (while (>= (setq twice (1- twice)) 0)
	    (cond ((and (re-search-forward
			 (concat "\\<\\(var\\|const\\)\\>\\|"
				 "\\<\\(begin\\|function\\|procedure\\)\\>")
			 start t)
			(not (match-end 2)))
		   ;; Check var/const declarations
		   (freepascal-get-completion-decl))
		  ((match-end 2)
		   (setq twice 0)))))))))


(defun freepascal-keyword-completion (keyword-list)
  "Give list of all possible completions of keywords in KEYWORD-LIST."
  (mapc #'(lambda (s) 
	   (if (string-match (concat "\\<" freepascal-str) s)
	       (if (or (null freepascal-pred)
		       (funcall freepascal-pred s))
		   (setq freepascal-all (cons s freepascal-all)))))
	keyword-list))

;; Function passed to completing-read, try-completion or
;; all-completions to get completion on STR. If predicate is non-nil,
;; it must be a function to be called for every match to check if this
;; should really be a match. If flag is t, the function returns a list
;; of all possible completions. If it is nil it returns a string, the
;; longest possible completion, or t if STR is an exact match. If flag
;; is 'lambda, the function returns t if STR is an exact match, nil
;; otherwise.

(defun freepascal-completion (freepascal-str freepascal-pred freepascal-flag)
  (save-excursion
    (let ((freepascal-all nil))
      ;; Set buffer to use for searching labels. This should be set
      ;; within functins which use freepascal-completions
      (set-buffer freepascal-buffer-to-use)

      ;; Determine what should be completed
      (let ((state (car (freepascal-calculate-indent))))
	(cond (;--Within a declaration or parameterlist
	       (or (eq state 'declaration) (eq state 'paramlist)
		   (and (eq state 'defun)
			(save-excursion
			  (re-search-backward ")[ \t]*:"
					      (freepascal-get-beg-of-line) t))))
	       (if (or (eq state 'paramlist) (eq state 'defun))
		   (freepascal-beg-of-defun))
	       (freepascal-type-completion)
	       (freepascal-keyword-completion freepascal-type-keywords))
	      (;--Starting a new statement
	       (and (not (eq state 'contexp))
		    (save-excursion
		      (skip-chars-backward "a-zA-Z0-9_.")
		      (backward-sexp 1)
		      (or (looking-at freepascal-nosemi-re)
			  (progn
			    (forward-sexp 1)
			    (looking-at "\\s *\\(;\\|:[^=]\\)")))))
	       (save-excursion (freepascal-var-completion))
	       (freepascal-func-completion 'procedure)
	       (freepascal-keyword-completion freepascal-start-keywords))
	      (t;--Anywhere else
	       (save-excursion (freepascal-var-completion))
	       (freepascal-func-completion 'function)
	       (freepascal-keyword-completion freepascal-separator-keywords))))
      
      ;; Now we have built a list of all matches. Give response to caller
      (freepascal-completion-response))))

(defun freepascal-completion-response ()
  (cond ((or (equal freepascal-flag 'lambda) (null freepascal-flag))
	 ;; This was not called by all-completions
	 (if (null freepascal-all)
	     ;; Return nil if there was no matching label
	     nil
	   ;; Get longest string common in the labels
	   (let* ((elm (cdr freepascal-all))
		  (match (car freepascal-all))
		  (min (length match))
		  exact tmp)
	     (if (string= match freepascal-str)
		 ;; Return t if first match was an exact match
		 (setq match t)
	       (while (not (null elm))
		 ;; Find longest common string
		 (if (< (setq tmp (freepascal-string-diff match (car elm))) min)
		     (progn
		       (setq min tmp)
		       (setq match (substring match 0 min))))
		 ;; Terminate with match=t if this is an exact match
		 (if (string= (car elm) freepascal-str)
		     (progn
		       (setq match t)
		       (setq elm nil))
		   (setq elm (cdr elm)))))
	     ;; If this is a test just for exact match, return nil ot t
	     (if (and (equal freepascal-flag 'lambda) (not (equal match 't)))
		 nil
	       match))))
	;; If flag is t, this was called by all-completions. Return
	;; list of all possible completions
	(freepascal-flag
	 freepascal-all)))

(defvar freepascal-last-word-numb 0)
(defvar freepascal-last-word-shown nil)
(defvar freepascal-last-completions nil)

(defun freepascal-complete-word ()
  "Complete word at current point.
\(See also `freepascal-toggle-completions', `freepascal-type-keywords',
`freepascal-start-keywords' and `freepascal-separator-keywords'.)"
  (interactive)
  (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_") (point)))
	 (e (save-excursion (skip-chars-forward "a-zA-Z0-9_") (point)))
	 (freepascal-str (buffer-substring b e))
	 ;; The following variable is used in freepascal-completion
	 (freepascal-buffer-to-use (current-buffer))
	 (allcomp (if (and freepascal-toggle-completions
			   (string= freepascal-last-word-shown freepascal-str))
		      freepascal-last-completions
		    (all-completions freepascal-str 'freepascal-completion)))
	 (match (if freepascal-toggle-completions
		    "" (try-completion
			freepascal-str (mapc #'(lambda (elm)
						(cons elm 0)) allcomp)))))
    ;; Delete old string
    (delete-region b e)

    ;; Toggle-completions inserts whole labels
    (if freepascal-toggle-completions
	(progn
	  ;; Update entry number in list
	  (setq freepascal-last-completions allcomp
		freepascal-last-word-numb 
		(if (>= freepascal-last-word-numb (1- (length allcomp)))
		    0
		  (1+ freepascal-last-word-numb)))
	  (setq freepascal-last-word-shown (elt allcomp freepascal-last-word-numb))
	  ;; Display next match or same string if no match was found
	  (if (not (null allcomp))
	      (insert "" freepascal-last-word-shown)
	    (insert "" freepascal-str)
	    (message "(No match)")))
      ;; The other form of completion does not necessarly do that.

      ;; Insert match if found, or the original string if no match
      (if (or (null match) (equal match 't))
	  (progn (insert "" freepascal-str)
		 (message "(No match)"))
	(insert "" match))
      ;; Give message about current status of completion
      (cond ((equal match 't)
	     (if (not (null (cdr allcomp)))
		 (message "(Complete but not unique)")
	       (message "(Sole completion)")))
	    ;; Display buffer if the current completion didn't help 
	    ;; on completing the label.
	    ((and (not (null (cdr allcomp))) (= (length freepascal-str)
						(length match)))
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list allcomp))
	     ;; Wait for a keypress. Then delete *Completion*  window
	     (momentary-string-display "" (point))
	     (delete-window (get-buffer-window (get-buffer "*Completions*")))
	     )))))

(defun freepascal-show-completions ()
  "Show all possible completions at current point."
  (interactive)
  (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_") (point)))
	 (e (save-excursion (skip-chars-forward "a-zA-Z0-9_") (point)))
	 (freepascal-str (buffer-substring b e))
	 ;; The following variable is used in freepascal-completion
	 (freepascal-buffer-to-use (current-buffer))
	 (allcomp (if (and freepascal-toggle-completions
			   (string= freepascal-last-word-shown freepascal-str))
		      freepascal-last-completions
		    (all-completions freepascal-str 'freepascal-completion))))
    ;; Show possible completions in a temporary buffer.
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list allcomp))
    ;; Wait for a keypress. Then delete *Completion*  window
    (momentary-string-display "" (point))
    (delete-window (get-buffer-window (get-buffer "*Completions*")))))


(defun freepascal-get-default-symbol ()
  "Return symbol around current point as a string."
  (save-excursion
    (buffer-substring (progn
			(skip-chars-backward " \t")
			(skip-chars-backward "a-zA-Z0-9_")
			(point))
		      (progn
			(skip-chars-forward "a-zA-Z0-9_")
			(point)))))

(defun freepascal-build-defun-re (str &optional arg)
  "Return function/procedure starting with STR as regular expression.
With optional second arg non-nil, STR is the complete name of the instruction."
  (if arg
      (concat "^\\(function\\|procedure\\)[ \t]+\\(" str "\\)\\>")
    (concat "^\\(function\\|procedure\\)[ \t]+\\(" str "[a-zA-Z0-9_]*\\)\\>")))

;; Function passed to completing-read, try-completion or
;; all-completions to get completion on any function name. If
;; predicate is non-nil, it must be a function to be called for every
;; match to check if this should really be a match. If flag is t, the
;; function returns a list of all possible completions. If it is nil
;; it returns a string, the longest possible completion, or t if STR
;; is an exact match. If flag is 'lambda, the function returns t if
;; STR is an exact match, nil otherwise.

(defun freepascal-comp-defun (freepascal-str freepascal-pred freepascal-flag)
  (save-excursion
    (let ((freepascal-all nil)
	  match)

      ;; Set buffer to use for searching labels. This should be set
      ;; within functins which use freepascal-completions
      (set-buffer freepascal-buffer-to-use)

      (let ((freepascal-str freepascal-str))
	;; Build regular expression for functions
	(if (string= freepascal-str "")
	    (setq freepascal-str (freepascal-build-defun-re "[a-zA-Z_]"))
	  (setq freepascal-str (freepascal-build-defun-re freepascal-str)))
	(goto-char (point-min))
	
	;; Build a list of all possible completions
	(while (re-search-forward freepascal-str nil t)
	  (setq match (buffer-substring (match-beginning 2) (match-end 2)))
	  (if (or (null freepascal-pred)
		  (funcall freepascal-pred match))
	      (setq freepascal-all (cons match freepascal-all)))))

      ;; Now we have built a list of all matches. Give response to caller
      (freepascal-completion-response))))

(defun freepascal-goto-defun ()
  "Move to specified Free Pascal function/procedure.
The default is a name found in the buffer around point."
  (interactive)
  (let* ((default (freepascal-get-default-symbol))
	 ;; The following variable is used in freepascal-comp-function
	 (freepascal-buffer-to-use (current-buffer))
	 (default (if (freepascal-comp-defun default nil 'lambda)
		      default ""))
	 (label (if (not (string= default ""))
		    ;; Do completion with default
		    (completing-read (concat "Label: (default " default ") ")
				     'freepascal-comp-defun nil t "")
		  ;; There is no default value. Complete without it
		  (completing-read "Label: "
				   'freepascal-comp-defun nil t ""))))
    ;; If there was no response on prompt, use default value
    (if (string= label "")
	(setq label default))
    ;; Goto right place in buffer if label is not an empty string
    (or (string= label "")
	(progn
	  (goto-char (point-min))
	  (re-search-forward (freepascal-build-defun-re label t))
	  (beginning-of-line)))))



;;;
;;; Free Pascal-outline-mode
;;;
(defvar freepascal-outline-map nil "Keymap used in Free Pascal Outline mode.")

(if freepascal-outline-map
    nil
  (if (fboundp 'set-keymap-name)
      (set-keymap-name freepascal-outline-map 'freepascal-outline-map))
  (if (not (fboundp 'set-keymap-parent))
      (setq freepascal-outline-map (copy-keymap freepascal-mode-map))
    (setq freepascal-outline-map (make-sparse-keymap))
    (set-keymap-parent freepascal-outline-map freepascal-mode-map))
  (define-key freepascal-outline-map "\M-\C-a"  'freepascal-outline-prev-defun)
  (define-key freepascal-outline-map "\M-\C-e"  'freepascal-outline-next-defun)
  (define-key freepascal-outline-map "\C-c\C-d" 'freepascal-outline-goto-defun)
  (define-key freepascal-outline-map "\C-c\C-s" 'freepascal-show-all)
  (define-key freepascal-outline-map "\C-c\C-h" 'freepascal-hide-other-defuns))

(defvar freepascal-outline-mode nil "Non-nil while using Free Pascal Outline mode.")
(make-variable-buffer-local 'freepascal-outline-mode)
(set-default 'freepascal-outline-mode nil)
(if (not (assoc 'freepascal-outline-mode minor-mode-alist))
    (setq minor-mode-alist (append minor-mode-alist
				   (list '(freepascal-outline-mode " Outl")))))

(defun freepascal-outline (&optional arg)
  "Outline-line minor mode for Free Pascal mode.
When in Free Pascal Outline mode, portions
of the text being edited may be made invisible. \\<freepascal-outline-map>

Free Pascal Outline mode provides some additional commands.

\\[freepascal-outline-prev-defun]\
\t- Move to previous function/procedure, hiding everything else.
\\[freepascal-outline-next-defun]\
\t- Move to next function/procedure, hiding everything else.
\\[freepascal-outline-goto-defun]\
\t- Goto function/procedure prompted for in minibuffer,
\t  hide all other functions.
\\[freepascal-show-all]\t- Show the whole buffer.
\\[freepascal-hide-other-defuns]\
\t- Hide everything but the current function (function under the cursor).
\\[freepascal-outline]\t- Leave freepascal-outline-mode."
  (interactive "P")
  (setq freepascal-outline-mode
	(if (null arg) (not freepascal-outline-mode) t))
  (if (fboundp 'redraw-mode-line)
      (redraw-mode-line))
  (if freepascal-outline-mode
      (progn
	(setq selective-display t)
	(use-local-map freepascal-outline-map))
    (progn
      (setq selective-display nil)
      (freepascal-show-all)
      (use-local-map freepascal-mode-map))))

(defun freepascal-outline-change (b e freepascal-flag)
  (let ((modp (buffer-modified-p)))
    (unwind-protect
	(subst-char-in-region b e (if (= freepascal-flag ?\n) 
				      ?\^M ?\n) freepascal-flag)
      (set-buffer-modified-p modp))))

(defun freepascal-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (freepascal-outline-change (point-min) (point-max) ?\n))

(defun freepascal-hide-other-defuns ()
  "Show only the current defun."
  (interactive)
  (save-excursion
    (let ((beg (progn (if (not (looking-at "\\(function\\|procedure\\)\\>"))
			  (freepascal-beg-of-defun))
		      (point)))
	  (end (progn (freepascal-end-of-defun)
		      (backward-sexp 1)
		      (search-forward "\n\\|\^M" nil t)
		      (point)))
	  (opoint (point-min)))
      (goto-char (point-min))

      ;; Hide all functions before current function
      (while (re-search-forward "^\\(function\\|procedure\\)\\>" beg 'move)
	(freepascal-outline-change opoint (1- (match-beginning 0)) ?\^M)
	(setq opoint (point))
	;; Functions may be nested
	(if (> (progn (freepascal-end-of-defun) (point)) beg)
	    (goto-char opoint)))
      (if (> beg opoint)
	  (freepascal-outline-change opoint (1- beg) ?\^M))

      ;; Show current function
      (freepascal-outline-change beg end ?\n)
      ;; Hide nested functions
      (forward-char 1)
      (while (re-search-forward "^\\(function\\|procedure\\)\\>" end 'move)
	(setq opoint (point))
	(freepascal-end-of-defun)
	(freepascal-outline-change opoint (point) ?\^M))

      (goto-char end)
      (setq opoint end)

      ;; Hide all function after current function
      (while (re-search-forward "^\\(function\\|procedure\\)\\>" nil 'move)
	(freepascal-outline-change opoint (1- (match-beginning 0)) ?\^M)
	(setq opoint (point))
	(freepascal-end-of-defun))
      (freepascal-outline-change opoint (point-max) ?\^M)

      ;; Hide main program
      (if (< (progn (forward-line -1) (point)) end)
	  (progn
	    (goto-char beg)
	    (freepascal-end-of-defun)
	    (backward-sexp 1)
	    (freepascal-outline-change (point) (point-max) ?\^M))))))

(defun freepascal-outline-next-defun ()
  "Move to next function/procedure, hiding all others."
  (interactive)
  (freepascal-end-of-defun)
  (freepascal-hide-other-defuns))

(defun freepascal-outline-prev-defun ()
  "Move to previous function/procedure, hiding all others."
  (interactive)
  (freepascal-beg-of-defun)
  (freepascal-hide-other-defuns))

(defun freepascal-outline-goto-defun ()
  "Move to specified function/procedure, hiding all others."
  (interactive)
  (freepascal-goto-defun)
  (freepascal-hide-other-defuns))

;;; Tags for inserting freepascal language constructs 
;;; code mainly copied from html-helper-mode

(defvar freepascal-use-expert-menu nil
  "*If not nil, then use the full freepascal menu.")

;; control over what types of tags to load. By default, we load all the
;; ones we know of.

(defvar freepascal-types-to-install
  '(control types  misc)
  "*List of tag types to install when html-helper-mode is first loaded.
If you want to not install some type of tag, override this variable.
Order is significant: menus go in this order.")

(require 'tempo)			;essential part of html-helper-mode
(condition-case nil			;menu support, standard in emacs19
    (require 'auc-menu)			;add-on for XEmacs. *why* does this
  (error (require 'easymenu)))		;package have to have two names?

(defvar freepascal-mode-menu nil
  "Menu for freepascal Clobbered and rebuilt by `html-helper-install-menu'")

(defconst freepascal-type-alist nil
  "Alist: type of tag -> keymap, keybinding, menu, menu string.
Add to this with `freepascal-add-type-to-alist'.")

;;{{{ accessor functions for freepascal-type-alist

(defun freepascal-keymap-for (type)
  "Accessor function for alist: for type, return keymap or nil"
  (nth 0 (cdr-safe (assq type freepascal-type-alist))))

(defun freepascal-key-for (type)
  "Accessor function for alist: for type, return keybinding or nil"
  (nth 1 (cdr-safe (assq type freepascal-type-alist))))

(defun freepascal-menu-for (type)
  "Accessor function for alist: for type, return menu or nil"
  (nth 2 (cdr-safe (assq type freepascal-type-alist))))

(defun freepascal-menu-string-for (type)
  "Accessor function for alist: for type, return menustring or nil"
  (nth 3 (cdr-safe (assq type freepascal-type-alist))))

(defun freepascal-normalized-menu-for (type)
  "Helper function for building menus from submenus: add on string to menu."
  (cons (freepascal-menu-string-for type)
	(eval (freepascal-menu-for type))))

;;}}}

(defun freepascal-add-type-to-alist (type)
  "Add a type specification to the alist.
The spec goes (type . (keymap-symbol keyprefix menu-symbol menu-string)).
See code for an example."
  (setq freepascal-type-alist (cons type freepascal-type-alist)))

;; Here are the types provided by freepascal-mode.
(mapc 'freepascal-add-type-to-alist
      '((control . (freepascal-control-map "\C-c\C-s" freepascal-control-menu "Insert Control Statements"))
	(types   . (freepascal-types-map "\C-c\C-t" freepascal-types-menu "Insert Type Definitions"))
	(misc    . (freepascal-misc-map "\C-c\C-m"  freepascal-misc-menu "Insert Other Elements"))
	))

;; Once freepascal-mode is aware of a type, it can then install the
;; type: arrange for keybindings, menus, etc.

(defconst freepascal-installed-types nil
  "The types that have been installed (used when building menus).
There is no support for removing a type once it has been installed.")

(defun freepascal-install-type (type)
  "Install a new tag type: add it to the keymap, menu structures, etc.
For this to work, the type must first have been added to the list of types
with freepascal-add-type-to-alist."
  (setq freepascal-installed-types (cons type freepascal-installed-types))
  (let ((keymap (freepascal-keymap-for type))
	(key (freepascal-key-for type))
	(menu (freepascal-menu-for type))
	(menu-string (freepascal-menu-string-for type)))
    (and key
	 (progn
	   (set keymap nil)
	   (define-prefix-command keymap)
	   (define-key freepascal-mode-map key keymap)))
    (and menu
	 (progn
	   (set menu nil)))))

;; install the default types.
(mapc 'freepascal-install-type freepascal-types-to-install)

;; special mode keys
(mapc
 (function (lambda (l) (define-key freepascal-mode-map (car l) (nth 1 l))))
 '(("?" tempo-backward-mark)
   ("\M-\t"   tempo-complete-tag)))

;;{{{ freepascal-add-tag function for building basic tags

(defvar freepascal-tempo-tags nil
  "List of tags used in completion.")

;; this while loop is awfully Cish
;; isn't there an emacs lisp function to do this?
(defun freepascal-string-to-symbol (input-string)
  "Given a string, downcase it and replace spaces with -.
We use this to turn menu entries into good symbols for functions.
It's not entirely successful, but fortunately emacs lisp is forgiving."
  (let* ((s (copy-sequence input-string))
	 (l (1- (length s))))
    (while (> l 0)
      (if (char-equal (aref s l) ?\ )
	  (aset s l ?\-))
      (setq l (1- l)))
    (concat "freepascal-" (downcase s))))


(defun freepascal-add-tag (l)
  "Add a new tag to freepascal-mode.
Builds a tempo-template for the tag and puts it into the
appropriate keymap if a key is requested. Format:
`(freepascal-add-tag '(type keybinding completion-tag menu-name template doc)'"
  (let* ((type (car l))
	 (keymap (freepascal-keymap-for type))
	 (menu (freepascal-menu-for type))
	 (key (nth 1 l))
	 (completer (nth 2 l))
	 (name (nth 3 l))
	 (tag (nth 4 l))
	 (doc (nth 5 l))
	 (command (tempo-define-template (freepascal-string-to-symbol name)
					 tag completer doc
					 'freepascal-tempo-tags)))

    (if (null (memq type freepascal-installed-types))    ;type loaded?
	t                                                 ;no, do nothing.
      (if (stringp key)			                  ;bind key somewhere?
	  (if keymap			                  ;special keymap?
	      (define-key (eval keymap) key command)      ;t:   bind to prefix
	    (define-key freepascal-mode-map key command));nil: bind to global
	t)
      (if menu				                  ;is there a menu?
	  (set menu			                  ;good, cons it in
	       (cons (vector name command t) (eval menu))))
      )))

;; for backwards compatability
(fset 'freepascal-add-cookie 'freepascal-add-tag)

;;}}}

;;{{{ Free Pascal templates

(mapc
 'freepascal-add-tag
 '(
   ;;control statements
   ;; compound statement 
   (control "b" "begin" "block" ('> "begin" (freepascal-reindent-line) 'n> 'r 
				    'n "end;" (freepascal-reindent-line) 'n>)) 
   ;; if's without elses
   (control  "i"   "ifthen" "if"      ('> "if " 'p " then" 'n> 'p))
   (control  "I"   "ifbegin" "if b e" ('> "if " 'p " then" 'n>  "begin"
					  (freepascal-reindent-line) 'n> 'p 'n
					  "end;" (freepascal-reindent-line) 
					  'n>))
   ;; if's with elses
   (control  "j"   "ife1"  "if else" ('> "if " 'p " then" 'n> 'p 'n
					 "else" (freepascal-reindent-line) 'n>
					 'p))
   (control  "J"   "ife2"  "if b e b" ('> "if " 'p " then" 'n "begin"
					  (freepascal-reindent-line) 'n> 'p 'n
					  "end" (freepascal-reindent-line) 'n>
					  "else" 'n "begin"
					  (freepascal-reindent-line) 'n> 'p 'n
					  "end;" (freepascal-reindent-line) 'n>))
   (control  "h"   "ife3"  "if b else" ('> "if " 'p " then" 'n
					   "begin" (freepascal-reindent-line)
					   'n> 'p 'n
					   "end" (freepascal-reindent-line) 'n
					   "else" (freepascal-reindent-line) 
					   'n> 'p))
   (control  "H"   "ife4"  "if else b" ('> "if " 'p " then" 'n> 'p 'n
					   "else" (freepascal-reindent-line) 
					   'n> "begin"
					   (freepascal-reindent-line) 'n> 'p 
					   'n "end;" 
					   (freepascal-reindent-line) 'n>))
   ;; Loops and with
   (control "r" "repeat" "repeat" ('> "repeat" 'n> 'r 'n "until " 'p ";"
				      (freepascal-reindent-line)))
   (control "w" "while"  "while" ('> "while " 'p " do" 'n> 'p))
   (control "W" "with"   "with" ('> "with " 'p " do" 'n> 'p))
   (control "f" "for"    "for"  ('> "for " 'p " := " 'p " to " 'p " do" 'n>))
   (control "F" "dfor"   "for downto"  ('> "for " 'p " := " 'p " downto " 
					   'p " do" 'n>))
   ;; try statements

   (control "t" "tryexcept" "try except" ('> "try" 'n> 'r 'n "except"
					     (freepascal-reindent-line) 'n> 'p 'n
					     "end;" (freepascal-reindent-line) 'n))
   (control "T" "tryfinally" "try finally" ('> "try" 'n> 'r "finally"
					       (freepascal-reindent-line) 'n> 'p 'n
					       "end;" (freepascal-reindent-line) 'n))

   (types     "a"   "array"   "array" ("array[" 'p "] of " 'p ";"))
   (types     "A"   "parray"   "array (packed)" ("packed array[" 'p "] of " 
						 'p ";"))
   (types     "c"   "class" "class" ("class(" 'p ")" 'n> 'p 'n "end;"
				     (freepascal-reindent-line)))
   (types     "d"   "double" "double" ("double"))
   (types     "i"   "integer"   "integer" ("integer"))
   (types     "s"   "string"   "string" ("string"))
   (types     "S"   "set"   "set" ("set of"))
   (types     "r"   "record"    "record" ("record" '> 'n> 'p 'n> "end;" 
					  (freepascal-reindent-line)))
   (types     "R"   "precord"    "record (packed)" ("packed record" '> 'n> 'p
						    'n> "end;" 
						    (freepascal-reindent-line)))
   (types     "v"   "variant"   "variant" ("variant"))
   (misc  "c"  "Comment"          "Comment"  ("{ " 'p " }"))

   ))

(provide 'freepascal)



;;; freepascal.el ends here

