;-*- coding: utf-8 -*-
;; lookup-word-on-internet.el -- helpful commands for looking up the internet

;; Copyright © 2011 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )

;; You can redistribute this program and/or modify it. Please give credit and link. Thanks.

;;; DESCRIPTION

;; this package provides convenient commands for looking up the web.
;; The commands are:

;; lookup-word-on-internet
;; lookup-google
;; lookup-wikipedia
;; lookup-word-dict-org
;; lookup-word-definition
;; lookup-answers.com
;; lookup-wiktionary

;;; REQUIREMENT

;; You need to have 2 elisp util 〔xeu_elisp_util.el〕 and 〔xfrp_find_replace_pairs.el〕, available at
;; http://code.google.com/p/ergoemacs/source/browse/trunk/packages/xeu_elisp_util.el
;; http://code.google.com/p/ergoemacs/source/browse/trunk/packages/xfrp_find_replace_pairs.el

;; here's linux shell command to download them:
;; wget http://ergoemacs.googlecode.com/svn/trunk/packages/xeu_elisp_util.el
;; wget http://ergoemacs.googlecode.com/svn/trunk/packages/xfrp_find_replace_pairs.el

;;; INSTALL

;; To install, place this file 〔lookup-word-on-internet.el〕 in the directory 〔~/.emacs.d/〕.

;; also place the files 〔xeu_elisp_util.el〕 and 〔xfrp_find_replace_pairs.el〕 in 〔~/.emacs.d/〕, if you haven't done already.

;; Then, place the following code in your emacs init file

;; (autoload 'lookup-google "lookup-word-on-internet" "Lookup in browser" t)
;; (autoload 'lookup-wikipedia "lookup-word-on-internet" "Lookup in browser" t)
;; (autoload 'lookup-word-dict-org "lookup-word-on-internet" "Lookup in browser" t)
;; (autoload 'lookup-word-definition "lookup-word-on-internet" "Lookup in browser" t)
;; (autoload 'lookup-wiktionary "lookup-word-on-internet" "Lookup word in browser" t)

;; ;; Suggested keys
;; (global-set-key (kbd "<f1> 1") 'lookup-google)
;; (global-set-key (kbd "<f1> 2") 'lookup-wikipedia)
;; (global-set-key (kbd "<f1> 3") 'lookup-word-definition)
;; …

;;; DOCUMENTATION

;; Just some simple useful commands
;; For detail, see http://ergoemacs.org/emacs/emacs_lookup_ref.html

;; donate $3 please. Paypal to xah@xahlee.org , thanks.

;;; HISTORY

;; version 1.5, 2013-04-21 removed lookup-php-ref. Doesn't belong here.
;; version 1.4, 2013-03-23 added 2 more dict to the all-dictionaries. Good for vocabulary researchers
;; version 1.3, 2012-05-11 added “lookup-all-dictionaries”.
;; version 1.2, 2012-05-10 added “lookup-answers.com”. Improved inline docs.
;; version 1.1, 2012-05-09 changed the input from 「'symbol」 to 「'word」. Changed the English dictionary used from 「http://www.answers.com/main/ntquery?s=�」 to 「http://www.thefreedictionary.com/�」.
;; version 1.0, 2011-11-14 First released to public.

;;; Code:


(require 'xeu_elisp_util)

(defvar all-dictionaries nil "A vector of dictionaries. Used by `lookup-all-dictionaries'. http://wordyenglish.com/words/dictionary_tools.html ")
(setq all-dictionaries [
"http://www.dict.org/bin/Dict?Form=Dict2&Database=*&Query=�" ; 1913 Webster, WordNet
"http://www.thefreedictionary.com/�"                         ; AHD
"http://www.answers.com/main/ntquery?s=�"                    ; AHD
"http://en.wiktionary.org/wiki/�"
"http://www.google.com/search?q=define:+�" ; google
"http://www.etymonline.com/index.php?search=�" ; etymology
] )

(defun lookup-word-on-internet (&optional input-word site-to-use)
  "Look up current word or text selection in a online reference site.
This command launches/switches you to default browser.

Optional argument INPUT-WORD and SITE-TO-USE can be given.
SITE-TO-USE a is URL string in this form: 「http://en.wiktionary.org/wiki/�」.
the 「�」 is a placeholder for the query string.

If SITE-TO-USE is nil, Google Search is used.

For a list of online reference sites, see:
 URL `http://ergoemacs.org/emacs/emacs_lookup_ref.html'"
  (interactive)
  (let (ξword refUrl myUrl)
    (setq ξword
          (if input-word
              input-word
            (if (region-active-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
              (thing-at-point 'word) )) )

    (setq ξword (replace-regexp-in-string " " "%20" (asciify-text ξword)))

    (setq refUrl
          (if site-to-use
              site-to-use
            "http://www.google.com/search?q=�" ) )

    (setq myUrl (replace-regexp-in-string "�" ξword refUrl t t))
    (cond
     ((string-equal system-type "windows-nt") ; any flavor of Windows
      (browse-url-default-windows-browser myUrl)
      )
     ((string-equal system-type "gnu/linux")
      (browse-url myUrl)
      )
     ((string-equal system-type "darwin") ; Mac
      (browse-url myUrl) ) ) ))

(defun lookup-google (&optional input-word)
  "Lookup current word or text selection in Google Search.
See also `lookup-word-on-internet'."
  (interactive)
  (let ((dictUrl "http://www.google.com/search?q=�" ))
    (lookup-word-on-internet input-word dictUrl) ) )

(defun lookup-wikipedia (&optional input-word)
  "Lookup current word or text selection in Wikipedia.
See also `lookup-word-on-internet'."
  (interactive)
  (let ((dictUrl "http://en.wikipedia.org/wiki/�" ))
    (lookup-word-on-internet input-word dictUrl) ) )

(defun lookup-word-dict-org (&optional input-word)
  "Lookup definition of current word or text selection in URL `http://dict.org/'.
See also `lookup-word-on-internet'."
  (interactive)
  (let ((dictUrl "http://www.dict.org/bin/Dict?Form=Dict2&Database=*&Query=�" ))
    (lookup-word-on-internet input-word dictUrl)
    ) )

(defun lookup-word-definition (&optional input-word)
  "Lookup definition of current word or text selection in URL `http://thefreedictionary.com/'.
See also `lookup-word-on-internet'."
  (interactive)
  (let ((dictUrl "http://www.thefreedictionary.com/�") )
    (lookup-word-on-internet input-word dictUrl) ) )

(defun lookup-answers.com (&optional input-word)
  "Lookup current word or text selection in URL `http://answers.com/'.
See also `lookup-word-on-internet'."
  (interactive)
  (let ((dictUrl "http://www.answers.com/main/ntquery?s=�"
) )
    (lookup-word-on-internet input-word dictUrl) ) )

(defun lookup-wiktionary (&optional input-word)
  "Lookup definition of current word or text selection in URL `http://en.wiktionary.org/'
See also `lookup-word-on-internet'."
  (interactive)
  (let ((dictUrl "http://en.wiktionary.org/wiki/�" ))
    (lookup-word-on-internet input-word dictUrl) ) )

(defun lookup-all-dictionaries (&optional input-word)
  "Lookup definition in many dictionaries.
Current word or text selection is used as input.
The dictionaries used are in `all-dictionaries'.

See also `lookup-word-on-internet'."
  (interactive)
  (mapc (lambda (dictUrl) (lookup-word-on-internet input-word dictUrl)) all-dictionaries) )

(provide 'lookup-word-on-internet)
