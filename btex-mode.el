;;; btex-mode.el --- Major mode for btex (bananaspace tex) -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Ren Qingyu
;;
;; Author: Ren Qingyu <renqy2002@qq.com>
;; Maintainer: Ren Qingyu <renqy2002@qq.com>
;; Created: Oct 22, 2022
;; Modified: Oct 24, 2022
;; Version: 0.1.0
;; Keywords: languages
;; Homepage: https://github.com/rqy2002/btex-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:

;; A small package for btex (https://github.com/banana-space/btex),
;; containing:
;;   language highlight
;;   preview using btex-server
;;; Code:

(defconst btex-package-directory (file-name-directory load-file-name))
(defconst btex-resources-directory (concat btex-package-directory "/resources"))

;; syntax table

(defgroup btex nil
  "Group for Btex."
  :prefix "btex-"
  :group 'text)

(defvar btex-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?% "<" st)
    (modify-syntax-entry ?\n ">" st)
    st))

;; regular expressions

(defconst btex-header1 "^==\\s-+\\(?1:.*\\)\\s-+==\\s-*$" "Btex header 1.")
(defconst btex-header2 "^===\\s-+\\(?1:.*\\)\\s-+===\\s-*$" "Btex header 2.")
(defconst btex-header3 "^====\\s-+\\(?1:.*\\)\\s-+====\\s-*$" "Btex header 3.")
(defconst btex-escape  "\\\\@*[a-zA-Z]+\\|\\\\[^@a-zA-Z]" "Btex escape sequence.")
(defconst btex-inline-math
  "\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\\\\\)*\\$[^$]\\(?1:.*?\\)[^\\\\]\\(?:\\\\\\\\\\)*\\$"
  "Btex inline math.")
(defconst btex-display-math
  "\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\\\\\)*\\\\\\[\\(?1:\\(?:.\\|\n\\)*?\\)[^\\\\]\\(?:\\\\\\\\\\)*\\\\\\]"
  "Btex display math.")
(defconst btex-display-math2
  "\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\\\\\)*\\$\\$\\(?1:\\(?:.\\|\n\\)*?\\)[^\\\\]\\(?:\\\\\\\\\\)*\\$\\$"
  "Btex display math, another version.")

(defun btex-search-in-command (command &optional limit argnum)
  "Search forward the COMMAND, set point to the end, and return point.
COMMAND can be a regexp which matchs the command name without backslash;
and it cannot include regexp group.
If the brace is not closed, return nil.
If LIMIT is non-nil, it is a buffer position that bounds the search.
If ARGNUM is non-nil, match so many args.
Set the `match-data' to:
group 1, command escape; group 2 ~ ARGNUM+1, args."
  (let ((begin-marker (point-marker))
        (regexp (concat "\\(?:^\\|[^\n\\]\\)\\(?:\\\\\\\\\\)*\\(?1:\\\\"
                        command "\\)")))
    (if (not (re-search-forward regexp limit t)) nil
      (let ((pre-match-begin (car (match-data)))
            (pre-match-rest (cdr (cdr (match-data))))
            (content-begin (point-marker))
            (argnum (or argnum 1))
            (las (copy-marker (+ (point) 1)))
            nex
            (res ()))
        (while (and (> argnum 0)
                    (looking-at "[ \t]*{")
                    (setq nex (ignore-errors (scan-lists (point) 1 0)))
                    (<= nex limit))
          (setq argnum (- argnum 1))
          (push las res)
          (push (copy-marker (- nex 1)) res)
          (setq las (copy-marker (+ nex 1)))
          (goto-char nex))
        (if (> argnum 0) (progn (goto-char begin-marker) nil)
          (set-match-data
           (append (list pre-match-begin nex)
                   pre-match-rest
                   (nreverse res)))
          nex)))))

(defun btex-command-matcher (command &optional argnum)
  "Return a match function for btex command COMMAND with ARGNUM args.
For more information, see `btex-search-in-command'"
  (lambda (&optional limit) (btex-search-in-command command limit argnum)))

(fset 'btex-textbf (btex-command-matcher "textbf"))
(fset 'btex-textit (btex-command-matcher "textit"))
(fset 'btex-section (btex-command-matcher "section"))
(fset 'btex-subsection (btex-command-matcher "subsection"))
(fset 'btex-subsubsection (btex-command-matcher "subsubsection"))
(fset 'btex-url (btex-command-matcher "url"))
(fset 'btex-href (btex-command-matcher "href"))
(fset 'btex-begin (btex-command-matcher "begin"))
(fset 'btex-end (btex-command-matcher "end"))
(fset 'btex-code (btex-command-matcher "code\\(?:\\*\\|\\[.*\\]\\)?"))
(fset 'btex-codeblock (btex-command-matcher "codeblock"))
(fset 'btex-textcolor (btex-command-matcher "textcolor" 2))

;;; font-lock

(require 'font-lock)

(defgroup btex-faces nil
  "Faces used in Btex Mode"
  :group 'faces
  :group 'btex)

(defface btex-italic-face
  '((t (:inherit italic)))
  "Face for italic text."
  :group 'btex-faces)

(defface btex-bold-face
  '((t (:inherit bold)))
  "Face for bold text."
  :group 'btex-faces)

(defface btex-escape-face
  '((t (:foreground "#f87000")))
  "Face for esacpe sequences."
  :group 'btex-faces)

(defface btex-header1-face
  '((t (:inherit bold :height 1.5)))
  "Face for headers of level 1."
  :group 'btex-faces)

(defface btex-header2-face
  '((t (:inherit bold :height 1.3)))
  "Face for headers of level 2."
  :group 'btex-faces)

(defface btex-header3-face
  '((t (:inherit bold :height 1.0)))
  "Face for headers of level 3."
  :group 'btex-faces)

(defface btex-env-name-face
  '((t (:foreground "#70a000")))
  "Face for enviorment names."
  :group 'btex-faces)

(defface btex-link-face
  '((t (:underline t :foreground "#1088c8")))
  "Face for links."
  :group 'btex-faces)

(defface btex-math-face
  '((t (:foreground "SkyBlue4")))
  "Face for math."
  :group 'btex-faces)

(defface btex-code-face
  '((t (:foreground "#666666")))
  "Face for code."
  :group 'btex-faces)

(defun btex-colorize-match ()
  "Colorize the matched string with COLOR.
If MATCH is non-nil, colorize the corresponding subexp instead."
  (let ((color (match-string-no-properties 2)))
    (if (not (color-defined-p color)) (setq color (concat "#" color)))
    (if (color-defined-p color)
        (put-text-property
         (match-beginning 3) (match-end 3)
         'face `((:foreground ,color)))
      nil)))

(defconst btex-font-lock-keywords
  `((btex-textbf 2 'btex-bold-face append)
    (btex-textit 2 'btex-italic-face append)
    (btex-textcolor 3 (btex-colorize-match))
    (btex-section 2 'btex-header1-face)
    (btex-subsection 2 'btex-header2-face)
    (btex-subsubsection 2 'btex-header3-face)
    (btex-url 2 'btex-link-face)
    (btex-href 2 'btex-link-face)
    (btex-begin 2 'btex-env-name-face)
    (btex-end 2 'btex-env-name-face)
    (,btex-header1 1 'btex-header1-face)
    (,btex-header2 1 'btex-header2-face)
    (,btex-header3 1 'btex-header3-face)
    (,btex-escape 0 'btex-escape-face t)
    (,btex-inline-math 1 'btex-math-face append)
    (,btex-display-math 1 'btex-math-face append)
    (,btex-display-math2 1 'btex-math-face append)
    (btex-code 2 'btex-code-face t)
    (btex-codeblock 2 'btex-code-face t)))

;;; Btex render

(require 'json)

(defcustom btex-url "http://127.0.0.1"
  "Btex running url."
  :type 'string
  :group 'btex)

(defcustom btex-port 7200
  "Btex running port."
  :type 'number
  :group 'btex)

(defcustom btex-view-buffer "*btex-view*"
  "Btex view buffer name."
  :type 'string
  :group 'btex)

(defconst btex-banana-css-path (expand-file-name "banana.css" btex-resources-directory))
(defconst btex-katex-css-path (expand-file-name "katex/katex.min.css" btex-resources-directory))

(defun btex-html (output warnings errors)
  "Btex html from OUTPUT, WARNINGS and ERRORS."
  (let ((output output))
  (concat
   "<!DOCTYPE html>\n"
   "<head>\n"
   "<link rel='stylesheet' href='file://" btex-banana-css-path "' />\n"
   "<link rel='stylesheet' href='file://" btex-katex-css-path "' />\n"
   "</head>\n"
   "<body>\n"
   "<div class='b-page-body'>\n"
   output
   "</div>\n"
   "</body>\n")))

(defun btex-display-after-render (status)
  "Display btex content after render with STATUS."
  (let ((err (plist-get status :error)))
    (if err (princ (format "Btex server error `%s'" (car (cdr err))))
      (let* ((content0 (buffer-substring-no-properties url-http-end-of-headers (point-max)))
             (content (json-read-from-string (decode-coding-string content0 'utf-8)))
             (output (cdr (assoc 'html content)))
             (warnings (cdr (assoc 'warnings content)))
             (errors (cdr (assoc 'errors content))))
        (kill-buffer)
        (let ((html (btex-html output warnings errors)))
          (with-current-buffer (get-buffer-create btex-view-buffer)
            (erase-buffer)
            (insert html)
            (html-mode))
          (browse-url-of-buffer btex-view-buffer))))))

(defun btex-render ()
  "Render btex."
  (interactive)
  (let ((url (concat btex-url ":" (number-to-string btex-port)))
        (url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")))
        (url-request-data
         (encode-coding-string
          (json-encode `((code . ,(buffer-substring-no-properties (point-min) (point-max)))))
          'utf-8)))
    (url-retrieve url 'btex-display-after-render)))

;;;###autoload
(define-derived-mode btex-mode text-mode "Btex"
  "Major mode for btex."
  :syntax-table btex-syntax-table
  (setq font-lock-defaults
        '(btex-font-lock-keywords
          nil nil nil nil
          (font-lock-multiline . t)
          (font-lock-extra-managed-props
           . (composition display invisible rear-nonsticky keymap help-echo mouse-face))))
  (setq-local comment-start "%")
  (setq-local comment-start-skip "%[ \t]+"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.btex" . btex-mode))

(provide 'btex-mode)
;;; btex-mode.el ends here
