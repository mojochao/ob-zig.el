;;; ob-zig.el --- Org Babel functions for Zig evaluation  -*- lexical-binding: t; -*-

;; Copyright (C) Joel Boehland

;; Author: Joel Boehland
;; Maintainer: Allen Gooch <allen.gooch@gmail.com>
;; Keywords: languages, literate programming, reproducible research
;; Homepage: https://github.com/mojochao/ob-zig.el
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.1"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org Babel support for evaluating Zig code with `zig run' and `zig test'.
;; Requires Zig 0.16 or later on `exec-path'.
;;
;; A block without a `pub fn main' is wrapped in
;; `pub fn main(init: std.process.Init) !void { ... }', so the body can
;; use `init' and in particular `init.io' to write to stdout.
;;
;; Header arguments: :imports, :c-includes, :c-defines, :main, :flags,
;; :cmdline, :libs and :testsuite.  See `org-babel-header-args:zig'.

;;; Code:
(require 'ob)
(require 'ob-eval)
(require 'org-macs)
(require 'seq)

(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))

(add-to-list 'org-babel-tangle-lang-exts '("zig" . "zig"))

(defvar org-babel-default-header-args:zig '())

(defconst org-babel-header-args:zig '((imports    . :any)
                                      (c-includes . :any)
                                      (c-defines  . :any)
                                      (main       . :any)
                                      (flags      . :any)
                                      (cmdline    . :any)
                                      (libs       . :any)
                                      (testsuite  . ((yes no))))
  "Zig-specific header arguments.")

(defcustom org-babel-zig-compiler "zig"
  "Command used to run Zig source, a name on variable `exec-path' or a path."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-zig-integer-type "isize"
  "Zig type used for integer variables."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-zig-floating-point-type "f64"
  "Zig type used for floating-point variables."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-zig-string-type "[]const u8"
  "Zig type used for string variables."
  :group 'org-babel
  :type 'string)

(defun org-babel-zig--words (value)
  "Return header argument VALUE as a list of strings."
  (let ((v (org-babel-read value nil)))
    (mapcar (lambda (x) (format "%s" x))
            (if (stringp v) (split-string v) v))))

(defun org-babel-expand-body:zig (body params)
  "Expand BODY according to PARAMS, returning the Zig source to compile."
  (let ((vars (org-babel--get-vars params))
        (colnames (cdr (assq :colname-names params)))
        (main-p (not (string= (cdr (assq :main params)) "no")))
        (testsuite (string= (cdr (assq :testsuite params)) "yes"))
        (imports (org-babel-zig--words (cdr (assq :imports params))))
        (c-includes (org-babel-zig--words (cdr (assq :c-includes params))))
        (c-defines (org-babel-zig--words (cdr (assq :c-defines params)))))
    (mapconcat
     #'identity
     (delq nil
           (list
            (mapconcat (lambda (m) (format "const %s = @import(\"%s\");" m m))
                       imports "\n")
            (when (or c-includes c-defines)
              (org-babel-zig--c-import c-includes c-defines))
            (mapconcat #'org-babel-zig-var-to-zig-source vars "\n")
            (when colnames (org-babel-zig-utility-header-to-zig))
            (mapconcat (lambda (head)
                         (org-babel-zig-header-to-zig
                          head (cdr (assoc (car head) vars))))
                       colnames "\n")
            (if (and main-p (not testsuite))
                (org-babel-zig-ensure-main-wrap body)
              body)))
     "\n")))

(defun org-babel-zig--c-import (includes defines)
  "Return a `@cImport' block binding INCLUDES and DEFINES to `c'.
DEFINES is a flat list of NAME VALUE words; a trailing NAME has no value."
  (concat "const c = @cImport({\n"
          (mapconcat (lambda (h) (format "    @cInclude(%S);\n" h)) includes "")
          (mapconcat (lambda (d)
                       (format "    @cDefine(%S, %s);\n" (car d)
                               (if (cdr d) (format "%S" (cadr d)) "{}")))
                     (seq-partition defines 2) "")
          "});"))

(defun org-babel-zig-ensure-main-wrap (body)
  "Wrap BODY in a \"main\" function unless it already defines one.
The wrapper's `init' parameter, a `std.process.Init', is available to BODY."
  (if (string-match "^[ \t]*pub[ \t]+fn[ \t]+main[ \t]*(" body)
      body
    (format "pub fn main(init: @import(\"std\").process.Init) !void {
    _ = &init;
%s
}
" body)))

(defun org-babel-execute:zig (body params)
  "Execute BODY, a block of Zig code, with Babel according to PARAMS.
This function is called by `org-babel-execute-src-block'."
  (message "executing Zig source code block")
  (let* ((testsuite (string= (cdr (assq :testsuite params)) "yes"))
         (cmdline (cdr (assq :cmdline params)))
         (flags (org-babel-zig--words (cdr (assq :flags params))))
         (libs (org-babel-zig--words (or (cdr (assq :libs params))
                                         (org-entry-get nil "libs" t))))
         (tmp-src-file (org-babel-temp-file "Zig-src-" ".zig"))
         (command
          (mapconcat
           #'identity
           (append (list org-babel-zig-compiler (if testsuite "test" "run"))
                   flags
                   libs
                   ;; @cImport needs libc.
                   (when (assq :c-includes params) (list "-lc"))
                   (list (org-babel-process-file-name tmp-src-file))
                   (when (and cmdline (not testsuite)) (list "--" cmdline)))
           " ")))
    (with-temp-file tmp-src-file
      (insert (org-babel-expand-body:zig body params)))
    (let ((results (org-babel-eval command "")))
      (when results
        (setq results (org-remove-indentation results))
        (org-babel-reassemble-table
         (org-babel-result-cond (cdr (assq :result-params params))
           (org-babel-read results t)
           (let ((tmp-file (org-babel-temp-file "zig-")))
             (with-temp-file tmp-file (insert results))
             (org-babel-import-elisp-from-file tmp-file)))
         (org-babel-pick-name
          (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
         (org-babel-pick-name
          (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))))))

(defun org-babel-prep-session:zig (_session _params)
  "Signal an error: Zig is a compiled language with no session support."
  (error "Zig is a compiled language -- no support for sessions"))

;; helper functions

(defun org-babel-zig-val-to-base-type (val)
  "Return the base type of VAL as a symbol.
The result is `integerp' if VAL and all its elements are integers,
`floatp' if they are integers or floats, and `stringp' otherwise."
  (cond
   ((integerp val) 'integerp)
   ((floatp val) 'floatp)
   ((or (listp val) (vectorp val))
    (let ((type nil))
      (mapc (lambda (v)
              (pcase (org-babel-zig-val-to-base-type v)
                ('stringp (setq type 'stringp))
                ('floatp (when (memq type '(nil integerp)) (setq type 'floatp)))
                ('integerp (unless type (setq type 'integerp)))))
            val)
      type))
   (t 'stringp)))

(defun org-babel-zig--zig-type (base-type)
  "Return the Zig type used for values of BASE-TYPE."
  (pcase base-type
    ('integerp org-babel-zig-integer-type)
    ('floatp org-babel-zig-floating-point-type)
    ('stringp org-babel-zig-string-type)
    (_ (error "Unknown type %S" base-type))))

(defun org-babel-zig--string-literal (string)
  "Return STRING as a Zig string literal."
  (concat "\""
          (replace-regexp-in-string
           "[\\\"\n\t\r]"
           (lambda (m)
             (pcase m ("\n" "\\n") ("\t" "\\t") ("\r" "\\r") (_ (concat "\\" m))))
           string t t)
          "\""))

(defun org-babel-zig--literal (val base-type)
  "Return VAL as a Zig literal of BASE-TYPE."
  (if (eq base-type 'stringp)
      (org-babel-zig--string-literal (format "%s" val))
    (format "%S" val)))

(defun org-babel-zig-var-to-zig-source (pair)
  "Return Zig source declaring the variable in PAIR, a (NAME . VALUE) cons."
  (let* ((name (car pair))
         (val (cdr pair))
         (val (if (symbolp val) (symbol-name val) val))
         (base (org-babel-zig-val-to-base-type val))
         (type (org-babel-zig--zig-type base))
         (lit (lambda (v) (org-babel-zig--literal v base))))
    (cond
     ((and (listp val) (listp (car val)))
      (let ((cols (length (car val))))
        (format "const %s = [%d][%d]%s{\n%s};"
                name (length val) cols type
                (mapconcat (lambda (row)
                             (format "    [%d]%s{%s},\n" cols type
                                     (mapconcat lit row ",")))
                           val ""))))
     ((or (listp val) (vectorp val))
      (format "const %s = [%d]%s{%s};" name (length val) type
              (mapconcat lit val ",")))
     (t (format "const %s: %s = %s;" name type (funcall lit val))))))

(defun org-babel-zig-utility-header-to-zig ()
  "Return the Zig helper that maps a column name to its index."
  "
fn get_column_idx(header: []const []const u8, column: []const u8) usize {
    for (header, 0..) |col, i| {
        if (@import(\"std\").mem.eql(u8, col, column)) return i;
    }
    @panic(\"unknown column\");
}
")

(defun org-babel-zig-header-to-zig (head table)
  "Return Zig source for the column names in HEAD, a (NAME . COLUMNS) cons.
TABLE is the value of the table variable named by HEAD."
  (let ((name (car head))
        (type (org-babel-zig--zig-type (org-babel-zig-val-to-base-type table))))
    (format "const %s_header = [%d][]const u8{%s};
fn %s_h(row: usize, col: []const u8) %s {
    return %s[row][get_column_idx(&%s_header, col)];
}
"
            name (length (cdr head))
            (mapconcat (lambda (h) (org-babel-zig--string-literal (format "%s" h)))
                       (cdr head) ",")
            name type name name)))

(provide 'ob-zig)
;;; ob-zig.el ends here
