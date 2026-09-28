;;; test-ob-zig.el --- Tests for ob-zig.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Self-contained ERT suite; no org-test dependency.  Run with:
;;
;;   emacs -Q --batch -L . -l test-ob-zig.el -f ert-run-tests-batch-and-exit
;;
;; Execution tests are skipped when `org-babel-zig-compiler' is not found.

;;; Code:

(require 'ert)
(require 'bytecomp)
(require 'org)
(require 'ob-zig)

(defun ob-zig-test--run (src &optional header-args preamble)
  "Execute SRC as a zig block with HEADER-ARGS after PREAMBLE.
Return a plist with :result, the :commands Babel ran, and any
:errors as (EXIT-CODE . STDERR) pairs."
  (unless (executable-find org-babel-zig-compiler)
    (ert-skip "zig compiler not found"))
  (let* ((commands nil)
         (errors nil)
         (org-confirm-babel-evaluate nil)
         (record-cmd (lambda (cmd &rest _) (push cmd commands)))
         (record-err (lambda (code stderr) (push (cons code stderr) errors))))
    (advice-add 'org-babel-eval :before record-cmd)
    (advice-add 'org-babel-eval-error-notify :override record-err)
    (unwind-protect
        (with-temp-buffer
          (insert (or preamble "")
                  "\n#+begin_src zig :results silent " (or header-args "")
                  "\n" src "\n#+end_src\n")
          (org-mode)
          (goto-char (point-min))
          (re-search-forward "^#\\+begin_src")
          (list :result (org-babel-execute-src-block)
                :commands (nreverse commands)
                :errors (nreverse errors)))
      (advice-remove 'org-babel-eval record-cmd)
      (advice-remove 'org-babel-eval-error-notify record-err))))

(defun ob-zig-test--exit (run)
  "Exit code of the first failing command in RUN, or 0."
  (or (car (car (plist-get run :errors))) 0))

(defun ob-zig-test--print (fmt args)
  "Zig statements printing FMT with ARGS to stdout via the wrapper's `init'.
Assumes `std' is imported at file scope."
  (format "var buf: [512]u8 = undefined;
var w = std.Io.File.stdout().writer(init.io, &buf);
try w.interface.print(\"%s\", .{%s});
try w.interface.flush();" fmt args))

(defconst ob-zig-test--table-2col
  "#+name: t\n| A | B |\n|---+---|\n| 1 | 2 |\n| 3 | 4 |\n")

(defconst ob-zig-test--table-3col
  "#+name: t\n| A | B | C |\n|---+---+---|\n| 1 | 2 | 3 |\n| 4 | 5 | 6 |\n")

;;; main wrapper

(ert-deftest ob-zig/wrapped-body-gets-init ()
  "A body without main can reach stdout through `init', with no :imports."
  (let ((run (ob-zig-test--run
              "const zstd = @import(\"std\");
var buf: [64]u8 = undefined;
var w = zstd.Io.File.stdout().writer(init.io, &buf);
try w.interface.print(\"hello\", .{});
try w.interface.flush();")))
    (should (equal "hello" (plist-get run :result)))))

(ert-deftest ob-zig/wrapped-body-may-ignore-init ()
  "A body that never touches `init' still compiles."
  (let ((run (ob-zig-test--run "std.debug.print(\"x\", .{});" ":imports '(std)")))
    (should (= 0 (ob-zig-test--exit run)))))

(ert-deftest ob-zig/main-detection-ignores-comments ()
  (let ((run (ob-zig-test--run
              (concat "// pub fn main() lives elsewhere\n"
                      (ob-zig-test--print "hello" ""))
              ":imports '(std)")))
    (should (equal "hello" (plist-get run :result)))))

(ert-deftest ob-zig/explicit-main-is-not-wrapped ()
  (let ((run (ob-zig-test--run
              (format "pub fn main(init: std.process.Init) !void {\n%s\n}"
                      (ob-zig-test--print "hello" ""))
              ":imports '(std)")))
    (should (equal "hello" (plist-get run :result)))))

(ert-deftest ob-zig/main-no-disables-wrapping ()
  (let ((run (ob-zig-test--run
              (format "pub fn main(init: std.process.Init) !void {\n%s\n}"
                      (ob-zig-test--print "mainno" ""))
              ":imports '(std) :main no")))
    (should (equal "mainno" (plist-get run :result)))))

;;; header arguments

(ert-deftest ob-zig/flags-reach-the-compiler ()
  (let ((run (ob-zig-test--run
              (ob-zig-test--print "{s}" "@tagName(@import(\"builtin\").mode)")
              ":imports '(std) :flags -OReleaseFast")))
    (should (equal "ReleaseFast" (plist-get run :result)))))

(ert-deftest ob-zig/cmdline-reaches-the-program ()
  (let ((run (ob-zig-test--run
              (concat "const args = try init.minimal.args.toSlice(init.arena.allocator());\n"
                      (ob-zig-test--print "{d}" "args.len"))
              ":imports '(std) :cmdline a b")))
    (should (equal 3 (plist-get run :result)))))

(ert-deftest ob-zig/libs-reach-the-command-line ()
  (let ((run (ob-zig-test--run (ob-zig-test--print "ok" "") ":imports '(std) :libs -lc")))
    (should (equal "ok" (plist-get run :result)))
    (should (string-match-p " -lc .*\\.zig" (car (plist-get run :commands))))))

(ert-deftest ob-zig/imports-string-form ()
  (let ((run (ob-zig-test--run (ob-zig-test--print "ok" "") ":imports std")))
    (should (equal "ok" (plist-get run :result)))))

(ert-deftest ob-zig/c-includes-and-defines ()
  "C headers and defines are exposed through a `c' namespace."
  (let ((run (ob-zig-test--run
              "_ = c.printf(\"%d\", @as(c_int, c.FOO));"
              ":c-includes stdio.h :c-defines FOO 7")))
    (should (equal 7 (plist-get run :result)))))

(ert-deftest ob-zig/testsuite-runs-zig-test ()
  (let ((run (ob-zig-test--run
              "test \"t\" {
    var buf: [64]u8 = undefined;
    var w = std.Io.File.stdout().writer(std.testing.io, &buf);
    try w.interface.print(\"hello\", .{});
    try w.interface.flush();
}"
              ":imports '(std) :testsuite yes")))
    (should (equal "hello" (plist-get run :result)))
    (should (string-match-p "\\`zig test " (car (plist-get run :commands))))))

(ert-deftest ob-zig/session-is-rejected ()
  (should-error (org-babel-prep-session:zig "s" nil)))

;;; variables

(ert-deftest ob-zig/integer-vars ()
  (let ((run (ob-zig-test--run (ob-zig-test--print "{d}" "p + q")
                               ":imports '(std) :var p=10 :var q=12")))
    (should (equal 22 (plist-get run :result)))))

(ert-deftest ob-zig/scalar-var-is-comptime-known ()
  "A scalar :var can be used where Zig needs a comptime value."
  (let ((run (ob-zig-test--run
              (concat "const arr: [@intCast(n)]u8 = undefined;\n"
                      (ob-zig-test--print "{d}" "arr.len"))
              ":imports '(std) :var n=4")))
    (should (equal 4 (plist-get run :result)))))

(ert-deftest ob-zig/float-var-precision ()
  (let ((run (ob-zig-test--run (ob-zig-test--print "{d}" "f")
                               ":imports '(std) :var f=0.0000001")))
    (should (equal 1e-7 (plist-get run :result)))))

(ert-deftest ob-zig/string-var-with-quotes ()
  (let ((run (ob-zig-test--run (ob-zig-test--print "{s}" "q")
                               ":imports '(std) :var q=\"say \\\"hi\\\"\"")))
    (should (equal "say \"hi\"" (plist-get run :result)))))

(ert-deftest ob-zig/string-var-escapes-backslash-and-newline ()
  ;; (string 34 92 10) is: double quote, backslash, newline.
  (let ((run (ob-zig-test--run (ob-zig-test--print "{d}" "s.len")
                               ":imports '(std) :var s=(string 34 92 10)")))
    (should (equal 3 (plist-get run :result)))))

(ert-deftest ob-zig/single-char-symbol-is-a-string ()
  (let ((run (ob-zig-test--run (ob-zig-test--print "{s}" "c")
                               ":imports '(std) :var c='a")))
    (should (equal "a" (plist-get run :result)))))

(ert-deftest ob-zig/list-var ()
  (let ((run (ob-zig-test--run (ob-zig-test--print "{s}{s}{d}" "a[0], a[1], a.len")
                               ":imports '(std) :var a='(\"abc\" \"def\")")))
    (should (equal "abcdef2" (plist-get run :result)))))

;;; tables

(ert-deftest ob-zig/two-column-int-table-roundtrip ()
  (let ((run (ob-zig-test--run
              "var buf: [512]u8 = undefined;
var w = std.Io.File.stdout().writer(init.io, &buf);
for (t) |row| {
    for (row) |f| try w.interface.print(\"{d} \", .{f});
    try w.interface.print(\"\\n\", .{});
}
try w.interface.print(\"A1 {d}\\nB0 {d}\\n\", .{ t_h(1, \"A\"), t_h(0, \"B\") });
try w.interface.flush();"
              ":imports '(std) :var t=t"
              ob-zig-test--table-2col)))
    (should (equal '((1 2) (3 4) ("A1" 3) ("B0" 2)) (plist-get run :result)))))

(ert-deftest ob-zig/wide-table-column-helper ()
  (let ((run (ob-zig-test--run (ob-zig-test--print "{d}" "t_h(1, \"C\")")
                               ":imports '(std) :var t=t"
                               ob-zig-test--table-3col)))
    (should (equal 6 (plist-get run :result)))))

(ert-deftest ob-zig/table-helper-without-std-import ()
  (let ((run (ob-zig-test--run
              "const zstd = @import(\"std\");
var buf: [64]u8 = undefined;
var w = zstd.Io.File.stdout().writer(init.io, &buf);
try w.interface.print(\"{d}\", .{t_h(1, \"A\")});
try w.interface.flush();"
              ":var t=t"
              ob-zig-test--table-2col)))
    (should (equal 3 (plist-get run :result)))))

(ert-deftest ob-zig/numeric-header-cells ()
  (let ((run (ob-zig-test--run (ob-zig-test--print "{d}" "t_h(0, \"2024\")")
                               ":imports '(std) :var t=t"
                               "#+name: t\n| 2023 | 2024 |\n|------+------|\n| 1 | 2 |\n")))
    (should (equal 2 (plist-get run :result)))))

(ert-deftest ob-zig/unknown-column-fails-loudly ()
  (let ((run (ob-zig-test--run (ob-zig-test--print "{d}" "t_h(0, \"Z\")")
                               ":imports '(std) :var t=t"
                               ob-zig-test--table-2col)))
    (should-not (eql 0 (ob-zig-test--exit run)))
    (should (string-match-p "unknown column" (cdr (car (plist-get run :errors)))))))

;;; hygiene

(ert-deftest ob-zig/byte-compiles-without-warnings ()
  (let* ((src (file-name-with-extension (locate-library "ob-zig") "el"))
         (elc (make-temp-file "ob-zig" nil ".elc"))
         (byte-compile-warnings t)
         (byte-compile-dest-file-function (lambda (_) elc)))
    (when (get-buffer byte-compile-log-buffer)
      (kill-buffer byte-compile-log-buffer))
    (unwind-protect
        (progn
          (should (byte-compile-file src))
          (should-not
           (and (get-buffer byte-compile-log-buffer)
                (with-current-buffer byte-compile-log-buffer
                  (string-match-p "Warning\\|Error" (buffer-string))))))
      (delete-file elc))))

(provide 'test-ob-zig)
;;; test-ob-zig.el ends here
