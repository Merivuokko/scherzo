%% A replacement init.ly for exporting LilyPond internal music structure as an s-expression
%% Copyright 2024 Aura Kelloniemi
%% This file is licensed under the terms of the GNU General Public License version 3.

#(begin
  (use-modules (ice-9 ftw))
  (load (string-append
         (dirname (current-filename))
         "/sexp-export.scm")))

#(define sexp-export-port
  (let ((option (ly:get-option 'sexp-export-file)))
   (if (not option)
    (current-output-port)
    (open-output-file (symbol->string option)))))

#(define (default-toplevel-book-handler book)
  (export-sexp book sexp-export-port)
  #f)

%% Include original init file
$(ly:parser-include-string
  (format #f "\\include \"~a\""
   (string-append (ly:effective-prefix) "/ly/init.ly")))
