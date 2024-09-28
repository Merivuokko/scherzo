; This file provides routines for exporting LilyPond's internal
; representation as Sexp data structure.
;
; This code is originally based on the xml-export.ily file from the python-ly
; package (https://github.com/frescobaldi/python-ly/) written by Wilbert
; Berendsen.

(use-modules (ice-9 pretty-print))

(define (map-alist proc alist)
  "Map over values of an alist"
  (map (lambda (pair)
         (cons (car pair) (proc (cdr pair))))
       alist))

(define (filter-duplicates-alist . alists)
  "Filter out non-unique elements from an alist"
  (define (do alist seen)
    (cond
      ((null? alist) '())
      ((member (caar alist) seen)
       (do (cdr alist) seen))
      (else
       (cons (car alist)
             (do (cdr alist) (cons (caar alist) seen))))))
  (do (apply append alists) '()))

(define (module->sexp mod)
  "Export most contents of a module (required by LilyPond headers)"
  (filter-map (lambda (var)
                (if (eq? (car var) '%module-public-interface)
                    #f
                    (ly->sexp var)))
              (ly:module->alist mod)))

(define (ly->sexp obj)
  "Recursively convert any LilyPond object to an s-expression"
   (cond
    ((ly:book? obj)
     (cons 'book
      `((header . ,(ly->sexp (ly:book-header obj)))
        (book-parts . ,(map ly->sexp (reverse (ly:book-book-parts obj))))
        (scores . ,(map ly->sexp (reverse (ly:book-scores obj)))))))
    ((ly:duration? obj)
     (cons 'duration
       `((log . ,(ly:duration-log obj))
         (dots . ,(ly:duration-dot-count obj))
         (scaling-factor . ,(ly:duration-factor obj)))))
    ((ly:input-location? obj)
     (cons 'origin (ly:input-file-line-char-column obj)))
    ((ly:moment? obj)
     (cons 'moment
      `((main-numerator . ,(ly:moment-main-numerator obj))
        (main-denominator . ,(ly:moment-main-denominator obj))
        (grace-numerator . ,(ly:moment-grace-numerator obj))
        (grace-denominator . ,(ly:moment-grace-denominator obj)))))
    ((ly:music? obj)
     (let* ((prop-names '(name element elements pitch duration articulations tweaks origin))
            (immutable-props
             (map (lambda (key) (cons key (ly:music-property obj key))) prop-names))
            (mutable-props (ly:music-mutable-properties obj))
            (unique-props (filter-duplicates-alist immutable-props mutable-props))
            (filtered-props
             (filter-map
              (lambda (v)
                (if (null? (cdr v))
                    #f
                    (cons (car v) (ly->sexp (cdr v)))))
              unique-props)))
      (cons 'music filtered-props)))
    ((ly:pitch? obj)
     (cons 'pitch
       `((octave . ,(ly:pitch-octave obj))
         (note-name . ,(ly:pitch-notename obj))
         (alteration . ,(ly:pitch-alteration obj)))))
    ((ly:score? obj)
     (cons 'score
      `((header . ,(ly->sexp (ly:score-header obj)))
        (music . ,(ly->sexp (ly:score-music obj))))))
    ((ly:stencil? obj)
     (cons 'stencil
      `((x-min . ,(car (ly:stencil-extent obj X)))
        (x-max . ,(cdr (ly:stencil-extent obj X)))
        (y-min . ,(car (ly:stencil-extent obj Y)))
        (y-max . ,(cdr (ly:stencil-extent obj Y)))
        (expr . ,(ly->sexp (ly:stencil-expr obj))))))
    ((module? obj)
     (module->sexp obj))
    ((pair? obj)
     (cons (ly->sexp (car obj)) (ly->sexp (cdr obj))))
    ((procedure? obj)
     (let* ((name (procedure-name obj))
            (attrs (if name `((name . ,name)) '())))
      (cons 'procedure attrs)))
    (else obj)))

(define (export-sexp expr port)
  "Export LilyPond's internal music representation to an s-expression and pretty-print it as a string to a given port."
  (pretty-print (ly->sexp expr) port))
