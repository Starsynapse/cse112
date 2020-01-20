#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr
#lang racket
;; $Id: sbi.scm,v 1.19 2020-01-14 16:58:47-08 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))
(define *arg-list* (vector->list (current-command-line-arguments)))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))


;;(define (write-program-by-line filename program)
;;  (printf "==================================================~n")
;;  (printf "~a: ~a~n" *run-file* filename)
;;  (printf "==================================================~n")
;;  (printf "(~n")
;;  (for-each (lambda (line) (printf "~a~n" line)) program)
;;  (printf ")~n"))

;;(define (save-program filename program)
;;    (for-each (lambda (line) (for-each (lambda (word) (printf "~a~n" word)) line)) program))


;; Traverses the 'code' in a given line of 'code' and returns it as a list
(define (program-iterator construct)
    (cond
    ((null? (cdr construct))
        (printf "~a (end)~n" (car construct))
        (cons (car construct) null))
    (else
        (printf "~a~n" (car construct))
        (cons (car construct)
            (program-iterator (cdr construct))))))

;; Traverses the lines of 'code' in a given file and returns them as a list
(define (save-program program)
    (cond 
    ((not (null? (car (car program))))
        (if (not (null? (cdr program))) 
            (cons (program-iterator (car program)) (save-program (cdr program)))
            (cons (program-iterator (car program)) null)))
    (else
        (printf "bye!"))))

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (save-program program))))

(if (terminal-port? *stdin*)
    (main *arg-list*)
    (printf "sbi.scm: interactive mode~n"))

