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


;; (depricated) Traverses the 'code' in a given line of 'code' and returns it as a list
;;(define (program-iterator construct)
;;    (cond
;;    ((pair? (car construct))
;;        (printf "~a (pair)~n" (car construct))
;;        (save-program construct))
;;    ((null? (cdr construct))
;;        (printf "~a (end)~n" (car construct))
;;        (cons (car construct) null))
;;    (else
;;        (printf "~a~n" (car construct))
;;        (cons (car construct)
;;            (program-iterator (cdr construct))))))

;; (depricated) Traverses the lines of 'code' in a given file and returns them as a list
;;(define (save-program program)
;;    (cond 
;;    ((not (null? (car (car program))))
;;        (if (not (null? (cdr program)))
;;            (cons (program-iterator (car program)) (save-program (cdr program)))
;;            (cons (program-iterator (car program)) null)))))

;; Create *label-table*
(define *label-table* (make-hash))

;; Creates and entry in the *label-table* if it finds a label
(define (top-traversal-iterator list-construct top-node)
    (cond 
        ((and (not (null? (cdr list-construct)))
              (not (null? (cdr (cdr list-construct)))) 
              (null? (cdr (cdr (cdr list-construct)))))
            ;;(printf "~a (retreived)~n" (car (cdr list-construct)))
            (hash-set! *label-table* (car (cdr list-construct)) top-node))
        ((and (not (null? (cdr list-construct)))
              (null? (cdr (cdr list-construct)))
              (not (pair? (car (cdr list-construct)))))
            ;;(printf "~a (retreived2)~n" (car (cdr list-construct)))
            (hash-set! *label-table* (car (cdr list-construct)) top-node))))

;; Scans the top level lines of the program
(define (top-traversal program-list)
    (cond ((not (null? (car (car program-list))))
        (cond 
            ((not (null? (cdr program-list)))
                ;;(printf "~a (test)~n" (cdr program-list))
                (top-traversal-iterator (car program-list) program-list)
                (top-traversal (cdr program-list)))
            (else
                ;;(printf "~a (test)~n" (car program-list))
                (top-traversal-iterator (car program-list) program-list))))))

;; Create *function-table*
(define *function-table* (make-hash))

(define (interpret-print print-statement)
    ;;(printf "~a (print-start)~n" print-statement)
    (cond
    ((pair? (car (cdr print-statement)))
        (printf "~a (p-pair)~n" (program-iterator (cdr print-statement))))
    ((null? (cdr (cdr print-statement)))
        (printf "~a (p-null)~n" (car (cdr print-statement))))
    (else
        (printf "~a" (car (cdr print-statement)))
        (interpret-print (cdr print-statement)))))

(define (interpret-+ equation)
    (cond
    ((and (pair? (car (cdr equation))) (pair? (cdr (cdr equation))))
        (printf "~a (+-pair1)~n" (cdr equation))
        (+ (program-iterator (cdr equation))
        (interpret-+ (cdr equation))))
    ((pair? (car (cdr equation)))
        (printf "~a (+-pair2)~n" (cdr equation))
        (program-iterator (cdr equation)))
    ((null? (cdr (cdr equation)))
        (printf "~a (+-null)~n" (car (cdr equation)))
        (car (cdr equation)))
    (else
        (+ (car (cdr equation)) (interpret-+ (cdr equation))))
    ))

(for-each
    (lambda (pair)
        (hash-set! *function-table* (car pair) (cadr pair)))
    `(
        (print  ,interpret-print)
        (+      ,interpret-+)
        ;(-)
        ;(*)
        ;(/)
    ))

;; (depricated) Traverses the 'code' in a given line of 'code'
(define (program-iterator construct)
    (cond
    ((pair? (car construct))
        ;;(printf "~a (pair)~n" (car construct))
        ((hash-ref *function-table* (car (car construct))) (car construct)))
    ((null? (cdr construct))
        ;;(printf "~a (end)~n" (car construct))
        (cdr construct))
    (else
        ;;(printf "~a~n" (car construct))
        (program-iterator (cdr construct)))))

;; (depricated) Traverses the lines of 'code' in a given file
(define (interpret-program program)
    (cond 
    ((not (null? (car (car program))))
        (cond 
        ((not (null? (cdr program)))
            (program-iterator (car program))
            (interpret-program (cdr program)))
        (else
            (program-iterator (car program)))))))

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (top-traversal program)
              (interpret-program program))))

(if (terminal-port? *stdin*)
    (main *arg-list*)
    (printf "sbi.scm: interactive mode~n"))

