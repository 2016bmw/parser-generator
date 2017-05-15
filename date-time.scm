;;;; Example: Dates and Times

(load "primitives")

(define (leap-year? year)
  (define (divisible-by? n)
    (zero? (remainder year n)))
  (and (divisible-by? 4)
       (or (not (divisible-by? 100))
           (divisible-by? 400))))

(define (days-in-month month year)
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    ((2) (if (leap-year? year) 29 28))
    (else (error "invalid month" month))))

(define (valid-date? parse-tree)
  (let* ((data (parse-tree-data parse-tree))
	 (year (car data))
	 (month (cadr data))
	 (day (caddr data)))
    (and (<= 1 month 12)
         (<= 1 day (days-in-month month year)))))

(define (valid-time? parse-tree)
  (let* ((data (parse-tree-data parse-tree))
	(hour (car data))
        (minute (cadr data))
        (second (caddr data)))
    (and (<= 0 hour 23)
         (<= 0 minute 59)
         (<= 0 second 59))))

;; Matches ISO date/times
(define date-time-grammar
  (letrec* ((digits (lambda (digits)
                      (p:transform
                       (lambda (parse-tree)
                         (string->number (parse-tree-text parse-tree)))
                       (p:repeat (p:char-predicate char-numeric?) digits digits))))
            (date (p:rule 'date
                          (p:require valid-date?
                                     (p:separated-by (p:string "-") #f
                                                     (digits 4) (digits 2) (digits 2)))))
            (time (p:rule 'time
                          (p:require valid-time?
                                     (p:separated-by (p:string ":") #f
                                                     (digits 2) (digits 2) (digits 2)))))
            (date-time (p:rule 'date-time
                               (p:separated-by (p:string "T") #f
                                               date time))))
    date-time))

;;; Tests

;; UNIX Epoch
(date-time-grammar "1970-01-01T00:00:00"
                   (lambda (date-time num-consumed)
                     (parse-tree-data date-time)))
;; ;Value: (date-time (date 1970 1 1) (time 0 0 0))

;; Invalid month
(date-time-grammar "2001-13-01T00:00:00"
                   (lambda (date-time num-consumed)
                     (parse-tree-data date-time)))
;; ;Value: #f


;; Invalid day
(date-time-grammar "2001-02-30T00:00:00"
                   (lambda (date-time num-consumed)
                     (parse-tree-data date-time)))
;; ;Value: #f

;; Leap year
(date-time-grammar "2004-02-29T00:00:00"
                   (lambda (date-time num-consumed)
                     (parse-tree-data date-time)))
;; ;Value: (date-time (date 2004 2 29) (time 0 0 0))

;; Invalid times
(date-time-grammar "2000-01-01T15:14:70"
                   (lambda (date-time num-consumed)
                     (prase-tree-data date-time)))
;; ;Value: #f
