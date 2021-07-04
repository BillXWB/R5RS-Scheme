(load "src/stdlib.scm")

(define (range beg end)
    (if (> beg end)
        '()
        (unfold (curry + 1) beg (curry <= end))
    )
)

(define (sieve d lst)
    (filter
        (lambda (n) (/= 0 (mod n d)))
        lst
    )
)

(define (erastosthenes-sieve n)
    (define (implement rest)
        (if (null? rest)
            '()
            (cons
                (car rest)
                (implement (sieve (car rest) (cdr rest))))
        )
    )
    (implement (range 2 n))
)

(define (input hint)
    (write hint)
    (read)
)

(define n (input "input a number n:"))
(write "prime number(s) in [2, n]:")
(erastosthenes-sieve n)
