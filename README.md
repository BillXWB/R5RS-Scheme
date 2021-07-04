# R5RS Scheme Interpreter

对着 Write Yourself a Scheme in 48 Hours ~~抄写~~实现了一遍代码。不懂 Lisp，不懂 Haskell，教程说啥就写啥:zipper_mouth_face:. 练习题和后面是没有关联的，比如有次让支持 float，后来的正文就没有管，默认全当 number 处理了。

教程链接：

- [Wikibooks](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours "Wikibooks")

- [大佬翻译的中文版](https://www.jianshu.com/p/b80a06bfd3a7 "简书")

## Usage

GHC 8.10.4，依赖 parsec-3.1.14.0.

- 解释文件：

  ```
  $ ./main <source-file> [<arg>...]
  ```

  可选的若干 `<arg>` 参数会事先绑定到名为 `args` 的 string list 上，而后解释执行 `<source-file>`.

- 交互执行：

  ```
  $ ./main
  Lisp>>> (load "src/stdlib.scm")
  "Loaded the Standard Library."
  Lisp>>> quit
  ```

## Demo

学 [Haskell 官网](https://www.haskell.org/ "Haskell")整了个 [Erastosthenes 筛](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes "Wikipedia")：

```scheme
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
```

```
$ ./main demo.scm
"input a number n:"
25
"prime number(s) in [2, n]:"
(2 3 5 7 11 13 17 19 23)
```
