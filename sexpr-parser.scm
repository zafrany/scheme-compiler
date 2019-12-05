(load "pc.scm")

(define <whitespace>
     (const 
          (lambda (ch)
              (<= (char->integer ch) 32)))
)

(define <end-of-line-comment>
    (new 
      (*parser <whitespace>) *star
      (*parser (char #\;))
      (*parser <any-char>)
      (*parser (char #\newline)) 
      *diff *star
      (*caten 2)
      (*parser <end-of-input>)
      (*parser (char #\newline))
      (*disj 2)
      (*caten 2)
      (*caten 2)
      done))

(define <sexpr-comment> 
    (new
        (*parser (word "#;"))
        (*delayed (lambda () <sexpr>))
        (*caten 2)
        (*pack-with (lambda (a b)  b))
        done))

(define <skippable>
  (new
    (*parser <whitespace>)
    (*parser <end-of-line-comment>) 
    (*parser <sexpr-comment>)
    (*disj 3) *star
    done))

(define <boolean>
  (new 
    (*parser <skippable>)
    (*parser (word-ci "#t"))
    (*pack (lambda(_) #t))
    (*parser (word-ci "#f"))
    (*pack (lambda(_) #f))
    (*disj 2)
    (*parser <skippable>)
    (*caten 3)
    (*pack-with (lambda(a b c) b))
    done))

(define <meta-char>
  (lambda (str ch)
    (new (*parser (word-ci str))
   (*pack (lambda (_) ch))
   done)))

(define <named-char>
  (new 
       (*parser (<meta-char> "space" #\space))
       (*parser (<meta-char> "nul" (integer->char 0)))
       (*parser (<meta-char> "newline" #\newline))
       (*parser (<meta-char> "return" #\return))
       (*parser (<meta-char> "tab" #\tab))
       (*parser (<meta-char> "page" #\page)) ; formfeed
       (*parser (<meta-char> "lambda" (integer->char 955)))
       (*disj 7)
       done))

(define <visiable-simple-char>
  (new
     (*parser <any-char>)
     (*parser <whitespace>) *diff 
     done))

(define <hex-unicode-char>
  (new 
    (*parser (char #\x))
    (*delayed (lambda () <XXXX>))
    (*delayed (lambda () <XX>))
    (*delayed (lambda () <hex-digit>))
    (*disj 3)
    (*caten 2)
    (*pack-with (lambda (a b) (integer->char b)))
    done
    ))

(define <char>
 (new
    (*parser <skippable>)
    (*parser (word "#\\"))
    (*parser <named-char>)
    (*parser <hex-unicode-char>)
    (*parser <visiable-simple-char>)
    (*disj 3)
    (*caten 2)
    (*parser <skippable>)
    (*caten 3)
    (*pack-with (lambda(a b c) (cadr b)))
    done
  )) 

(define <digit-1-9>
     (range #\1 #\9))

(define <digit-0-9>
     (range #\0 #\9))

(define <nat-number>
  (new 
       (*parser <digit-0-9>) *plus
       (*pack (lambda (a)
          (string->number (list->string a))))
       done)
  )

(define <integer>
  (new (*parser (char #\+))
       (*parser <nat-number>)
       (*caten 2)
       (*pack-with
          (lambda (++ n) n))

       (*parser (char #\-))
       (*parser <nat-number>)
       (*caten 2)
       (*pack-with
          (lambda (-- n) (- n)))

       (*parser <nat-number>)
       (*disj 3)
       done))

(define <fraction>
  (new (*parser <integer>)
       (*parser (char #\/))
       (*parser <nat-number>)
       (*only-if (lambda (n) (not (zero? n))))
       (*caten 3)
       (*pack-with
          (lambda (num div den)
            (/ num den)))
       done))

(define <positive-number>
  (new
    (*parser <nat-number>)
    (*parser (char #\/))
    *not-followed-by
    (*parser <fraction>)
    (*disj 2)
    done
  ))

(define <number>
  (new
    (*parser <skippable>)
    
    (*parser <integer>)
    
    (*parser (char #\/))
    *not-followed-by
    (*parser <fraction>)

    (*disj 2)

    (*parser <skippable>)
    (*caten 3)

    (*pack-with (lambda(a b c) b))
    done
  ))

(define <string-visiable-char>
  (new
  (*parser <any-char>)
  (*parser (char #\"))
   *diff
   done
  ))

(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
         (*pack (lambda (_) ch))
   done)))

(define <string-meta-char>
  (new (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page)) ; formfeed
       (*parser (^<meta-char> "\\{lambda}" (integer->char 955)))
       ;(*parser (^<meta-char> "\\{alef}" (integer->char 1488)))
       ;(*parser (^<meta-char> "\\{bismillah}" (integer->char 65021)))
       ;(*parser (^<meta-char> "\\{smiley}" (integer->char 9786)))
       (*disj 7)
       done))

(define <hex-digit>
  (let ((zero (char->integer #\0))
  (lc-a (char->integer #\a))
  (uc-a (char->integer #\A)))
    (new (*parser (range #\0 #\9))
   (*pack
    (lambda (ch)
      (- (char->integer ch) zero)))

   (*parser (range #\a #\f))
   (*pack
    (lambda (ch)
      (+ 10 (- (char->integer ch) lc-a))))

   (*parser (range #\A #\F))
   (*pack
    (lambda (ch)
      (+ 10 (- (char->integer ch) uc-a))))

   (*disj 3)
   done)))

(define <XX>
  (new (*parser <hex-digit>)
       (*parser <hex-digit>)
       (*caten 2)
       (*pack-with
  (lambda (h l)
    (+ l (* h 16))))
       done))

(define <XXXX>
  (new (*parser <XX>)
       (*parser <XX>)
       (*caten 2)
       (*pack-with
  (lambda (h l)
    (+ l (* 256 h))))
       done))

(define <string-hex-char>
  (new (*parser (word-ci "\\x"))
       (*parser <XXXX>)
       (*parser <XX>)
       (*parser <hex-digit>)
       (*disj 3)
       (*pack integer->char)
       (*parser (char #\;))
       (*caten 3)
       (*pack-with (lambda (_< ch _>) ch))
       done))

(define <string-char>
  (new
    (*parser <string-meta-char>)
    (*parser <string-hex-char>)
    (*parser <string-visiable-char>)
    (*disj 3)
    done
    ))

(define <string>
  (new
  	(*parser <skippable>)
    (*parser (char #\"))
    (*parser <string-char>) *star
    (*parser (char #\"))
    (*parser <skippable>)
    (*caten 5)
    (*pack-with
        (lambda (a open-delim chars close-delim b)
            (list->string chars)))
    done
  ))

(define <symbol-char>
  (new
    (*parser <digit-0-9>)
    (*parser (range-ci #\a #\z))
    (*parser (char #\!))
    (*parser (char #\$))
    (*parser (char #\^))
    (*parser (char #\*))
    (*parser (char #\-))
    (*parser (char #\_))
    (*parser (char #\=))
    (*parser (char #\+))
    (*parser (char #\<))
    (*parser (char #\>))
    (*parser (char #\?))
    (*parser (char #\/))
    (*parser (char #\:))
    (*disj 15)
  done
  ))

(define <symbol>
  (new
    (*parser <skippable>)
    (*parser <symbol-char>) *plus
    (*parser <skippable>)
    (*caten 3)
    (*pack-with (lambda (a b c) (string->symbol (list->string b))))
    done)
  )

(define <left-bracket>
     (const 
          (lambda (ch)
              (= (char->integer ch) 40)))
)

(define <right-bracket>
     (const 
          (lambda (ch)
              (= (char->integer ch) 41)))
)

(define <left-bracket-curved>
     (const 
          (lambda (ch)
              (= (char->integer ch) 123)))
)

(define <right-bracket-curved>
     (const 
          (lambda (ch)
              (= (char->integer ch) 125)))
)

(define <proper-list>
  (new
    (*parser <skippable>)
    (*parser <left-bracket>)
    (*parser <skippable>)
    (*delayed (lambda () <sexpr>)) 
    *star
    (*parser <skippable>)
    (*parser <right-bracket>)
    (*parser <skippable>)
    (*caten 7)
    (*pack-with (lambda (a b c d e f g) d))
  done
  ))

(define <sexpr-star>
  (new
    (*delayed (lambda () <sexpr>))
    *star
    done))

(define <improper-list>
  (new
    (*parser <skippable>)
    (*parser <left-bracket>)
    (*parser <skippable>)
    (*delayed (lambda () <sexpr>)) *plus
    (*parser (char #\.))
    (*delayed (lambda () <sexpr>))
    (*parser <skippable>)
    (*parser <right-bracket>)
    (*parser <skippable>)
    (*caten 9)
    (*pack-with (lambda (a b c d e f g h i) `(,@d . ,f)))
    done
  ))

(define <vector>
  (new
    (*parser <skippable>)
    (*parser (char #\#))
    (*parser <left-bracket>)
    (*parser <skippable>)
    (*delayed (lambda () <sexpr>)) *star
    (*parser <skippable>)
    (*parser <right-bracket>)
    (*parser <skippable>)
    (*caten 8)
    (*pack-with (lambda (a b c d e f g h)  (list->vector e)))
  done
  ))

(define <quoted>
  (new
    (*parser <skippable>)
    (*parser (char #\'))
    (*parser <skippable>)
    (*delayed (lambda () <sexpr>))
    (*parser <skippable>)
    (*caten 5)
    (*pack-with (lambda (a b c d e)  (append '(quote) (list d) )))
  done)
  )

(define <quasi-quoted>
  (new
    (*parser <skippable>)
    (*parser (char #\`))
    (*parser <skippable>)
    (*delayed (lambda () <sexpr>))
    (*parser <skippable>)
    (*caten 5)
    (*pack-with (lambda (a b c d e) (append '(quasiquote) (list d) )))
  done)
  )

(define <unquoted>
  (new
    (*parser <skippable>)
    (*parser (char #\,))
  	(*parser (char #\@))
  	*not-followed-by  
    (*parser <skippable>)
    (*delayed (lambda () <sexpr>))
    (*parser <skippable>)
    (*caten 5)
    (*pack-with (lambda (a b c d e) (append '(unquote) (list d) )))
    done)
  )
  
(define <unquote-and-splice>
  (new
    (*parser <skippable>)
    (*parser (word ",@"))
    (*parser <skippable>)
    (*delayed (lambda () <sexpr>))
    (*parser <skippable>)
    (*caten 5)
    (*pack-with (lambda (a b c d e)  (append '(unquote-splicing) (list d) )))
    done)
  )

(define <CBName>
	(new
	  (*parser <skippable>)
	  (*delayed (lambda () <CBNameSyntax1>))
	  (*delayed (lambda () <CBNameSyntax2>))
	  (*disj 2)
	  (*parser <skippable>)
	  (*caten 3)
	  (*pack-with (lambda (a b c) b))
	  done)
)

(define <CBNameSyntax1>
	(new
	  (*parser <skippable>)
	  (*parser (char #\@))
	  (*parser <skippable>)
	  (*delayed (lambda () <sexpr>))
	  (*parser <skippable>)
	  (*caten 5)
	  (*pack-with (lambda (a b c d e) `(cbname ,d)))
	  done)
)

(define <CBNameSyntax2>
	(new
	  (*parser <skippable>)
	  (*parser <left-bracket-curved>)
	  (*parser <skippable>)
	  (*delayed (lambda () <sexpr>))
	  (*parser <skippable>)
	  (*parser <right-bracket-curved>)
	  (*parser <skippable>)
	  (*caten 7)
	  (*pack-with (lambda (a b c d e f g) `(cbname ,d)))
	  done)
)

(define <infix-prefix-extension-prefix>
  (new
    (*parser (word "##"))
    (*parser (word "#%"))
    (*disj 2)
    done
    ))

(define <arith-symbol>
  (new
    (*parser (char #\+))
    (*parser (char #\-))
    (*parser (char #\*))
    (*parser (word "**"))
    (*parser (char #\^))
    (*parser (char #\/)) ; /. ??
    (*disj 6)
    done
    ))

(define <infix-symbol>
  (new
   (*parser <symbol-char>)
   (*parser <arith-symbol>)
    *diff 
    *plus
      (*pack (lambda (a) (string->symbol (list->string a))))
  done
  ))


(define <power-symbol>
  (new
    (*parser (char #\^))
    (*parser (word "**"))
    (*disj 2)
    done
    ))

(define <left-square-bracket>
    (const 
          (lambda (ch)
              (= (char->integer ch) 91)))
)

(define <right-square-bracket>
    (const 
          (lambda (ch)
              (= (char->integer ch) 93)))
)

(define <infix-sexpr-escape>
  (new
    (*parser <infix-prefix-extension-prefix>)
    (*delayed (lambda () <sexpr>))
    (*caten 2)
    (*pack-with (lambda (a b) b))    
    done
))

(define <infix-L0> ;infix-add/sub
  (new
    (*delayed (lambda () <infix-L1>))

    (*parser (char #\+))
    (*parser (char #\-))
    (*disj 2)
    (*delayed (lambda () <infix-L1>))
    (*caten 2)
    (*pack-with (lambda (a b) (lambda(c) (list (string->symbol (string a)) c b))))
    *star

    (*caten 2)
    (*pack-with (lambda (a b)
      (fold-left (lambda (c d) (d c)) a b)))
    
    done
  ))

(define <infix--L0>
  (new
    (*parser (char#\-))
    (*delayed (lambda () <infix-L1>))
    (*caten 2)
    (*pack-with (lambda (a b) `(- ,b)))
    *star

    (*delayed (lambda () <infix-L1>)) 
    (*caten 2)
    done
  ))

(define <infix-L1> ;infix-mul/div
  (new
    (*delayed (lambda () <infix-L2>))

    (*parser (char #\*))
    (*parser (char #\/))
    (*disj 2)
    (*delayed (lambda () <infix-L2>))
    (*caten 2)
    (*pack-with (lambda (a b) (lambda(c) (list 
                                                (string->symbol (string a)) c b)
                                                                                )))
    *star
    (*caten 2)
    (*pack-with (lambda (a b)
      (fold-left (lambda (c d) (d c)) a b)))
     done
  ))

(define <infix-L2> ;infix-pow
  (new
    (*delayed (lambda () <infix-L3>))
    (*parser <power-symbol>)
    (*caten 2)

    (*pack-with (lambda (a b) 
                    (lambda (c)
                      `(expt ,a ,c))))
    *star
    
    (*delayed (lambda () <infix-L3>))
    (*caten 2)
    (*pack-with (lambda (lst last) 
        (fold-right (lambda (op elem) 
                          (op elem)) last lst)))
    done
  ))  

(define <infix-L3> ;infix array-get
  (new
    (*delayed (lambda () <infix-L4>))

    (*parser <left-square-bracket>)
    (*delayed (lambda () <infix-L0>))
    (*parser <right-square-bracket>)
    (*delayed (lambda () <infix-skippable>))    
    (*caten 4) 
    (*pack-with (lambda (a b c d) (lambda(e) `(vector-ref ,e ,b))))
    *star
    (*caten 2)  
    (*pack-with (lambda (a b)
      (fold-left (lambda (c d) (d c)) a b)))  
    done
  ))   

  (define <infix-arg-list>
  (new
    (*parser <infix-L0>)

    (*parser (char #\,))
    (*parser <infix-L0>)
    (*caten 2)
    (*pack-with (lambda (a b) b))
    *star

    (*caten 2)
    (*pack-with (lambda (a b) `(,a ,@b)))
    (*parser <epsilon>)
    (*disj 2)
    done
    ))

(define <infix-L4>        ;infix-func-call
  (new
    (*delayed (lambda() <infix-L5>))
    (*parser <left-bracket>)
    (*parser <infix-arg-list>)
    (*parser <right-bracket>)
    (*delayed (lambda () <infix-skippable>))
    (*caten 4)
    (*pack-with (lambda (a b c d) b))
    (*caten 2)
    (*pack-with (lambda (a b) `(,a ,@b)))

    (*delayed (lambda() <infix-L5>))
    (*disj 2)

    done
    ))

(define <infix-L5> ;infix-paren/number/escape/ (last level)
  (new
    (*delayed (lambda () <infix-skippable>))

    (*parser <positive-number>)

    (*parser <infix-symbol>)

    (*parser <left-bracket>)
    (*parser <infix-L0>)
    (*parser <right-bracket>)
    (*caten 3)
    (*pack-with (lambda (a b c) b))

    (*parser (char #\-))
    (*parser <infix--L0>)
    (*caten 2)
    (*pack-with (lambda (a b) `(- ,@(cdr b))))

    (*parser <infix-sexpr-escape>)

    (*disj 5)

    (*delayed (lambda () <infix-skippable>))
    (*caten 3)
    (*pack-with (lambda (a b c) b))
    done
  ))

(define <infix-expr-comment> 
    (new
        (*parser (word "#;"))
        (*parser <infix-L0>)
        (*caten 2)
        (*pack-with (lambda (a b)  b))
        done))

(define <infix-skippable>
  (new
    (*parser <whitespace>)
    (*parser <end-of-line-comment>) 
    (*parser <infix-expr-comment>)
    (*disj 3) *star
    done))

(define <infix-extension>
  (new
    (*parser <skippable>)
    (*parser <infix-prefix-extension-prefix>)
    (*parser <infix-L0>) ; infix-expression
    (*parser <skippable>)
    (*caten 4)
    (*pack-with (lambda (a b c d) c))
    done
  ))

(define <sexpr>
  (new
   (*parser <boolean>)
   (*parser <char>)
   (*parser <number>)
   (*parser <string>)
   (*parser <symbol>)
   (*parser <proper-list>)
   (*parser <improper-list>)
   (*parser <vector>)
   (*parser <quoted>)
   (*parser <quasi-quoted>)
   (*parser <unquoted>)
   (*parser <unquote-and-splice>)
   (*parser <CBName>)
   (*parser <infix-extension>)
   (*disj 14)
  done)
  )


  (define <sexpr-star>
  	(new
  		(*parser <sexpr>)
  		*star
  		done))