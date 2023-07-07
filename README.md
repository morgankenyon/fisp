# fisp

A lisp interpreter implemented in F#.

This loosely follows the tutorial set out in [Build Your Own Lisp](https://www.buildyourownlisp.com/)

Goals of the project:

* Create my second language interpreter
* Learn Lisp

## Notes

In the [notes](/notes) folder, I plan on writing my thoughts about this project periodically. They're categorized by date, so feel free to take a look if you're curious what is going on with this project lately.

## Todo

* See what error message this expression gives and why that is: `(+ (5 10) (+ 15 20))`
* Work on supporting escaped strings (see `LexerTests.fs` for a unit test on the subject)
* Investigate what Lisp/Racket does when combining expressions of different types. Should these give parser errors:
    *  (`(+ 1 "hello world")`)
* How does Lisp/Racket do string concatenation. Through a basic operator (`+`) or some defined method
