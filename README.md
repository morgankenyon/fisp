# fisp

A lisp interpreter implemented in F#.

This loosely follows the tutorial set out in [Build Your Own Lisp](https://www.buildyourownlisp.com/)

Goals of the project:

* Create my second language interpreter
* Learn Lisp

## Notes

In the [notes](/notes) folder, I plan on writing my thoughts about this project periodically. They're categorized by date, so feel free to take a look if you're curious what is going on with this project lately.

## Language Spec

Here are the things that I this version of Lisp to do:

### Numbers

Can perform basic arithmetic:
```
fisp> + 1 2
3
fisp> * 1 2
2
fisp> / 2 1
2
fisp> - 1 2
- 1
```

But arithmetic is limited to 2 expressions
```
fisp> + 1 2 3
Error: + operator requires 2 expressions, 3 given
```

Divide by zero is not allowed:
```
fisp> / 10 0
Error: Cannot divide by zero
```

### Boolean Logic

True and false are simple values:

```
fisp> #t
#t
fisp> #f
#f
```

Several boolean operators exist:
```
fisp> > 1 0
#t
fisp> > 0 1
#f
```

### Strings

String literals are supported:

```
fisp> "Hello world"
"Hello world"
```
## Todo

* See what error message this expression gives and why that is: `(+ (5 10) (+ 15 20))`
* Work on supporting escaped strings (see `LexerTests.fs` for a unit test on the subject)
* Investigate what Lisp/Racket does when combining expressions of different types. Should these give parser errors:
    *  (`(+ 1 "hello world")`)
* How does Lisp/Racket do string concatenation. Through a basic operator (`+`) or some defined method
* See what is breaking unicode support for strings, is it REPL or something internal to language.