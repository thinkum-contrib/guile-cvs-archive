(vector? OBJ) => BOOLEAN                                    [R5RS]
  Returns #t if obj is a vector, otherwise returns #f.

(make-vector K [FILL]) => VECTOR                            [R5RS]
  Returns a newly allocated vector of K elements. If a second
  argument is given, then each element is initialized to
  fill. Otherwise the initial contents of each element is
  unspecified.

(vector obj ...) => vector                                  [R5RS]
  Returns a newly allocated vector whose elements contain the
  given arguments.

  Example:
    (vector 'a 'b 'c) => #(a b c)

(vector-length vector) => number                            [R5RS]
  Returns the number of elements in vector as an exact integer.

(vref VECTOR K) => OBJ                                      [R5RS]
  Alias for vector-ref

(vector-ref vector k) => obj                                [R5RS]
  k must be a valid index of vector. Vector-ref returns the
  contents of element k of vector.

  Example:
    (vector-ref #(1 1 2 3 5 8 13 21) 5) => 8

(vset! VECTOR K OBJ) => #undefined                       [QScheme]
  Alias for vector-set!

(vector-set! vector k obj) => #undefined                    [R5RS]
  Stores obj in element k of vector. The value returned by
  vector-set! is unspecified.

(vector->list VECTOR) => LIST                               [R5RS]
  Returns a newly allocated list of the objects contained in the
  elements of VECTOR.

(list->vector LIST) => VECTOR                               [R5RS]
  Returns a newly created VECTOR initialized to the elements of
  the list LIST.

(vector-fill! VECTOR FILL) => #undefined                    [R5RS]
  Stores FILL in every element of VECTOR.

(vector-resize VECTOR NEWSIZE) => VECTOR                 [QScheme]
  Change the size of VECTOR to NEWSIZE.

(vector-append VECTOR OBJ) => VECTOR                     [QScheme]
  Append object OBJ to vector VECTOR, incrementing it's size

(vector-sort! VECTOR COMP) => #undefined                 [QScheme]
  Sort vector. COMP is a simple function with 2 arguments testing
  comparing 2 elements.

(vector-qsort! VECTOR) => VECTOR                         [QScheme]
  Destructively sort a vector using the qsort algorythm. The
  vector is supposed to contain number only.

(begin EXPR...) => ANY                                    [SYNTAX]
  Evaluates EXPR sequencially from left to right. The value the
  last expression is returned

(if TEST CONSEQUENT [ALTERNATE]) => OBJ                   [SYNTAX]
  Evaluates CONSEQUENT if the TEST expression evaluates to
  #t. Otherwise evaluates ALTERNATE.

(define VAR [EXPR]) => EXPR                               [SYNTAX]
  Create a new variable VAR, bind it to a fresh location and store
  the value of EXPR. If EXPR is not given, #unbound is assigned

(define (VAR FORMAL) BODY) => PROC                        [SYNTAX]
  Create a variable VAR and assign the procedure equivalent to
  (lambda (FORMAL) BODY).

(define (VAR ARG [:local LOC] [:rest SYM]) BODY) => PROC [QScheme]
  Same as define but also creates local variables and a binding
  for the rest of arguments

(set! VAR EXPR) => VALUE                                  [SYNTAX]
  Evaluates EXPR and stores the resulting value in the location
  to which VAR is bound.

(let BINDING EXPR...) => OBJ                              [SYNTAX]
  Creates local variables as described in BINDING and evaluates in
  the new environemnent the expressions. Returns the value of last
  EXPR. The BINDING are evaluated using the enclosing environment

(let NAME BINDING EXPR...) => OBJ                         [SYNTAX]
  Does the same as LET except that it binds NAME to the body of
  the let construct. Thus the execution of the body EXPRs may be
  repeated by invoking the procedure named NAME.

(let* ((VAR INIT) ...) BODY) => OBJ                       [SYNTAX]
  Let* is similar to let, but the bindings are performed
  sequentially from left to right, and the region of a binding
  indicated by (VAR INIT) is that part of the let* expression to
  the right of the binding. Thus the second binding is done in
  an environment in which the first binding is visible, and so on.

(letrec ((VAR INIT) ...) BODY) => OBJ                     [SYNTAX]
  The VARs are bound to fresh locations holding undefined values,
  the INITs are evaluated in the resulting environment (in some
  unspecified order), each VAR is assigned to the result of the
  corresponding INIT, the BODY is evaluated in the resulting
  environment, and the value of the last expression in BODY is
  returned. Each binding of a VAR has the entire letrec expression
  as its region, making it possible to define mutually recursive
  procedures.

(lambda FORMALS BODY) => PROCEDURE                        [SYNTAX]
  Returns a new procedure. The environment in effect when the
  lambda expression was evaluated is remembered as part of the
  procedure. When the procedure is later called with some actual
  arguments, the environment in which the lambda expression was
  evaluated will be extended by binding the variables in the
  FORMAL argument list to fresh locations, the corresponding
  actual argument values will be stored in those locations, and
  the expressions in the BODY of the lambda expression will be
  evaluated sequentially in the extended environment. The result
  of the last expression in the BODY will be returned as the
  result of the procedure call.

(quote X) => X                                            [SYNTAX]
  Return X, unevaluated

(quasiquote TEMPLATE) => LIST                             [SYNTAX]
  If no comma appears in the TEMPLATE, just behaves like
  quote. Otherwise the values following the comma are evaluated
  and the result is inserted in place.

(the-env) => ENV                                          [SYNTAX]
  Returns the current compilation environment

(and TEST1 ...) => OBJ                                    [SYNTAX]
  Expressions are evaluated from left to right, and the value of
  the first expression that evaluates to a false is returned. Any
  remaining expressions are not evaluated. If all the expressions
  evaluate to true values, the value of the last expression is
  returned. If there are no expressions then #t is returned.

(or TEST1 ...) => OBJ                                     [SYNTAX]
  expressions are evaluated from left to right, and the value of
  the first expression that evaluates to a true is returned. Any
  remaining expressions are not evaluated. If all expressions
  evaluate to false values, the value of the last expression is
  returned. If there are no expressions then #f is returned.

(cond (TEST EXPR...) ... [(else EXPR...)] ) => OBJ        [SYNTAX]
  Each TEST is evaluated until one evaluates to true or the final
  else is reached. When TEST is true, the rest of EXPR is evaluated
  and the result of last is returned.

(while TEST EXPR ...) => OBJ                             [QScheme]
  Evaluates the EXPR while TEST evaluates to TRUE.

(until TEST EXPR ...) => OBJ                             [QScheme]
  Evaluates the EXPR until TEST evaluates to TRUE.

(do ((VAR INIT STEP)...)(TST EXPR...) CMD...) => OBJ      [SYNTAX]
  Binds VAR to INIT and start iterate. First evaluate TEST. If
  TEST is true, then EXPR are evaluated and the result of last
  EXPR is returned. If TST is false then CMD are evaluated and
  the iteration restarts by evaluating the STEP expr and binding
  the result to VAR again.

(catch TAG HANDLER EXPR...) => OBJ                        [SYNTAX]
  Catch exceptions occuring during evaluation of the EXPR. TAG is
  either a list of symbol or #t or #f. HANDLER is a function to
  handle the exception. When an execption occurs, the TAG list is
  search for a matching symbol and the HANDLER procedure is called.

(execute-macro MACRO FORM ENV) => OBJ                    [QScheme]
(macro-expand EXPR ENV) => OBJ                           [QScheme]
(string->atom STRING) => ATOM                            [QScheme]
  Returns the atom representing the string

(atom->string ATOM) => STRING                            [QScheme]
  Returns the STRING representing the ATOM.

(not OBJ) => BOOLEAN                                        [R5RS]
  Returns #t if obj is false, and returns #f otherwise.

(boolean? OBJ) => BOOLEAN                                   [R5RS]
  Returns #t if obj is either #t or #f and returns #f otherwise.

(eq? OBJ1 OBJ2) => BOOLEAN                                  [R5RS]
  Returns #t if OBJ1 and OBJ2 represents the same cell.

(eqv? OBJ1 OBJ2) => BOOLEAN                                 [R5RS]
  Returns #t if OBJ1 and OBJ2 should normally be regarded as the
  same object, #f otherwise.

(equal? obj1 obj2) => BOOLEAN                               [R5RS]
  Equal? recursively compares the contents of pairs, vectors,
  and strings, applying eqv? on other objects such as numbers
  and symbols. A rule of thumb is that objects are generally
  equal? if they print the same. Equal? may fail to terminate if
  its arguments are circular data structures.

(throw TAG MSG) => #undefined                               [R5RS]
  Send an exception of type TAG and message MSG to the first
  exception handler who has the TAG in it's tag list (or #t). If
  no matching handler are found, the default toplevel handler
  is called.

(segv) => #undefined                                     [QScheme]
  Generates a segmentation fault by reading the content of address
  0x0. Used for debugging.

(char? OBJ) => BOOLEAN                                      [R5RS]
  Returns #t if OBJ is a character, otherwise returns #f.

(char=? CHAR1 CHAR2) => BOOLEAN                             [R5RS]
  Returns #t if CHAR1 equals CHAR2, #f otherwise.

(char<? CHAR1 CHAR2) => BOOLEAN                             [R5RS]
  Returns #t if CHAR1 less than CHAR2 according to the ASCII char
  set ordering, #f otherwise.

(char<=? CHAR1 CHAR2) => BOOLEAN                            [R5RS]
  Returns #t if CHAR1 less or equal to CHAR2 according to the
  ASCII char set ordering, #f otherwise.

(char>? CHAR1 CHAR2) => BOOLEAN                             [R5RS]
  Returns #t if CHAR1 greater than CHAR according to the ASCII
  char set ordering2, #f otherwise.

(char>=? CHAR1 CHAR2) => BOOLEAN                            [R5RS]
  Returns #t if CHAR1 greater or equal to CHAR2 according to the
  ASCII char set ordering, #f otherwise.

(char-ci=? CHAR1 CHAR2) => BOOLEAN                          [R5RS]
  Returns #t if CHAR1 equals CHAR2, #f otherwise. The comparison
  is case insensitive.

(char-ci<? CHAR1 CHAR2) => BOOLEAN                          [R5RS]
  Returns #t if CHAR1 less than CHAR2 according to the ASCII char
  set ordering, #f otherwise. The comparison is case insensitive.

(char-ci<=? CHAR1 CHAR2) => BOOLEAN                         [R5RS]
  Returns #t if CHAR1 less or equal to CHAR2 according to the
  ASCII char set ordering, #f otherwise. The comparison is case
  insensitive.

(char-ci>? CHAR1 CHAR2) => BOOLEAN                          [R5RS]
  Returns #t if CHAR1 greater than CHAR according to the ASCII char
  set ordering2, #f otherwise. The comparison is case insensitive.

(char-ci>=? CHAR1 CHAR2) => BOOLEAN                         [R5RS]
  Returns #t if CHAR1 greater or equal to CHAR2 according to
  the ASCII char set ordering, #f otherwise. The comparison is
  case insensitive.

(char-alphabetic? CHAR) => BOOLEAN                          [R5RS]
  Return #t if CHAR is alphabetic, #f otherwise

(char-numeric? CHAR) => BOOLEAN                             [R5RS]
  Return #t if CHAR is numeric, #f otherwise

(char-whitespace? CHAR) => BOOLEAN                          [R5RS]
  Return #t if CHAR is a whitespace, #f otherwise

(char-upper-case? CHAR) => BOOLEAN                          [R5RS]
  Return #t if CHAR is a upper case, #f otherwise

(char-lower-case? CHAR) => BOOLEAN                          [R5RS]
  Return #t if CHAR is a lower case, #f otherwise

(char->integer CHAR) => INT                                 [R5RS]
  Returns the ASCII code of the CHAR.

(integer->char INT) => CHAR                                 [R5RS]
  Returns the character CHAR corresponding to ASCII code INT.

(char-upcase CHAR) => CHAR                                  [R5RS]
  Return the upper case character of CHAR.

(char-downcase CHAR) => CHAR                                [R5RS]
  Return the lower case character of CHAR.

(load-library NAME) => #t                                [QScheme]
  Load a dynamic library and link it to the current scheme
  interpreter. Functions and variables of this library can be
  accessed with 'make-extfunc' and 'make-extvar'

(make-extfunc LIB RET NAME '(ARG...)) => EXTFUNC         [QScheme]
  Create a new external function that can be called just like
  if it's a native scheme procedure. RET and ARG are keyword
  reprenting the type of return and argument. NAME is the name
  of the function as defined in the symbol table of the dynamic
  library LIB.

  Example:
    (define printf (make-extfunc "" :int "printf" '(:string
    . :any)))

(external-exists? LIB NAME) => BOOLEAN                   [QScheme]
  Return #t if symbol NAME is defined in the dynamic library LIB.

(call-external LIB RET NAME '(TYPE ...) (ARG ...))       [QScheme]
  Directly call a library function. Argument are converted
  according to type and the value returned is converted to scheme
  value as specified by the RET keyword

(make-external-pointer LIB NAME) => POINTER              [QScheme]
#{ ... } => HASH                                         [QScheme]
  generate a hash literal

(make-hash [TYPE] ) => HASH                              [QScheme]
  Create a new hash of type TYPE. TYPE is 0 for generic, 1 for *
  symbol and 2 for atom hashes. If no type is given, creates a
  general hash.

(make-generic-hash) => HASH                              [QScheme]
  Create a generic hash

(make-symbol-hash) => SYMBOL-HASH                        [QScheme]
  Create a symbol hash

(make-atom-hash) => ATOM-HASH                            [QScheme]
  Create a atom hash. Atom hash cannot have any value associated
  to the key

(hash-set! HASH KEY VALUE) => ASSOC                      [QScheme]
  Create or change an the entry matching KEY in HASH to VALUE.

(hash-ref HASH KEY) => OBJ | #f                          [QScheme]
  Return the value OBJ matching KEY in hash HASH. If KEY is not
  found, #f is returned

(hash-search HASH KEY) => ASSOC | #f                     [QScheme]
  Search for KEY in HASH. If found, the matching ASSOC is returned,
  otherwise #f is returned.

(hash-remove! HASH KEY) -> HASH                          [QScheme]
  Remove KEY from HASH.

(hash->list HASH) => ALIST                               [QScheme]
  Returned the content of the HASH as association list ALIST

(list->hash ALIST) => HASH                               [QScheme]
  Return a new HASH filled with the assiotiation found in ALIST.

(hash-stat HASH) => #undefined                           [QScheme]
  Display the statistics for the HASH

(hash? OBJ) => BOOLEAN                                   [QScheme]
  Returns #t if OBJ is a hash, #f otherwise

(atom-hash? OBJ) => BOOLEAN                              [QScheme]
  Returns #t if OBJ is an atom hash, #f otherwise

(symbol-hash? OBJ) => BOOLEAN                            [QScheme]
  Returns #t if OBJ is a symbol hash, #f otherwise

(normal-hash? OBJ) => BOOLEAN                            [QScheme]
  Returns #t if OBJ is a generic (normal) hash, #f otherwise

(generic-hash? OBJ) => BOOLEAN                           [QScheme]
  Returns #t if OBJ is a generic (normal) hash, #f otherwise

(pair? OBJ) => BOOLEAN                                      [R5RS]
  Returns #t if obj is a pair, and otherwise returns #f.

(cons OBJ1 OBJ2) => PAIR                                    [R5RS]
  Returns a newly allocated pair whose car is OBJ1 and whose cdr
  isOBJ2. The pair is guaranteed to be different (in the sense
  of eqv?) from every existing object.

(cons2 OBJ1 OBJ2 OBJ3) => PAIR                           [QScheme]
  Return a new pair construct whose car is OBJ1 and cdr is a cons
  of OBJ2 and OBJ3. Example: (cons2 1 2 3) => (1 2 . 3)

(car PAIR) => OBJ                                           [R5RS]
  Returns the contents of the car field of pair. Note that it is
  an error to take the car of the empty list.

(cdr PAIR) => OBJ                                           [R5RS]
  Returns the contents of the cdr field of pair. Note that it is
  an error to take the cdr of the empty list.

(set-car! PAIR OBJ) => #undefined                           [R5RS]
  Stores OBJ in the car field of PAIR.

(set-cdr! PAIR OBJ) => #undefined                           [R5RS]
  Stores OBJ in the cdr field of PAIR.

(null? OBJ) => BOOLEAN                                      [R5RS]
  Returns #t if obj is the empty list, otherwise returns #f.

(list? OBJ) => BOOLEAN                                      [R5RS]
  Returns #t if obj is a list, otherwise returns #f.

(list OBJ ...) => LIST                                      [R5RS]
  Returns a newly allocated list of its arguments.

(length LIST) => NUMBER                                     [R5RS]
  Returns the length of LIST.

(append LIST ...) => LIST                                   [R5RS]
  Returns a list consisting of the elements of the first list
  followed by the elements of the other lists.

(append2 LIST1 LIST2) => LIST                            [QScheme]
  Returns a list consisting of the elements of the LIST1 followed
  by the elements of the LIST2

(reverse LIST) => LIST                                      [R5RS]
  Returns a newly allocated list consisting of the elements of
  list in reverse order.

(list-tail LIST K) => LIST                                  [R5RS]
  Returns the sublist of LIST obtained by omitting the first K
  elements. It is an error if LIST has fewer than K elements.

(list-ref LIST K) => OBJ                                    [R5RS]
  Returns the Kth element of LIST. (This is the same as the car
  of (list-tail LIST K).) It is an error if LIST has fewer than
  K elements.

(memq OBJ LIST) => LIST | #f                                [R5RS]
  Return the first sublist of LIST whose car is OBJ. If OBJ does
  not occur in LIST, then #f is returned. Memq uses eq? to compare
  OBJ with the elements of LIST.

(memv OBJ LIST) => LIST | #f                                [R5RS]
  Return the first sublist of LIST whose car is OBJ. If OBJ
  does not occur in LIST, then #f is returned. Memq uses eqv? to
  compare OBJ with the elements of LIST.

(member OBJ LIST) => LIST | #f                              [R5RS]
  Return the first sublist of LIST whose car is OBJ. If OBJ does
  not occur in LIST, then #f is returned. Memq uses equal? to
  compare OBJ with the elements of LIST.

(assq OBJ ALIST) => PAIR | #f                               [R5RS]
  Return the first PAIR in ALIST whose car field eq? OBJ. If no
  pair in ALIST has OBJ as its car, then #f is returned.

(assv OBJ ALIST) => PAIR | #f                               [R5RS]
  Return the first PAIR in ALIST whose car field eqv? OBJ. If no
  pair in ALIST has OBJ as its car, then #f is returned.

(assoc OBJ ALIST) => PAIR | #f                              [R5RS]
  Return the first PAIR in ALIST whose car field equal? OBJ. If
  no pair in ALIST has OBJ as its car, then #f is returned.

(map PROC LIST1 LIST2 ...) => LIST                          [R5RS]
  The lists must be lists, and proc must be a procedure taking
  as many arguments as there are lists and returning a single
  value. If more than one list is given, then they must all be
  the same length. Map applies PROC element-wise to the elements
  of the lists and returns a list of the results, in order.

  Example:
    (map cadr '((a b) (d e) (g h))) => (b e h)

(for-each proc list1 list2 ...) => #undefined               [R5RS]
  The arguments to for-each are like the arguments to map, but
  for-each calls proc for its side effects rather than for its
  values. Unlike map, for-each is guaranteed to call proc on
  the elements of the lists in order from the first element(s)
  to the last, and the value returned by for-each is unspecified.

(nth K LIST) => OBJ                                      [QScheme]
  Returns the Kth element of the LIST.

(list-remove! LIST OBJ) => LIST                          [QScheme]
  Remove the pair containing OBJ from LIST. The original LIST is
  modified by this function. eqv? is used to locate the OBJ

  Example:
    (list-remove! '(a b c d) 'c) => (a b d)

(list-replace! LIST OBJ NEW) => LIST                     [QScheme]
  Replace OBJ in LIST with the NEW object. The original LIST is
  modified by this function. eqv? is used to locate the OBJ

  Example:
    (list-replace! '(a b c d) 'c 'k) => (a b k d)

(make-macro CODE) => MACRO                               [QScheme]
(macro->lambda MACRO) => PROC                            [QScheme]
(macro-func MACRO) => PROC                               [QScheme]
(macro-set-func! MACRO FUNC) => MACRO                    [QScheme]
(macro-code MACRO) => PROC                               [QScheme]
(macro-set-code! MACRO CODE) -> MACRO                    [QScheme]
(gc) => #undefined                                       [QScheme]
  Fires up a gc right now.

(gc-stat) => #undefined                                  [QScheme]
  Display current GC statistics

(gc-verbosity NUMBER) => #undefined                      [QScheme]
  Change garbage collector verbosity. 0 => quiet, 1 => min,
  2 => max

(whatis STR) => NUMBER                                   [QScheme]
  Prints all description of STR found in the whatis data
  base. Returns the number of element found. If the last character
  of the string STR is '~', the description of all words starting
  with STR will be displayed.

  Example:
    (whatis "whatis") => display the description of the whatis
    function

(set-prompt STR) => #undef                               [QScheme]
  set a new prompt

(version) => STRING                                      [QScheme]
  Return current version of qscheme as a string

module-hash => HASH                                      [QScheme]
  contains the reference to all defined modules

(make-module NAME) => MODULE                             [QScheme]
  Returns a new module with name NAME.

(module? OBJ) => BOOLEAN                                 [QScheme]
  Returns #t if OBJ is a module, #f otherwise

(current-module) => MODULE                               [QScheme]
  Returns the current module.

(set-current-module MODULE) => MODULE                    [QScheme]
  Set MODULE as current module for symbol search.

(import MODULE...) => LIST                               [QScheme]
  Import symbols from modules given as argument.

(export SYM ...) => LIST                                 [QScheme]
  NOT IMPLEMENTED YET.

(module-exports MODULE) => LIST                          [QScheme]
  Returns the list of exported symbols

(module-imports MODULE) => LIST                          [QScheme]
  Returns the list of imported modules.

(module-symbols MODULE) => LIST                          [QScheme]
  Returns the list of modules symbols.

(find-module NAME) => MODULE                             [QScheme]
  Return the MODULE having this NAME. If MODULE is not found,
  returns #f.

(module NAME BODY) => OBJ                                 [SYNTAX]
  Create a new module NAME or set NAME as current module and
  evaluates the BODY. All symbols defined in the body will be
  store in the module's symbol list and are only visible inside
  this module. Returns the value of last evaluated expression.

(export SYMBOL ...)                                       [SYNTAX]
  Add all symbols to the export list of the module

(import MODULE ...)                                       [SYNTAX]
  Add modules as arguement to the import list of current module

MODULE::SYMBOL => OBJ                                     [SYNTAX]
  Syntaxic sugar to access symbol of a module. Equivalent to
  (module MODULE SYMBOL)

(ashift NUMBER POS) => NUMBER                            [QScheme]
  Return NUMBER shifted by POS bits. If POS is positive, shift
  to the left, else shift to the right. Equivalent to NUMBER *
  expt(2, POS)

(bit-and N ...) => NUMBER                                [QScheme]
  Returns the logical and of it's argument.

(bit-or N ...) => NUMBER                                 [QScheme]
  Returns the logical or of it's argument.

(bit-xor N ...) => NUMBER                                [QScheme]
  Returns the logical exclusive or of it's argument.

(bit-not N) => NUMBER                                    [QScheme]
  Returns the logical not of N.

(pointer? OBJ) => BOOLEAN                                [QScheme]
  Returns #t if OBJ is a pointer, #f otherwise.

(null-pointer? OBJ) => BOOLEAN                           [QScheme]
  Returns #t if OBJ is a pointer pointing to NULL, #f otherwise.

(port? OBJ) => BOOLEAN                                   [QScheme]
  Returns #t if OBJ is a port, #f otherwise.

(input-port? OBJ) => BOOLEAN                                [R5RS]
  Returns #t if OBJ is a port and is a readable port, #f otherwise.

(output-port? OBJ) => BOOLEAN                               [R5RS]
  Returns #t if OBJ is a port and is a writeable port, #f
  otherwise.

(current-input-port) => PORT                                [R5RS]
  Returns the current input port.

(current-output-port) => PORT                               [R5RS]
  Returns the current output port.

(current-error-port) => PORT                             [QScheme]
  Returns the current error port.

(with-input-from-file FILENAME THUNK) => OBJ                [R5RS]
  The file FILENAME is open for reading and the THUNK procedure
  is evaluated with it's current-input-port pointing to the just
  opened file.

(with-output-to-file FILENAME THUNK) => OBJ                 [R5RS]
  The file FILENAME is open for writing and the THUNK procedure
  is evaluated with it's current-output-port pointing to the just
  opened file.

(with-input-from-string STRING THUNK) => OBJ             [QScheme]
  The file STRING is opened for reading and the THUNK procedure
  is evaluated with it's current-input-port pointing to the just
  opened string.

(with-output-to-string THUNK) => OBJ                     [QScheme]
  The file STRING is opened for writing and the THUNK procedure
  is evaluated with it's current-output-port pointing to the just
  opened string.

(open-input-file NAME) => PORT                              [R5RS]
  Open file NAME for reading. If file does not exist an error
  occurs.

(open-output-file NAME) => PORT                             [R5RS]
  Open file NAME for writing. File is truncated or created.

(open-input-string STRING) => PORT                       [QScheme]
  Open STRING for reading.

(open-output-string) => PORT                             [QScheme]
  Open a new string for writing. The string will be returned when
  port is closed.

(get-output-string PORT) => STRING                       [QScheme]
  Get current string for an output string port.

(close-port PORT) => BOOLEAN | STRING                       [R5RS]
  Close this port. Returns a STRING if port is an output string
  port, a BOOLEAN otherwise.

(close-input-port PORT) => BOOLEAN                          [R5RS]
  Close PORT and returns #t if no error ocurred, #f otherwise

(close-output-port PORT) => BOOLEAN | STRING                [R5RS]
  Close PORT and return a STRING if port was an output string,
  a BOOLEAN otherwise.

(read [PORT]) => OBJ                                        [R5RS]
  Read and parse an object from PORT if specified or from
  current-input-port.

(read-char [PORT]) => CHAR                                  [R5RS]
  Read a char from a PORT if specified or from current-input-port.

(peek-char [PORT]) => CHAR                                  [R5RS]
  Read a char from a PORT if specified or from
  current-input-port. The file pointer is not advanced, so next
  call to read-char or peek-char will return the same CHAR.

(eof-object? OBJ) => BOOLEAN                                [R5RS]
  Returns #t if OBJ is the eof object, #f otherwise

(read-line STR PORT) => BOOL                             [QScheme]
  Read a full line from PORT. If end-of file is reached, #f is
  returned. NOTE: STRING will not contain a newline character.

(char-ready? [PORT]) => BOOLEAN                             [R5RS]
  NOT IMPLEMENTED

(write OBJ [PORT]) => #undefined                            [R5RS]
  Writes a written representation of obj to the given port. Strings
  that appear in the written representation are enclosed in
  doublequotes, and within those strings backslash and doublequote
  characters are escaped by backslashes. If no PORT argument is
  given, the current-output-port is used.

(display OBJ [PORT]) => #undefined                          [R5RS]
  Writes a representation of obj to the given port. Strings
  that appear in the written representation are not enclosed
  in doublequotes, and no characters are escaped within those
  strings. Character objects appear in the representation as if
  written by write-char instead of by write. If no PORT argument
  is given, the current-output-port is used.

(newline [PORT]) => #undefined                              [R5RS]
  Writes an end of line to port. If no PORT argument is given,
  the current-output-port is used.

(write-char CHAR [PORT])                                    [R5RS]
  Writes the character CHAR to the given PORT. If no PORT argument
  is given, the current-output-port is used.

(flush-output [PORT]) => #undefined                      [QScheme]
  Write anything that have been buffered in the PORT. If no PORT
  argument is given, the current-output-port is used.

(file-position PORT [POS]) => NUMBER | BOOLEAN           [QScheme]
  When POS is given, set the file position for the port, if
  possible and returns a #t if operation was successfull. If POS
  is not given, returns the position number for the PORT.

(load FILENAME) => VALUE                                    [R5RS]
  FILENAME should be a string naming an existing file
  containing Scheme source code. The load procedure reads
  expressions and definitions from the file and evaluates them
  sequentially. Returns the value of last evaluated expression.

(transcript-on STRING) => #undefined                        [R5RS]
  NOT IMPLEMENTED

(transcript-off) => #undefined                              [R5RS]
  NOT IMPLEMENTED

(float-precision N) => OLDPREC                           [QScheme]
  Set the default number of fractional position that will be
  outputed when writing float numbers. Returns the precision that
  was in effect before calling this function.

(procedure? OBJ) => BOOLEAN                                 [R5RS]
  Returns #t if OBJ is a procedure, #f otherwise

(environment? OBJ) => BOOLEAN                            [QScheme]
  Returns #t if OBJ is an environment, #f otherwise

(closure? OBJ) => BOOLEAN                                [QScheme]
  Returns #t if OBJ is a closure, #f otherwise

(primitive? OBJ) => BOOLEAN                              [QScheme]
  Returns #t if OBJ is a primitive, #f otherwise

(cprimitive? OBJ) => BOOLEAN                             [QScheme]
  Returns #t if OBJ is a C primitive, #f otherwise

(syntax? OBJ) => BOOLEAN                                 [QScheme]
  Returns #t if OBJ is a syntax, #f otherwise

(cprimitive-arity PRIM) => NUMBER                        [QScheme]
(primitive-address PRIM) => POINTER                      [QScheme]
(primitive-arity PRIM) => NUMBER                         [QScheme]
(internal-type-list) => LIST                             [QScheme]
  Return the list of the name of all known types.

(make-type NAME) => INT                                  [QScheme]
  Creates a new internal type with name STR. Returns the type
  number.

(add-type-finalizer TYPE PROC)                           [QScheme]
  Add a procedure PROC to be called when an object of type TYPE
  is about to be destroyed.

(null-aux? OBJ) => BOOLEAN                               [QScheme]
  Returns #t if aux of OBJ is NULL, #f otherwise

(set-aux! OBJ VAL) => NULL                               [QScheme]
  Associate a value to the aux pointer of object. THIS IS A
  DANGEROUS FUNCTION.

(get-aux OBJ) => AUX                                     [QScheme]
  Returns the content of the aux field of OBJ. THIS IS A DANGEROUS
  FUNCTION.

(clear-aux! OBJ)                                         [QScheme]
  Stores NULL in the aux field of OBJ. THIS IS A DANGEROUS
  FUNCTION.

(unbound) => #unbound                                    [QScheme]
  Returns a reference to the #unbound object

(string-append-char STRING CHAR) => STRING               [QScheme]
  Returns a new STRING with CHAR appended.

(string? OBJ) => BOOLEAN                                    [R5RS]
  Returns #t if OBJ is a string, #f otherwise

(make-string LEN [CHAR]) => STRING                          [R5RS]
  Returns a newly allocated string of length LEN. If CHAR is
  given, then all elements of the string are initialized to CHAR,
  otherwise the contents of the string are unspecified.

(string CHAR ...) => STRING                                 [R5RS]
  Returns a newly allocated string composed of the arguments. NOTE:
  * CHAR may also be a number representing the ASCII code of
  the char.

(string-length STRING) => NUMBER                            [R5RS]
  Returns the number of characters in the given string.

(string-ref STRING K) => CHAR                               [R5RS]
  Returns character K of STRING using zero-origin indexing.

(string-set! STRING K CHAR) => #undefined                   [R5RS]
  Stores CHAR in element K of STRING and returns an unspecified
  value.

(string<? STRING1 STRING2) => BOOLEAN                       [R5RS]
  Returns #t if STRING1 is lexicographically less than STRING2,
  #f otherwise.

(string<=? STRING1 STRING2) => BOOLEAN                      [R5RS]
  Returns #t if STRING1 is lexicographically less or equal than
  STRING2, #f otherwise.

(string=? STRING1 STRING2) => BOOLEAN                       [R5RS]
  Returns #t if STRING1 is the same as STRING2, #f otherwise.

(string>? STRING1 STRING2) => BOOLEAN                       [R5RS]
  Returns #t if STRING1 is lexicographically greater than STRING2,
  #f otherwise.

(string>=? STRING1 STRING2) => BOOLEAN                      [R5RS]
  Returns #t if STRING1 is lexicographically greater or equal
  than STRING2, #f otherwise.

(string-ci<? STRING1 STRING2) => BOOLEAN                    [R5RS]
  Returns #t if STRING1 is lexicographically less than STRING2,
  #f otherwise. The comparison is case insensitive.

(string-ci<=? STRING1 STRING2) => BOOLEAN                   [R5RS]
  Returns #t if STRING1 is lexicographically less or equal than
  STRING2, #f otherwise. The comparison is case insensitive.

(string-ci=? STRING1 STRING2) => BOOLEAN                    [R5RS]
  Returns #t if STRING1 is the same as STRING2, #f otherwise. The
  comparison is case insensitive.

(string-ci>? STRING1 STRING2) => BOOLEAN                    [R5RS]
  Returns #t if STRING1 is lexicographically greater than STRING2,
  #f otherwise. The comparison is case insensitive.

(string-ci>=? STRING1 STRING2) => BOOLEAN                   [R5RS]
  Returns #t if STRING1 is lexicographically greater or equal
  than STRING2, #f otherwise. The comparison is case insensitive.

(substring START END) => STRING                             [R5RS]
  Returns a newly allocated string formed from the characters of
  string beginning with index start (inclusive) and ending with
  index end (exclusive).

(string-append STRING ...) => STRING                        [R5RS]
  Returns a newly allocated string whose characters form the
  concatenation of the given strings.

(string-append2 STR1 STR2) => STRING                        [R5RS]
  Returns a newly allocated string whose characters form the
  concatenation of STR1 and STR2

(string-append! STR1 STR2) => #undefined                 [QScheme]
  Append STR2 to STR1.

(string->list STRING) => LIST                               [R5RS]
  Returns a newly allocated list of the characters that make up
  the given STRING.

(string->symbol STRING) => SYMBOL                           [R5RS]
  Returns the symbol whose name is string.

(symbol->string SYMBOL) => STRING                           [R5RS]
  Returns the name of symbol as a string.

(list->string LIST) => STRING                               [R5RS]
  Returns a newly allocated STRING formed from the characters in
  the list LIST, which must be a list of characters.

(string-copy STRING) => STRING                              [R5RS]
  Returns a newly allocated copy of the given STRING.

(string-fill! STRING CHR) => #undefined                     [R5RS]
  Stores CHAR in every element of the given STRING and returns
  an unspecified value.

(string-index STRING SEARCHED) => INDEX | #f             [QScheme]
  Returns the index of first occurence of the SEARCHED string in
  STRING. If no occurence of SEARCHED is found, returns #f

(string-chop STR) => STRING                              [QScheme]
  Modifies STR in such way that everything from the first NEWLINE
  to the end of line is removed.

(string-split DELIM STRING) => LIST                      [QScheme]
  Returns a LIST of strings created by splitting STRING at each
  character that is in the DELIM argument.

  Example:
    (string-split "." "comp.os.linux") => ("comp" "os" "linux")

(string-join SEP LIST) => STRING                         [QScheme]
  Return a STRING which is the result of the concatenation of
  each string of LIST separated by SEP.

  Example:
    (string-join "." '("comp" "os" "linux")) => "comp.os.linux"

(string-lower STR1) => STRING                            [QScheme]
  Returns a newly allocated STRING which is a copy of STR1 with
  all characters converted to lower case

(string-upper STRING) => STRING                          [QScheme]
  Returns a newly allocated STRING which is a copy of STR1 with
  all characters converted to upper case

(string-translate STR WHAT REPL) => STRING               [QScheme]
  Returns a newly allocated string where all chars of STR having
  a match in the WHAT string are replaced by the corresponding
  char in the REPL string.

  Example:
    (string-translate "comp.os.linux" "." "-") => "comp-os-linux"

(string-pack TEMPLATE OBJ...) => STRING                  [QScheme]
  Return a new string containing a binary structure. The TEMPLATE
  is a string giving order and type of value to convert. Values
  are taken from the list of OBJ. TEMPLATE format is the same as
  the perl one.

(string-unpack TEMPLATE STRING) => LIST                  [QScheme]
  Unpack a string containing a binary structure to a list of
  elements. Convertions is driven by the content of the TEMPLATE
  string

(string-resize! STR LEN) => STR                          [QScheme]
  Change the size of the string STR to LEN. Returns the STR.

(keyword-display-type NUMBER) => NUMBER                  [QScheme]
  Changes the way of keyword display. When NUMBER is 0, the
  keywords are displayed prefixed by ':', when NUMBER is 1 they
  are prefixed with '#!' and when NUMBER is 2 they are suffixed
  with ':'

(gensym [PREFIX]) => SYMBOL                              [QScheme]
  Returns a new symbol which is guaranted to be unique (during
  this scheme session).

(make-symbol NAME) => SYMBOL                             [QScheme]
  Create an unbound symbol.

(symbol-name SYM) => NAME                                [QScheme]
  Returns the name associated to symbol SYM. NAME is an atom.

(symbol-value SYM) => VALUE                              [QScheme]
  Returns the value associated to symbol SYM. VALUE is any
  scheme object.

(external-variable? OBJ) => BOOL                         [QScheme]
  Returns #t if OBJ is an external variable, #f otherwise

(make-extern-variable LIB TYPE NAME) => EXTVAR           [QScheme]
  Declare an externaly defined variable. LIB is the name of
  the dynamic library where the NAME symbol is defined. TYPE is
  a keyword describing the type of variable and can take this
  values: :char, :short, :int, :long, :float, :double, :string,
  :string-buffer and :cscheme.

atom-hash => HASH                                        [QScheme]
  This hash contains the list of all atoms.

symbol-hash => HASH                                      [QScheme]
  Contains the list of all symbols

keyword-hash => HASH                                     [QScheme]
  Contains the list of all keywords. Keywords are stored without
  prefix or suffixes.

stdin-port => PORT                                       [QScheme]
stdout-port => PORT                                      [QScheme]
stderr-port => PORT                                      [QScheme]
process-list => LIST                                     [QScheme]
  The list of processe created with make-process

(process? OBJ) => BOOLEAN                                [QScheme]
  Returns #t if OBJ is a process, #f otherwise.

(process-pid PROCESS) => PID                             [QScheme]
  Returns the PID of the process PROCESS

(process-input PROCESS) => PORT                          [QScheme]
  Returns the process input port

(process-output PROCESS) => PORT                         [QScheme]
  Returns the process output port

(process-error PROCESS) => PORT                          [QScheme]
  return the process error port

(process-status PROCESS) => STATUS                       [QScheme]
  Returns the process status of the process PROCESS.

(make-process IN OUT ERR [ARG...|LIST|ARRAY]) => PROCESS [QScheme]
  Create a new process. IN, OUT or ERR indicates the type of port
  to open for the new process. :null or '() means that no file
  is opened, :pipe or "-" means open to a pipe, a string means
  open to a file, NUMBER means redirect to descriptor.

  Example:
    (make-process :null :pipe 1 "ls" "-al")

(process-wait PROC) => STATUS                            [QScheme]
  Wait for process to terminate and return the exit code of
  the process.

(process-use-execv FLAG) => OLD                          [QScheme]
  Determine if next make-process will use execv() or execvp(). If
  FLAG is #t, execv() will be used. Returns the previous mode

(cformat FMT ARG...) => STR                              [QScheme]
(thread-dump THR) => #undefined                          [QScheme]
  Dump the content of a thread. For debugging purpose.

(make-mutex) => MUTEX                                    [QScheme]
  Create a new MUTEX

(mutex? OBJ) => BOOL                                     [QScheme]
  Returns #t if OBj is a mutex, #f otherwise

(mutex-lock MUTEX) => #undef                             [QScheme]
  Lock MUTEX. If MUTEX is already locked, thread execution is
  suspended until MUTEX is unlocked

(mutex-try-lock MUTEX) => BOOLEAN                        [QScheme]
  Try to lock MUTEX. If MUTEX is already locked, #f is returned
  immediately. otherwise, the MUTEX is locked and #t is returned.

(mutex-unlock MUTEX) => #undefined                       [QScheme]
  Unlock mutex

(make-semaphore [VAL]) => SEM                            [QScheme]
(semaphore? OBJ) => BOOL                                 [QScheme]
  Returns #t if OBJ is a semaphore, #f otherwise.

(semaphore-wait SEM) => #undefined                       [QScheme]
  Suspend the calling thread until SEM has non-zero count, then
  decrease the semaphore count.

(semaphore-try-wait SEM) => BOOL                         [QScheme]
  Non blocking variant of semaphore-wait. If SEM has non-zero
  count, the count is atomically decreased and #t is returned. If
  SEM count is zero, #f is returned.

(semaphore-post SEM) => #undefined                       [QScheme]
  Atomically increases the count of SEM.

(semaphore-get-value SEM) => NUM                         [QScheme]
  Return the count of the semaphore SEM.

(thread THUNK) => THREAD                                 [QScheme]
  Invoke THUNK in a new thread. Returns THREAD, a thread descriptor

(thread? OBJ) => BOOLEAN                                 [QScheme]
  Returns #t if OBJ is a thread descriptor, false otherwise

(thread-id THREAD) => NUMBER                             [QScheme]
  Returns the pthread id associated with THREAD.

(current-thread) => THREAD                               [QScheme]
  Returns the THREAD for current thread

(thread-running? THREAD) => BOOL                         [QScheme]
  Returns #t if THREAD is running, #f otherwise

(thread-wait THREAD) => #undefined                       [QScheme]
  Wait until thread THREAD finishes

(thread-kill THREAD) => #undefined                       [QScheme]
  Terminates THREAD

(thread-exit) => ??? (never returns)                     [QScheme]
  Terminates current thread.

(thread-suspend THREAD) => #undefined                    [QScheme]
  Temporary suspend the THREAD

(thread-resume THREAD) => #undefined                     [QScheme]
  Restart the thread at point where it was stopped

(number? OBJ) => BOOLEAN                                    [R5RS]
  Returns #t if OBJ is a number, #f otherwise

(integer? OBJ) => BOOLEAN                                   [R5RS]
  Returns #t if OBJ is an integer number, #f otherwise

(real? OBJ) => BOOLEAN                                      [R5RS]
  Returns #t if OBJ is an real number, #f otherwise.

(complex? OBJ) => BOOLEAN                                   [R5RS]
  Returns #t if OBJ is an complex number, #f otherwise.

(rational? OBJ) => BOOLEAN                                  [R5RS]
  Returns #t if OBJ is an rational number, #f otherwise.

(exact? OBJ) => BOOLEAN                                     [R5RS]
  Returns #t if OBJ is an exact number, #f otherwise.

(inexact? OBJ) => BOOLEAN                                   [R5RS]
  Returns #t if OBJ is an inexact number, #f otherwise.

(zero? OBJ) => BOOLEAN                                      [R5RS]
  Return #t if OBJ is zero, #f otherwise

(positive? OBJ) => BOOLEAN                                  [R5RS]
  Return #t if OBJ is positive, #f otherwise

(negative? OBJ) => BOOLEAN                                  [R5RS]
  Return #t if OBJ is negative, #f otherwise

(odd? OBJ) => BOOLEAN                                       [R5RS]
  Return #t if OBJ is odd, #f otherwise

(even? OBJ) => BOOLEAN                                      [R5RS]
  Return #t if OBJ is even, #f otherwise

(min X1 X2 ...) => NUMBER                                   [R5RS]
  Return the minimum of its arguments

(max X1 X2 ...) => NUMBER                                   [R5RS]
  Return the maximum of its arguments

(+ N1 ...) => NUMBER                                        [R5RS]
  return the sum of its arguments

(* N1 ...) => NUMBER                                        [R5RS]
  Return the product of its arguments

(- N1 ...) => NUMBER                                        [R5RS]
  Returns the difference of it's arguments. With one argument,
  return the additive inverse of the argument

(/ N1 ...) => NUMBER                                        [R5RS]
  Return the quotient of it's argument. With one argument, return
  the inverse of it's argument

(abs X) => NUMBER                                           [R5RS]
  Returns the absolute value of its argument.

(quotient N1 N2) => INTEGER                                 [R5RS]
  Returns the quotient of N1/N2 rounded toward zero.

(remainder N1 N2) => INTEGER                                [R5RS]
  Returns the quotient of N1/N2.

(modulo N1 N2) => NUMBER                                    [R5RS]
  Returns the modulo of N1/N2.

(gcd N1 ...) => NUMBER                                      [R5RS]
  Return the greatest common divisor of it's arguments.

(lcm N1 ...) => NUMBER                                      [R5RS]
  Return the least common multiple of it's arguments

(floor N) => INTEGER                                        [R5RS]
  Returns the largest integer not larger than N.

(ceil N) => INTEGER                                         [R5RS]
  Returns the smallest integer not smaller than N.

(truncate N) => INTEGER                                     [R5RS]
  Returns the integer closest to N whose absolute value is not
  larger than the absolute value of N.

(round N) => INTEGER                                        [R5RS]
  Returns the closest integer to N, rounding to even when N is
  halfway between two integers.

(exp X) => NUMBER                                           [R5RS]
  Returns the value of e (the base of natural logarithms) raised
  to the power of X.

(log X) => NUMBER                                           [R5RS]
  Returns the natural logarithm of X.

(log10 X) => NUMBER                                      [QScheme]
  Returns the base-10 logarithm of X.

(sin X) => NUMBER                                           [R5RS]
  Returns the sine of X, where X is given in radians.

(cos X) => NUMBER                                           [R5RS]
  Returns the cosine of X, where X is given in radians.

(tan X) => NUMBER                                           [R5RS]
  Returns the tangent of X, where X is given in radians.

(asin X) => NUMBER                                          [R5RS]
  Returns the arc sine of X; that is the value whose sine is X.

(acos X) => NUMBER                                          [R5RS]
  Returns the arc cosine of X; that is the value whose sine is X.

(atan X) => NUMBER                                          [R5RS]
  Returns the arc tangent of X in radians.

(atan Y X) => NUMBER                                        [R5RS]
  calculates the arc tangent of the two variables X and Y. It is
  similar to calculating the arc tangent of Y / X, except that
  the signs of both arguments are used to determine the quadrant
  of the result.

(sqrt X) => NUMBER                                          [R5RS]
  Returns the principal square root of X.

(expt X Y) => NUMBER                                        [R5RS]
  Returns X raised to the power Y.

(random) => FLOAT                                        [QScheme]
  Returns a random number in range 0-1.0.

(exact->inexact Z) => NUMBER                                [R5RS]
  Returns an inexact representation of Z. The value returned is
  the inexact number that is numerically closest to the argument.

(inexact->exact z) => NUMBER                                [R5RS]
  Returns an exact representation of Z. The value returned is
  the exact number that is numerically closest to the argument.

(number->string Z [RADIX]) => STRING                        [R5RS]
  Returns as a string an external representation of the given
  number in the given radix

(string->number STRING RADIX) => NUMBER                     [R5RS]
  Returns a number of the maximally precise representation
  expressed by the given string.

(1+ X) => NUMBER                                         [QScheme]
  Returns X + 1.

(2+ X) => NUMBER                                         [QScheme]
  Returns X + 2.

(1- X) => NUMBER                                         [QScheme]
  Returns X - 1.

(2- X) => NUMBER                                         [QScheme]
  Returns X - 1.

(list2 OBJ1 OBJ2) => LIST                                [QScheme]
  Returns a newly allocated list of 2 elements.

(set-cdr! PAIR OBJ) => #undefined                           [R5RS]
  Change the cdr field of the PAIR to OBJ.

(newline [PORT]) => #undefined                              [R5RS]
  Output a newline char on port PORT. If no PORT argument is given,
  newline is sended to current-output-port

(not OBJ) => BOOL                                           [R5RS]
  Returns #t if OBJ is #f, #f otherwise

(boolean? OBJ) => BOOL                                      [R5RS]
  Returns #t if OBJ is either #t or #f. Otherwise #f is returned.

(eq? OBJ1 OBJ2) => BOOL                                     [R5RS]
  Returns #t if OBJ1 and OBJ2 refer to same scheme object.

(load FILE) => OBJ                                       [QScheme]
  Interpret the content of the file which name is given in
  STR. Returns the value of the last evaluated expression

(apply PROC ARG1 ARG2 ... ARGS) => VALUE                    [R5RS]
  Build an argument list such as (append (list arg1 arg2 ...) args)
  and call proc with this list as argument

(symbol? OBJ) => BOOL                                       [R5RS]
  Returns #t if OBJ is a symbol, #f otherwise

(pure-symbol? OBJ) => BOOL                               [QScheme]
  Returns #t if OBJ is a pure symbol, #f otherwise. Pure symbols
  * are binding a name with a value. Quoted symbols are not pure
  symbols, they are atoms.

(keyword? OBJ) => BOOL                                   [QScheme]
  Returns #t if OBJ is a keyword, #f otherwise

(keyword->string KEYW) => STR                            [QScheme]
  Convert a keyword to a string representation

(string->keyword STR) => KEYWORD                         [QScheme]
  Returns the keyword corresponding to the string STR.

(get-keyword KEYW LIST DEFAULT) => VALUE                 [QScheme]
  Search KEYW in the LIST. Returns the value following the keyword
  or DEFAULT if not found.

(atom? OBJ) => BOOL                                      [QScheme]
  Returns #t if OBJ is an atom, #f otherwise

(undefined-object? OBJ) => BOOL                          [QScheme]
  Returns #t if OBJ is undefined, #f otherwise

(unbound-object? OBJ) => BOOL                            [QScheme]
  Returns #t if OBJ is unbound, #f otherwise

(macro? OBJ) => BOOL                                     [QScheme]
  Returns #t if OBJ is a macro, #f otherwise

(list2 OBJ1 OBJ2) => LIST                                [QScheme]
  Returns a newly allocated list of 2 elements.

(set-car! PAIR OBJ) => #undefined                           [R5RS]
  Change the car field of the PAIR to OBJ.

(set-cdr! PAIR OBJ) => #undefined                           [R5RS]
  Change the cdr field of the PAIR to OBJ.

(nth ELT LIST) => OBJ                                    [QScheme]
(qq-append2 L1 L2) => (L1 L2)                            [QScheme]
(newline [PORT]) => #undefined                              [R5RS]
  Output a newline char on port PORT. If no PORT argument is given,
  newline is sended to current-output-port

(not OBJ) => BOOL                                           [R5RS]
  Returns #t if OBJ is #f, #f otherwise

(boolean? OBJ) => BOOL                                      [R5RS]
  Returns #t if OBJ is either #t or #f. Otherwise #f is returned.

(eq? OBJ1 OBJ2) => BOOL                                     [R5RS]
  Returns #t if OBJ1 and OBJ2 refer to same scheme object.

(load FILE) => OBJ                                       [QScheme]
  Interpret the content of the file which name is given in
  STR. Returns the value of the last evaluated expression

(apply PROC ARG1 ARG2 ... ARGS) => VALUE                    [R5RS]
  Build an argument list such as (append (list arg1 arg2 ...) args)
  and call proc with this list as argument

(symbol? OBJ) => BOOL                                       [R5RS]
  Returns #t if OBJ is a symbol, #f otherwise

(pure-symbol? OBJ) => BOOL                               [QScheme]
  Returns #t if OBJ is a pure symbol, #f otherwise. Pure symbols
  * are binding a name with a value. Quoted symbols are not pure
  symbols, they are atoms.

(keyword? OBJ) => BOOL                                   [QScheme]
  Returns #t if OBJ is a keyword, #f otherwise

(keyword->string KEYW) => STR                            [QScheme]
  Convert a keyword to a string representation

(string->keyword STR) => KEYWORD                         [QScheme]
  Returns the keyword corresponding to the string STR.

(get-keyword KEYW LIST DEFAULT) => VALUE                 [QScheme]
  Search KEYW in the LIST. Returns the value following the keyword
  or DEFAULT if not found.

(atom? OBJ) => BOOL                                      [QScheme]
  Returns #t if OBJ is an atom, #f otherwise

(undefined-object? OBJ) => BOOL                          [QScheme]
  Returns #t if OBJ is undefined, #f otherwise

(unbound-object? OBJ) => BOOL                            [QScheme]
  Returns #t if OBJ is unbound, #f otherwise

(macro? OBJ) => BOOL                                     [QScheme]
  Returns #t if OBJ is a macro, #f otherwise

(add2 n1 n2) => n                                        [QScheme]
