;; Lightning arrester

(define-module (rod)
  :use-module (ice-9 regex)
  :use-module (ice-9 rdelim))

(define jitop-rgx 
  (make-regexp "^ *#define *jit_([a-z]*)(_([a-z]*))?\\((.*)\\)"))

;; Operation kinds
;;
;; - 3op
;;
;;   Generic 3 operand instruction with args (d, s1, s2) or (d, rs,
;;   is).  D and S1 are registers.  When S2 is an integer use
;;   jit_NAMEi, else it must be a register.  Use jit_NAMEr then.
;;
;; - 2op
;;
;;   Generic 2 operand instruction with args (d, rs) or (d, is).  D is
;;   a registers.  When S1 is an integer use jit_NAMEi, else it must
;;   be a register.  Use jit_NAMEr then.
;;
;; - arg
;;
;;   Argument declaration pseudo instruction with NAME "arg".
;;
;; - getarg
;;
;;   Argument getter with args (reg, ofs).
;;
;; - branch
;;
;;   Branch instruction with arg (label, s1, s2) or (label, rs, is)
;;
;; - 1rop
;;
;;   One argument instruction with args (rs).  Argument is always a
;;   register.
;;
;; - 2rop
;;
;;   Two argument instruction with args (r1, r2).  Arguments are
;;   always registers.
;;
;; - 1iop
;;
;;   One argument instruction with args (is).  Argument is always
;;   immediate.
;;
;; - 2sop
;;
;;   Two argument instruction with args (id, rs) or (rd, rs).
;;
;; - 3sop
;;
;;   Two argument instruction with args (id, r1, r2) or (rd, r1, r2).
;;
;; - 0op
;;
;;   No argument instruction
;;
;; - jump
;;
;;   Jump to immediate or register
;;
;; - jumpi
;; 
;;   Jump to immediate

(define (make-op name type kind)
  (vector name type kind))

(define (op-name o) (vector-ref o 0))
(define (op-type o) (vector-ref o 1))
(define (op-kind o) (vector-ref o 2))

(define ops '())

(define (add-op key name type kind)
  (set! ops (cons (cons key (make-op name type kind)) ops)))

(define (butlastchar string)
  (substring string 0 (1- (string-length string))))

(define (add-op* op trunc? type kind . default-type)
  (let ((op1 (if trunc? (butlastchar op) op))
	(default-type (if (null? default-type) "l" (car default-type))))
    (add-op (string->symbol (if type (string-append op1 "." type) op1))
	    op1 type kind)
    (if (and type (string=? type default-type))
	(add-op (string->symbol op1) op1 type kind))))
	
(with-input-from-file "lightning.ops"
  (lambda ()
    (let loop ((l (read-line)))
      (cond
       ((eof-object? l))
       ((regexp-exec jitop-rgx l)
	=> (lambda (m)
	     (let ((op (match:substring m 1))
		   (type (match:substring m 3))
		   (args (match:substring m 4)))
	       (cond
		((string=? op "patch"))
		((or (string=? op "prepare")
		     (string=? op "prolog")
		     (string=? op "leaf"))
		 (add-op* op #f type '1iop))
		((or (string=? op "jmpi")
		     (string=? op "jmpr"))
		 (add-op* op #t type 'jump))
		((or (string=? op "calli")
		     (string=? op "finish"))
		 (add-op* op #f type 'jumpi))
		((string=? op "arg")
		 (add-op* op #f type 'arg))
		((string=? args "")
		 (add-op* op #f type '0op))
		((string=? op "getarg")
		 (add-op* op #f type 'getarg))
		((or (string=? args "label, s1, s2")
		     (string=? args "label, rs, is"))
		 (add-op* op #t type 'branch))
		((string=? args "rs")
		 (add-op* op #f type '1rop))
		((string=? args "rd")
		 (add-op* op #f type '1rop "i"))
		((string-match "^stx" op)
		 (add-op* op #t type '3sop))
		((string-match "^st" op)
		 (add-op* op #t type '2sop))
		((and (or (string=? args "d, s1, s2")
			  (string=? args "d, r1, r2")
			  (string=? args "d, rs, is"))
		      (or (string-match "r$" op)
			  (string-match "i$" op)))
		 (if (string-match "r$" op)
		     (add-op* op #t type '3op)))
		((and (or (string=? args "d, rs")
			  (string=? args "d, is"))
		      (or (string-match "r$" op)
			  (string-match "i$" op)))
		 (if (string-match "r$" op)
		     (add-op* op #t type '2op)))
		((or (string=? op "ntoh")
		     (string=? op "hton"))
		 (add-op* op #f type '2rop))
		(else
		 (add-op* op #f type 'unknown))))
	     (loop (read-line))))
       (else
	(loop (read-line)))))))

(define (@ . args)
  (apply format #t args))

(for-each (lambda (o)
	    (let ((insn (car o))
		  (op (cdr o)))
	      (@ "if (!strcmp (~S, insn_op)) {\n" (symbol->string insn))
	      (case (op-kind op)
		((3op)
		 (@ "  unsigned long d, s1, s2;\n")
		 (@ "  ASSERT_LEN (3);\n")
		 (@ "  d = AS_REG (insn_1);\n")
		 (@ "  s1 = AS_REG (insn_2);\n")
		 (@ "  if (IS_REG (insn_3)) {\n")
		 (@ "    s2 = AS_REG (insn_3);\n")
		 (@ "    jit_~Ar_~A (d, s1, s2);\n"
		    (op-name op) (op-type op))
		 (@ "  } else {\n")
		 (@ "    s2 = AS_INT (insn_3);\n")
		 (@ "    jit_~Ai_~A (d, s1, s2);\n"
		    (op-name op) (op-type op))
		 (@ "  }\n"))
		((2op)
		 (@ "  unsigned long d, s1;\n")
		 (@ "  ASSERT_LEN (2);\n")
		 (@ "  d = AS_REG (insn_1);\n")
		 (@ "  if (IS_REG (insn_2)) {\n")
		 (@ "    s1 = AS_REG (insn_2);\n")
		 (@ "    jit_~Ar_~A (d, s1);\n"
		    (op-name op) (op-type op))
		 (@ "  } else {\n")
		 (@ "    s1 = AS_INT (insn_2);\n")
		 (@ "    jit_~Ai_~A (d, s1);\n"
		    (op-name op) (op-type op))
		 (@ "  }\n"))
		((arg)
		 (@ "  ASSERT_LEN (1);\n")
		 (@ "  ASSERT_SYM (insn_1);\n")
		 (@ "  def_arg (arg_hash, insn_1, jit_~A_~A ());\n"
		    (op-name op) (op-type op)))
		((getarg)
		 (@ "  int r, a;\n")
		 (@ "  ASSERT_LEN (2);\n")
		 (@ "  r = AS_REG (insn_1);\n")
		 (@ "  ASSERT_SYM (insn_2);\n")
		 (@ "  a = get_arg (arg_hash, insn_2);\n")
		 (@ "  jit_~A_~A (r, a);\n" (op-name op) (op-type op)))
		((branch)
		 (@ "  unsigned long s1, s2;\n")
		 (@ "  jit_insn *lab, *ref;\n")
		 (@ "  ASSERT_LEN (3);\n")
		 (@ "  ASSERT_SYM (insn_1);\n")
		 (@ "  lab = get_label_def (label_hash, insn_1);\n")
		 (@ "  if (lab == NULL) lab = jit_forward ();\n")
		 (@ "  s1 = AS_REG (insn_2);\n")
		 (@ "  if (IS_REG (insn_3)) {\n")
		 (@ "    s2 = AS_REG (insn_3);\n")
		 (@ "    ref = jit_~Ar_~A (lab, s1, s2);\n"
		    (op-name op) (op-type op))
		 (@ "  } else {\n")
		 (@ "    s2 = AS_INT (insn_3);\n")
		 (@ "    ref = jit_~Ai_~A (lab, s1, s2);\n"
		    (op-name op) (op-type op))
		 (@ "  }\n")
		 (@ "  add_label_ref (label_hash, insn_1, ref);\n"))
		((1rop)
		 (@ "  int r;\n")
		 (@ "  ASSERT_LEN (1);\n")
		 (@ "  r = AS_REG (insn_1);\n")
		 (@ "  jit_~A_~A (r);\n" (op-name op) (op-type op)))
		((2rop)
		 (@ "  int r1, r2;\n")
		 (@ "  ASSERT_LEN (2);\n")
		 (@ "  r1 = AS_REG (insn_1);\n")
		 (@ "  r2 = AS_REG (insn_2);\n")
		 (@ "  jit_~A_~A (r1, r2);\n" (op-name op) (op-type op)))
		((1iop)
		 (@ "  int i;\n")
		 (@ "  ASSERT_LEN (1);\n")
		 (@ "  i = AS_INT (insn_1);\n")
		 (if (op-type op)
		     (@ "  jit_~A_~A (i);\n" (op-name op) (op-type op))
		     (@ "  jit_~A (i);\n" (op-name op))))
		((2sop)
		 (@ "  unsigned long d, s1;\n")
		 (@ "  ASSERT_LEN (2);\n")
		 (@ "  s1 = AS_REG (insn_2);\n")
		 (@ "  if (IS_REG (insn_1)) {\n")
		 (@ "    d = AS_REG (insn_1);\n")
		 (@ "    jit_~Ar_~A (d, s1);\n"
		    (op-name op) (op-type op))
		 (@ "  } else {\n")
		 (@ "    d = AS_INT (insn_1);\n")
		 (@ "    jit_~Ai_~A (d, s1);\n"
		    (op-name op) (op-type op))
		 (@ "  }\n"))
		((3sop)
		 (@ "  unsigned long d, s1, s2;\n")
		 (@ "  ASSERT_LEN (3);\n")
		 (@ "  s1 = AS_REG (insn_2);\n")
		 (@ "  s2 = AS_REG (insn_3);\n")
		 (@ "  if (IS_REG (insn_1)) {\n")
		 (@ "    d = AS_REG (insn_1);\n")
		 (@ "    jit_~Ar_~A (d, s1, s2);\n"
		    (op-name op) (op-type op))
		 (@ "  } else {\n")
		 (@ "    d = AS_INT (insn_1);\n")
		 (@ "    jit_~Ai_~A (d, s1, s2);\n"
		    (op-name op) (op-type op))
		 (@ "  }\n"))
		((0op)
		 (@ "  ASSERT_LEN (0);\n")
		 (@ "  jit_~A ();\n" (op-name op)))
		((jump)
		 (@ "  unsigned long t;\n")
		 (@ "  ASSERT_LEN (1);\n")
		 (@ "  if (IS_REG (insn_1)) {\n")
		 (@ "    t = AS_REG (insn_1);\n")
		 (@ "    jit_~Ar (t);\n" (op-name op))
		 (@ "  } else {\n")
		 (@ "    t = AS_INT (insn_1);\n")
		 (@ "    jit_~Ai (t);\n" (op-name op))
		 (@ "  }\n"))
		((jumpi)
		 (@ "  unsigned long t;\n")
		 (@ "  ASSERT_LEN (1);\n")
		 (@ "  t = AS_INT (insn_1);\n")
		 (@ "  jit_~A (t);\n" (op-name op)))
		(else
		 (error "unrecognized op-kind" (op-kind op))))
	      (@ "} else ")))
	  (reverse ops))
(@ "{\n")
(format #t "  scm_misc_error (~S, ~S, SCM_LIST1 (SCM_CAR(insn)));\n"
	"assemble" "unrecognized instruction: ~A")
(format #t "}\n")

