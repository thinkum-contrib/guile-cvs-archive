;
; Specific unix syscalls
;

(define *lib* "")

; some stdlib functions

(define system  (make-extfunc "" :int "system" '(:string)))

; stdio : semi automatically imported with h2scm
;
; int  remove(__const char *__filename)
(define remove (make-extfunc *lib* :int "remove" '(:string)))

; int  rename(__const char *__old, __const char *__new)
(define rename (make-extfunc *lib* :int "rename" '(:string :string)))

; FILE * tmpfile(void)
(define tmpfile (make-extfunc *lib* :item "tmpfile" '()))

; char * tmpnam(char *__s)
(define tmpnam (make-extfunc *lib* :string "tmpnam" '(:string)))

; int  fclose(FILE *__stream)
(define fclose (make-extfunc *lib* :int "fclose" '(:item)))

; int  fflush(FILE *__stream)
(define fflush (make-extfunc *lib* :int "fflush" '(:item)))

; FILE * fopen(__const char * __filename, __const char * __modes)
(define fopen (make-extfunc *lib* :item "fopen" '(:string :string)))

; FILE * freopen(__const char * __filename, __const char * __modes, FILE * __stream)
(define freopen (make-extfunc *lib* :item "freopen" '(:string :string :item)))

; FILE * fdopen(int __fd, __const char *__modes)
(define fdopen (make-extfunc *lib* :item "fdopen" '(:int :string)))

; void  setbuf(FILE * __stream, char * __buf)
(define setbuf (make-extfunc *lib* :void "setbuf" '(:item :string)))

; Modes for setvbuf
(define _IOFBF 0)	; Fully buffured
(define _IOLBF 1)   ; Line buffered.
(define _IONBF 2)	; No buffering.

; int  setvbuf(FILE * __stream, char * __buf, int __modes, size_t __n)
(define setvbuf (make-extfunc *lib* :int "setvbuf" '(:item :string :int :uint)))

; void  setbuffer(FILE * __stream, char * __buf, size_t __size)
(define setbuffer (make-extfunc *lib* :void "setbuffer" '(:item :string :uint)))

; void  setlinebuf(FILE *__stream)
(define setlinebuf (make-extfunc *lib* :void "setlinebuf" '(:item)))

; int  fprintf(FILE * __stream, __const char * __format, ...)
(define fprintf (make-extfunc *lib* :int "fprintf" '(:item :string . :any)))

; int  printf(__const char * __format, ...)
(define printf (make-extfunc *lib* :int "printf" '(:string . :any)))

; int  sprintf(char * __s, __const char * __format, ...)
; (define sprintf (make-extfunc *lib* :int "sprintf" '(:string :string . :any)))

; int  fgetc(FILE *__stream)
(define fgetc (make-extfunc *lib* :int "fgetc" '(:item)))

; int  getc(FILE *__stream)
(define getc (make-extfunc *lib* :int "getc" '(:item)))

; int  getchar(void)
(define getchar (make-extfunc *lib* :int "getchar" '()))

; int  fputc(int __c, FILE *__stream)
(define fputc (make-extfunc *lib* :int "fputc" '(:int :item)))

; int  putc(int __c, FILE *__stream)
(define putc (make-extfunc *lib* :int "putc" '(:int :item)))

; int  putchar(int __c)
(define putchar (make-extfunc *lib* :int "putchar" '(:int)))

; int  getw(FILE *__stream)
(define getw (make-extfunc *lib* :int "getw" '(:item)))

; int  putw(int __w, FILE *__stream)
(define putw (make-extfunc *lib* :int "putw" '(:int :item)))

; char * fgets(char * __s, int __n, FILE * __stream)
(define fgets (make-extfunc *lib* :string "fgets" '(:string :int :item)))

; char * gets(char *__s)
;(define gets (make-extfunc *lib* :string "gets" '(:string)))

; int  fputs(__const char * __s, FILE * __stream)
(define fputs (make-extfunc *lib* :int "fputs" '(:string :item)))

; int  puts(__const char *__s)
(define puts (make-extfunc *lib* :int "puts" '(:string)))

; int  ungetc(int __c, FILE *__stream)
(define ungetc (make-extfunc *lib* :int "ungetc" '(:int :item)))

; size_t  fread(void * __ptr, size_t __size, size_t __n, FILE * __stream)
(define fread (make-extfunc *lib* :uint "fread" '(:item :uint :uint :item)))

; size_t  fwrite(__const void * __ptr, size_t __size, size_t __n, FILE * __s)
(define fwrite (make-extfunc *lib* :uint "fwrite" '(:item :uint :uint :item)))

; Possible values for the __whence argument of fseek
(define SEEK_SET	0)	; Seek from beginning of file.
(define SEEK_CUR	1)	; Seek from current position.
(define SEEK_END	2)	; Seek from end of file.

; int  fseek(FILE *__stream, long int __off, int __whence)
(define fseek (make-extfunc *lib* :int "fseek" '(:item :long :int)))

; long int  ftell(FILE *__stream)
(define ftell (make-extfunc *lib* :int "ftell" '(:item)))

; void  rewind(FILE *__stream)
(define rewind (make-extfunc *lib* :void "rewind" '(:item)))

; void  clearerr(FILE *__stream)
(define clearerr (make-extfunc *lib* :void "clearerr" '(:item)))

; int  feof(FILE *__stream)
(define feof (make-extfunc *lib* :int "feof" '(:item)))

; int  ferror(FILE *__stream)
(define ferror (make-extfunc *lib* :int "ferror" '(:item)))

; void  perror(__const char *__s)
(define perror (make-extfunc *lib* :void "perror" '(:string)))

; int  fileno(FILE *__stream)
(define fileno (make-extfunc *lib* :int "fileno" '(:item)))

; FILE * popen(__const char *__command, __const char *__modes)
(define popen (make-extfunc *lib* :item "popen" '(:string :string)))

; int  pclose(FILE *__stream)
(define pclose (make-extfunc *lib* :int "pclose" '(:item)))

; char * ctermid(char *__s)
(define ctermid (make-extfunc *lib* :string "ctermid" '(:string)))

; directories
(define (getcwd)
  (let ((*getcwd* (make-extfunc *lib* :string "getcwd" '(:string :int)))
		(s (make-string 1024)))
	(*getcwd* s 1024)))

(define chdir 
  (make-extfunc *lib* :int "chdir" '(:string)))


; misc
(define sleep
  (make-extfunc *lib* :void "sleep" '(:int)))

; Exemple of stdio library code
;
(define (type-file file)
  (let ((fd) (buf) (bufsize 256))
    (set! fd (fopen file "r"))
    (if (not (null-pointer? fd))
	(begin
	  (while 
	   (not (null? 
		 (fgets
		  (set! buf (make-string bufsize #\space))
		  bufsize
		  fd)))
	   (printf "%s\n" (string-chop buf)))
	  (fclose fd))
	(print "cannot open"))))

#t