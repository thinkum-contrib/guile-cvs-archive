# This is a sample .gdbinit posted by Bill Schottstaedt (modified to
# use `set' instead of `call' in some places).
#
# Bill writes:
#
#   so in gdb if you see something useless like:
#
#   #32 0x081ae8f4 in scm_primitive_load (filename=1112137128) at load.c:129
#
#   You can get the file name with gp:
#
#   (gdb) gp 1112137128
#   $1 = 0x40853fac "\"/home/bil/test/share/guile/1.5.0/ice-9/session.scm\""

define gp
set gdb_print($arg0)
print gdb_output
end
document gp
Executes (object->string arg)
end

define ge
call gdb_read($arg0)
call gdb_eval(gdb_result)
set gdb_print(gdb_result)
print gdb_output
end
document ge
Executes (print (eval (read arg))): ge "(+ 1 2)" => 3
end

define gh
call g_help(scm_str2symbol($arg0), 20)
set gdb_print($1)
print gdb_output
end
document gh
Prints help string for arg: gh "enved-target"
end

# .gdbinit ends here
