# Converts a list of symbols into C expressions which define the symbols
# in Guile.
{
print "#ifdef " $0;
print "scm_environment_intern (env, \""$0"\", SCM_MAKINUM ("$0"));";
print "#endif"
}
