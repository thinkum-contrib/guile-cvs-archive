#
# dump stack in the engine
#

define sdump
	echo TOS:
	call (void)scm_cprint(TOS)
	call sdump(vm)
end

define rdump
	set scm_rp = rp
	call rdump()
end

define car
	print ($arg0)->data.pair.car
end

define cdr
	print ($arg0)->data.pair.cdr
end

define cprint
    call scm_cprint($arg0)
end

handle SIGPWR  nostop print pass
handle SIGXCPU nostop print pass
