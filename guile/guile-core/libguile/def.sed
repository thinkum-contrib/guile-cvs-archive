/#def[ \t]/{  	s/#def/#define/
		s/$/ \\/
		n

		:getmacro
		/#end/ b gotmacro
		s/$/ \\/
		N
		b getmacro

		:gotmacro
		s/#end//
	}

/#def;/	{  	s/#def;/#define/
		s/$/ \\/
		n

		:sgetmacro
		/#end/ b sgotmacro
		s/$/ \\/
		N
		b sgetmacro

		:sgotmacro
		s/; \\.#end//
		G
	}


