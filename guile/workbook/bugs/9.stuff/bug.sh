#!/bin/sh

guile=${GUILE-guile}

# part 1 -- expect stop w/ exit value 0
$guile -s bug.scm > /dev/null
if [ x$? = x0 ] ; then
    echo "part 1 ok"
else
    echo "badness on part 1!"
    exit 1
fi

# part 2 -- expect quit after 3 sec w/ exit value 0
echo '(sleep 3)(quit)' | $guile -l bug.scm > /dev/null
if [ x$? = x0 ] ; then
    echo "part 2 ok"
else
    echo "badness on part 2!"
    exit 1
fi

exit 0
