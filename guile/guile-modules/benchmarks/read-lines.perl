#!/usr/bin/perl
use Benchmark;

$t = timeit (100, '
    open (data2, "port-data-2");
    $total=0;
    while (<data2>) {
	$total += length ($_);
    }
    close (data2);
');
print timestr($t), "\n";
