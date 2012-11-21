<?php

require_once ( getenv( 'MW_INSTALL_PATH' ) !== false
	? getenv( 'MW_INSTALL_PATH' ) . "/maintenance/commandLine.inc"
	: dirname( __FILE__ ) . '/../../maintenance/commandLine.inc' );
require( 'Expr.php' );

$tests = file( 'exprTests.txt' );

$pass = $fail = 0;

// Each test is on one line. The test must always evaluate to '1'.
$parser = new ExprParser;
foreach ( $tests as $test ) {
	$test = trim( $test );
	if ( in_string( ';', $test ) )
		list( $input, $expected ) = explode( ';', $test );
	else {
		$input = $test;
		$expected = 1;
	}

	$expected = trim( $expected );
	$input = trim( $input );

	$result = $parser->doExpression( $input );
	if ( $result != $expected ) {
		print
			"FAILING test -- $input
 gave a final result of $result, instead of $expected.\n";
		$fail++;
	} else {
		print "PASSED test $test\n";
		$pass++;
	}
}

print "Passed $pass tests, failed $fail tests, out of a total of " . ( $pass + $fail ) . "\n";