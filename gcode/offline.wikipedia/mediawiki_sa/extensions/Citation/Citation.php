<?php
# Example use :
#
# There is a <citation>author=Manske M ||title="The best paper ever" ||journal=''Biochemistry'' ||volume='''5''', 11</citation> citation here!


$wgHooks['ParserBeforeTidy'][] = 'citation_hooker';
$wgHooks['ParserClearState'][] = 'citation_clear_state';

$wgExtensionFunctions[] = "wfCitation";

function wfCitation() {
	global $wgParser ;
	$wgParser->setHook( "citation" , 'parse_citation' ) ;
	$wgParser->isMainParser = true ;
}

$wgCitationCache = array() ;
$wgCitationCounter = 1 ;
$wgCitationRunning = false ;

function citation_hooker( &$parser, &$text ) {
	global $wgCitationCache , $wgCitationCounter , $wgCitationRunning ;
	if ( $wgCitationRunning )
		return true;
	if ( count( $wgCitationCache ) == 0 )
		return true;
	if ( !isset( $parser->isMainParser ) )
		return true;
	$ret = "" ;
	foreach ( $wgCitationCache AS $num => $entry ) {
		$x = "<li>" . $entry . " <a href='#citeback{$num}'>↑</a></li>\n" ;
		$ret .= $x ;
	}
	$ret = "<hr /><ol>" . $ret . "</ol>" ;
	
	$text .= $ret ;
	
	return true;
}

function citation_clear_state() {
	global $wgCitationCache, $wgCitationCounter, $wgCitationRunning;
	$wgCitationCache = array();
	$wgCitationCounter = 1 ;
	$wgCitationRunning = false;
	
	return true;
}

function parse_citation( $text , $params , $parser ) {
	global $wgCitationRunning ;
	if ( $wgCitationRunning ) return ;
	$ret = "" ;
	$attheend = false ;
	$res = array() ;
	$res2 = array() ;
	$href = "" ;
	$a = explode( "||" , $text ) ;
	
	foreach ( $a AS $line ) {
		$data = explode( "=" , $line , 2 ) ;
		while ( count( $data ) < 2 ) $data[] = "" ;
		$key = urlencode( trim( strtolower( array_shift( $data ) ) ) ) ;
		$value = array_shift( $data ) ;
		
		// Parsed now : "$key" = "$value"
		if ( substr( $value , 0 , 3 ) == "{{{" ) { } // Unset variable, ignore 
		else if ( $key == "attheend" ) $attheend = true ;
		else if ( $key == "href" ) $href = $value ;
		else if ( $value != "" ) {
			$x = array( "key" => $key , "value" => $value ) ;
			$res[] = $x ;
			$res2[$key] = $value ;
		}
	}
	
	// Creating output string
	foreach ( $res AS $item ) {
		$key = $item["key"] ;
		$value = $item["value"] ;
		$key2 = urldecode( $key ) ;
		if ( strtolower( substr( $key2 , 0 , 3 ) ) == "if:" ) {
			$key2 = trim( substr( $key2 , 3 ) ) ;
			$key = urlencode( $key2 ) ;
		}
		if ( isset( $res2[$key] ) ) $ret .= $value ;
	}

	if ( $href != "" ) $ret .= " [{$href}]" ;
	
	// Adding to footer list or showing inline
	$localParser = new Parser ;
	$wgCitationRunning = true ;
	$ret = $localParser->parse( $ret , $parser->getTitle(), $parser->getOptions(), false ) ;
	$wgCitationRunning = false ;
	$ret = $ret->getText();

	if ( $attheend ) {
		global $wgCitationCache , $wgCitationCounter ;
		$ret = "<a name='citation{$wgCitationCounter}'></a>{$ret}" ;
		$wgCitationCache[$wgCitationCounter] = $ret ;
		$ret = "<a href='#citation{$wgCitationCounter}' name='citeback{$wgCitationCounter}'>{" . $wgCitationCounter . "}</a>" ;
		$wgCitationCounter++ ;
	} else {
		$ret = "<span style='font-size:8pt'>[{$ret}]</span>" ;
	}

	return $ret ;
}




