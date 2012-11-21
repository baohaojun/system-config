<?php

// fnatter 2006-11-02:
$wgCommandLineMode = true;
// make it not complain when we use a web-browser for testing
unset($_SERVER);

/**
 * Main wiki script; see docs/design.txt
 * @package MediaWiki
 */

$wgRequestTime = microtime(true);

# getrusage() does not exist on the Microsoft Windows platforms, catching this
if ( function_exists ( 'getrusage' ) ) {
	$wgRUstart = getrusage();
} else {
	$wgRUstart = array();
}

unset( $IP );
@ini_set( 'allow_url_fopen', 0 ); # For security...

if ( isset( $_REQUEST['GLOBALS'] ) ) {
	die( '<a href="http://www.hardened-php.net/index.76.html">$GLOBALS overwrite vulnerability</a>');
}

# Valid web server entry point, enable includes.
# Please don't move this line to includes/Defines.php. This line essentially
# defines a valid entry point. If you put it in includes/Defines.php, then
# any script that includes it becomes an entry point, thereby defeating
# its purpose.
define( 'MEDIAWIKI', true );

# Load up some global defines.
require_once( './includes/Defines.php' );

# Include this site setttings
require_once( './LocalSettings.php' );
require_once("extensions/ParserFunctions/ParserFunctions.php");
require_once( "extensions/Cite/Cite.php" );
// load ParserFunctions extension

# Prepare MediaWiki
require_once( 'includes/Setup.php' );

# Initialize MediaWiki base class
require_once( "includes/Wiki.php" );
$mediaWiki = new MediaWiki();

wfProfileIn( 'main-misc-setup' );
OutputPage::setEncodings(); # Not really used yet

# Query string fields
$action = $wgRequest->getVal( 'action', 'view' );
$title = $wgRequest->getVal( 'title' );

#
# Send Ajax requests to the Ajax dispatcher.
#
if ( $wgUseAjax && $action == 'ajax' ) {
	require_once( 'AjaxDispatcher.php' );

	$dispatcher = new AjaxDispatcher();
	$dispatcher->performAction();

	exit;
}
/*
$wgTitle = $mediaWiki->checkInitialQueries( $title,$action,$wgOut, $wgRequest, $wgContLang );
if ($wgTitle == NULL) {
	unset( $wgTitle );
}

wfProfileOut( 'main-misc-setup' );

# Setting global variables in mediaWiki
$mediaWiki->setVal( 'Server', $wgServer );
$mediaWiki->setVal( 'DisableInternalSearch', $wgDisableInternalSearch );
$mediaWiki->setVal( 'action', $action );
$mediaWiki->setVal( 'SquidMaxage', $wgSquidMaxage );
$mediaWiki->setVal( 'EnableDublinCoreRdf', $wgEnableDublinCoreRdf );
$mediaWiki->setVal( 'EnableCreativeCommonsRdf', $wgEnableCreativeCommonsRdf );
$mediaWiki->setVal( 'CommandLineMode', $wgCommandLineMode );
$mediaWiki->setVal( 'UseExternalEditor', $wgUseExternalEditor );
$mediaWiki->setVal( 'DisabledActions', $wgDisabledActions );


$wgArticle = $mediaWiki->initialize ( $wgTitle, $wgOut, $wgUser, $wgRequest );
$mediaWiki->finalCleanup ( $wgDeferredUpdateList, $wgLoadBalancer, $wgOut );

# Not sure when $wgPostCommitUpdateList gets set, so I keep this separate from finalCleanup
$mediaWiki->doUpdates( $wgPostCommitUpdateList );

$mediaWiki->restInPeace( $wgLoadBalancer );
*/
?>

<?php



// very useful for debugging:
$wgShowExceptionDetails = true;

include_once("includes/Parser.php");
if (empty($argv[1]))
  $argv[1] = "intro.wikimarkup";


if ($argv[1] == "-") {
  $markup = "";
  while (!feof(STDIN))
    $markup .= fread(STDIN, 16384);
} else
  $markup = file_get_contents($argv[1]);

// title is in first line, extract it
$newlineidx = strpos($markup, "\n");
$articletitle = trim(substr($markup, 0, $newlineidx));
$markup = substr($markup, $newlineidx + 1);

$p = new Parser();

// hook up Cite extension
$p->setHook( 'ref' , array( &$GLOBALS['hackCiteObj'], 'ref' ) );
$p->setHook( 'references' , array( &$GLOBALS['hackCiteObj'], 'references' ) );
$wgHooks['ParserClearState'][] = array( &$GLOBALS['hackCiteObj'], 'clearState' );

// hook up ParserFunctions extension
//wfParserFunctionsLanguageGetMagic($magicwords, "de");
$wgExtParserFunctions = new ExtParserFunctions();
// these lines are modified not to use references to $wgExtParserFunctions, as
// I got problems on php5.1.6
$p->setFunctionHook( 'expr', array( $wgExtParserFunctions, 'expr' ) );
$p->setFunctionHook( 'if', array( $wgExtParserFunctions, 'ifHook' ) );
$p->setFunctionHook( 'ifeq', array( $wgExtParserFunctions, 'ifeq' ) );
$p->setFunctionHook( 'ifexpr', array( $wgExtParserFunctions, 'ifexpr' ) );
$p->setFunctionHook( 'switch', array( $wgExtParserFunctions, 'switchHook' ) );
$p->setFunctionHook( 'ifexist', array( $wgExtParserFunctions, 'ifexist' ) );
$p->setFunctionHook( 'time', array( $wgExtParserFunctions, 'time' ) );
$p->setFunctionHook( 'rel2abs', array( $wgExtParserFunctions, 'rel2abs' ) );
//echo "<pre>p.mFH=";print_r($p->mFunctionHooks['if']); echo"</pre>";
$wgMessageCache->addMessage( 'pfunc_time_error', "Error: invalid time" );
$wgMessageCache->addMessage( 'pfunc_time_too_long', "Error: too many #time calls" );
$wgMessageCache->addMessage( 'pfunc_rel2abs_invalid_depth', "Error: Invalid depth in path: \"$1\" (tried to access a node above the root node)" );
// this does not work for some reason...
//$wgHooks['ParserClearState'][] = array( $wgExtParserFunctions, 'clearState' );


$title = new Title();
$options = new ParserOptions(null); // 1st arg: $user
$options->setUseTeX(true);
$options->setEditSection(false);
//require('skins/CologneBlue.php');
require('skins/Simple.php');
$options->setSkin(new SkinSimple());
// output_obj is of type ParserOutput
$output_obj = $p->parse($markup, $title, $options);
$text = $output_obj->getText();
// http://localhost/~felix/mediawiki_sa/index.php?title=Wolf&action=edit
$text = preg_replace("/scripts\/index\.php\?title=([^&]*)&amp;action=edit/", "article/$1/", $text);
// /%7Efelix/mediawiki_sa/index.php/Mammal"
$text = preg_replace("/scripts\/index\.php\/([^\"]*)/", "article/$1/", $text); #woc

//$out = mb_convert_encoding($text, "UTF-8", "UTF-8");

//$out = iconv("UTF-8", "UTF-8//IGNORE", $text);
// iconv is not necessary any more
$out = $text;

//$out = utf8_decode($text);
//$out = utf8_encode($out);

// send through tidy
/*
tidy_set_encoding('utf8');
tidy_setopt('output-xhtml', true);
tidy_setopt('add-xml-decl', true);
tidy_setopt('show-errors', true);
tidy_setopt('show-warnings', true);
tidy_setopt('indent', true);
tidy_setopt('clean-output', true);
tidy_setopt('clean', true);
tidy_setopt('input-encoding', 'utf8');
tidy_setopt('output-encoding', 'utf8');
tidy_parse_string($text);
tidy_clean_repair();
// TODO: tidy_error_count()
$out = tidy_get_output();
*/


/* this would be possible with the php5 pear package
$tidy_config = array('output-xhtml' => true);
$tidy = new tidy;
$tidy->parseString($text, $config, 'utf8');
$tidy->cleanRepair();
echo $tidy;
*/

//echo "<hr><pre>images: "; print_r($output_obj->getImages()); echo "</pre>";
//echo "<hr><pre>language links: "; print_r($output_obj->getLanguageLinks()); echo "</pre>";
//echo "<hr><pre>category links: "; print_r($output_obj->getCategoryLinks()); echo "</pre>";
//echo "<hr><pre>title text: "; print_r($output_obj->getTitleText()); echo "</pre>";
//echo "<hr><pre>external links: "; print_r($output_obj->getExternalLinks()); echo "</pre>";

?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <meta content="text/html; charset=utf-8" http-equiv="Content-Type">
    <title><?php echo $articletitle ?></title>
  </head>
<body>
  <?php echo $out; ?>
</body>
</html>
