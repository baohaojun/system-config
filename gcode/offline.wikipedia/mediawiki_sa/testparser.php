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
$wgDebugLogGroups  = array(
    'bhj'	=> '/dev/stderr',
);

// load ParserFunctions extension
if( file_exists("$IP/StartProfiler.php") ) {
	require_once( "$IP/StartProfiler.php" );
} else {
	require_once( "$IP/includes/ProfilerStub.php" );
}

# Prepare MediaWiki
require_once( 'includes/Setup.php' );
require_once("extensions/ParserFunctions/ParserFunctions.php");
require_once( "extensions/Cite/Cite.php" );
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

?>

<?php

$wgShowExceptionDetails = true;

include_once("includes/parser/Parser.php");
if (empty($argv[1]))
  $argv[1] = "intro.wikimarkup";


if ($argv[1] == "-") {
  $markup = "";
  while (!feof(STDIN))
    $markup .= fread(STDIN, 16384);
} else
  $markup = file_get_contents($argv[1]);

$newlineidx = strpos($markup, "\n");
$articletitle = trim(substr($markup, 0, $newlineidx));
$markup = substr($markup, $newlineidx + 1);

$p = new Parser();

$p->setHook( 'ref' , array( &$GLOBALS['hackCiteObj'], 'ref' ) );
$p->setHook( 'references' , array( &$GLOBALS['hackCiteObj'], 'references' ) );
$wgHooks['ParserClearState'][] = array( &$GLOBALS['hackCiteObj'], 'clearState' );

wfSetupParserFunctions();

$wgMessageCache->addMessage( 'pfunc_time_error', "Error: invalid time" );
$wgMessageCache->addMessage( 'pfunc_time_too_long', "Error: too many #time calls" );
$wgMessageCache->addMessage( 'pfunc_rel2abs_invalid_depth', "Error: Invalid depth in path: \"$1\" (tried to access a node above the root node)" );


$title = Title::newFromText($articletitle);
$options = new ParserOptions(null); // 1st arg: $user
$options->setEditSection(false);
require('skins/Simple.php');
$options->setSkin(new SkinSimple());
$output_obj = $p->parse($markup, $title, $options);
$out_page = new OutputPage;
$out_page->addParserOutputNoText( $output_obj );

$text = $output_obj->getText();
$text = preg_replace("/scripts\/index\.php\?title=([^&]*)&amp;action=edit/", "article/$1/", $text);
$text = preg_replace("/scripts\/index\.php\/([^\"]*)/", "article/$1/", $text); #woc

$out = $text;

?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html class="linux firefox firefox3 gecko gecko1" dir="ltr" xmlns="http://www.w3.org/1999/xhtml" lang="en"><head>

<?php echo "<title>" . htmlspecialchars($articletitle) . " - Semi-Offline Wikipedia, the free encyclopedia</title>"; ?>
<?php echo $out_page->getScript(); ?>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<meta http-equiv="Content-Style-Type" content="text/css">
<meta name="generator" content="MediaWiki 1.16wmf4">
<link rel="canonical" href="http://en.wikipedia.org/wiki/Mathematics">
<link rel="apple-touch-icon" href="http://en.wikipedia.org/apple-touch-icon.png">
<link rel="shortcut icon" href="http://en.wikipedia.org/favicon.ico">
<link rel="search" type="application/opensearchdescription+xml" href="http://en.wikipedia.org/w/opensearch_desc.php" title="Wikipedia (en)">
<link rel="copyright" href="http://creativecommons.org/licenses/by-sa/3.0/">
<link rel="alternate" type="application/atom+xml" title="Wikipedia Atom feed" href="http://en.wikipedia.org/w/index.php?title=Special:RecentChanges&amp;feed=atom">
<link rel="stylesheet" href="/scripts/images/presentation/main-ltr.css" type="text/css" media="screen">
<link rel="stylesheet" href="/scripts/images/presentation/shared.css" type="text/css" media="screen">
<link rel="stylesheet" href="/scripts/images/presentation/commonPrint.css" type="text/css" media="print">
<link rel="stylesheet" href="/scripts/images/presentation/combined.css" type="text/css" media="all">
<link rel="stylesheet" href="/scripts/images/presentation/jquery-ui-1.css" type="text/css" media="all">
<link rel="stylesheet" href="/scripts/images/presentation/index_004.css" type="text/css" media="all">
<link rel="stylesheet" href="/scripts/images/presentation/index_005.css" type="text/css" media="print">
<link rel="stylesheet" href="/scripts/images/presentation/index_002.css" type="text/css" media="handheld">
<link rel="stylesheet" href="/scripts/images/presentation/index_003.css" type="text/css" media="all">
<link rel="stylesheet" href="/scripts/images/presentation/index.css" type="text/css" media="all">
<script type="text/javascript">
var skin="vector",
stylepath="http://bits.wikimedia.org/skins-1.5",
wgUrlProtocols="http\\:\\/\\/|https\\:\\/\\/|ftp\\:\\/\\/|irc\\:\\/\\/|gopher\\:\\/\\/|telnet\\:\\/\\/|nntp\\:\\/\\/|worldwind\\:\\/\\/|mailto\\:|news\\:|svn\\:\\/\\/",
wgArticlePath="/wiki/$1",
wgScriptPath="/w",
wgScriptExtension=".php",
wgScript="/w/index.php",
wgVariantArticlePath=false,
wgActionPaths={},
wgServer="http://en.wikipedia.org",
wgCanonicalNamespace="",
wgCanonicalSpecialPageName=false,
wgNamespaceNumber=0,
wgPageName="Mathematics",
wgTitle="Mathematics",
wgAction="view",
wgArticleId=18831,
wgIsArticle=true,
wgUserName=null,
wgUserGroups=null,
wgUserLanguage="en",
wgContentLanguage="en",
wgBreakFrames=false,
wgCurRevisionId=391883248,
wgVersion="1.16wmf4",
wgEnableAPI=true,
wgEnableWriteAPI=true,
wgSeparatorTransformTable=["", ""],
wgDigitTransformTable=["", ""],
wgMainPageTitle="Main Page",
wgFormattedNamespaces={"-2": "Media", "-1": "Special", "0": "", "1": "Talk", "2": "User", "3": "User talk", "4": "Wikipedia", "5": "Wikipedia talk", "6": "File", "7": "File talk", "8": "MediaWiki", "9": "MediaWiki talk", "10": "Template", "11": "Template talk", "12": "Help", "13": "Help talk", "14": "Category", "15": "Category talk", "100": "Portal", "101": "Portal talk", "108": "Book", "109": "Book talk"},
wgNamespaceIds={"media": -2, "special": -1, "": 0, "talk": 1, "user": 2, "user_talk": 3, "wikipedia": 4, "wikipedia_talk": 5, "file": 6, "file_talk": 7, "mediawiki": 8, "mediawiki_talk": 9, "template": 10, "template_talk": 11, "help": 12, "help_talk": 13, "category": 14, "category_talk": 15, "portal": 100, "portal_talk": 101, "book": 108, "book_talk": 109, "wp": 4, "wt": 5, "image": 6, "image_talk": 7},
wgSiteName="Wikipedia",
wgCategories=["Wikipedia semi-protected pages", "Articles containing Greek language text", "Articles containing Ancient Greek language text", "Articles containing Latin language text", "Articles containing French language text", "All articles with unsourced statements", "Articles with unsourced statements from August 2009", "All articles with specifically marked weasel-worded phrases", "Articles with specifically marked weasel-worded phrases from August 2009", "Articles with unsourced statements from October 2010", "Mathematics", "Mathematical sciences", "Formal sciences", "Greek loanwords"],
wgDBname="enwiki",
wgMWSuggestTemplate="http://en.wikipedia.org/w/api.php?action=opensearch\x26search={searchTerms}\x26namespace={namespaces}\x26suggest",
wgSearchNamespaces=[0],
wgMWSuggestMessages=["with suggestions", "no suggestions"],
wgRestrictionEdit=["autoconfirmed"],
wgRestrictionMove=["sysop"],
wgFlaggedRevsParams={"tags": {"status": {"levels": 1, "quality": 2, "pristine": 3}}},
wgStableRevisionId=0,
wgWikimediaMobileUrl="http://en.m.wikipedia.org/wiki",
wgCollapsibleNavBucketTest=false,
wgCollapsibleNavForceNewVersion=false,
wgVectorPreferences={"collapsiblenav": {"enable": 1}, "editwarning": {"enable": 1}, "simplesearch": {"enable": 1, "disablesuggest": 0}},
wgVectorEnabledModules={"collapsiblenav": true, "collapsibletabs": true, "editwarning": true, "expandablesearch": false, "footercleanup": false, "simplesearch": true},
wgArticleAssessmentJUIPath="http://bits.wikimedia.org/w/extensions/UsabilityInitiative/js/js2stopgap/jui.combined.min.js",
Geo={"city": "", "country": ""},
wgNoticeProject="wikipedia";
</script><script src="/scripts/images/presentation/wikibits.js" type="text/javascript"></script>
<script type="text/javascript" src="/scripts/images/presentation/jquery.js"></script>
<script src="/scripts/images/presentation/ajax.js" type="text/javascript"></script>
<script src="/scripts/images/presentation/mwsuggest.js" type="text/javascript"></script>
<script src="/scripts/images/presentation/MobileRedirect.js" type="text/javascript"></script>
<script src="/scripts/images/presentation/plugins.js" type="text/javascript"></script>
<script src="/scripts/images/presentation/Vector.js" type="text/javascript"></script>
<script type="text/javascript">mw.usability.addMessages({'vector-collapsiblenav-more':'More languages','vector-editwarning-warning':'Leaving this page may cause you to lose any changes you have made.\nIf you are logged in, you can disable this warning in the \"Editing\" section of your preferences.','vector-simplesearch-search':'Search','vector-simplesearch-containing':'containing...'});</script>
<script src="/scripts/images/presentation/SpecialBannerController" type="text/javascript"></script><style type="text/css">
#centralNotice .siteNoticeSmall {display:none;}
#centralNotice.collapsed .siteNoticeBig {display:none;}
#centralNotice.collapsed .siteNoticeSmall {display:block;}
</style>

<!--[if lt IE 7]><style type="text/css">body{behavior:url("/w/skins-1.5/vector/csshover.htc")}</style><![endif]-->
<script src="/scripts/images/presentation/index_002.php" type="text/javascript"></script><script type="text/javascript" src="/scripts/images/presentation/index.php"></script>
</head>
<body  class="mediawiki ltr ns-0 ns-subject" style="margin: 5px; padding: 15px; ">
  <?php echo $out; ?>
</body>
</html>
