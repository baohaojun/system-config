<?php

if ( !defined( 'MEDIAWIKI' ) ) {
	die( 'This file is a MediaWiki extension, it is not a valid entry point' );
}

/**
 * CONFIGURATION
 * These variables may be overridden in LocalSettings.php after you include the
 * extension file.
 */

/**
 * Defines the maximum length of a string that string functions are allowed to operate on
 * Prevention against denial of service by string function abuses.
 */
$wgPFStringLengthLimit = 1000;

/**
 * Enable string functions.
 *
 * Set this to true if you want your users to be able to implement their own
 * parsers in the ugliest, most inefficient programming language known to man:
 * MediaWiki wikitext with ParserFunctions.
 *
 * WARNING: enabling this may have an adverse impact on the sanity of your users.
 * An alternative, saner solution for embedding complex text processing in
 * MediaWiki templates can be found at: http://www.mediawiki.org/wiki/Extension:Lua
 */
$wgPFEnableStringFunctions = false;

/** REGISTRATION */
$wgExtensionFunctions[] = 'wfSetupParserFunctions';
$wgExtensionCredits['parserhook'][] = array(
	'path' => __FILE__,
	'name' => 'ParserFunctions',
	'version' => '1.4.0',
	'url' => 'http://www.mediawiki.org/wiki/Extension:ParserFunctions',
	'author' => array( 'Tim Starling', 'Robert Rohde', 'Ross McClure', 'Juraj Simlovic' ),
	'descriptionmsg' => 'pfunc_desc',
);

$wgAutoloadClasses['ExtParserFunctions'] = dirname( __FILE__ ) . '/ParserFunctions_body.php';
$wgExtensionMessagesFiles['ParserFunctions'] = dirname( __FILE__ ) . '/ParserFunctions.i18n.php';
$wgExtensionMessagesFiles['ParserFunctionsMagic'] = dirname( __FILE__ ) . '/ParserFunctions.i18n.magic.php';

$wgParserTestFiles[] = dirname( __FILE__ ) . "/funcsParserTests.txt";
$wgParserTestFiles[] = dirname( __FILE__ ) . "/stringFunctionTests.txt";

function wfSetupParserFunctions() {
	global $wgPFHookStub, $wgHooks;

	$wgPFHookStub = new ParserFunctions_HookStub;

	$wgHooks['ParserFirstCallInit'][] = array( &$wgPFHookStub, 'registerParser' );

	$wgHooks['ParserClearState'][] = array( &$wgPFHookStub, 'clearState' );
}

/**
 * Stub class to defer loading of the bulk of the code until a parser function is
 * actually used.
 */
class ParserFunctions_HookStub {
	var $realObj;

	function registerParser( $parser ) {
		global $wgPFEnableStringFunctions;

		if ( defined( get_class( $parser ) . '::SFH_OBJECT_ARGS' ) ) {
			// These functions accept DOM-style arguments
			$parser->setFunctionHook( 'if', array( &$this, 'ifObj' ), SFH_OBJECT_ARGS );
			$parser->setFunctionHook( 'ifeq', array( &$this, 'ifeqObj' ), SFH_OBJECT_ARGS );
			$parser->setFunctionHook( 'switch', array( &$this, 'switchObj' ), SFH_OBJECT_ARGS );
			$parser->setFunctionHook( 'ifexist', array( &$this, 'ifexistObj' ), SFH_OBJECT_ARGS );
			$parser->setFunctionHook( 'ifexpr', array( &$this, 'ifexprObj' ), SFH_OBJECT_ARGS );
			$parser->setFunctionHook( 'iferror', array( &$this, 'iferrorObj' ), SFH_OBJECT_ARGS );
		} else {
			$parser->setFunctionHook( 'if', array( &$this, 'ifHook' ) );
			$parser->setFunctionHook( 'ifeq', array( &$this, 'ifeq' ) );
			$parser->setFunctionHook( 'switch', array( &$this, 'switchHook' ) );
			$parser->setFunctionHook( 'ifexist', array( &$this, 'ifexist' ) );
			$parser->setFunctionHook( 'ifexpr', array( &$this, 'ifexpr' ) );
			$parser->setFunctionHook( 'iferror', array( &$this, 'iferror' ) );
		}

		$parser->setFunctionHook( 'expr', array( &$this, 'expr' ) );
		$parser->setFunctionHook( 'time', array( &$this, 'time' ) );
		$parser->setFunctionHook( 'timel', array( &$this, 'localTime' ) );
		$parser->setFunctionHook( 'rel2abs', array( &$this, 'rel2abs' ) );
		$parser->setFunctionHook( 'titleparts', array( &$this, 'titleparts' ) );

		// String Functions
		if ( $wgPFEnableStringFunctions ) {
			$parser->setFunctionHook( 'len',       array( &$this, 'runLen'       ) );
			$parser->setFunctionHook( 'pos',       array( &$this, 'runPos'       ) );
			$parser->setFunctionHook( 'rpos',      array( &$this, 'runRPos'      ) );
			$parser->setFunctionHook( 'sub',       array( &$this, 'runSub'       ) );
			$parser->setFunctionHook( 'count',     array( &$this, 'runCount'     ) );
			$parser->setFunctionHook( 'replace',   array( &$this, 'runReplace'   ) );
			$parser->setFunctionHook( 'explode',   array( &$this, 'runExplode'   ) );
			$parser->setFunctionHook( 'urldecode', array( &$this, 'runUrlDecode' ) );
		}

		return true;
	}

	/** Defer ParserClearState */
	function clearState( $parser ) {
		if ( !is_null( $this->realObj ) ) {
			$this->realObj->clearState( $parser );
		}
		return true;
	}

	/** Pass through function call */
	function __call( $name, $args ) {
		if ( is_null( $this->realObj ) ) {
			$this->realObj = new ExtParserFunctions;
			$this->realObj->clearState( $args[0] );
		}
		return call_user_func_array( array( $this->realObj, $name ), $args );
	}
}
