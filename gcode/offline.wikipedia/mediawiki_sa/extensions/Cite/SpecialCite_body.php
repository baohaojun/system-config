<?php
if ( !defined( 'MEDIAWIKI' ) ) die();

global $wgContLang, $wgContLanguageCode, $wgCiteDefaultText;

$dir = dirname( __FILE__ ) . DIRECTORY_SEPARATOR;
$code = $wgContLang->lc( $wgContLanguageCode );
$file = file_exists( "${dir}cite_text-$code" ) ? "${dir}cite_text-$code" : "${dir}cite_text";
$wgCiteDefaultText = file_get_contents( $file );

class SpecialCite extends SpecialPage {
	function __construct() {
		parent::__construct( 'Cite' );
	}

	function execute( $par ) {
		global $wgRequest, $wgUseTidy;
		wfLoadExtensionMessages( 'SpecialCite' );

		// Having tidy on causes whitespace and <pre> tags to
		// be generated around the output of the CiteOutput
		// class TODO FIXME.
		$wgUseTidy = false;

		$this->setHeaders();
		$this->outputHeader();

		$page = isset( $par ) ? $par : $wgRequest->getText( 'page' );
		$id = $wgRequest->getInt( 'id' );

		$title = Title::newFromText( $page );
		if ( $title ) {
			$article = new Article( $title );
		}
		$cform = new CiteForm( $title );

		if ( !$title || ! $article->exists() )
			$cform->execute();
		else {
			$cform->execute();

			$cout = new CiteOutput( $title, $article, $id );
			$cout->execute();
		}
	}
}

class CiteForm {
	var $mTitle;

	function __construct( &$title ) {
		$this->mTitle =& $title;
	}

	function execute() {
		global $wgOut, $wgTitle;

		$wgOut->addHTML(
			Xml::element( 'form',
				array(
					'id' => 'specialcite',
					'method' => 'get',
					'action' => $wgTitle->escapeLocalUrl()
				),
				null
			) .
				Xml::openElement( 'label' ) .
					wfMsgHtml( 'cite_page' ) . ' ' .
					Xml::element( 'input',
						array(
							'type' => 'text',
							'size' => 30,
							'name' => 'page',
							'value' => is_object( $this->mTitle ) ? $this->mTitle->getPrefixedText() : ''
						),
						''
					) .
					' ' .
					Xml::element( 'input',
						array(
							'type' => 'submit',
							'value' => wfMsgHtml( 'cite_submit' )
						),
						''
					) .
				Xml::closeElement( 'label' ) .
			Xml::closeElement( 'form' )
		);
	}

}

class CiteOutput {
	var $mTitle, $mArticle, $mId;
	var $mParser, $mParserOptions;

	function __construct( &$title, &$article, $id ) {
		global $wgHooks, $wgParser;

		$this->mTitle =& $title;
		$this->mArticle =& $article;
		$this->mId = $id;

		$wgHooks['ParserGetVariableValueVarCache'][] = array( $this, 'varCache' );

		$this->genParserOptions();
		$this->genParser();

		$wgParser->setHook( 'citation', array( $this, 'CiteParse' ) );
	}

	function execute() {
		global $wgOut, $wgParser, $wgHooks, $wgCiteDefaultText;

		$wgHooks['ParserGetVariableValueTs'][] = array( $this, 'timestamp' );

		$msg = wfMsgForContentNoTrans( 'cite_text' );
		if ( $msg == '' ) {
			$msg = $wgCiteDefaultText;
		}
		$this->mArticle->fetchContent( $this->mId, false );
		$ret = $wgParser->parse( $msg, $this->mTitle, $this->mParserOptions, false, true, $this->mArticle->getRevIdFetched() );
		$wgOut->addHTML( $ret->getText() );
	}

	function genParserOptions() {
		global $wgUser;
		$this->mParserOptions = ParserOptions::newFromUser( $wgUser );
		$this->mParserOptions->setDateFormat( MW_DATE_DEFAULT );
		$this->mParserOptions->setEditSection( false );
	}

	function genParser() {
		$this->mParser = new Parser;
	}

	function CiteParse( $in, $argv ) {
		global $wgTitle;

		$ret = $this->mParser->parse( $in, $wgTitle, $this->mParserOptions, false );

		return $ret->getText();
	}

	function varCache() { return false; }

	function timestamp( &$parser, &$ts ) {
		if ( isset( $parser->mTagHooks['citation'] ) )
			$ts = wfTimestamp( TS_UNIX, $this->mArticle->getTimestamp() );

		return true;
	}
}
