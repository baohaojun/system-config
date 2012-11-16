<?php

class SyntaxHighlight_GeSHi {

	/**
	 * Has GeSHi been initialised this session?
	 */
	private static $initialised = false;

	/**
	 * List of languages available to GeSHi
	 */
	private static $languages = null;

	/**
	 * Parser hook
	 *
	 * @param string $text
	 * @param array $args
	 * @param Parser $parser
	 * @return string
	 */
	public static function parserHook( $text, $args = array(), $parser ) {
		global $wgSyntaxHighlightDefaultLang;
		wfProfileIn( __METHOD__ );
		self::initialise();
		$text = rtrim( $text );
		// Don't trim leading spaces away, just the linefeeds
		$text = preg_replace( '/^\n+/', '', $text );
		// Validate language
		if( isset( $args['lang'] ) && $args['lang'] ) {
			$lang = $args['lang'];
		} else {
			// language is not specified. Check if default exists, if yes, use it.
			if ( !is_null( $wgSyntaxHighlightDefaultLang ) ) {
				$lang = $wgSyntaxHighlightDefaultLang;
			} else {
				$error = self::formatLanguageError( $text );
				wfProfileOut( __METHOD__ );
				return $error;
			}
		}
		$lang = strtolower( $lang );
		if( !preg_match( '/^[a-z_0-9-]*$/', $lang ) ) {
			$error = self::formatLanguageError( $text );
			wfProfileOut( __METHOD__ );
			return $error;
		}
		$geshi = self::prepare( $text, $lang );
		if( !$geshi instanceof GeSHi ) {
			$error = self::formatLanguageError( $text );
			wfProfileOut( __METHOD__ );
			return $error;
		}

		$enclose = self::getEncloseType( $args );

		// Line numbers
		if( isset( $args['line'] ) ) {
			$geshi->enable_line_numbers( GESHI_FANCY_LINE_NUMBERS );
		}
		// Highlighting specific lines
		if( isset( $args['highlight'] ) ) {
			$lines = self::parseHighlightLines( $args['highlight'] );
			if ( count($lines) ) {
				$geshi->highlight_lines_extra( $lines );
			}
		}
		// Starting line number
		if( isset( $args['start'] ) ) {
			$geshi->start_line_numbers_at( $args['start'] );
		}
		$geshi->set_header_type( $enclose );
		// Strict mode
		if( isset( $args['strict'] ) ) {
			$geshi->enable_strict_mode();
		}
		// Format
		$out = $geshi->parse_code();
		if ( $geshi->error == GESHI_ERROR_NO_SUCH_LANG ) {
			// Common error :D
			$error = self::formatLanguageError( $text );
			wfProfileOut( __METHOD__ );
			return $error;
		}
		$err = $geshi->error();
		if( $err ) {
			// Other unknown error!
			$error = self::formatError( $err );
			wfProfileOut( __METHOD__ );
			return $error;
		}
		// Armour for Parser::doBlockLevels()
		if( $enclose === GESHI_HEADER_DIV )
			$out = str_replace( "\n", '', $out );
		// Register CSS
		$parser->mOutput->addHeadItem( self::buildHeadItem( $geshi ), "source-{$lang}" );

		$encloseTag = $enclose === GESHI_HEADER_NONE ? 'span' : 'div';
		$attribs = Sanitizer::validateTagAttributes( $args, $encloseTag );

		//lang is valid in HTML context, but also used on GeSHi
		unset( $attribs['lang'] );

		if ( $enclose === GESHI_HEADER_NONE ) {
			$attribs = self::addAttribute( $attribs, 'class', 'mw-geshi ' . $lang . ' source-' . $lang );
		} else {
			if ( !isset( $attribs['dir'] ) ) {
				$attribs = self::addAttribute( $attribs, 'dir', 'ltr' );
			}

			$attribs = self::addAttribute( $attribs, 'class', 'mw-geshi' );
			$attribs = self::addAttribute( $attribs, 'style', 'text-align: left;' );
		}
		$out = Xml::tags( $encloseTag, $attribs, $out );

		wfProfileOut( __METHOD__ );
		return $out;
	}

	private static function addAttribute( $attribs, $name, $value ) {
		if( isset( $attribs[$name] ) ) {
			$attribs[$name] = $value . ' ' . $attribs[$name];
		} else {
			$attribs[$name] = $value;
		}
		return $attribs;
	}

	/**
	 * Take an input specifying a list of lines to highlight, returning
	 * a raw list of matching line numbers.
	 *
	 * Input is comma-separated list of lines or line ranges.
	 *
	 * @input string
	 * @return array of ints
	 */
	protected static function parseHighlightLines( $arg ) {
		$lines = array();
		$values = array_map( 'trim', explode( ',', $arg ) );
		foreach ( $values as $value ) {
			if ( ctype_digit($value) ) {
				$lines[] = (int) $value;
			} elseif ( strpos( $value, '-' ) !== false ) {
				list( $start, $end ) = array_map( 'trim', explode( '-', $value ) );
				if ( self::validHighlightRange( $start, $end ) ) {
					for ($i = intval( $start ); $i <= $end; $i++ ) {
						$lines[] = $i;
					}
				} else {
					wfDebugLog( 'geshi', "Invalid range: $value\n" );
				}
			} else {
				wfDebugLog( 'geshi', "Invalid line: $value\n" );
			}
		}
		return $lines;
	}

	/**
	 * Validate a provided input range
	 */
	protected static function validHighlightRange( $start, $end ) {
		// Since we're taking this tiny range and producing a an
		// array of every integer between them, it would be trivial
		// to DoS the system by asking for a huge range.
		// Impose an arbitrary limit on the number of lines in a
		// given range to reduce the impact.
		$arbitrarilyLargeConstant = 10000;
		return
			ctype_digit($start) &&
			ctype_digit($end) &&
			$start > 0 &&
			$start < $end &&
			$end - $start < $arbitrarilyLargeConstant;
	}

	static function getEncloseType( $args ) {
		// Since version 1.0.8 geshi can produce valid pre, but we need to check for it
		if ( defined('GESHI_HEADER_PRE_VALID') ) {
			$pre = GESHI_HEADER_PRE_VALID;
		} else {
			$pre = GESHI_HEADER_PRE;
		}

		// "Enclose" parameter
		$enclose = $pre;
		if ( isset( $args['enclose'] ) ) {
			if ( $args['enclose'] === 'div' ) {
				$enclose = GESHI_HEADER_DIV;
			} elseif ( $args['enclose'] === 'none' ) {
				$enclose = GESHI_HEADER_NONE;
			}
		}

		if( isset( $args['line'] ) && $pre === GESHI_HEADER_PRE ) {
			// Force <div> mode to maintain valid XHTML, see
			// http://sourceforge.net/tracker/index.php?func=detail&aid=1201963&group_id=114997&atid=670231
			$enclose = GESHI_HEADER_DIV;
		}

		return $enclose;
	}

	/**
	 * Hook into Article::view() to provide syntax highlighting for
	 * custom CSS and JavaScript pages
	 *
	 * @param string $text
	 * @param Title $title
	 * @param OutputPage $output
	 * @return bool
	 */
	public static function viewHook( $text, $title, $output ) {
		// Determine the language
		$matches = array();
		preg_match( '!\.(css|js)$!u', $title->getText(), $matches );
		$lang = $matches[1] == 'css' ? 'css' : 'javascript';
		// Attempt to format
		$geshi = self::prepare( $text, $lang );
		if( $geshi instanceof GeSHi ) {
			$out = $geshi->parse_code();
			if( !$geshi->error() ) {
				// Done
				$output->addHeadItem( "source-$lang", self::buildHeadItem( $geshi ) );
				$output->addHTML( "<div dir=\"ltr\">{$out}</div>" );
				return false;
			}
		}
		// Bottle out
		return true;
	}

	/**
	 * Initialise a GeSHi object to format some code, performing
	 * common setup for all our uses of it
	 *
	 * @param string $text
	 * @param string $lang
	 * @return GeSHi
	 */
	private static function prepare( $text, $lang ) {
		self::initialise();
		$geshi = new GeSHi( $text, $lang );
		if( $geshi->error() == GESHI_ERROR_NO_SUCH_LANG ) {
			return null;
		}
		$geshi->set_encoding( 'UTF-8' );
		$geshi->enable_classes();
		$geshi->set_overall_class( "source-$lang" );
		$geshi->enable_keyword_links( false );
		return $geshi;
	}

	/**
	 * Prepare a CSS snippet suitable for use as a ParserOutput/OutputPage
	 * head item
	 *
	 * @param GeSHi $geshi
	 * @return string
	 */
	private static function buildHeadItem( $geshi ) {
		global $wgUseSiteCss, $wgSquidMaxage;
		$lang = $geshi->language;
		$css = array();
		$css[] = '<style type="text/css">/*<![CDATA[*/';
		$css[] = ".source-$lang {line-height: normal;}";
		$css[] = ".source-$lang li, .source-$lang pre {";
		$css[] = "\tline-height: normal; border: 0px none white;";
		$css[] = "}";
		$css[] = $geshi->get_stylesheet( false );
		$css[] = '/*]]>*/';
		$css[] = '</style>';
		if( $wgUseSiteCss ) {
			$title = Title::makeTitle( NS_MEDIAWIKI, 'Geshi.css' );
			$q = "usemsgcache=yes&action=raw&ctype=text/css&smaxage={$wgSquidMaxage}";
			$css[] = '<style type="text/css">/*<![CDATA[*/';
			$css[] = '@import "' . $title->getLocalUrl( $q ) . '";';
			$css[] = '/*]]>*/';
			$css[] = '</style>';
		}
		return implode( "\n", $css );
	}

	/**
	 * Format an 'unknown language' error message and append formatted
	 * plain text to it.
	 *
	 * @param string $text
	 * @return string HTML fragment
	 */
	private static function formatLanguageError( $text ) {
		$error = self::formatError( htmlspecialchars( wfMsgForContent( 'syntaxhighlight-err-language' ) ), $text );
		return $error . '<pre>' . htmlspecialchars( $text ) . '</pre>';
	}

	/**
	 * Format an error message
	 *
	 * @param string $error
	 * @return string
	 */
	private static function formatError( $error = '' ) {
		$html = '';
		if( $error ) {
			$html .= "<p>{$error}</p>";
		}
		$html .= '<p>' . htmlspecialchars( wfMsgForContent( 'syntaxhighlight-specify' ) )
			. ' <samp>&lt;source lang=&quot;html4strict&quot;&gt;...&lt;/source&gt;</samp></p>'
			. '<p>' . htmlspecialchars( wfMsgForContent( 'syntaxhighlight-supported' ) ) . '</p>'
			. self::formatLanguages();
		return "<div style=\"border: solid red 1px; padding: .5em;\">{$html}</div>";
	}

	/**
	 * Format the list of supported languages
	 *
	 * @return string
	 */
	private static function formatLanguages() {
		$langs = self::getSupportedLanguages();
		$list = array();
		if( count( $langs ) > 0 ) {
			foreach( $langs as $lang ) {
				$list[] = '<samp>' . htmlspecialchars( $lang ) . '</samp>';
			}
			return '<p class="mw-collapsible mw-collapsed" style="padding: 0em 1em;">' . implode( ', ', $list ) . '</p><br style="clear: all"/>';
		} else {
			return '<p>' . htmlspecialchars( wfMsgForContent( 'syntaxhighlight-err-loading' ) ) . '</p>';
		}
	}

	/**
	 * Get the list of supported languages
	 *
	 * @return array
	 */
	private static function getSupportedLanguages() {
		if( !is_array( self::$languages ) ) {
			self::initialise();
			self::$languages = array();
			foreach( glob( GESHI_LANG_ROOT . "/*.php" ) as $file ) {
				self::$languages[] = basename( $file, '.php' );
			}
			sort( self::$languages );
		}
		return self::$languages;
	}

	/**
	 * Initialise messages and ensure the GeSHi class is loaded
	 */
	private static function initialise() {
		if( !self::$initialised ) {
			if( !class_exists( 'GeSHi' ) ) {
				require( 'geshi/geshi.php' );
			}
			self::$initialised = true;
		}
		return true;
	}

	/**
	 * Get the GeSHI's version information while Special:Version is read.
	 */
	public static function hSpecialVersion_GeSHi( &$extensionTypes ) {
		global $wgExtensionCredits;
		self::initialise();
		$wgExtensionCredits['parserhook']['SyntaxHighlight_GeSHi']['version'] = GESHI_VERSION;
		return true;
	}

	/**
	 * @see SyntaxHighlight_GeSHi::hSpecialVersion_GeSHi
	 */
	public static function hOldSpecialVersion_GeSHi( &$sp, &$extensionTypes ) {
		return self::hSpecialVersion_GeSHi( $extensionTypes );
	}
	
}