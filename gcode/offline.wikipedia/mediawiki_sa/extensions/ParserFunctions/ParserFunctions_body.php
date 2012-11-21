<?php

class ExtParserFunctions {
	var $mExprParser;
	var $mTimeCache = array();
	var $mTimeChars = 0;
	var $mMaxTimeChars = 6000; # ~10 seconds

	function clearState( $parser ) {
		$this->mTimeChars = 0;
		$parser->pf_ifexist_breakdown = array();
		$parser->pf_markerRegex = null;
		return true;
	}

	/**
	 * Get the marker regex. Cached.
	 */
	function getMarkerRegex( $parser ) {
		if ( isset( $parser->pf_markerRegex ) ) {
			return $parser->pf_markerRegex;
		}

		wfProfileIn( __METHOD__ );

		$prefix = preg_quote( $parser->uniqPrefix(), '/' );

		// The first line represents Parser from release 1.12 forward.
		// subsequent lines are hacks to accomodate old Mediawiki versions.
		if ( defined( 'Parser::MARKER_SUFFIX' ) )
			$suffix = preg_quote( Parser::MARKER_SUFFIX, '/' );
		elseif ( isset( $parser->mMarkerSuffix ) )
			$suffix = preg_quote( $parser->mMarkerSuffix, '/' );
		elseif ( defined( 'MW_PARSER_VERSION' ) &&
				strcmp( MW_PARSER_VERSION, '1.6.1' ) > 0 )
			$suffix = "QINU\x07";
		else $suffix = 'QINU';

		$parser->pf_markerRegex = '/' . $prefix . '(?:(?!' . $suffix . ').)*' . $suffix . '/us';

		wfProfileOut( __METHOD__ );
		return $parser->pf_markerRegex;
	}

	// Removes unique markers from passed parameters, used by string functions.
	private function killMarkers ( $parser, $text ) {
		return preg_replace( $this->getMarkerRegex( $parser ), '' , $text );
	}

	function &getExprParser() {
		if ( !isset( $this->mExprParser ) ) {
			if ( !class_exists( 'ExprParser' ) ) {
				require( dirname( __FILE__ ) . '/Expr.php' );
			}
			$this->mExprParser = new ExprParser;
		}
		return $this->mExprParser;
	}

	function expr( $parser, $expr = '' ) {
		try {
			return $this->getExprParser()->doExpression( $expr );
		} catch ( ExprError $e ) {
			return $e->getMessage();
		}
	}

	function ifexpr( $parser, $expr = '', $then = '', $else = '' ) {
		try {
			$ret = $this->getExprParser()->doExpression( $expr );
			if ( is_numeric( $ret ) ) {
				$ret = floatval( $ret );
			}
			if ( $ret ) {
				return $then;
			} else {
				return $else;
			}
		} catch ( ExprError $e ) {
			return $e->getMessage();
		}
	}

	function ifexprObj( $parser, $frame, $args ) {
		$expr = isset( $args[0] ) ? trim( $frame->expand( $args[0] ) ) : '';
		$then = isset( $args[1] ) ? $args[1] : '';
		$else = isset( $args[2] ) ? $args[2] : '';
		$result = $this->ifexpr( $parser, $expr, $then, $else );
		if ( is_object( $result ) ) {
			$result = trim( $frame->expand( $result ) );
		}
		return $result;
	}

	function ifHook( $parser, $test = '', $then = '', $else = '' ) {
		if ( $test !== '' ) {
			return $then;
		} else {
			return $else;
		}
	}

	function ifObj( $parser, $frame, $args ) {
		$test = isset( $args[0] ) ? trim( $frame->expand( $args[0] ) ) : '';
		if ( $test !== '' ) {
			return isset( $args[1] ) ? trim( $frame->expand( $args[1] ) ) : '';
		} else {
			return isset( $args[2] ) ? trim( $frame->expand( $args[2] ) ) : '';
		}
	}

	function ifeq( $parser, $left = '', $right = '', $then = '', $else = '' ) {
		if ( $left == $right ) {
			return $then;
		} else {
			return $else;
		}
	}

	function ifeqObj( $parser, $frame, $args ) {
		$left = isset( $args[0] ) ? trim( $frame->expand( $args[0] ) ) : '';
		$right = isset( $args[1] ) ? trim( $frame->expand( $args[1] ) ) : '';
		if ( $left == $right ) {
			return isset( $args[2] ) ? trim( $frame->expand( $args[2] ) ) : '';
		} else {
			return isset( $args[3] ) ? trim( $frame->expand( $args[3] ) ) : '';
		}
	}

	function iferror( $parser, $test = '', $then = '', $else = false ) {
		if ( preg_match( '/<(?:strong|span|p|div)\s(?:[^\s>]*\s+)*?class="(?:[^"\s>]*\s+)*?error(?:\s[^">]*)?"/', $test ) ) {
			return $then;
		} elseif ( $else === false ) {
			return $test;
		} else {
			return $else;
		}
	}

	function iferrorObj( $parser, $frame, $args ) {
		$test = isset( $args[0] ) ? trim( $frame->expand( $args[0] ) ) : '';
		$then = isset( $args[1] ) ? $args[1] : false;
		$else = isset( $args[2] ) ? $args[2] : false;
		$result = $this->iferror( $parser, $test, $then, $else );
		if ( $result === false ) {
			return '';
		} else {
			return trim( $frame->expand( $result ) );
		}
	}

	function switchHook( $parser /*,...*/ ) {
		$args = func_get_args();
		array_shift( $args );
		$primary = trim( array_shift( $args ) );
		$found = $defaultFound = false;
		$parts = null;
		$default = null;
		$mwDefault =& MagicWord::get( 'default' );
		foreach ( $args as $arg ) {
			$parts = array_map( 'trim', explode( '=', $arg, 2 ) );
			if ( count( $parts ) == 2 ) {
				# Found "="
				if ( $found || $parts[0] == $primary ) {
					# Found a match, return now
					return $parts[1];
				} elseif ( $defaultFound || $mwDefault->matchStartAndRemove( $parts[0] ) ) {
					$default = $parts[1];
				} # else wrong case, continue
			} elseif ( count( $parts ) == 1 ) {
				# Multiple input, single output
				# If the value matches, set a flag and continue
				if ( $parts[0] == $primary ) {
					$found = true;
				} elseif ( $mwDefault->matchStartAndRemove( $parts[0] ) ) {
					$defaultFound = true;
				}
			} # else RAM corruption due to cosmic ray?
		}
		# Default case
		# Check if the last item had no = sign, thus specifying the default case
		if ( count( $parts ) == 1 ) {
			return $parts[0];
		} elseif ( !is_null( $default ) ) {
			return $default;
		} else {
			return '';
		}
	}

	function switchObj( $parser, $frame, $args ) {
		if ( count( $args ) == 0 ) {
			return '';
		}
		$primary = trim( $frame->expand( array_shift( $args ) ) );
		$found = $defaultFound = false;
		$default = null;
		$lastItemHadNoEquals = false;
		$mwDefault =& MagicWord::get( 'default' );
		foreach ( $args as $arg ) {
			$bits = $arg->splitArg();
			$nameNode = $bits['name'];
			$index = $bits['index'];
			$valueNode = $bits['value'];

			if ( $index === '' ) {
				# Found "="
				$lastItemHadNoEquals = false;
				if ( $found ) {
					# Multiple input match
					return trim( $frame->expand( $valueNode ) );
				} else {
					$test = trim( $frame->expand( $nameNode ) );
					if ( $test == $primary ) {
						# Found a match, return now
						return trim( $frame->expand( $valueNode ) );
					} elseif ( $defaultFound || $mwDefault->matchStartAndRemove( $test ) ) {
						$default = $valueNode;
					} # else wrong case, continue
				}
			} else {
				# Multiple input, single output
				# If the value matches, set a flag and continue
				$lastItemHadNoEquals = true;
				$test = trim( $frame->expand( $valueNode ) );
				if ( $test == $primary ) {
					$found = true;
				} elseif ( $mwDefault->matchStartAndRemove( $test ) ) {
					$defaultFound = true;
				}
			}
		}
		# Default case
		# Check if the last item had no = sign, thus specifying the default case
		if ( $lastItemHadNoEquals ) {
			return $test;
		} elseif ( !is_null( $default ) ) {
			return trim( $frame->expand( $default ) );
		} else {
			return '';
		}
	}

	/**
	 * Returns the absolute path to a subpage, relative to the current article
	 * title. Treats titles as slash-separated paths.
	 *
	 * Following subpage link syntax instead of standard path syntax, an
	 * initial slash is treated as a relative path, and vice versa.
	 */
	public function rel2abs( $parser , $to = '' , $from = '' ) {

		$from = trim( $from );
		if ( $from == '' ) {
			$from = $parser->getTitle()->getPrefixedText();
		}

		$to = rtrim( $to , ' /' );

		// if we have an empty path, or just one containing a dot
		if ( $to == '' || $to == '.' ) {
			return $from;
		}

		// if the path isn't relative
		if ( substr( $to , 0 , 1 ) != '/' &&
		 substr( $to , 0 , 2 ) != './' &&
		 substr( $to , 0 , 3 ) != '../' &&
		 $to != '..' )
		{
			$from = '';
		}
		// Make a long path, containing both, enclose it in /.../
		$fullPath = '/' . $from . '/' .  $to . '/';

		// remove redundant current path dots
		$fullPath = preg_replace( '!/(\./)+!', '/', $fullPath );

		// remove double slashes
		$fullPath = preg_replace( '!/{2,}!', '/', $fullPath );

		// remove the enclosing slashes now
		$fullPath = trim( $fullPath , '/' );
		$exploded = explode ( '/' , $fullPath );
		$newExploded = array();

		foreach ( $exploded as $current ) {
			if ( $current == '..' ) { // removing one level
				if ( !count( $newExploded ) ) {
					// attempted to access a node above root node
					wfLoadExtensionMessages( 'ParserFunctions' );
					return '<strong class="error">' . wfMsgForContent( 'pfunc_rel2abs_invalid_depth', $fullPath ) . '</strong>';
				}
				// remove last level from the stack
				array_pop( $newExploded );
			} else {
				// add the current level to the stack
				$newExploded[] = $current;
			}
		}

		// we can now join it again
		return implode( '/' , $newExploded );
	}

	function incrementIfexistCount( $parser, $frame ) {
		// Don't let this be called more than a certain number of times. It tends to make the database explode.
		global $wgExpensiveParserFunctionLimit;
		$parser->mExpensiveFunctionCount++;
		if ( $frame ) {
			$pdbk = $frame->getPDBK( 1 );
			if ( !isset( $parser->pf_ifexist_breakdown[$pdbk] ) ) {
				$parser->pf_ifexist_breakdown[$pdbk] = 0;
			}
			$parser->pf_ifexist_breakdown[$pdbk] ++;
		}
		return $parser->mExpensiveFunctionCount <= $wgExpensiveParserFunctionLimit;
	}

	function ifexist( $parser, $title = '', $then = '', $else = '' ) {
		return $this->ifexistCommon( $parser, false, $title, $then, $else );
	}

	function ifexistCommon( $parser, $frame, $titletext = '', $then = '', $else = '' ) {
		global $wgContLang;
		$title = Title::newFromText( $titletext );
		$wgContLang->findVariantLink( $titletext, $title, true );
		if ( $title ) {
			if ( $title->getNamespace() == NS_MEDIA ) {
				/* If namespace is specified as NS_MEDIA, then we want to
				 * check the physical file, not the "description" page.
				 */
				if ( !$this->incrementIfexistCount( $parser, $frame ) ) {
					return $else;
				}
				$file = wfFindFile( $title );
				if ( !$file ) {
					return $else;
				}
				$parser->mOutput->addImage( $file->getName() );
				return $file->exists() ? $then : $else;
			} elseif ( $title->getNamespace() == NS_SPECIAL ) {
				/* Don't bother with the count for special pages,
				 * since their existence can be checked without
				 * accessing the database.
				 */
				return SpecialPage::exists( $title->getDBkey() ) ? $then : $else;
			} elseif ( $title->isExternal() ) {
				/* Can't check the existence of pages on other sites,
				 * so just return $else.  Makes a sort of sense, since
				 * they don't exist _locally_.
				 */
				return $else;
			} else {
				$pdbk = $title->getPrefixedDBkey();
				$lc = LinkCache::singleton();
				if ( !$this->incrementIfexistCount( $parser, $frame ) ) {
					return $else;
				}
				if ( 0 != ( $id = $lc->getGoodLinkID( $pdbk ) ) ) {
					$parser->mOutput->addLink( $title, $id );
					return $then;
				} elseif ( $lc->isBadLink( $pdbk ) ) {
					$parser->mOutput->addLink( $title, 0 );
					return $else;
				}
				$id = $title->getArticleID();
				$parser->mOutput->addLink( $title, $id );
				if ( $id ) {
					return $then;
				}
			}
		}
		return $else;
	}

	function ifexistObj( $parser, $frame, $args ) {
		$title = isset( $args[0] ) ? trim( $frame->expand( $args[0] ) ) : '';
		$then = isset( $args[1] ) ? $args[1] : null;
		$else = isset( $args[2] ) ? $args[2] : null;

		$result = $this->ifexistCommon( $parser, $frame, $title, $then, $else );
		if ( $result === null ) {
			return '';
		} else {
			return trim( $frame->expand( $result ) );
		}
	}

	function time( $parser, $format = '', $date = '', $local = false ) {
		global $wgContLang, $wgLocaltimezone;
		if ( isset( $this->mTimeCache[$format][$date][$local] ) ) {
			return $this->mTimeCache[$format][$date][$local];
		}

		# compute the timestamp string $ts
		# PHP >= 5.2 can handle dates before 1970 or after 2038 using the DateTime object
		# PHP < 5.2 is limited to dates between 1970 and 2038

		$invalidTime = false;

		if ( class_exists( 'DateTime' ) ) { # PHP >= 5.2
			# the DateTime constructor must be used because it throws exceptions
			# when errors occur, whereas date_create appears to just output a warning
			# that can't really be detected from within the code
			try {
				# Determine timezone
				if ( $local ) {
					 # convert to MediaWiki local timezone if set
					if ( isset( $wgLocaltimezone ) ) {
						$tz = new DateTimeZone( $wgLocaltimezone );
					} else {
						$tz = new DateTimeZone( date_default_timezone_get() );
					}
				} else {
					# if local time was not requested, convert to UTC
					$tz = new DateTimeZone( 'UTC' );
				}

				# Parse date
				if ( $date !== '' ) {
					$dateObject = new DateTime( $date, $tz );
				} else {
					# use current date and time
					$dateObject = new DateTime( 'now', $tz );
				}

				# Generate timestamp
				$ts = $dateObject->format( 'YmdHis' );
			} catch ( Exception $ex ) {
				$invalidTime = true;
			}
		} else { # PHP < 5.2
			if ( $date !== '' ) {
				$unix = @strtotime( $date );
			} else {
				$unix = time();
			}

			if ( $unix == -1 || $unix == false ) {
				$invalidTime = true;
			} else {
				if ( $local ) {
					# Use the time zone
					if ( isset( $wgLocaltimezone ) ) {
						$oldtz = getenv( 'TZ' );
						putenv( 'TZ=' . $wgLocaltimezone );
					}
					wfSuppressWarnings(); // E_STRICT system time bitching
					$ts = date( 'YmdHis', $unix );
					wfRestoreWarnings();
					if ( isset( $wgLocaltimezone ) ) {
						putenv( 'TZ=' . $oldtz );
					}
				} else {
					$ts = wfTimestamp( TS_MW, $unix );
				}
			}
		}

		# format the timestamp and return the result
		if ( $invalidTime ) {
			wfLoadExtensionMessages( 'ParserFunctions' );
			$result = '<strong class="error">' . wfMsgForContent( 'pfunc_time_error' ) . '</strong>';
		} else {
			$this->mTimeChars += strlen( $format );
			if ( $this->mTimeChars > $this->mMaxTimeChars ) {
				wfLoadExtensionMessages( 'ParserFunctions' );
				return '<strong class="error">' . wfMsgForContent( 'pfunc_time_too_long' ) . '</strong>';
			} else {
				$result = $wgContLang->sprintfDate( $format, $ts );
			}
		}
		$this->mTimeCache[$format][$date][$local] = $result;
		return $result;
	}

	function localTime( $parser, $format = '', $date = '' ) {
		return $this->time( $parser, $format, $date, true );
	}

	/**
	 * Obtain a specified number of slash-separated parts of a title,
	 * e.g. {{#titleparts:Hello/World|1}} => "Hello"
	 *
	 * @param Parser $parser Parent parser
	 * @param string $title Title to split
	 * @param int $parts Number of parts to keep
	 * @param int $offset Offset starting at 1
	 * @return string
	 */
	public function titleparts( $parser, $title = '', $parts = 0, $offset = 0 ) {
		$parts = intval( $parts );
		$offset = intval( $offset );
		$ntitle = Title::newFromText( $title );
		if ( $ntitle instanceof Title ) {
			$bits = explode( '/', $ntitle->getPrefixedText(), 25 );
			if ( count( $bits ) <= 0 ) {
				 return $ntitle->getPrefixedText();
			} else {
				if ( $offset > 0 ) {
					--$offset;
				}
				if ( $parts == 0 ) {
					return implode( '/', array_slice( $bits, $offset ) );
				} else {
					return implode( '/', array_slice( $bits, $offset, $parts ) );
				}
			}
		} else {
			return $title;
		}
	}

	// Verifies parameter is less than max string length.
	private function checkLength( $text ) {
		global $wgPFStringLengthLimit;
		return ( mb_strlen( $text ) < $wgPFStringLengthLimit );
	}

	// Generates error message.  Called when string is too long.
	private function tooLongError() {
		global $wgPFStringLengthLimit, $wgContLang;
		wfLoadExtensionMessages( 'ParserFunctions' );

		return '<strong class="error">' .
			wfMsgExt( 'pfunc_string_too_long',
				array( 'escape', 'parsemag', 'content' ),
				$wgContLang->formatNum( $wgPFStringLengthLimit ) ) .
			'</strong>';
	}

	/**
	 * {{#len:string}}
	 *
	 * Reports number of characters in string.
	 */
	function runLen ( $parser, $inStr = '' ) {
		wfProfileIn( __METHOD__ );

		$inStr = $this->killMarkers( $parser, (string)$inStr );
		$len = mb_strlen( $inStr );

		wfProfileOut( __METHOD__ );
		return $len;
	}

	/**
	 * {{#pos: string | needle | offset}}
	 *
	 * Finds first occurrence of "needle" in "string" starting at "offset".
	 *
	 * Note: If the needle is an empty string, single space is used instead.
	 * Note: If the needle is not found, empty string is returned.
	 */
	function runPos ( $parser, $inStr = '', $inNeedle = '', $inOffset = 0 ) {
		wfProfileIn( __METHOD__ );

		$inStr = $this->killMarkers( $parser, (string)$inStr );
		$inNeedle = $this->killMarkers( $parser, (string)$inNeedle );

		if ( !$this->checkLength( $inStr ) ||
		    !$this->checkLength( $inNeedle ) ) {
			wfProfileOut( __METHOD__ );
			return $this->tooLongError();
		}

		if ( $inNeedle == '' ) { $inNeedle = ' '; }

		$pos = mb_strpos( $inStr, $inNeedle, $inOffset );
		if ( $pos === false ) { $pos = ""; }

		wfProfileOut( __METHOD__ );
		return $pos;
	}

	/**
	 * {{#rpos: string | needle}}
	 *
	 * Finds last occurrence of "needle" in "string".
	 *
	 * Note: If the needle is an empty string, single space is used instead.
	 * Note: If the needle is not found, -1 is returned.
	 */
	function runRPos ( $parser, $inStr = '', $inNeedle = '' ) {
		wfProfileIn( __METHOD__ );

		$inStr = $this->killMarkers( $parser, (string)$inStr );
		$inNeedle = $this->killMarkers( $parser, (string)$inNeedle );

		if ( !$this->checkLength( $inStr ) ||
		    !$this->checkLength( $inNeedle ) ) {
			wfProfileOut( __METHOD__ );
			return $this->tooLongError();
		}

		if ( $inNeedle == '' ) { $inNeedle = ' '; }

		$pos = mb_strrpos( $inStr, $inNeedle );
		if ( $pos === false ) { $pos = -1; }

		wfProfileOut( __METHOD__ );
		return $pos;
	}

	/**
	 * {{#sub: string | start | length }}
	 *
	 * Returns substring of "string" starting at "start" and having
	 * "length" characters.
	 *
	 * Note: If length is zero, the rest of the input is returned.
	 * Note: A negative value for "start" operates from the end of the
	 *   "string".
	 * Note: A negative value for "length" returns a string reduced in
	 *   length by that amount.
	 */
	function runSub ( $parser, $inStr = '', $inStart = 0, $inLength = 0 ) {
		wfProfileIn( __METHOD__ );

		$inStr = $this->killMarkers( $parser, (string)$inStr );

		if ( !$this->checkLength( $inStr ) ) {
			wfProfileOut( __METHOD__ );
			return $this->tooLongError();
		}

		if ( intval( $inLength ) == 0 ) {
			$result = mb_substr( $inStr, $inStart );
		} else {
			$result = mb_substr( $inStr, $inStart, $inLength );
		}

		wfProfileOut( __METHOD__ );
		return $result;
	}

	/**
	 * {{#count: string | substr }}
	 *
	 * Returns number of occurrences of "substr" in "string".
	 *
	 * Note: If "substr" is empty, a single space is used.
	 */
	function runCount ( $parser, $inStr = '', $inSubStr = '' ) {
		wfProfileIn( __METHOD__ );

		$inStr = $this->killMarkers( $parser, (string)$inStr );
		$inSubStr = $this->killMarkers( $parser, (string)$inSubStr );

		if ( !$this->checkLength( $inStr ) ||
		    !$this->checkLength( $inSubStr ) ) {
			wfProfileOut( __METHOD__ );
			return $this->tooLongError();
		}

		if ( $inSubStr == '' ) { $inSubStr = ' '; }

		$result = mb_substr_count( $inStr, $inSubStr );

		wfProfileOut( __METHOD__ );
		return $result;
	}

	/**
	 * {{#replace:string | from | to | limit }}
	 *
	 * Replaces each occurrence of "from" in "string" with "to".
	 * At most "limit" replacements are performed.
	 *
	 * Note: Armored against replacements that would generate huge strings.
	 * Note: If "from" is an empty string, single space is used instead.
	 */
	function runReplace( $parser, $inStr = '',
			$inReplaceFrom = '', $inReplaceTo = '', $inLimit = -1 ) {
		global $wgPFStringLengthLimit;
		wfProfileIn( __METHOD__ );

		$inStr = $this->killMarkers( $parser, (string)$inStr );
		$inReplaceFrom = $this->killMarkers( $parser, (string)$inReplaceFrom );
		$inReplaceTo = $this->killMarkers( $parser, (string)$inReplaceTo );

		if ( !$this->checkLength( $inStr ) ||
		    !$this->checkLength( $inReplaceFrom ) ||
		    !$this->checkLength( $inReplaceTo ) ) {
			wfProfileOut( __METHOD__ );
			return $this->tooLongError();
		}

		if ( $inReplaceFrom == '' ) { $inReplaceFrom = ' '; }

		// Precompute limit to avoid generating enormous string:
		$diff = mb_strlen( $inReplaceTo ) - mb_strlen( $inReplaceFrom );
		if ( $diff > 0 ) {
			$limit = ( ( $wgPFStringLengthLimit - mb_strlen( $inStr ) ) / $diff ) + 1;
		} else {
			$limit = -1;
		}

		$inLimit = intval( $inLimit );
		if ( $inLimit >= 0 ) {
			if ( $limit > $inLimit || $limit == -1 ) { $limit = $inLimit; }
		}

		// Use regex to allow limit and handle UTF-8 correctly.
		$inReplaceFrom = preg_quote( $inReplaceFrom, '/' );
		$inReplaceTo = StringUtils::escapeRegexReplacement( $inReplaceTo );

		$result = preg_replace( '/' . $inReplaceFrom . '/u',
						$inReplaceTo, $inStr, $limit );

		if ( !$this->checkLength( $result ) ) {
			wfProfileOut( __METHOD__ );
			return $this->tooLongError();
		}

		wfProfileOut( __METHOD__ );
		return $result;
	}


	/**
	 * {{#explode:string | delimiter | position | limit}}
	 *
	 * Breaks "string" into chunks separated by "delimiter" and returns the
	 * chunk identified by "position".
	 *
	 * Note: Negative position can be used to specify tokens from the end.
	 * Note: If the divider is an empty string, single space is used instead.
	 * Note: Empty string is returned if there are not enough exploded chunks.
	 */
	function runExplode ( $parser, $inStr = '', $inDiv = '', $inPos = 0, $inLim = null ) {
		wfProfileIn( __METHOD__ );

		$inStr = $this->killMarkers( $parser, (string)$inStr );
		$inDiv = $this->killMarkers( $parser, (string)$inDiv );

		if ( $inDiv == '' ) { $inDiv = ' '; }

		if ( !$this->checkLength( $inStr ) ||
		    !$this->checkLength( $inDiv ) ) {
			wfProfileOut( __METHOD__ );
			return $this->tooLongError();
		}

		$inDiv = preg_quote( $inDiv, '/' );

		$matches = preg_split( '/' . $inDiv . '/u', $inStr, $inLim );

		if ( $inPos >= 0 && isset( $matches[$inPos] ) ) {
			$result = $matches[$inPos];
		} elseif ( $inPos < 0 && isset( $matches[count( $matches ) + $inPos] ) ) {
			$result = $matches[count( $matches ) + $inPos];
		} else {
			$result = '';
		}

		wfProfileOut( __METHOD__ );
		return $result;
	}

	/**
	 * {{#urldecode:string}}
	 *
	 * Decodes URL-encoded (like%20that) strings.
	 */
	function runUrlDecode( $parser, $inStr = '' ) {
		wfProfileIn( __METHOD__ );

		$inStr = $this->killMarkers( $parser, (string)$inStr );
		if ( !$this->checkLength( $inStr ) ) {
			wfProfileOut( __METHOD__ );
			return $this->tooLongError();
		}

		$result = urldecode( $inStr );

		wfProfileOut( __METHOD__ );
		return $result;
	}
}
