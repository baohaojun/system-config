<?php

/**#@+
 * A parser extension that adds two tags, <ref> and <references> for adding
 * citations to pages
 *
 * @ingroup Extensions
 *
 * @link http://www.mediawiki.org/wiki/Extension:Cite/Cite.php Documentation
 * @link http://www.w3.org/TR/html4/struct/text.html#edef-CITE <cite> definition in HTML
 * @link http://www.w3.org/TR/2005/WD-xhtml2-20050527/mod-text.html#edef_text_cite <cite> definition in XHTML 2.0
 *
 * @bug 4579
 *
 * @author Ævar Arnfjörð Bjarmason <avarab@gmail.com>
 * @copyright Copyright © 2005, Ævar Arnfjörð Bjarmason
 * @license http://www.gnu.org/copyleft/gpl.html GNU General Public License 2.0 or later
 */

class Cite {
	/**#@+
	 * @access private
	 */
	
	/**
	 * Datastructure representing <ref> input, in the format of:
	 * <code>
	 * array(
	 * 	'user supplied' => array(
	 *		'text' => 'user supplied reference & key',
	 *		'count' => 1, // occurs twice
	 * 		'number' => 1, // The first reference, we want
	 * 		               // all occourances of it to
	 * 		               // use the same number
	 *	),
	 *	0 => 'Anonymous reference',
	 *	1 => 'Another anonymous reference',
	 *	'some key' => array(
	 *		'text' => 'this one occurs once'
	 *		'count' => 0,
	 * 		'number' => 4
	 *	),
	 *	3 => 'more stuff'
	 * );
	 * </code>
	 *
	 * This works because:
	 * * PHP's datastructures are guarenteed to be returned in the
	 *   order that things are inserted into them (unless you mess
	 *   with that)
	 * * User supplied keys can't be integers, therefore avoiding
	 *   conflict with anonymous keys
	 *
	 * @var array
	 **/
	var $mRefs = array();
	
	/**
	 * Count for user displayed output (ref[1], ref[2], ...)
	 *
	 * @var int
	 */
	var $mOutCnt = 0;
	var $mGroupCnt = array();

	/**
	 * Internal counter for anonymous references, separate from
	 * $mOutCnt because anonymous references won't increment it,
	 * but will incremement $mOutCnt
	 *
	 * @var int
	 */
	var $mInCnt = 0;

	/**
	 * The backlinks, in order, to pass as $3 to
	 * 'cite_references_link_many_format', defined in
	 * 'cite_references_link_many_format_backlink_labels
	 *
	 * @var array
	 */
	var $mBacklinkLabels;


	/**
	 * The links to use per group, in order.
	 *
	 * @var array
	 */
	var $mLinkLabels = array();
	
	/**
	 * @var object
	 */
	var $mParser;
	
	/**
	 * True when a <ref> tag is being processed.
	 * Used to avoid infinite recursion
	 * 
	 * @var boolean
	 */
	var $mInCite = false;

	/**
	 * True when a <references> tag is being processed.
	 * Used to detect the use of <references> to define refs
	 * 
	 * @var boolean
	 */
	var $mInReferences = false;

	/**
	 * Error stack used when defining refs in <references>
	 * 
	 * @var array
	 */
	var $mReferencesErrors = array();

	/**
	 * Group used when in <references> block
	 * 
	 * @var string
	 */
	var $mReferencesGroup = '';

	/**
	 * <ref> call stack 
	 * Used to cleanup out of sequence ref calls created by #tag
	 * See description of function rollbackRef.
	 * 
	 * @var array
	 */
	var $mRefCallStack = array();

	/**#@-*/

	/**
	 * Constructor
	 */
	function __construct() {
		$this->setHooks();
	}

	/**#@+ @access private */

	/**
	 * Callback function for <ref>
	 *
	 * @param string $str Input
	 * @param array $argv Arguments
	 * @return string
	 */
	function ref( $str, $argv, $parser ) {
		wfLoadExtensionMessages( 'Cite' );
		if ( $this->mInCite ) {
			return htmlspecialchars( "<ref>$str</ref>" );
		} else {
			$this->mInCite = true;
			$ret = $this->guardedRef( $str, $argv, $parser );
			$this->mInCite = false;
			return $ret;
		}
	}

	function guardedRef( $str, $argv, $parser, $default_group = CITE_DEFAULT_GROUP ) {
		$this->mParser = $parser;
		
		# The key here is the "name" attribute.
		list( $key, $group, $follow ) = $this->refArg( $argv );
		
		# Split these into groups.
		if ( $group === null ) {
			if ( $this->mInReferences ) {
				$group = $this->mReferencesGroup;
			} else {
				$group = $default_group;
			}
		}
		
		# This section deals with constructions of the form 
		#
		# <references>
		# <ref name="foo"> BAR </ref>
		# </references>
		#
		if ( $this->mInReferences ) {
			if ( $group != $this->mReferencesGroup ) {
				# <ref> and <references> have conflicting group attributes.
				$this->mReferencesErrors[] =
					$this->error( 'cite_error_references_group_mismatch', htmlspecialchars( $group ) );
			} elseif ( $str !== '' ) {
				if ( !isset( $this->mRefs[$group] ) ) {
					# Called with group attribute not defined in text.
					$this->mReferencesErrors[] =
						$this->error( 'cite_error_references_missing_group', htmlspecialchars( $group ) );
				} elseif ( $key === null || $key === '' ) {
					# <ref> calls inside <references> must be named
					$this->mReferencesErrors[] =
						$this->error( 'cite_error_references_no_key' );
				} elseif ( !isset( $this->mRefs[$group][$key] ) ) {
					# Called with name attribute not defined in text.
					$this->mReferencesErrors[] =
						$this->error( 'cite_error_references_missing_key', $key );
				} else {
					# Assign the text to corresponding ref
					$this->mRefs[$group][$key]['text'] = $str;
				}
			} else {
				# <ref> called in <references> has no content.
				$this->mReferencesErrors[] =
					$this->error( 'cite_error_empty_references_define', $key );
			}
			return '';
		}

		if ( $str === '' ) {
			# <ref ...></ref>.  This construct is  invalid if
			# it's a contentful ref, but OK if it's a named duplicate and should
			# be equivalent <ref ... />, for compatability with #tag.
			if ( $key == false ) {
				$this->mRefCallStack[] = false;
				return $this->error( 'cite_error_ref_no_input' );
			} else {
				$str = null;
			}
		}
				
		if ( $key === false ) {
			# TODO: Comment this case; what does this condition mean?
			$this->mRefCallStack[] = false;
			return $this->error( 'cite_error_ref_too_many_keys' );
		}

		if ( $str === null and $key === null ) {
			# Something like <ref />; this makes no sense.
			$this->mRefCallStack[] = false;
			return $this->error( 'cite_error_ref_no_key' );
		}
		
		if ( preg_match( '/^[0-9]+$/', $key ) || preg_match( '/^[0-9]+$/', $follow ) ) {
			# Numeric names mess up the resulting id's, potentially produ-
			# cing duplicate id's in the XHTML.  The Right Thing To Do
			# would be to mangle them, but it's not really high-priority
			# (and would produce weird id's anyway).

			$this->mRefCallStack[] = false;
			return $this->error( 'cite_error_ref_numeric_key' );
		}

		if ( preg_match(
			'/<ref\b[^<]*?>/',
			preg_replace( '#<([^ ]+?).*?>.*?</\\1 *>|<!--.*?-->#', '', $str )
		) ) {
			# (bug 6199) This most likely implies that someone left off the
			# closing </ref> tag, which will cause the entire article to be
			# eaten up until the next <ref>.  So we bail out early instead.
			# The fancy regex above first tries chopping out anything that
			# looks like a comment or SGML tag, which is a crude way to avoid
			# false alarms for <nowiki>, <pre>, etc.
			#
			# Possible improvement: print the warning, followed by the contents
			# of the <ref> tag.  This way no part of the article will be eaten
			# even temporarily.

			$this->mRefCallStack[] = false;
			return $this->error( 'cite_error_included_ref' );
		}

		if ( is_string( $key ) or is_string( $str ) ) {
			# We don't care about the content: if the key exists, the ref
			# is presumptively valid.  Either it stores a new ref, or re-
			# fers to an existing one.  If it refers to a nonexistent ref,
			# we'll figure that out later.  Likewise it's definitely valid
			# if there's any content, regardless of key.

			return $this->stack( $str, $key, $group, $follow, $argv );
		}

		# Not clear how we could get here, but something is probably
		# wrong with the types.  Let's fail fast.
		$this->croak( 'cite_error_key_str_invalid', serialize( "$str; $key" ) );
	}

	/**
	 * Parse the arguments to the <ref> tag
	 * 
	 *  "name" : Key of the reference.
	 *  "group" : Group to which it belongs. Needs to be passed to <references /> too.
	 *  "follow" : If the current reference is the continuation of another, key of that reference.
	 *
	 * @static
	 *
	 * @param array $argv The argument vector
	 * @return mixed false on invalid input, a string on valid
	 *               input and null on no input
	 */
	function refArg( $argv ) {
		global $wgAllowCiteGroups;
		$cnt = count( $argv );
		$group = null;
		$key = null;
		$follow = null;

		if ( $cnt > 2 )
			// There should only be one key or follow parameter, and one group parameter
			// FIXME : this looks inconsistent, it should probably return a tuple 
			return false;
		else if ( $cnt >= 1 ) {
			if ( isset( $argv['name'] ) && isset( $argv['follow'] ) ) {
				return array( false, false, false );
			}
			if ( isset( $argv['name'] ) ) {
				// Key given.
				$key = Sanitizer::escapeId( $argv['name'], 'noninitial' );
				unset( $argv['name'] );
				--$cnt;
			}
			if ( isset( $argv['follow'] ) ) {
				// Follow given.
				$follow = Sanitizer::escapeId( $argv['follow'], 'noninitial' );
				unset( $argv['follow'] );
				--$cnt;
			}
			if ( isset( $argv['group'] ) ) {
				if ( ! $wgAllowCiteGroups ) return array( false ); // remove when groups are fully tested.
				// Group given.
				$group = $argv['group'];
				unset( $argv['group'] );
				--$cnt;
			}

			if ( $cnt == 0 )
				return array ( $key, $group, $follow );
			else
				// Invalid key
				return array( false, false, false );
		}
		else
			// No key
			return array( null, $group, false );
	}

	/**
	 * Populate $this->mRefs based on input and arguments to <ref>
	 *
	 * @param string $str Input from the <ref> tag
	 * @param mixed $key Argument to the <ref> tag as returned by $this->refArg()
	 * @return string 
	 */
	function stack( $str, $key = null, $group, $follow, $call ) {
		if ( ! isset( $this->mRefs[$group] ) )
			$this->mRefs[$group] = array();
		if ( ! isset( $this->mGroupCnt[$group] ) )
			$this->mGroupCnt[$group] = 0;

		if ( $follow != null ) {
			if ( isset( $this->mRefs[$group][$follow] ) && is_array( $this->mRefs[$group][$follow] ) ) {
				// add text to the note that is being followed
				$this->mRefs[$group][$follow]['text'] = $this->mRefs[$group][$follow]['text'] . ' '. $str;
			} else {
				// insert part of note at the beginning of the group
				for( $k=0 ; $k< count( $this->mRefs[$group] ) ; $k++) {
					if( $this->mRefs[$group][$k]['follow'] == null ) break;
				}
				array_splice( $this->mRefs[$group], $k, 0, 
					       array( array( 'count' => - 1, 
						      'text' => $str, 
						      'key' => ++$this->mOutCnt , 
						      'follow' => $follow) ) );
				array_splice( $this->mRefCallStack, $k, 0, 
					       array( array( 'new', $call, $str, $key, $group, $this->mOutCnt ) ) );
				$this->mInCnt++; 
			}
			// return an empty string : this is not a reference
			return '';
		}
		if ( $key === null ) {
			// No key
			// $this->mRefs[$group][] = $str;
			$this->mRefs[$group][] = array( 'count' => - 1, 'text' => $str, 'key' => ++$this->mOutCnt );
			$this->mRefCallStack[] = array( 'new', $call, $str, $key, $group, $this->mOutCnt );

			return $this->linkRef( $group, $this->mInCnt++ );
		} else if ( is_string( $key ) ) {
			// Valid key
			if ( ! isset( $this->mRefs[$group][$key] ) || ! is_array( $this->mRefs[$group][$key] ) ) {
				// First occurance
				$this->mRefs[$group][$key] = array(
					'text' => $str,
					'count' => 0,
					'key' => ++$this->mOutCnt,
					'number' => ++$this->mGroupCnt[$group]
				);
				$this->mRefCallStack[] = array( 'new', $call, $str, $key, $group, $this->mOutCnt );

				$this->mInCnt++;
				return
					$this->linkRef(
						$group,
						$key,
						$this->mRefs[$group][$key]['key'] . "-" . $this->mRefs[$group][$key]['count'],
						$this->mRefs[$group][$key]['number'],
						"-" . $this->mRefs[$group][$key]['key']
					);
			} else {
				// We've been here before
				if ( $this->mRefs[$group][$key]['text'] === null && $str !== '' ) {
					// If no text found before, use this text
					$this->mRefs[$group][$key]['text'] = $str;
					$this->mRefCallStack[] = array( 'assign', $call, $str, $key, $group,
						$this->mRefs[$group][$key]['key'] );
				} else {
					$this->mRefCallStack[] = array( 'increment', $call, $str, $key, $group,
						$this->mRefs[$group][$key]['key'] );
				}
				return
					$this->linkRef(
						$group,
						$key,
						$this->mRefs[$group][$key]['key'] . "-" . ++$this->mRefs[$group][$key]['count'],
						$this->mRefs[$group][$key]['number'],
						"-" . $this->mRefs[$group][$key]['key']
					); }
		}

		else
			$this->croak( 'cite_error_stack_invalid_input', serialize( array( $key, $str ) ) );
	}
	
	/**
	 * Partially undoes the effect of calls to stack()
	 * 
	 * Called by guardedReferences()
	 *
	 * The option to define <ref> within <references> makes the 
	 * behavior of <ref> context dependent.  This is normally fine
	 * but certain operations (especially #tag) lead to out-of-order
	 * parser evaluation with the <ref> tags being processed before
	 * their containing <reference> element is read.  This leads to
	 * stack corruption that this function works to fix.
	 *
	 * This function is not a total rollback since some internal
	 * counters remain incremented.  Doing so prevents accidentally
	 * corrupting certain links.
	 */
	function rollbackRef( $type, $key, $group, $index ) {
		if ( !isset( $this->mRefs[$group] ) ) { return; }

		if ( $key === null ) {
			foreach ( $this->mRefs[$group] as $k => $v ) {
				if ( $this->mRefs[$group][$k]['key'] === $index ) {
					$key = $k;
					break;
				}
			}
		}

		# Sanity checks that specified element exists.
		if ( $key === null ) { return; }
		if ( !isset( $this->mRefs[$group][$key] ) ) { return; }
		if ( $this->mRefs[$group][$key]['key'] != $index ) { return; }

		switch( $type ) {
		case 'new':
			# Rollback the addition of new elements to the stack.
			unset( $this->mRefs[$group][$key] );
			if ( count( $this->mRefs[$group] ) == 0 ) {
				unset( $this->mRefs[$group] );
				unset( $this->mGroupCnt[$group] );
			}
			break;
		case 'assign':
			# Rollback assignment of text to pre-existing elements.
			$this->mRefs[$group][$key]['text'] = null;
			# continue without break
		case 'increment':
			# Rollback increase in named ref occurences.
			$this->mRefs[$group][$key]['count']--;
			break;
		}
	}

	/**
	 * Callback function for <references>
	 *
	 * @param string $str Input
	 * @param array $argv Arguments
	 * @return string
	 */
	function references( $str, $argv, $parser ) {
		wfLoadExtensionMessages( 'Cite' );
		if ( $this->mInCite || $this->mInReferences ) {
			if ( is_null( $str ) ) {
				return htmlspecialchars( "<references/>" );
			} else {
				return htmlspecialchars( "<references>$str</references>" );
			}
		} else {
			$this->mInReferences = true;
			$ret = $this->guardedReferences( $str, $argv, $parser );
			$this->mInReferences = false;
			return $ret;
		}
	}

	function guardedReferences( $str, $argv, $parser, $group = CITE_DEFAULT_GROUP ) {
		global $wgAllowCiteGroups;

		$this->mParser = $parser;
		
		if ( isset( $argv['group'] ) and $wgAllowCiteGroups ) {
			$group = $argv['group'];
			unset ( $argv['group'] );
		}
		
		if ( strval( $str ) !== '' ) {
			$this->mReferencesGroup = $group;
			
			# Detect whether we were sent already rendered <ref>s
			# Mostly a side effect of using #tag to call references
			$count = substr_count( $str, $parser->uniqPrefix() . "-ref-" );
			for ( $i = 1; $i <= $count; $i++ ) {
				if ( count( $this->mRefCallStack ) < 1 ) break;

				# The following assumes that the parsed <ref>s sent within 
				# the <references> block were the most recent calls to
				# <ref>.  This assumption is true for all known use cases,
				# but not strictly enforced by the parser.  It is possible
				# that some unusual combination of #tag, <references> and
				# conditional parser functions could be created that would
				# lead to malformed references here.
				$call = array_pop( $this->mRefCallStack );
				if ( $call !== false ) {
					list( $type, $ref_argv, $ref_str,
						$ref_key, $ref_group, $ref_index ) = $call;

					# Undo effects of calling <ref> while unaware of containing <references> 
					$this->rollbackRef( $type, $ref_key, $ref_group, $ref_index );

					# Rerun <ref> call now that mInReferences is set.
					$this->guardedRef( $ref_str, $ref_argv, $parser );
				}
			}

			# Parse $str to process any unparsed <ref> tags.
			$parser->recursiveTagParse( $str );

			# Reset call stack
			$this->mRefCallStack = array();
		}

		if ( count( $argv ) && $wgAllowCiteGroups )
			return $this->error( 'cite_error_references_invalid_parameters_group' );
		elseif ( count( $argv ) )
			return $this->error( 'cite_error_references_invalid_parameters' );
		else {
			$s = $this->referencesFormat( $group );
			if ( $parser->getOptions()->getIsSectionPreview() ) return $s;
			
			# Append errors generated while processing <references>
			if ( count( $this->mReferencesErrors ) > 0 ) {
				$s .= "\n" . implode( "<br />\n", $this->mReferencesErrors );
				$this->mReferencesErrors = array();
			}
			return $s;
		}
	}

	/**
	 * Make output to be returned from the references() function
	 *
	 * @return string XHTML ready for output
	 */
	function referencesFormat( $group ) {
		if ( ( count( $this->mRefs ) == 0 ) or ( empty( $this->mRefs[$group] ) ) )
			return '';
		
		wfProfileIn( __METHOD__ );
		wfProfileIn( __METHOD__ . '-entries' );
		$ent = array();
		foreach ( $this->mRefs[$group] as $k => $v )
			$ent[] = $this->referencesFormatEntry( $k, $v );
		
		$prefix = wfMsgForContentNoTrans( 'cite_references_prefix' );
		$suffix = wfMsgForContentNoTrans( 'cite_references_suffix' );
		$content = implode( "\n", $ent );

		// Let's try to cache it.
		$parserInput = $prefix . $content . $suffix;
		global $wgMemc;
		$cacheKey = wfMemcKey( 'citeref', md5( $parserInput ), $this->mParser->Title()->getArticleID() );

		wfProfileOut( __METHOD__ . '-entries' );
		
		global $wgCiteCacheReferences;
		if ( $wgCiteCacheReferences ) {
			wfProfileIn( __METHOD__ . '-cache-get' );
			$data = $wgMemc->get( $cacheKey );
			wfProfileOut( __METHOD__ . '-cache-get' );
		}
		
		if ( empty( $data ) ) {
			wfProfileIn( __METHOD__ . '-parse' );
			
			// Live hack: parse() adds two newlines on WM, can't reproduce it locally -ævar
			$ret = rtrim( $this->parse( $parserInput ), "\n" );
			
			if ( $wgCiteCacheReferences ) {
				$serData = $this->mParser->serialiseHalfParsedText( $ret );
				$wgMemc->set( $cacheKey, $serData, 86400 );
			}
			
			wfProfileOut( __METHOD__ . '-parse' );
		} else {
			$ret = $this->mParser->unserialiseHalfParsedText( $data );
		}

		wfProfileOut( __METHOD__ );
		
		// done, clean up so we can reuse the group
		unset ( $this->mRefs[$group] );
		unset( $this->mGroupCnt[$group] );
			
		return $ret;
	}

	/**
	 * Format a single entry for the referencesFormat() function
	 *
	 * @param string $key The key of the reference
	 * @param mixed $val The value of the reference, string for anonymous
	 *                   references, array for user-suppplied
	 * @return string Wikitext
	 */
	function referencesFormatEntry( $key, $val ) {
		// Anonymous reference
		if ( ! is_array( $val ) )
			return
				wfMsgForContentNoTrans(
					'cite_references_link_one',
					$this->referencesKey( $key ),
					$this->refKey( $key ),
					$val
				);
		else if ( isset( $val['follow'] ) ) 
			return
				wfMsgForContentNoTrans(
					'cite_references_no_link',
					$this->referencesKey( $val['follow'] ),
					$val['text']
				);
		else if ( $val['text'] == '' ) return
				wfMsgForContentNoTrans(
					'cite_references_link_one',
					$this->referencesKey( $key ),
					$this->refKey( $key, $val['count'] ),
					$this->error( 'cite_error_references_no_text', $key )
				);
		if ( $val['count'] < 0 )
			return
				wfMsgForContentNoTrans(
					'cite_references_link_one',
					$this->referencesKey( $val['key'] ),
					# $this->refKey( $val['key'], $val['count'] ),
					$this->refKey( $val['key'] ),

					( $val['text'] != '' ? $val['text'] : $this->error( 'cite_error_references_no_text', $key ) )
				);
		// Standalone named reference, I want to format this like an
		// anonymous reference because displaying "1. 1.1 Ref text" is
		// overkill and users frequently use named references when they
		// don't need them for convenience
		else if ( $val['count'] === 0 )
			return
				wfMsgForContentNoTrans(
					'cite_references_link_one',
					$this->referencesKey( $key . "-" . $val['key'] ),
					# $this->refKey( $key, $val['count'] ),
					$this->refKey( $key, $val['key'] . "-" . $val['count'] ),
					( $val['text'] != '' ? $val['text'] : $this->error( 'cite_error_references_no_text', $key ) )
				);
		// Named references with >1 occurrences
		else {
			$links = array();
		// for group handling, we have an extra key here.
			for ( $i = 0; $i <= $val['count']; ++$i ) {
				$links[] = wfMsgForContentNoTrans(
						'cite_references_link_many_format',
						$this->refKey( $key, $val['key'] . "-$i" ),
						$this->referencesFormatEntryNumericBacklinkLabel( $val['number'], $i, $val['count'] ),
						$this->referencesFormatEntryAlternateBacklinkLabel( $i )
				);
			}

			$list = $this->listToText( $links );

			return
				wfMsgForContentNoTrans( 'cite_references_link_many',
					$this->referencesKey( $key . "-" . $val['key'] ),
					$list,
					( $val['text'] != '' ? $val['text'] : $this->error( 'cite_error_references_no_text', $key ) )
				);
		}
	}

	/**
	 * Generate a numeric backlink given a base number and an
	 * offset, e.g. $base = 1, $offset = 2; = 1.2
	 * Since bug #5525, it correctly does 1.9 -> 1.10 as well as 1.099 -> 1.100
	 *
	 * @static
	 *
	 * @param int $base The base
	 * @param int $offset The offset
	 * @param int $max Maximum value expected.
	 * @return string
	 */
	function referencesFormatEntryNumericBacklinkLabel( $base, $offset, $max ) {
		global $wgContLang;
		$scope = strlen( $max );
		$ret = $wgContLang->formatNum(
			sprintf( "%s.%0{$scope}s", $base, $offset )
		);
		return $ret;
	}

	/**
	 * Generate a custom format backlink given an offset, e.g.
	 * $offset = 2; = c if $this->mBacklinkLabels = array( 'a',
	 * 'b', 'c', ...). Return an error if the offset > the # of
	 * array items
	 *
	 * @param int $offset The offset
	 *
	 * @return string
	 */
	function referencesFormatEntryAlternateBacklinkLabel( $offset ) {
		if ( !isset( $this->mBacklinkLabels ) ) {
			$this->genBacklinkLabels();
		}
		if ( isset( $this->mBacklinkLabels[$offset] ) ) {
			return $this->mBacklinkLabels[$offset];
		} else {
			// Feed me!
			return $this->error( 'cite_error_references_no_backlink_label' );
		}
	}

	/**
	 * Generate a custom format link for a group given an offset, e.g.
	 * the second <ref group="foo"> is b if $this->mLinkLabels["foo"] = 
	 * array( 'a', 'b', 'c', ...). 
	 * Return an error if the offset > the # of array items
	 *
	 * @param int $offset The offset
	 * @param string $group The group name
	 * @param string $label The text to use if there's no message for them.
	 *
	 * @return string
	 */
	function getLinkLabel( $offset, $group, $label) {
		$message = "cite_link_label_group-$group";
		if ( !isset( $this->mLinkLabels[$group] ) ) {
			$this->genLinkLabels($group, $message);
		}
		if ($this->mLinkLabels[$group] === false) {
			// Use normal representation, ie. "$group 1", "$group 2"...
			return $label;
		}
		
		if ( isset( $this->mLinkLabels[$group][$offset - 1] ) ) {
			return $this->mLinkLabels[$group][$offset - 1];
		} else {
			// Feed me!
			return $this->error( 'cite_error_no_link_label_group', array( $group, $message ) );
		}
	}

	/**
	 * Return an id for use in wikitext output based on a key and
	 * optionally the number of it, used in <references>, not <ref>
	 * (since otherwise it would link to itself)
	 *
	 * @static
	 *
	 * @param string $key The key
	 * @param int $num The number of the key
	 * @return string A key for use in wikitext
	 */
	function refKey( $key, $num = null ) {
		$prefix = wfMsgForContent( 'cite_reference_link_prefix' );
		$suffix = wfMsgForContent( 'cite_reference_link_suffix' );
		if ( isset( $num ) )
			$key = wfMsgForContentNoTrans( 'cite_reference_link_key_with_num', $key, $num );
		
		return $prefix . $key . $suffix;
	}

	/**
	 * Return an id for use in wikitext output based on a key and
	 * optionally the number of it, used in <ref>, not <references>
	 * (since otherwise it would link to itself)
	 *
	 * @static
	 *
	 * @param string $key The key
	 * @param int $num The number of the key
	 * @return string A key for use in wikitext
	 */
	function referencesKey( $key, $num = null ) {
		$prefix = wfMsgForContent( 'cite_references_link_prefix' );
		$suffix = wfMsgForContent( 'cite_references_link_suffix' );
		if ( isset( $num ) )
			$key = wfMsgForContentNoTrans( 'cite_reference_link_key_with_num', $key, $num );
		
		return $prefix . $key . $suffix;
	}

	/**
	 * Generate a link (<sup ...) for the <ref> element from a key
	 * and return XHTML ready for output
	 *
	 * @param string $key The key for the link
	 * @param int $count The index of the key, used for distinguishing
	 *                   multiple occurances of the same key
	 * @param int $label The label to use for the link, I want to
	 *                   use the same label for all occourances of
	 *                   the same named reference.
	 * @return string
	 */
	function linkRef( $group, $key, $count = null, $label = null, $subkey = '' ) {
		global $wgContLang;
		$label = is_null( $label ) ? ++$this->mGroupCnt[$group] : $label;
		
		return
			$this->parse(
				wfMsgForContentNoTrans(
					'cite_reference_link',
					$this->refKey( $key, $count ),
					$this->referencesKey( $key . $subkey ),
					$this->getLinkLabel( $label, $group, 
						( ( $group == CITE_DEFAULT_GROUP ) ? '':"$group " ) . $wgContLang->formatNum( $label ) )
				)
			);
	}

	/**
	 * This does approximately the same thing as
	 * Language::listToText() but due to this being used for a
	 * slightly different purpose (people might not want , as the
	 * first separator and not 'and' as the second, and this has to
	 * use messages from the content language) I'm rolling my own.
	 *
	 * @static
	 *
	 * @param array $arr The array to format
	 * @return string
	 */
	function listToText( $arr ) {
		$cnt = count( $arr );

		$sep = wfMsgForContentNoTrans( 'cite_references_link_many_sep' );
		$and = wfMsgForContentNoTrans( 'cite_references_link_many_and' );

		if ( $cnt == 1 )
			// Enforce always returning a string
			return (string)$arr[0];
		else {
			$t = array_slice( $arr, 0, $cnt - 1 );
			return implode( $sep, $t ) . $and . $arr[$cnt - 1];
		}
	}

	/**
	 * Parse a given fragment and fix up Tidy's trail of blood on
	 * it...
	 *
	 * @param string $in The text to parse
	 * @return string The parsed text
	 */
	function parse( $in ) {
		if ( method_exists( $this->mParser, 'recursiveTagParse' ) ) {
			// New fast method
			return $this->mParser->recursiveTagParse( $in );
		} else {
			// Old method
			$ret = $this->mParser->parse(
				$in,
				$this->mParser->mTitle,
				$this->mParser->mOptions,
				// Avoid whitespace buildup
				false,
				// Important, otherwise $this->clearState()
				// would get run every time <ref> or
				// <references> is called, fucking the whole
				// thing up.
				false
			);
			$text = $ret->getText();
			
			return $this->fixTidy( $text );
		}
	}

	/**
	 * Tidy treats all input as a block, it will e.g. wrap most
	 * input in <p> if it isn't already, fix that and return the fixed text
	 *
	 * @static
	 *
	 * @param string $text The text to fix
	 * @return string The fixed text
	 */
	function fixTidy( $text ) {
		global $wgUseTidy;

		if ( ! $wgUseTidy )
			return $text;
		else {
			$text = preg_replace( '~^<p>\s*~', '', $text );
			$text = preg_replace( '~\s*</p>\s*~', '', $text );
			$text = preg_replace( '~\n$~', '', $text );
			
			return $text;
		}
	}

	/**
	 * Generate the labels to pass to the
	 * 'cite_references_link_many_format' message, the format is an
	 * arbitrary number of tokens separated by [\t\n ]
	 */
	function genBacklinkLabels() {
		wfProfileIn( __METHOD__ );
		$text = wfMsgForContentNoTrans( 'cite_references_link_many_format_backlink_labels' );
		$this->mBacklinkLabels = preg_split( '#[\n\t ]#', $text );
		wfProfileOut( __METHOD__ );
	}

	/**
	 * Generate the labels to pass to the
	 * 'cite_reference_link' message instead of numbers, the format is an
	 * arbitrary number of tokens separated by [\t\n ]
	 */
	function genLinkLabels($group, $message) {
		global $wgMessageCache;
		
		wfProfileIn( __METHOD__ );
		$text = false;
		if (is_object($wgMessageCache))
			$text = $wgMessageCache->get( $message, true, false );
		$this->mLinkLabels[$group] = ($text == '') ? false : preg_split( '#[\n\t ]#', $text );
		wfProfileOut( __METHOD__ );
	}

	/**
	 * Gets run when Parser::clearState() gets run, since we don't
	 * want the counts to transcend pages and other instances
	 */
	function clearState() {
		# Don't clear state when we're in the middle of parsing
		# a <ref> tag
		if ( $this->mInCite || $this->mInReferences )
			return true;
 
		$this->mGroupCnt = array();
		$this->mOutCnt = - 1;
		$this->mInCnt = 0;
		$this->mRefs = array();
		$this->mReferencesErrors = array();
		$this->mRefCallStack = array();

		return true;
	}

	/**
	 * Called at the end of page processing to append an error if refs were 
	 * used without a references tag.
	 */
	function checkRefsNoReferences( &$parser, &$text ) {
		if ( $parser->getOptions()->getIsSectionPreview() ) return true;

		foreach ( $this->mRefs as $group => $refs ) {
			if ( count( $refs ) == 0 ) continue;
			$text .= "\n<br />";
			if ( $group == CITE_DEFAULT_GROUP ) {
				$text .= $this->error( 'cite_error_refs_without_references' );
			} else {
				$text .= $this->error( 'cite_error_group_refs_without_references', htmlspecialchars( $group ) );
			}
		}
		return true;
	}

	/**
	 * Initialize the parser hooks
	 */
	function setHooks() {
		global $wgParser, $wgHooks;

		$wgParser->setHook( 'ref' , array( &$this, 'ref' ) );
		$wgParser->setHook( 'references' , array( &$this, 'references' ) );

		$wgHooks['ParserClearState'][] = array( &$this, 'clearState' );
		$wgHooks['ParserBeforeTidy'][] = array( &$this, 'checkRefsNoReferences' );
	}

	/**
	 * Return an error message based on an error ID
	 *
	 * @param string $key   Message name for the error
	 * @param string $param Parameter to pass to the message
	 * @return string XHTML ready for output
	 */
	function error( $key, $param = null ) {
		# We rely on the fact that PHP is okay with passing unused argu-
		# ments to functions.  If $1 is not used in the message, wfMsg will
		# just ignore the extra parameter.
		return
			$this->parse(
				'<strong class="error">' .
				wfMsgNoTrans( 'cite_error', wfMsgNoTrans( $key, $param ) ) .
				'</strong>'
			);
	}

	/**
	 * Die with a backtrace if something happens in the code which
	 * shouldn't have
	 *
	 * @param int $error  ID for the error
	 * @param string $data Serialized error data
	 */
	function croak( $error, $data ) {
		wfDebugDieBacktrace( wfMsgForContent( 'cite_croak', $this->error( $error ), $data ) );
	}

	/**#@-*/
}
