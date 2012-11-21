<?php
/**
 * Syntax highlighting extension for MediaWiki 1.5 using GeSHi
 * Copyright (C) 2005 Brion Vibber <brion@pobox.com>
 * http://www.mediawiki.org/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 * http://www.gnu.org/copyleft/gpl.html
 */

/**
 * @file
 * @ingroup Extensions
 * @author Brion Vibber
 *
 * This extension wraps the GeSHi highlighter: http://qbnz.com/highlighter/
 *
 * Unlike the older GeSHi MediaWiki extension floating around, this makes
 * use of the new extension parameter support in MediaWiki 1.5 so it only
 * has to register one tag, <source>.
 *
 * A language is specified like: <source lang="c">void main() {}</source>
 * If you forget, or give an unsupported value, the extension spits out
 * some help text and a list of all supported languages.
 */

if( !defined( 'MEDIAWIKI' ) ) {
	die();
}

$wgExtensionCredits['parserhook']['SyntaxHighlight_GeSHi'] = array(
	'path'           => __FILE__,
	'name'           => 'SyntaxHighlight',
	'author'         => array( 'Brion Vibber', 'Tim Starling', 'Rob Church', 'Niklas Laxström' ),
	'descriptionmsg' => 'syntaxhighlight-desc',
	'url'            => 'http://www.mediawiki.org/wiki/Extension:SyntaxHighlight_GeSHi',
);

$wgSyntaxHighlightDefaultLang = null; //Change this in LocalSettings.php
$dir = dirname(__FILE__) . '/';
$wgExtensionMessagesFiles['SyntaxHighlight_GeSHi'] = $dir . 'SyntaxHighlight_GeSHi.i18n.php';
$wgAutoloadClasses['SyntaxHighlight_GeSHi'] = $dir . 'SyntaxHighlight_GeSHi.class.php';
$wgHooks['ShowRawCssJs'][] = 'SyntaxHighlight_GeSHi::viewHook';
$wgHooks['ParserFirstCallInit'][] = 'efSyntaxHighlight_GeSHiSetup';

if ( version_compare( $wgVersion, '1.17alpha', '>=' ) ) {
	// For MediaWiki 1.17 alpha and later.
	$wgHooks['ExtensionTypes'][] = 'SyntaxHighlight_GeSHi::hSpecialVersion_GeSHi';
} else {
	// For pre-MediaWiki 1.17 alpha.
	$wgHooks['SpecialVersionExtensionTypes'][] = 'SyntaxHighlight_GeSHi::hOldSpecialVersion_GeSHi';
}

/**
 * Register parser hook
 *
 * @param $parser Parser
 */
function efSyntaxHighlight_GeSHiSetup( &$parser ) {
	$parser->setHook( 'source', array( 'SyntaxHighlight_GeSHi', 'parserHook' ) );
	$parser->setHook( 'syntaxhighlight', array( 'SyntaxHighlight_GeSHi', 'parserHook' ) );
	return true;
}
