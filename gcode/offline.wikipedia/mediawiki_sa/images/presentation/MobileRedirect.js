/** Mobile Redirect Helper
 *
 *  Redirects to a wikimedia-mobile installation for viewers on iPhone, iPod 
 *  Touch, Palm Pre, and Android devices.
 *
 *  You can turn off the redirect by setting the cookie "stopMobileRedirect=true"
 */
if ( /(Android|iPhone|iPod|webOS|NetFront|Opera Mini|SEMC-Browser|PlayStation Portable|Nintendo Wii|BlackBerry)/
	.test( navigator.userAgent ) )
{
	(function () {
		function haveStopCookie() {
			return (document.cookie.indexOf("stopMobileRedirect=true") >= 0);
		}

		function getMobileUrl() {
			var mainPage = wgMainPageTitle.replace(/ /g, '_');
			var url = wgWikimediaMobileUrl + '/';
			if (wgPageName == mainPage) {
				url += '::Home'; // Special case
			} else {
				url += encodeURIComponent(wgPageName).replace('%2F','/').replace('%3A',':');
			}
			url += '?wasRedirected=true';
			return url;
		}

		// Don't redirect if we have the stop cookie
		if (haveStopCookie()) return;

		// Don't redirect special pages
		if (wgNamespaceNumber < 0) return;

		// Don't redirect URLs that aren't simple page views
		if (document.location.search && document.location.search.length > 0) {
			var params = document.location.search.substr(1).split('&');
			for (var i = 0; i < params.length; i++) {
				var paramParts = params[i].split('=');
				if (paramParts.length && paramParts[0] != 'title') {
					return;
				}
			}
		}

		document.location = getMobileUrl();
	})();
}
