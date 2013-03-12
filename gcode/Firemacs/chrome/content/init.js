////////////////////////////////////////////////////////////////
//
// Initializing Firemacs
//

(function() {
    var ua = navigator.userAgent;
//  This does not work on a new frame of FF 3.0.
//  var firefoxVersion = gFindBar && gFindBar.onFindCommand ? 3 : 2;
    var firefoxVersion = ua.search('Firefox/2\\.|Iceweasel/2\\.|BonEcho/2\\.') > 0 ? 2 : 3;
    var preference = Firemacs.Preference.initialize(Firemacs.Config);
    var status = Firemacs.Status.initialize(preference);
    var binder = Firemacs.KeyBinder.initialize(preference, Firemacs.Config);
    var sfun = Firemacs.SubFunc.initialize(firefoxVersion);
    binder.init(sfun);
    var observer = Firemacs.Observer.initialize(binder, Firemacs.Config);
    observer.addObserver();
    var keyhandler = Firemacs.KeyHandler.initialize(firefoxVersion, binder, preference, status);
    window.addEventListener('keypress',
			    function(e) { keyhandler.keypress(e); },
			    true);
    window.addEventListener('DOMContentLoaded',
			    function(e) { keyhandler.contentLoad(e); },
			    false);
    window.addEventListener('DOMTitleChanged',
			    function(e) { keyhandler.contentSelect(e); },
			    false);
})();
