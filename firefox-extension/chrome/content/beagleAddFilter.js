function beaglePrefsAddFilterClicked() {
	var pattern = document.getElementById('beagle.add.filter.pattern').value;
    var name = document.getElementById('beagle.add.filter.name').value;
    var patternType = document.getElementById('beagle.add.filter.patterntype').selectedItem.value;
	var type = window.arguments[0];
	var elementId = 'beagle.'+type+'.list';
	var listbox = window.opener.document.getElementById(elementId);
    appendRow(listbox,name,pattern,patternType);
}


