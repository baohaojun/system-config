#!/bin/bash

cd
echo '<html>
<HEAD>
<META http-equiv=content-type content="text/html; charset=GBK">
</head>
</html>' >.lqqm.html

cat lqqm.html >>.lqqm.html
rm lqqm.html&
"/c/Program Files/Mozilla Firefox/firefox" .lqqm.html&
