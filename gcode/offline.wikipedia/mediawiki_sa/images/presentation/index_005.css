/* <source lang="css"> */

/* Do not print:
   1: When in mainspace: Article message boxes,
      navboxes, sister project boxes, disambig links,
      and items marked as metadata.
   2: Privacy, about, disclaimer, redirect and section edit links.
   3: Show/hide toggles for collapsible items.
   4: Items marked as noprint.
*/
.ns-0 .ambox, 
.ns-0 .navbox, 
.ns-0 .infobox.sisterproject, 
.ns-0 .dablink, 
.ns-0 .metadata, 
#footer-places,
.editlink,
span.collapseButton, a.NavToggle,
.noprint {
    display: none;
}

/* Add formatting to make sure that "external references" from templates
   like [[Template:Ref]] do not get URL expansion, not even when printed.
   The anchor itself has class "external autonumber" and the url expansion
   is inserted when printing (see the common printing style sheet at
   http://en.wikipedia.org/skins-1.5/common/commonPrint.css) using the
   ":after" pseudo-element of CSS. Also hide in <cite> elements.
*/
#content cite a.external.text:after,
.nourlexpansion a.external.text:after,
.nourlexpansion a.external.autonumber:after {
    display: none !important;
}

/* Uncollapse collapsible tables/divs.
   The proper way to do this for tables is to use display:table-row,
   but this is not supported by all browsers, so use display:block as fallback.
*/
table.collapsible tr, div.NavPic, div.NavContent {
    display: block !important;
}
table.collapsible tr {
    display: table-row !important;
}

/* Hiding some items when printing with Simple skin */
.skin-simple div#column-one,
.skin-simple div#f-poweredbyico,
.skin-simple div#f-copyrightico,
.skin-simple .editsection { 
    display: none; 
}

/* wikitable class for skinning normal tables */
table.wikitable {
    margin: 1em 1em 1em 0;
    border: 1px #aaa solid;
    border-collapse: collapse;
}
.wikitable th, .wikitable td {
    border: 1px #aaa solid;
    padding: 0.2em;
}
.wikitable th {
    text-align: center;
    background: #F2F2F2;
    font-weight: bold;
}
.wikitable caption {
    font-weight: bold;
}

/* </source> */