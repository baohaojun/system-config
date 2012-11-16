// [[image:Erioll_world.svg|18px]] '''WikiMiniAtlas''' <br>
// Script to embed interactive maps into pages that have coordinate templates <br>
// also check my user page [[User:Dschwen]] for more tools<pre>
//
// Revision 13

var wikiminiatlas =
{
 config:
 {
  width  : 600,
  height : 400,
  timeout : 5000,
  zoom : -1,
  quicklink : false,
  quicklinkurl : 'http://maps.google.com/maps?ll={latdegdec},{londegdec}&spn={span},{span}&q={latdegdec},{londegdec}',
  enabled : true,
  onlytitle : false,
  iframeurl : 'http://toolserver.org/~dschwen/wma/iframe.html',
  imgbase   : 'http://toolserver.org/~dschwen/wma/tiles/',
  coordurls : [
                'http://stable.toolserver.org/geohack/geohack.php?',
                'http://stable.ts.wikimedia.org/geohack/geohack.php?',
                'http://toolserver.org/~magnus/geo/geohack.php?',
                'http://toolserver.org/~geohack/geohack.php?',
                'http://tools.wikimedia.de/~magnus/geo/geohack.php?',
                'http://www8.brinkster.com/erikbaas/wiki/maps.asp?',
                'http://www.nsesoftware.nl/wiki/maps.asp?' 
              ],
  buttonImage: 
'http://upload.wikimedia.org/wikipedia/commons/thumb/9/9a/Erioll_world.svg/18px-Erioll_world.svg.png'
 },

 strings:
 {
  buttonTooltip : {
   af:'Vertoon ligging op \'n interaktiwe kaart.',
   als:'Ort uf dr interaktivä Chartä zeigä',
   ar:'شاهد الموقع على الخريطة التفاعلية',
   'be-tarask':'паказаць месцазнаходжаньне на інтэрактыўнай мапе',
   'be-x-old':'паказаць месцазнаходжаньне на інтэрактыўнай мапе',
   bg:'покажи местоположението на интерактивната карта',
   bpy:'জীবন্ত মানচিত্রগর মা মাপাহান দেখাদিতই',
   br:'diskouez al lec\'hiadur war ur gartenn etrewezhiat',
   ca:'mostra la localització en un mapa interactiu',
   cs:'zobraz místo na interaktivní mapě',
   da:'vis beliggenhed på interaktivt kort',
   de:'Ort auf interaktiver Karte anzeigen',
   el:'εμφάνιση τοποθεσίας σε διαδραστικό χάρτη',
   en:'show location on an interactive map',
   eo:'Montru lokigon sur interaktiva karto',
   eu:'erakutsi kokalekua mapa interaktibo batean',
   es:'mostrar el lugar en un mapa interactivo',
   fr:'Montrer la localisation sur une carte interactive',
   fur:'mostre la localizazion suntune mape interative',
   fy:'it plak op in oanpasbere kaart oanjaan',
   gl:'Amosar o lugar nun mapa interactivo',
   he:'הראה מיקום במפה האינטראקטיבית',
   hi:'सक्रिय नक्शे पर लोकेशन या स्थान दिखायें', 
   hr:'prikaži lokaciju na interaktivnom zemljovidu',
   hu:'Mutasd a helyet egy interaktív térképen!',
   hy:'ցուցադրել դիրքը ինտերակտիվ քարտեզի վրա',
   it:'mostra la località su una carta interattiva',
   is:'sýna staðsetningu á gagnvirku korti',
   id:'Tunjukkan letak di peta interaktif',
   ja:'インタラクティブ地図上に位置を表示',
   km:'បង្ហាញទីតាំងនៅលើផែនទីអន្តរកម្ម',
   ko:'인터랙티브 지도에 위치를 표시',
   lt:'Rodyti vietą interaktyviame žemėlapyje',
   mk:'прикажи положба на интерактивна карта',
   nl:'de locatie op een interactieve kaart tonen',
   no:'vis beliggenhet på interaktivt kart',
   pl:'Pokaż lokalizację na mapie interaktywnej',
   pt:'mostrar a localidade num mapa interactivo',
   ro:'arată locaţia pe o hartă interactivă',
   ru:'показать положение на интерактивной карте',
   sk:'zobraz miesto na interaktívnej mape',
   sl:'Prikaži lego na interaktivnem zemljevidu',
   sq:'trego vendndodhjen në hartë',
   fi:'näytä paikka interaktiivisella kartalla',
   sv:'visa platsen på en interaktiv karta',
   uk:'показати положення на інтерактивній карті',
   vi:'xem vị trí này trên bản đồ tương tác',
   vo:'Jonön topi su kaed itjäfidik',
   zh:'显示该地在地图上的位置',
   'zh-cn':'显示该地在地图上的位置',
   'zh-sg':'显示该地在地图上的位置',
   'zh-tw':'顯示該地在地圖上的位置',
   'zh-hk':'顯示該地在地圖上的位置'
  },
  close : {
   af:'Sluit',
   als:'Zuä machä',
   ar:'غلق',
   'be-tarask':'закрыць',
   'be-x-old':'закрыць',
   bg:'затвори',
   bpy:'জিপা',
   br:'serriñ',
   ca:'tanca',
   cs:'zavřít',
   da:'luk',
   de:'schließen',
   el:'έξοδος',
   en:'close',
   eo:'fermu', 
   eu:'itxi',
   es:'cerrar',
   fr:'Quitter',
   fur:'siere',
   fy:'ticht',
   gl:'pechar',
   he:'לסגור',
   hi:'बंद करें',
   hr:'zatvori',
   hu:'bezárás', 
   hy:'փակել',
   id:'tutup',
   is:'loka',
   it:'chiudi',
   ja:'閉じる',
   km:'បិទ',
   ko:'닫기',
   lt:'uždaryti',
   mk:'затвори',
   nl:'sluiten',
   no:'lukk',
   pl:'zamknij',
   pt:'fechar',
   ro:'închide',
   ru:'закрыть',
   sk:'zatvoriť',
   sl:'zapri',
   sq:'mbylle',
   fi:'sulje',
   sv:'stäng',
   uk:'закрити',
   vi:'đóng',
   vo:'färmükön',
   zh:'关闭',
   'zh-cn':'关闭',
   'zh-sg':'关闭',
   'zh-tw':'關閉',
   'zh-hk':'關閉'
  }
 },

 bodyc : null,

 language : '',
 site: '',
 iframe : { div: null, iframe: null, closebutton: null },

 coord_index: 0,
 coord_filter: /^([\d+-.]+)_([\d+-.]*)_?([\d+-.]*)_?([NS])_([\d+-.]+)_([\d+-.]*)_?([\d+-.]*)_?([EOW])/,

 quicklinkbox : null,
 quicklinkdest : null,

 region_index : 0,
 coordinate_region : '',

 WikiMiniAtlasHTML : '',

 hookUpMapbutton : function( mb )
 {
  var mapparam = mb.mapparam,
      mapy     = wikiminiatlas.totalOffset( mb, 0 ) + 20;

  function doEvent()
  {
   wikiminiatlas.toggleIFrame( mapparam, mapy );
   return true;
  }
  mb.onclick = doEvent;
 },

 // vertikale position auf der Seite bestimmen
 totalOffset : function( obj, offset )
 {
  if( obj.offsetParent === null || obj.offsetParent.id === 'content' ) {
   return offset + obj.offsetTop;
  }
  else {
   return wikiminiatlas.totalOffset(obj.offsetParent, offset + obj.offsetTop );
  }
 },

 // move iframe around and toggle visibility
 toggleIFrame : function( mp, my )
 {
  var w = wikiminiatlas,
      wi = w.iframe,
      newurl = w.config.iframeurl + '?' + mp;

  if( wi.div.style.visibility !== "visible" ||
    ( ( wi.iframe.src !== newurl ) && ( my !== undefined ) ) )
  {
   if( wi.iframe.src !== newurl )
   {
    wi.iframe.src = newurl;
   }
   wi.div.style.top = my + 'px';
   wi.div.style.visibility = "visible";
   wi.div.style.display = "block";
  }
  else
  {
   wi.div.style.visibility = "hidden";
   wi.div.style.display = "none";
  }
  return false;
 },

 // start the timer to fade in the quicklink tooltip
 qlStart : function()
 {
 },

 // abort the timer, hide the tooltip 
 qlStop : function()
 {
 },

 // show the tooltip menu
 qlShowMenu : function()
 {
 },

 // fill in the map-service templates 
 qlURL : function( lat, lon, zoom )
 {
  var url  = wikiminiatlas.config.quicklinkurl,
      span = Math.pow( 2.0, zoom) / 150.0;

  url = url.replace( /\{latdegdec\}/g, lat );
  url = url.replace( /\{londegdec\}/g, lon );
  url = url.replace( /\{span\}/g, span.toFixed(4) );

  return url;
 },

 // Check against coordinate urls
 isMaplink : function( url_orig )
 {
  var w = wikiminiatlas,
      url, key;

  if( typeof(url_orig) !== 'string' ) { return false; }

  // needed for the russian WP
  try { url = decodeURI( url_orig ); } catch(err) { url = url_orig; }

  for( key in w.config.coordurls ) {
   if( url.substr(0,w.config.coordurls[key].length) === w.config.coordurls[key] ) {
    return true;
   }
  }

  return false;
 },

 // Insert the IFrame into the page.
 loader : function()
 {
  var w = wikiminiatlas,
      wi = w.iframe,
      wc = w.config,
      ds_filter = /(dim|scale):([\d+-.]+)(km|)/,
      marker = { lat:0, lon:0 },
      link, links, coordinates, quicklinkbox,
      key, len, coord_title, coord_params, icons, startTime, zoomlevel, content, mapbutton;

  // apply settings
  if( typeof(wma_settings) === 'object' ) {
   for ( key in wma_settings) {
    if( typeof(wma_settings[key]) === typeof(wc[key]) ) {
     wc[key] = wma_settings[key];
    }
   }
  }

  if( wc.enabled === false ) { return; }

  w.site = window.location.host.substr( 0, window.location.host.indexOf('.') );
  w.language = wgUserLanguage;

  // remove icons from title coordinates
  coord_title = document.getElementById('coordinates') || document.getElementById('coordinates-title');
  if( coord_title ) {
   icons = coord_title.getElementsByTagName('a');
   len = icons.length;
   for( key = 0; key < len; key++ ) {
    if( typeof(icons[key]) === 'object' &&
        icons[key] !== null &&
        icons[key].className === 'image' ) {
     icons[key].parentNode.removeChild(icons[key]);
    }
   }
  }

  if( wc.onlytitle ) {
   w.bodyc = document.getElementById('coordinates') || document.getElementById('coordinates-title');
   if( w.bodyc === null ) { return; }
  }
  else {
   // the french moved their title coordinates outside of bodyContent!
   if( w.site === 'fr' ) {
     w.bodyc = document.getElementById('content') || document;
   } else {
     w.bodyc = document.getElementById('bodyContent') || document;
   }
  }

  startTime = (new Date()).getTime();

  links = w.bodyc.getElementsByTagName('a');
  len = links.length;
  for( key = 0; key < len; key++ )
  {
   link = links[key];

   // check for timeout (every 10 links only)
   if( key % 10 === 9 && (new Date()).getTime() > startTime + wc.timeout ) { break; }

   if( link.className !== 'external text'  ||
       link.href.match(/_globe:(?!earth)/i) !== null ) { continue; }

   coordinates = link.href.replace( /−/g, '-' );
   coord_params = coordinates.match(/&params=([^&=<>|]{7,255})/);

   if(!coord_params) { continue; }
   coord_params = coord_params[1];

   if(w.coord_filter.test(coord_params)) {
    w.coord_filter.exec(coord_params);
    marker.lat=(1.0*RegExp.$1) + ((RegExp.$2||0)/60.0) + ((RegExp.$3||0)/3600.0);
    if( RegExp.$4 === 'S' ) { marker.lat*=-1; }
    marker.lon=(1.0*RegExp.$5) + ((RegExp.$6||0)/60.0) + ((RegExp.$7||0)/3600.0);
    if( RegExp.$8 === 'W' ) { marker.lon*=-1; }
   }

   // Find a sensible Zoom-level based on type
   zoomlevel = 1;
   if( coord_params.indexOf('_type:landmark') >= 0 ) {
    zoomlevel = 8;
   } else if( coord_params.indexOf('_type:city') >= 0 ) {
    zoomlevel = 4;
   }

   // If given use dim or scale for a zoomlevel
   if( ds_filter.test(coord_params) )
   {
    ds_filter.exec(coord_params);
    // wma shows dim approx 4e7m at zoom 0 or 1.5e8 is the scale of zoomlevel 0
    zoomlevel = (RegExp.$1 === 'dim' ?
     ( RegExp.$3 === 'km' ? Math.log( 4e4/RegExp.$2 ) : Math.log( 4e7/RegExp.$2 ) ) :
     Math.log( 1.5e8/RegExp.$2 ) ) / Math.log(2);
    if( zoomlevel > 10 ) { zoomlevel = 10; }
   }

   if( wc.zoom !== -1 ) { zoomlevel = wc.zoom; }

   // Test the unicode Symbol
   if( w.site === 'de' && link.parentNode.id !== 'coordinates' ) {
    mapbutton = document.createElement('SPAN');
    mapbutton.appendChild( document.createTextNode('♁') );
    mapbutton.style.color = 'blue';
   } else {
    mapbutton = document.createElement('img');
    mapbutton.src = wc.buttonImage;
   }
   mapbutton.title = w.strings.buttonTooltip[w.language] || w.strings.buttonTooltip.en;
   mapbutton.alt = '';
   mapbutton.style.padding = '0px 3px 0px 0px';
   mapbutton.style.cursor = 'pointer';
   mapbutton.className = 'noprint';
   mapbutton.mapparam =
    marker.lat + '_' + marker.lon + '_' +
    wc.width + '_' + wc.height + '_' +
    w.site + '_' + zoomlevel + '_' + w.language;

   // link.parentNode.insertBefore(mapbutton, link.nextSibling);
   link.parentNode.insertBefore(mapbutton,link);
   w.hookUpMapbutton(mapbutton);

   if ( wc.quicklink ) {
    link.href = w.qlURL( marker.lat, marker.lon, zoomlevel );
    link.onmouseover = w.qlStart;
    link.onmouseout = w.qlStop;
   }

  } //for

  // prepare quicklink menu box
  if ( coordinates !== null && wc.quicklink ) {
   quicklinkbox = document.createElement('div');
   // more to come :-)
  }

  // prepare iframe to house the map
  if ( coordinates !== null ) {
   wi.div = document.createElement('div');
   wi.div.style.visibility = 'hidden';
   wi.div.style.display = 'none';
   wi.div.style.width = (wc.width+2)+'px';
   wi.div.style.height = (wc.height+2)+'px';
   wi.div.style.margin = '0px';
   wi.div.style.padding = '0px';
   wi.div.style.backgroundColor = 'white';
   wi.div.style.position = 'absolute';
   wi.div.style.right = '2em';
   wi.div.style.top = '1em';
   wi.div.style.border = '1px solid gray';
   wi.div.style.zIndex = 13;

   wi.closebutton = document.createElement('img');
   wi.closebutton.title = w.strings.close[w.language] || w.strings.close.en;
   // was: config.imgbase + 'button_hide.png'
   wi.closebutton.src = 'http://upload.wikimedia.org/wikipedia/commons/d/d4/Button_hide.png';
   wi.closebutton.style.zIndex = 15;
   wi.closebutton.style.position = 'absolute';
   wi.closebutton.style.right = '11px';
   wi.closebutton.style.top = '9px';
   wi.closebutton.style.width = '18px';
   wi.closebutton.style.cursor = 'pointer';
   wi.closebutton.mapparam = '';

   wi.closebutton.onclick = w.toggleIFrame;

   wi.iframe = document.createElement('iframe');
   wi.iframe.scrolling = 'no';
   wi.iframe.frameBorder = '0';
   wi.iframe.style.zIndex = 14;
   wi.iframe.style.position = 'absolute';
   wi.iframe.style.right = '1px';
   wi.iframe.style.top = '1px';
   wi.iframe.style.width = (wc.width)+'px';
   wi.iframe.style.height = (wc.height)+'px';
   wi.iframe.style.margin = '0px';
   wi.iframe.style.padding = '0px';

   wi.div.appendChild(wi.iframe);
   wi.div.appendChild(wi.closebutton);

   content = document.getElementById('content') || document.getElementById('mw_content');
   if(content) { content.insertBefore(wi.div,content.childNodes[0]); }
  }
 }
};

//
// Hook up installation function
//
addOnloadHook(wikiminiatlas.loader);

//</pre>