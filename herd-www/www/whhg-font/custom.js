/*=яжеохрэ("'";A1;"': '";B1;"', ";еякх(нярюр(ярпнйю(A1);5)=0;"|";""))
  =яжеохрэ(".";A1;":before{content:'\";B1;"'}")
  =яжеохрэ("<li><i class=""";A1;"""></i> ";A1;"</li>")*/
(function(d, s, id) {
  var js, fjs = d.getElementsByTagName(s)[0];
  if (d.getElementById(id)) return;
  js = d.createElement(s); js.id = id;
  js.src = "//connect.facebook.net/en_US/all.js#xfbml=1&appId=216775695049843";
  fjs.parentNode.insertBefore(js, fjs);
}(document, 'script', 'facebook-jssdk'));
function hslToRgb(h, s, l) {
    var r, g, b;

    if (!s) {
        r = g = b = l; // achromatic
    }
    else {
        function hue2rgb(p, q, t) {
            if(t < 0) t += 1;
            if(t > 1) t -= 1;
            if(t < 1/6) return p + (q - p) * 6 * t;
            if(t < 1/2) return q;
            if(t < 2/3) return p + (q - p) * (2/3 - t) * 6;
            return p;
        }

        var q = l < 0.5 ? l * (1 + s) : l + s - l * s;
        var p = 2 * l - q;
        r = hue2rgb(p, q, h + 1/3);
        g = hue2rgb(p, q, h);
        b = hue2rgb(p, q, h - 1/3);
    }
    r = 'rgb('+Math.round(r * 255)+', '+Math.round(g * 255)+', '+Math.round(b * 255)+')';
    return r;
}
function applySliders() {
   var
     	color = $('[name=color]').val(),
       shadow = $('[name=shadow]').val();
   $('.result').css('font-size', $('[name=size]').val()+'px')
  		.css('color', hslToRgb(color/100, 0.5, 0.5))
  		.css('text-shadow', shadow+'px ' + shadow+'px '
       		+ $('[name=blur]').val() + 'px black')
  		.css('filter', 'progid:DXImageTransform.Microsoft.Shadow(Color=black, direction=135, strength='+shadow+')');
}
function findIcons() {
 var iconame = $.trim($('.searchIcons input[type=text]').val()), numCols = 6, i, s, prevTab;
 $('#searchResults .panIcons').html('');
 prevTab = $('.tabs .current');
 if (!iconame) {
   prevTab.click();
   return;
 }
 $('#searchTab a').click().removeClass('current');
 prevTab.addClass('current');
 s = '';  i = 0;
 $('.panIcons [class*="'+iconame+'"]').each(function() {
    i++;
    if (i == numCols) {
      s += '</tr><tr>';
      i = 0;
    }
    s += '<td>'+$(this).parent().html()+'</td>';
 });
 if (s)
   s = '<table><tr>'+s+'</tr></table>';
 $('#searchResults .panIcons').html(s);
 $('.panes .panel').hide();
 $('#searchResults').show();
}
function shuffle(o) {
    for (var j, x, i = o.length; i; j = parseInt(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
    return o;
}
function preparePreviewAndSearch(ligatures) {
 var values = [], copyvalues, previcons = [], s = '', i;
 ligatures = ligatures.split("\n");
 for (i in ligatures)
   if (ligatures[i].substr(0, 6) == '.icon-')
     values.push(ligatures[i].split(':')[0].substr(1));
 copyvalues = values;
 for (i=0; i<50; i++) {
   var tmp = copyvalues.splice(Math.floor(Math.random()*copyvalues.length), 1);
   previcons.push(tmp[0]);
 }
 for (i in previcons)
   s += ' <span class="'+previcons[i]+'"></span>';
 $('#preview').html('<h2>Random icons preview</h2>' + s);

 $('.searchIcons input[type=text]').autocomplete({lookup:values, deferRequestBy:50, onSelect: findIcons});
 $('.searchIcons input[type=text]').keyup(findIcons);
}

$(document).ready(function() {
 $('.sliders input').rangeinput({
   css: {handle: 'knob'},
   onSlide: applySliders,
   change: applySliders,
 });
 applySliders();
 
 $.get("css/whhg.css", preparePreviewAndSearch);
 
 // initiate page scroller plugin
 $('body').pageScroller({navigation: '.ps'});
});

$(function() {
    // setup ul.tabs to work as tabs for each div directly under div.panes
    $("ul.tabs").tabs("div.panes > .panel");
});