// globals
var ignitionButton = null;
var WMSQueryButton = null;
var mymap = null;
$(document).ready(function () {
 $(document).on('change', '.leaflet-control-container', function(e) {
    console.log("Control interaction:", e.target);
   // autoGroupLeafletLayers(".leaflet-control-layers");
});
});

function dateFromToday(back=0){
  const yesterday = new Date();
  yesterday.setDate(yesterday.getDate() - back);

  const yyyy = yesterday.getFullYear();
  const mm = String(yesterday.getMonth() + 1).padStart(2, '0');
  const dd = String(yesterday.getDate()).padStart(2, '0');

  const dateStr = `${yyyy}-${mm}-${dd}`;
  return(dateStr);
}

var   queryWMS = function(lat, lon, layer,  back=0)  {
    const time = dateFromToday(back);

    const buffer = 0.00001;
    var latlng = L.latLng(lat, lon);
      const bbox = [
        lon - buffer,
        lat - buffer,
        lon + buffer,
        lat + buffer
    ].join(',');


    const url = "https://maps.effis.emergency.copernicus.eu/gwis" +
        L.Util.getParamString({
            service: 'WMS',
            request: 'GetFeatureInfo',
            version: '1.1.1',
            layers: 'ecmwf.query',
            query_layers: 'ecmwf.query',
            styles: '',
            bbox: bbox,
            FEATURE_COUNT: 2,
            height: 3,
            width: 3,
            srs: 'EPSG:4326',
            info_format: 'text/html',   // JSON often NOT supported here
            x: 2,
            y: 2,
            time: time                 // 🔥 critical for EFFIS
        });

    $.ajax({
        url: url,
        success: function (data) {
            Shiny.setInputValue('WMSQueryReturned', {coords:[lon, lat], datast:data}, {priority: 'event'});
        }
    });
}

var autoGroupLeafletLayers = function (controlSelector) {
  console.log("grouping layers");
  /*
    Groups layers by text before the first dash "-"
    Layers without dash remain ungrouped
    Display text after the dash only
  */

  var $control = $(controlSelector);
  if ($control.length === 0) return;

  var $form = $control.find('> form');
  var $labels = $form.find('label');

  var groups = {};
  var ungrouped = []; // labels without dash

  $labels.each(function() {
    var $label = $(this);
    var $span = $label.find('span').first(); // get the span containing the name
    var fullText = $span.text().trim();

    if (fullText.includes(" - ")) {
      var parts = fullText.split(" - ");
      var prefix = parts[0].trim();
      var suffix = "\u00A0"+parts.slice(1).join(" - ").trim();

      // update span to show only the suffix
      $span.text(suffix);

      if (!groups[prefix]) groups[prefix] = [];
      groups[prefix].push($label);
    } else {
      ungrouped.push($label);
    }
  });

  // rebuild form
  $form.empty();

  // add grouped layers
  Object.keys(groups).forEach(function(prefix) {
    var $header = $('<div class="leaflet-layer-group-header"><strong>' + prefix + ' ▼</strong></div>');
    var $container = $('<div class="leaflet-layer-group-container"></div>');

    $container.css({ 'padding-left': '10px' });
    groups[prefix].forEach(function($label) {
      $container.append($label);
    });

    $form.append($header).append($container);

    // toggle group on click
    $header.on('click', function() {
      $container.toggle();
      var text = $container.is(':visible') ? '▼' : '►';
      $(this).find('strong').text(prefix + ' ' + text);
    });
  });

  // append ungrouped layers at the end
  ungrouped.forEach(function($label) {
    $form.append($label);
  });
}



var toggleWMSQueryButton = function(force=false){
  var el = WMSQueryButton;

  if(ignitionButton !== null) L.DomUtil.removeClass(ignitionButton, 'pressed');
  if (force || L.DomUtil.hasClass(el, 'pressed')) {
    L.DomUtil.removeClass(el, 'pressed');
    document.getElementById('map').style.cursor = 'pointer';
    Shiny.setInputValue('map_mode', null, {priority: 'event'});
  } else {
    L.DomUtil.addClass(el, 'pressed');
    document.getElementById('map').style.cursor = 'crosshair';
    Shiny.setInputValue('map_mode', 'WMSQuery', {priority: 'event'});
  }
}

var toggleIgnitionButton = function(force=false){
  var el = ignitionButton;
  if(WMSQueryButton !== null) L.DomUtil.removeClass(WMSQueryButton, 'pressed');

  if (force || L.DomUtil.hasClass(el, 'pressed')) {
    L.DomUtil.removeClass(el, 'pressed');
    document.getElementById('map').style.cursor = 'pointer';
    Shiny.setInputValue('map_mode', null, {priority: 'event'});
  } else {
    L.DomUtil.addClass(el, 'pressed');
    document.getElementById('map').style.cursor = 'crosshair';
    Shiny.setInputValue('map_mode', 'ignitionPoint', {priority: 'event'});
  }
}

Shiny.addCustomMessageHandler('scrollLog', function(id) {
  var el = document.getElementById(id);
  if (el) {
    el.scrollTop = el.scrollHeight;
  }
});

$(document).on('click', '#upload_table_weather', function(e) {
  $('#upload_table_weather_input, input[type=file]').click();
});
$(document).on('click', '#upload_table_FBP', function(e) {
  $('#upload_table_FBP_input, input[type=file]').click();
});

$(document).on('click', '#upload_table_ignition', function(e) {
  $('#upload_table_ignition_input, input[type=file]').click();
});

$("#showLegendCLS").on("click", function(e) {
  alert();
  e.stopPropagation();
});




var updateControl = function() {

    const overlays = $('.leaflet-control-layers-overlays');
    if (overlays.length && overlays.find('#inputRaster').length === 0) {

      const labels = overlays.children('label');

      const container = $('<div id="inputRaster"></div>');
      const title = $('<div class="layers-subtitle">Input Raster</div>');

      container.append(title);
      container.append(labels);

      $('.leaflet-control-layers-overlays').append(container);
      console.log(labels);
      console.log(container);
      console.log(overlays);
    }
};
