// globals
var ignitionButton = null;
var WMSQueryButton = null;
var infoPanelButton = null;
var mymap = null;
var ttinstances = null;

function makeTooltips(){
  
  $('[title]').each(function() {
     $(this).attr('data-tippy-content', $(this).attr('title'));
     $(this).removeAttr('title');
   }); 
        
  ttinstances = tippy('[data-tippy-content]', {
    theme: 'cool',
    allowHTML: true,
    arrow: true,
    animation: 'scale',
    placement: 'top',
    trigger: 'mouseenter focus',
    interactive: true,
    interactiveBorder: 10
  });
}

$(document).ready(function () {
  makeTooltips();
  ttinstances.forEach(i => i.disable());
});

Shiny.addCustomMessageHandler("layersControlReady", function(message) {
    setTimeout(function() {
     autoGroupLeafletLayers(".leaflet-control-layers");
  }, 500);
 
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



var getFromOpenMeteo = function(){
  var center = mymap.getCenter();  
  $.ajax({
    url: "https://api.open-meteo.com/v1/forecast?latitude="+center.lat+"&longitude="+center.lng+"&current=temperature_2m,relative_humidity_2m,weather_code,wind_speed_10m,wind_direction_10m&wind_speed_unit=ms",
    dataType: "json",
    success: function(data) {
       Shiny.setInputValue('openmeteoInput', data, {priority: 'event'});
    },
    error: function(jqXHR, textStatus, errorThrown) {
      console.error("API call failed:", textStatus, errorThrown);
      alert("API Meteo call failed: " +textStatus + ": " + errorThrown);
    },
    complete: function() { 
      loader.hide();
    }
  });

}

var   queryWMS = function(lat=null, lon=null,   pop=false)  {
    if(lat===null){
      var center = mymap.getCenter();
      lat =center.lat;
      lon =center.lng;
    }
    const time = dateFromToday(0); 
    const buffer = 0.00001;
    var latlng = L.latLng(lat, lon);
      const bbox = [
        lon - buffer,
        lat - buffer,
        lon + buffer,
        lat + buffer
    ].join(',');

 
    var outputf = 'text/html';
    const url = "https://maps.effis.emergency.copernicus.eu/gwis" +
        L.Util.getParamString({
            service: 'WMS',
            request: 'GetFeatureInfo',
            version: '1.1.1',
            layers: 'ecmwf.query',
            query_layers: 'ecmwf.query',
            styles: '',
            bbox: bbox,
            FEATURE_COUNT: 1,
            height: 3,
            width: 3,
            srs: 'EPSG:4326',
            info_format: outputf,   // JSON often NOT supported here
            x: 2,
            y: 2,
            time: time                 // 🔥 critical for EFFIS
        });

    $.ajax({
        url: url,
        success: function (data) {
          console.log("wms effis ")
          console.log( pop)
            if(pop) {
              Shiny.setInputValue('WMSQueryReturnedPop', {coords:[lon, lat], datast:data}, {priority: 'event'});
            } else {
              Shiny.setInputValue('WMSQueryReturned', {coords:[lon, lat], datast:data}, {priority: 'event'});
            }
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
    var title = $label.attr("title");
    var fullText = $span.text().trim();
    if (title != null) {
      fullText = title;
    }
    
    $label.attr("title", fullText);

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
    var $header = $('<div class="leaflet-layer-group-header"><strong> ▼ ' + prefix + ' </strong></div>');
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
      $(this).find('strong').text(text + ' ' + prefix);
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

var toggleInfoPanelButton = function(force=false){ 
  var el = infoPanelButton;
  if (force || L.DomUtil.hasClass(el, 'pressed')) {
    L.DomUtil.removeClass(el, 'pressed'); 
  } else {
    L.DomUtil.addClass(el, 'pressed'); 
  }
  $('#map > .leaflet-control-container > .leaflet-bottom.leaflet-right').toggle();
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

// Simple global loader object
window.loader = {
  show: function() {
    document.getElementById('page-loader').classList.add('show');
  },
  hide: function() {
    document.getElementById('page-loader').classList.remove('show');
  }
};

// Example: automatically show loader on all AJAX calls (optional)
if (window.jQuery) {
  $(document).ajaxStart(() => loader.show());
  $(document).ajaxStop(() => loader.hide());
}