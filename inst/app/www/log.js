// globals
var ignitionButton = null;
var WMSQueryButton = null;
var infoPanelButton = null;
var mymap = null;
var ttinstances = null;

function copyToClipboard(id) {
  var text = document.getElementById(id).innerText;
  navigator.clipboard.writeText(text);
  alert("Code copied to clipboard")
}

var tooltipsStrings = {
  Instance: "Instance",
  datetime: "Date and time of  specific weather scenario", 
  RH: "💧 Relative Humidity: % of moisture in the air",
  WD: "🧭 Dir: wind source direction in degrees where 0° is from East to West and values increase clockwise",
  WS: "💨 Wind speed in km/h",
  TMP: "🌡️ Temp: air temperature in °C",
  FFMC: "🔥 FFMC: Fine Fuel Moisture Code (0–100+) — dryness of fine fuels, easier ignition — see EFFIS FWI system <a href=https://forest-fire.emergency.copernicus.eu/about-effis/technical-background/fire-danger-forecast target=_blank>LINK HERE</a>",
  DMC:  "🌾 DMC: Duff Moisture Code (0–200+) — moisture of medium fuels, influences fuel availability — see EFFIS FWI system <a href=https://forest-fire.emergency.copernicus.eu/about-effis/technical-background/fire-danger-forecast  target=_blank>LINK HERE</a>",
  DC:   "🏜️ DC: Drought Code (0–800+) — deep layer dryness, longer lasting fires — see EFFIS FWI system <a href=https://forest-fire.emergency.copernicus.eu/about-effis/technical-background/fire-danger-forecast target=_blank >LINK HERE</a>",
  ISI:  "🌬️ ISI: Initial Spread Index (0–15+) — expected potential fire spread combining wind & FFMC — see EFFIS FWI description <a href=https://docs.argos-emergency.com/en/docs/data/hazard/nwp/effis.html target=_blank >LINK HERE</a>",
  BUI:  "🌲 BUI: Buildup Index (0–180+) — total fuel available combining DMC+DC — see EFFIS FWI description <a href=https://docs.argos-emergency.com/en/docs/data/hazard/nwp/effis.html  target=_blank>LINK HERE</a>",
  FWI:  "🔥📈 FWI: Fire Weather Index (0–50+) — overall fire danger rating combining ISI & BUI — see EFFIS overview <a href=https://forest-fire.emergency.copernicus.eu/about-effis/technical-background/fire-danger-forecast  target=_blank>LINK HERE</a>",
  FireScenario: "For Scott&Burgan can be from 1 to 4 ....."
};



function runToolTipsEdu(){
  
}


function makeTooltips(size=13){
  assignTitles();  
  $('[title]').each(function() {
     $(this).attr('data-tippy-content', $(this).attr('title'));
     $(this).removeAttr('title');
   }); 
   
   
  $('[data-value]').each(function() {
     $(this).attr('data-tippy-content', $(this).attr('data-value'));
   }); 
        
  ttinstances = tippy('[data-tippy-content]', {
    theme: 'cool',
    allowHTML: true,
    appendTo: document.body,  
    arrow: true,
    animation: 'scale', 
    trigger: 'mouseenter focus',
    interactive: true,
    interactiveBorder: 10,
    onShow(instance) {  instance.popper.querySelector('.tippy-box').style.fontSize = size+'px'; }
  });
  
   
}








$(document).ready(function () {
  makeTooltips();
  ttinstances.forEach(i => i.disable());
 
   assignTitles();       
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



var getFromOpenMeteo = function(lat=null, lon=null,   pop=false){
  
      if(lat===null){
      var center = mymap.getCenter();
      lat =center.lat;
      lon =center.lng;
    } 
  $.ajax({
    url: "https://api.open-meteo.com/v1/forecast?latitude="+ lat+"&longitude="+lon+"&current=temperature_2m,relative_humidity_2m,weather_code,wind_speed_10m,wind_direction_10m&wind_speed_unit=ms",
    dataType: "json",
          timeout: 3000,  
    success: function(data) {
       Shiny.setInputValue('openmeteoInput', data, {priority: 'event'}); 
      var speed = Math.round(parseFloat(data.current.wind_speed_10m));
      var dir = data.current.wind_direction_10m;
      $('#speed').val(speed).trigger('input');  
      $('#wdir').text(dir).trigger('change');  
      $('#wtmp').text(data.current.temperature_2m + "°C").trigger('change'); 
    },
    error: function(jqXHR, textStatus, errorThrown) {
      console.error("API call failed:", textStatus, errorThrown); 
       Shiny.setInputValue('openmeteoInput', "Open Meteo API call failed: " + textStatus + " - " + errorThrown, {priority: 'event'});
       
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

autoGroupLeafletLayers = function (controlSelector) {
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
    e.stopPropagation();
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

var runContainer = null;
var stopSimlog = function(){
  var $box = $('#logSim');
  runContainer = $('<div class="run-card"/>');
  $box.append(runContainer);
  var timestamp = new Date().toLocaleTimeString();
  runContainer.append($('<div class="run-id-header"/>').text("Run Triggered at " + timestamp));

}
var startSimlog = function(){
  var $box = $('#logSim');
  runContainer = $('<div class="run-card"/>');
  $box.append(runContainer);
  var timestamp = new Date().toLocaleTimeString();
  runContainer.append($('<div class="run-id-header"/>').text("Run Triggered at " + timestamp));
}

var assignTitles = function(){
 $(".text").each(function() {
   console.log($(this).text());
     $(this).attr('data-tippy-content',  cell2fireArgumentVoc[$(this).text()] ); 
   }); 
}

// 2. Add a timestamp or title so you know when this run happened

Shiny.addCustomMessageHandler('appendLog', function(msg) {
  var $box = $('#logSim');
  var $boxErr = $('#logSimErr');
  if ($box.length === 0) return;
  if ($boxErr.length === 0) return;
  if (msg === null) return;

  if(msg.out &&   Array.isArray(msg.out)) msg.out.forEach(function(line) { 
    var text = line.text.trim();
    var $div = $('<div/>');
    // 1. Detect Section Headers: ------ Text ------
    if (text.startsWith('---')) {
        $div.addClass('log-section').text(text.replace(/-/g, ''));
    } 
    // 2. Detect Warnings: No file found...
    else if (text.startsWith('No ') && text.includes('.tif')) {
        $div.addClass('log-warning').text('⚠ ' + text);
    }
    // 3. Detect Table Rows: Available, Burnt, etc.
    else if (["Available", "Burnt", "Non-Burnable", "Firebreak", "Total"].some(word => text.startsWith(word))) {
        // Split by multiple spaces to create a grid-like row
        var parts = text.split(/\s{2,}/); 
        $div.addClass('log-table-row');
        parts.forEach(part => $div.append($('<span/>').text(part)));
    }
    // 4. Detect Key-Value pairs: InFolder: data/...
    else if (text.includes(':') && !text.includes('http')) {
        var parts = text.split(':');
        var key = parts.shift();
        var val = parts.join(':');
        $div.addClass('log-entry')
            .append($('<span class="log-key"/>').text(key + ':'))
            .append($('<span class="log-value"/>').text(val));
    } 
    // 5. Default fallback
    else {
        $div.addClass('log-default').text(text);
    }
    
    runContainer.append($div);
  });

if(msg.err &&   Array.isArray(msg.err)) {
    msg.err.forEach(function(line) { 
      var $div = $('<div/>')
        .text(line.text) 
      $boxErr.append($div);
     });
  }
  
  $box.scrollTop($box[0].scrollHeight);
  $boxErr.scrollTop($boxErr[0].scrollHeight);
    });
    
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