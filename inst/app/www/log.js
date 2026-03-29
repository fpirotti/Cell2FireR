// globals
var ignitionButton = null;

$(document).ready(function () {
 autoGroupLeafletLayers(".leaflet-control-layers");
});

function autoGroupLeafletLayers(controlSelector) {
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
      var suffix = parts.slice(1).join(" - ").trim();

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

var toggleIgnitionButton = function(force=false){
  var el = ignitionButton;
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
