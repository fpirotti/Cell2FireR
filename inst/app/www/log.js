$(document).ready(function () {


});


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
