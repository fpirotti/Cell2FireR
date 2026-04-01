## make sure leafem is version 0.2.5 or more!
pkgs <- c("terra", "DT", "sf", "shiny", "leaflet", "shinyjs", "optparse",
          "tools", "shinydashboard", "leafem", "cli", "shinydashboardPlus", "fresh",
          "htmlwidgets")
if (!requireNamespace("cli", quietly = TRUE)) {
  install.packages("cli")
}
warn <- cli::combine_ansi_styles("magenta", "italic")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    warn("Package ", cli::style_bold(p), " not found... installing it.")
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

white_tile <- "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR4nGNgYAAAAAMAASsJTYQAAAAASUVORK5CYII="

clcpluswms <- "https://image.discomap.eea.europa.eu/arcgis/services/CLC_plus/CLMS_CLCplus_RASTER_2021_010m_eu/ImageServer/WMSServer"
# Example global data
default_center <- c(lng = 11.96173904332453, lat = 45.342594242)
clcLayerName <- "Copernicus CLC+"
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#570a00"
  ),
  adminlte_sidebar(
    width = "300px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#000000",
    light_color = "#000000",
    light_submenu_color =  "#000000"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#eaeaea",
    info_box_bg = "#eaeaea"
  )
)

default_zoom <- 16

base_path <- "data"

# Example helper
popup_text <- function(name, value=NULL) {
  pp<-paste0("<b>", name, "</b>")
  if(!is.null(value)) pp<-paste0("<b>", name, ":</b> ", value)
  pp
}

lut_fbp <- read.csv("templates/fbp_lookup_table.csv")
lut_sb <- read.csv("templates/scottBurgan_lookup_table.csv")
center <- function(r){
  e <- terra::ext(r)
  list(
  lng = (e$xmin + e$xmax) / 2,
  lat = (e$ymin + e$ymax) / 2
  )
}



raster_info <- function(r) {
  ext <- terra::ext(r)
  lev <- try(terra::levels(r)[[1]], silent = TRUE)

  list(
    ncol = ncol(r),
    nrow = nrow(r),
    res  = paste(terra::res(r), collapse = " × "),
    crs  = ifelse(is.na(terra::crs(r)), "Not defined", terra::crs(r)),
    extent = sprintf(
      "xmin: %.2f, xmax: %.2f<br>ymin: %.2f, ymax: %.2f",
      ext$xmin, ext$xmax, ext$ymin, ext$ymax
    ),
    classes = if (!inherits(lev, "try-error")) {
      apply(lev, 1, function(x){ paste0(x[[1]], ": ", x[[2]] ) })
      } else NULL
  )
}

## FWI layers ------
base_layers <- list(
  osm="BASE - OSM",
  blank="BASE - Blank",
  light="BASE - Light",
  satellite= "BASE - Satellite"
)

fwi_layers <- list(
  "EFFIS - Fire Weather Index" = "ecmwf.fwi",
  "EFFIS - Initial Spread Index" = "ecmwf.isi",
  "EFFIS - Build Up Index" = "ecmwf.bui",
  "EFFIS - Fine Fuel Moisture Code" = "ecmwf.ffmc",
  "EFFIS - Duff Moisture Code" = "ecmwf.dmc",
  "EFFIS - Drought Code" = "ecmwf.dc"
)
view_settings <- list()
opacityControl  <- list()

opacityControl[[clcLayerName]] <- list(
  min = 0,
  max = 1,
  step = 0.1,
  default = 0.7,
  width = '100%',
  class = 'opacity-slider'
)
# print(clcLayerName)

addFWI <- function(m=NULL){

  if(is.null(m)){
    stop("should not be here")
    return(NA)
  }
  if (inherits(m, "leaflet_proxy")){
    for (name in names(fwi_layers)) {

      opacityControl [[name]] <<- list(    min = 0,
                                          max = 1,
                                          step = 0.1,
                                          default = 0.7,
                                          width = '100%',
                                          class = 'opacity-slider'  )
      m %>%
        addWMSTiles(
          baseUrl = "https://maps.effis.emergency.copernicus.eu/gwis",
          layers = fwi_layers[[name]],
          group = name,
          options = WMSTileOptions(
            format = "image/png",
            transparent = TRUE,
            opacity = 0.8,
            time = as.character(Sys.Date())
          )
        ) %>%
        hideGroup(name)  # start hidden
    }
  } else {
    for (name in names(fwi_layers)) {
      opacityControl [[name]] <<- list(    min = 0,
                                          max = 1,
                                          step = 0.1,
                                          default = 1,
                                          width = '100%',
                                          class = 'opacity-slider'  )
      m <- m %>%
        addWMSTiles(
          baseUrl = "https://maps.effis.emergency.copernicus.eu/gwis",
          layers = fwi_layers[[name]],
          group = name,
          options = WMSTileOptions(
            format = "image/png",
            transparent = TRUE,
            opacity = 0.8,
            time = as.character(Sys.Date())
          )
        ) %>%
        hideGroup(name)  # start hidden
    }
  }

  m
}

createLeaflet <- function(){

  m <- leaflet() |>
    addTiles(
      urlTemplate = white_tile,
      options = tileOptions(tileSize = 256),
      group = base_layers$blank
    )  |>
    addProviderTiles("OpenStreetMap", group =  base_layers$osm,
                     options=providerTileOptions(zIndex = 1) ) |>
    addProviderTiles("CartoDB.Positron", group =  base_layers$light,
                     options=providerTileOptions(zIndex = 1)) |>
    addProviderTiles("Esri.WorldImagery", group =  base_layers$satellite,
                     options=providerTileOptions(zIndex = 1)) |>
    addWMSTiles(
      baseUrl = clcpluswms,
      layers = "CLMS_CLCplus_RASTER_2021_010m_eu",
      group = clcLayerName,
      options = WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        opacity=0.6
      ),
      attribution = "Copernicus Land Monitoring Service"
    ) |>
    setView(
      lng = default_center["lng"],
      lat = default_center["lat"],
      zoom = 16
    ) |>
    leafem::garnishMap(leaflet::addScaleBar, leafem::addMouseCoordinates,
                       position = "bottomleft") |>
    addEasyButton(
      easyButton(
        icon = htmltools::span(class = "star", htmltools::HTML("🔥")),
        title = "Add ignition point mode - click on map to add ignition point to table - press esc or press again to exit this mode",
        onClick = JS("
            function(btn, map){
                var el = btn.button;
                ignitionButton = el;
                toggleIgnitionButton();
            }
          ")
      )
    )  |>
    addEasyButton(
      easyButton(
        icon = "fa-question",
        title = "Toggle info panel",
        onClick = JS("
            function(btn, map){
                var el = btn.button;
                if (L.DomUtil.hasClass(el, 'pressed')) {
                  L.DomUtil.removeClass(el, 'pressed');
                } else {
                  L.DomUtil.addClass(el, 'pressed');
                }
                $('#map > .leaflet-control-container > .leaflet-bottom.leaflet-right').toggle();
            }
          ")
      )
    )  |>
    addEasyButton(
      easyButton(
        icon = "fa-cloud-rain",
        title = "Query EFFIS Fire Weather Indices for the past 7 days and add them to weather table",
        onClick = JS("
            function(btn, map){
                var el = btn.button;
                WMSQueryButton = el;
                toggleWMSQueryButton();
            }
          ")
      )
    ) |> htmlwidgets::onRender("
      function(el, x) {
        console.log(el);
        
        mymap = HTMLWidgets.find(\"#map\").getMap();
        $('[title]').each(function() {
          $(this).attr('data-tippy-content', $(this).attr('title'));
          $(this).removeAttr('title');
        });

        // now initialize tippy
        tippy('[data-tippy-content]', {
          theme: 'light',
          animation: 'scale'
        });
        document.addEventListener('keydown', function(e) {
          // Fallback for older browsers
          if (e.keyCode === 27) {
            toggleIgnitionButton(true);
            toggleWMSQueryButton(true);
            document.getElementById('map').style.cursor = null;
            Shiny.setInputValue('map_mode', '', {priority: 'event'});
          }
        });
        const ctrl = el.querySelector('.leaflet-control-layers');
        if (!ctrl) return;
        console.log('Layers control ready');
        //autoGroupLeafletLayers(\".leaflet-control-layers\");
      }
   ")


  m <- addFWI(m)

  # add layer control
  m <- m %>%
    addLayersControl(
      baseGroups = unlist(unname(base_layers)),
      overlayGroups = c(clcLayerName, names(fwi_layers)),
      options = layersControlOptions(collapsed = FALSE)
    ) |>
    customizeLayersControl(
      view_settings =view_settings,
      opacityControl = opacityControl,
      home_btns = TRUE,
      # opacityControl = opacityControl,
      includelegends = TRUE,
      layersControlCSS = list("opacity" = 0.6),
      increaseOpacityOnHover = TRUE
    )

  # print(opacityControl)
   m
}

mymap <- createLeaflet()
isScottBurgan <- function(r){
  rs <- terra::unique((r[[1]]))
  if( sum(c(91,
            92,
            93,
            98,
            99)%in%rs[,1])>0 ){
    ## it is probably Scott&Burgan
    return(T)
  }

  if(  sum(rs[,1] > 190 ) > 0 ){
    ## it is not Scott&Burgan
    return(F)
  }
}

## fire icon ----
fireIcon <- leaflet::makeAwesomeIcon(
  icon = "fire",
  markerColor = "white",
  iconColor = "darkred",
  # squareMarker=TRUE,
  text="<span style='font-size:24px'>🔥</span>"
)

### source parser -----
source("inputPanels.R")

uiInputsArgs <- lapply(names(PANELS), function(op){
  shinydashboardPlus::box(width=12, collapsible=TRUE,title=op,
                          solidHeader = TRUE,status = "primary",
                          PANELS[[op]] )
})

# source("../../R/ParseInputs.R")
# mp <- make_parser()
# uiInputs <- lapply(mp@options, function(op){
#   out<-NULL
#  if(op@type=="integer"){
#    out<- div(title= op@help , numericInput(
#      inputId = gsub("-", ".", op@long_flag),
#      label   = op@long_flag,
#      value   = op@default,
#      min     = -1,
#      max     = 10000,
#      step = 1
#    ) )
#  }
#   if(op@type=="double"){
#     out<-  div(title= op@help , numericInput(
#       inputId = gsub("-", ".", op@long_flag),
#       label   = op@long_flag,
#       value   = op@default,
#       min     = -1,
#       max     = 10000,
#       step = 0.01
#     ) )
#   }
#   if(op@type=="character"){
#     out<-  div(title= op@help , textInput(
#       inputId = gsub("-", ".", op@long_flag),
#       label   = op@long_flag,
#       value   = op@default
#     ) )
#   }
#   if(is.null(op@type)|| op@type=="logical"){
#     out<-  div(title= op@help , checkboxInput(
#       inputId = gsub("-", ".", op@long_flag),
#       label   =  op@long_flag,
#       value   = op@default
#     ) )
#   }
#  out
# })


md_overwrite_ignition <- modalDialog(
  title = "Overwrite",size = "s",
  "Confirm you want to overwrite the selected file?",
  
  footer = tagList(
    actionButton("overwrite_file_confirm_yes", "Yes"),
    actionButton("overwrite_file_confirm_newFile", "Create a New file"),
    modalButton("Cancel")
  ),
  
  easyClose = TRUE
)
