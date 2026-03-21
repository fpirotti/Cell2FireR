# global.R
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

clcpluswms <- "https://image.discomap.eea.europa.eu/arcgis/services/CLC_plus/CLMS_CLCplus_RASTER_2021_010m_eu/ImageServer/WMSServer"
# Example global data
default_center <- c(lng = 11.96173904332453, lat = 45.342594242)
clcLayerName <- "Copernicus CLC+<span id='showLegendCLS'>📰</span>"
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
source("../../R/ParseInputs.R")
mp <- make_parser()
uiInputs <- lapply(mp@options, function(op){
  out<-NULL
 if(op@type=="integer"){
   out<- div(title= op@help , numericInput(
     inputId = gsub("-", ".", op@long_flag),
     label   = op@long_flag,
     value   = op@default,
     min     = -1,
     max     = 10000,
     step = 1
   ) )
 }
  if(op@type=="double"){
    out<-  div(title= op@help , numericInput(
      inputId = gsub("-", ".", op@long_flag),
      label   = op@long_flag,
      value   = op@default,
      min     = -1,
      max     = 10000,
      step = 0.01
    ) )
  }
  if(op@type=="character"){
    out<-  div(title= op@help , textInput(
      inputId = gsub("-", ".", op@long_flag),
      label   = op@long_flag,
      value   = op@default
    ) )
  }
  if(is.null(op@type)|| op@type=="logical"){
    out<-  div(title= op@help , checkboxInput(
      inputId = gsub("-", ".", op@long_flag),
      label   =  op@long_flag,
      value   = op@default
    ) )
  }
 out
})

white_tile <- "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR4nGNgYAAAAAMAASsJTYQAAAAASUVORK5CYII="
