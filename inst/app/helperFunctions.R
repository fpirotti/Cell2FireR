
center <- function(r){
  e <- terra::ext(r)
  list(
    lng = (e$xmin + e$xmax) / 2,
    lat = (e$ymin + e$ymax) / 2
  )
}

popup_text <- function(name, value=NULL) {
  pp<-paste0("<b>", name, "</b>")
  if(!is.null(value)) pp<-paste0("<b>", name, ":</b> ", value)
  pp
}

checkAPI <- function(url){ 
  res <- httr::GET(url) 
  httr::status_code(res)
}