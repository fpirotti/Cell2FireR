# server.R

server <- function(input, output, session) {

  options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB
  rasterInfo <- reactiveVal(NULL)
  ignitionPointsCoords <- reactiveVal(NULL)
  currentRasterStack <- NULL
  lut_fbp_local <- reactiveVal(NULL)
  logfile <- reactiveVal(NULL)
  lut_generic <- lut_fbp
  ## reactive log file  ----
  log_reader <- reactivePoll(
    intervalMillis = 1000,  # 1 second
    session = session,

    # cheap check: did the file change?
    checkFunc = function() {
      if(is.null(logfile())) return(NULL)
      if ( !file.exists(logfile())) return(NULL)
      file.info(logfile())$mtime
    },

    # expensive read: only when it DID change
    valueFunc = function() {
      if(is.null(logfile())) return("No logfile yet")
      if ( !file.exists(logfile())) return("Log file does not exist yet")
      readLines(logfile(), warn = FALSE)
    }
  )
  ## reactive log html ----
  log_html <- reactive({
    lines <- log_reader()

    tags$div(
      lapply(lines, function(line) {
        cls <- dplyr::case_when(
          grepl("ERROR", line, ignore.case = T) ~ "log-error",
          grepl("WARN",  line, ignore.case = T) ~ "log-warn",
          grepl("INFO",  line, ignore.case = T) ~ "log-info",
          TRUE                  ~ "log-default"
        )
        tags$div(class = cls, HTML(line))
      })
    )
  })

  ## render HTML to log ----
  output$log <- renderUI({
    tags$div(
      id="logbox",
      log_html()
    )
  })

  showNotification<-function(..., type="message", duration=4){
    text <- paste(..., collapse = " ")
    if( !is.element(type, c("default", "message", "warning", "error")) ){
      typesh <- "default"
    } else {
      typesh <- type
    }

    if(duration>0) {
      shiny::showNotification(
                     HTML(text),
                     duration = duration,
                     type = typesh
                      )
    }

    logf <- isolate(logfile())
    if(!is.null(logf) && dir.exists(dirname(logf) ) ){
      cat(
        format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), toupper(type),
        " | ",
        paste(text, collapse = " "),
        "\n",
        file = logf,
        append = TRUE
      )
    }

  }

  # change dataset folder ----
  observeEvent(input$inputfolder, {
    req(input$inputfolder)
    outfolder <- file.path(input$inputfolder, "output")
    dir.create(outfolder, recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(outfolder, "Logfile.log"), showWarnings = FALSE)
    logfile(file.path(outfolder, "Logfile.log"))
    if(dir.exists(outfolder)){
      showNotification(
        paste0("Directory ", outfolder, " exists, files will be overwritten if you continue!"),
        type = "warning",
        duration = 8
      )
    } else {
      showNotification(
        paste0("Output Directory: <b>", outfolder, "</b> does not exist and will be created"),
        type = "INFO",
        duration = 0
      )
    }

    # shiny::updateTextInput(session = session,
    #                        inputId = "..input.instance.folder",
    #                        value =  input$inputfolder)
    #
    # shiny::updateTextInput(session = session,
    #                        inputId = "..output.folder",
    #                        value =  outfolder)

    # shiny::updateTextInput(session = session,
    #                        inputId = "..output.folder",
    #                        value =  outfolder)

    ignitionPoints <- NULL
    currentRasterStack <<- NULL

    rfiles <- list.files(
      path = input$inputfolder,
      pattern = "\\.(asc|tif|tiff)$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    csvfiles <- list.files(
      path = input$inputfolder,
      pattern = "\\.(csv)$",
      full.names = TRUE,
      ignore.case = TRUE
    )

    ignitionFiles <- list.files(
      path = input$inputfolder,
      pattern = ".*ignition.*\\.(csv)$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    # bn<-
    names(ignitionFiles) <- basename(ignitionFiles)
    shinyWidgets::updatePickerInput(inputId = "chooseIgnitionFile",
                                    choices = c("", ignitionFiles),
                                    clearOptions = T  )

    ## IGNITION FILE ----
    # ignitionFile <-  grep("ignition", basename(tolower(csvfiles)), ignore.case = T )
    if(length(ignitionFiles)>0){
      ip <- read.csv(ignitionFiles[[1]] )
      if(anyNA(ip$Ncell)){
        showNotification(
          paste0("In file ", ignitionFiles[[1]] , " no valid ignition points found, please check file format."),
          type = "warning",
          duration = 2
        )
      } else {
        ignitionPoints <- ip$Ncell
      }
    }

    ## UPDATE LEAFLET ------
    leaflet::leafletProxy("map") |>
      leaflet::clearImages()  |>
      leaflet::clearControls() |> addFWI()

    crs <- NA
    layerNames <- clcLayerName
    rasters <- list()

    shinyjs::runjs("$('.info.legend.rastervals.leaflet-control').remove();")

    ### READ RASTERS ----
    withProgress(message = 'Adding layers',  value = 0, {


      strt <- 0
      for(fi in rfiles){
        strt <- strt + 1


        layerName <- toupper(tools::file_path_sans_ext(basename(fi)))
        layerNames <- c(layerNames, layerName)

        incProgress( strt/(length(rfiles)+2), detail = sprintf("Layer %s", layerName) )

        r2 <- terra::rast(fi)

        if(terra::crs(r2)==""){
          showNotification(
            HTML("<b>Raster Info:</b> NO CRS provided to raster, so a generic UTM projection assigned: <code>EPSG:32632</code>"),
            duration = 0,
            type = "warning"
          )
          # guess CRS of input
          cc<-center(r2)
          if( abs(cc$lng) > 360 && abs(cc$lat) > 90  ){
            terra::crs(r2) <- "EPSG:3857"
          }
        }
        #### forest map ------
        if(grepl("forest|fuel", layerName, ignore.case = T) ){
          r2 <- terra::as.factor(r2)

          levs <- data.frame(
            value = lut_fbp$grid_value,
            class = lut_fbp$descriptive_name,
            fuel = lut_fbp$fuel_type,
            color =   rgb(lut_fbp$r, lut_fbp$g, lut_fbp$b, maxColorValue = 255)
          )
          lut_generic <- lut_fbp
          if(isScottBurgan(r2)){

            levs <- data.frame(
              value = lut_sb$grid_value,
              class = as.character(lut_sb$descriptive_name),
              fuel = lut_sb$fuel_type,
              color =   rgb(lut_sb$r, lut_sb$g, lut_sb$b, maxColorValue = 255)
            )
            lut_generic <- lut_sb
          }
          levels(r2) <- levs
          copts <- leafem::colorOptions(palette = levs$color, domain=levs$value)


          na.color = "transparent"
          rasterInfo(raster_info(r2))

          if(!is.null(ignitionPoints)){
            cr <- terra::xyFromCell(r2, ignitionPoints)
            crv <- terra::vect(cr)
            terra::crs(crv) <- terra::crs(r2)

            crv2 <- crv |> terra::project("EPSG:4326")
            crvc <-  as.data.frame(crv2@pntr$coordinates())
            names(crvc) <- c("X","Y")
            ignitionPointsCoords(cbind(ip, crvc) )
          }
          rasters[["FUELMAP"]] <- r2


        } else {
          v <- spatSample(r2, size = 1e4, method = "regular", na.rm = TRUE)

          rrange <- quantile(v[,1], probs = c(0.1,0.9))
          palette = viridisLite::viridis(20)
          domain  = rrange
          rasters[[layerName]] <- r2
          copts <- colorOptions(palette=palette,
                       domain=unname(domain),
                       na.color = "#eaeaea00")
          pv2col <- NULL
          # pal2 <- colorNumeric("viridis", terra::values(r2), na.color = "transparent")
        }



        opacityControl [[toupper(layerName)]] <- list(
          min = 0,
          max = 1,
          step = 0.1,
          default = 1,
          width = '100%',
          class = 'opacity-slider'
           )

        view_settings[[toupper(layerName)]] <- list(coords = as.numeric(st_transform(st_bbox(r2), 4326))   )

        leaflet::leafletProxy("map") |>
          leafem::addGeoRaster( stars::st_as_stars(r2),
                                colorOptions = copts,
                                # pixelValuesToColorFn = pv2col,
                                opacity = 0.85, group = toupper(layerName),
                                autozoom = F,
                                options = leafletOptions(pane = "markerPane"),
                                tileOptions = leaflet::tileOptions(zIndex = 999),
                                imagequeryOptions = leafem::imagequeryOptions(digits=0,
                                                                              prefix="",
                                                                              position="bottomright",
                                                                              noData = "NA"),
                                layerId = toupper(layerName)) |>
          leaflet::hideGroup( toupper(layerName) )
      }
    })

    ## FBP FILE ----
    fbpFile <-  grep("fbp_lookup_table", basename(tolower(csvfiles)), ignore.case = T )
    if(length(fbpFile)>0){
      lut_fbp_local(read.csv(csvfiles[[fbpFile[[1]]]] ))
    } else {
      showNotification(
        paste0("Did not find file fbp_lookup_table.csv! Will fall back to default template file in 'templates' folder."),
        type = "warning",
        duration = 1
      )
      lut_fbp_local(lut_generic)
    }
    currentRasterStack <<- tryCatch({
      terra::rast(rasters)
      },
      error=function(e){
          showNotification(
            paste("ERROR:", e$message),
            type = "error",
            duration = 16
          )
        NULL
      })
    if(is.null(currentRasterStack)) return(NA)

    r2 <- terra::project(ext(r2), from=terra::crs(r2), to="epsg:4326")


    leafletProxy("map") |>

      addLayersControl(
        baseGroups = unlist(unname(base_layers)),
        overlayGroups = c(layerNames, names(fwi_layers)),
        options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
      )   |>
      customizeLayersControl(
        view_settings =view_settings ,
        # home_btns = TRUE,
        opacityControl = opacityControl,
        includelegends = TRUE,
        addCollapseButton = TRUE,
        layersControlCSS = list("opacity" = 0.6),
        increaseOpacityOnHover = TRUE
      ) |>
      leaflet::fitBounds( lng1 =  xmin(r2),
                                lat1 =  ymin(r2),
                                lng2 =  xmax(r2),
                                lat2 =  ymax(r2)
      )



    shinyjs::runjs("autoGroupLeafletLayers(\".leaflet-control-layers\");")

    if(is.null(currentRasterStack)){
      showNotification(
        "No Forest raster file found, please read Cell2Fire documentation on how to prepare a dataset",
        type = "error",
        duration = 6
      )
    }


  })

  ##render leaflet -----
  output$map <- renderLeaflet({
    mymap
  })

  # CLICK mode ----
  observeEvent(input$map_mode, {
    print(input$map_mode)
    if(input$map_mode==""){
      runjs("document.getElementById('map').style.cursor = null;")
    }
  })

  # WMSQueryReturned  ----
  observeEvent(input$WMSQueryReturned, {
    print(input$WMSQueryReturned)
    leafletProxy("map") |>
      addPopups(
        lng = input$WMSQueryReturned$coords[[1]],
        lat = input$WMSQueryReturned$coords[[2]],
        popup = input$WMSQueryReturned$data,
        layerId = "my_popup"  # optional, to remove/update later
      )
  })


  ## TABLE WEATHER table -----
  output$weather.table <- renderDT({
    req(input$inputfolder)
    wt <- file.path(input$inputfolder, "Weather.csv")
    if(file.exists(wt)){
      datatable(read.csv(wt), editable = TRUE)
    } else {
      showNotification(
        paste0("NO WEATHER FILE! I LOADED A TEMPLATE..."),
        type = "warning",
        duration = 8
      )
      df <- read.csv("templates/Weather.csv")
      datatable(df[1:2,], editable = TRUE)
    }
  })
  ## TABLE FBP table ----
  output$FBP.table <- renderDT({
    req(lut_fbp_local())
    datatable( lut_fbp_local() , editable = TRUE)
  })

  ## TABLE IGNITION and edits  ----
  output$ignitionInfo <- renderDT({
    req(ignitionPointsCoords())
    ip <- ignitionPointsCoords()
    ip$Tools <- ""



    DT::datatable(
      ip,
      escape = FALSE,
      extensions = "Buttons",
      editable = FALSE,
      options = list(
        dom = "Bfrtip",
        buttons = list(
          list(
            extend = "collection",
            text = 'Delete Selected 🗑️',
            action = DT::JS("function ( e, dt, node, config ) {
                              var rows = dt.rows({ selected: true }).indices().toArray();
                              Shiny.setInputValue('rows_to_delete', rows, {priority: 'event'});
                            }")
          )
        ),
        columnDefs = list(
          list(
            targets = ncol(ip),
            render = JS(
              "function(data, type, row, meta) {
             return '<button title=\"Zoom in\">🔎</button>';
           }"
            )
          )
        )
      )
    ) |> formatRound(
      columns = c('X', 'Y'),
      digits = 5
    )
    # datatable( ip , editable = TRUE)
  })
  observeEvent(input$ignitionInfo_cell_edit, {
    info <- input$ignitionInfo_cell_edit
    df <- isolate(ignitionPointsCoords())
    df[info$row, info$col] <- info$value
    ignitionPointsCoords(df)
  })
  
  save_table_ignition_final <- function(overwrite=F){
    df <- isolate(ignitionPointsCoords())
    if(overwrite && isTruthy(input$chooseIgnitionFile)) {
      write.csv(df, input$chooseIgnitionFile, row.names = F)
    } else {
      write.csv(df, file.path(input$inputfolder, 
                              sprintf("ignitionPoints_%s.csv", 
                                      format(Sys.time(), 
                                             "%Y-%m-%d_%H-%M-%S" ) )), row.names = F)
    }
    

    
    ignitionFiles <- list.files(
      path = input$inputfolder,
      pattern = ".*ignition.*\\.(csv)$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    names(ignitionFiles) <- basename(ignitionFiles)
    shinyWidgets::updatePickerInput(inputId = "chooseIgnitionFile",
                                    choices = ignitionFiles,
                                    clearOptions = T  )
  }
  
  
  observeEvent(input$delete_table_ignition, {
    req(input$chooseIgnitionFile)
    shinyWidgets::ask_confirmation(inputId = "delete_table_ignition_confirm",html = T,
                                   sprintf("Confirm you want to delete the selected ignition table: <br><u><b>%s</b></u>",
                                           basename(input$chooseIgnitionFile)  )
                                   )
  })
  
  observeEvent(input$delete_table_ignition_confirm, {
    req(input$chooseIgnitionFile)
    if(input$delete_table_ignition_confirm){
     file.remove(input$chooseIgnitionFile)
    } 
  })  
  observeEvent(input$save_table_ignition_confirm, {
    req(input$chooseIgnitionFile)
    if(input$save_table_ignition_confirm){
      save_table_ignition_final(T)
    } 
  })
  ## updates side bar ignition ----
  observeEvent(input$ignitionsTable, {
    updateBoxSidebar("ignitionSideBar")
  })
  observeEvent(input$save_table_ignition, {
    if(!isTruthy(input$chooseIgnitionFile) ) {
      save_table_ignition_final(F)
    } else { 
      shinyWidgets::ask_confirmation(inputId = "save_table_ignition_confirm",
                                     "Confirm you want to overwrite the selected ignition file")
    }
  })

  ## add ignitionpoints -----
  observe( {
    req(ignitionPointsCoords())
    ip<-ignitionPointsCoords()
    ipt <- na.omit(ip)
    leafletProxy("map") |> leaflet::clearMarkers()
    for(i in 1:nrow(ipt)){
      print(ipt[i,  ])
      leafletProxy("map") |>
        addAwesomeMarkers(  as.numeric(ipt[i, 3] ),
                   as.numeric(ipt[i, 4]),
                   icon=  fireIcon,
                   popup = paste0(
                     popup_text("Year:",  ipt[i, 1]  ),
                     "<br>",
                     popup_text("NCell:",  ipt[i, 2]  ),
                     "<br>",
                     popup_text("Lat:", round(as.numeric(ipt[i, 3]),6) ),
                     "<br>",
                     popup_text("Lon:",  round(ipt[i, 4], 6) ) )
      )
    }
  })
  # Handle map mode and clicks ----
  observeEvent(input$map_click, {
    req(input$map_mode)
    lng <- input$map_click$lng
    lat <- input$map_click$lat

    if ( input$map_mode == "WMSQuery") {
      js <- sprintf("queryWMS(%f, %f, '%s')", lat, lng, paste(fwi_layers, collapse=",")   )
      # print(js)
      shinyjs::runjs(js)
    }

    if(is.null(currentRasterStack)){
      showNotification(
        paste0(
          "You did not load a dataset with rasters. Only coordinates returned: ",
          sprintf("%.6f", lat), ", ",
          sprintf("%.6f", lng)
        ),
        type = "message",
        duration = 0
      )

      # runjs("Shiny.setInputValue('map_mode', '', {priority: 'event'});")

      return(NULL)
    }

    crr <- currentRasterStack[[1]]

    # if(terra::nlyr(currentRasterStack)>1 ){
    #   crr <- currentRasterStack$FUELMAP
    # }
    ### ignition add click -----
    if ( input$map_mode == "ignitionPoint") {

      pt4326 <- terra::vect( matrix(c(lng, lat),ncol = 2), crs="EPSG:4326" )

      pt <- pt4326 |> terra::project( crr )
      ptmt <- as.matrix( unname(as.data.frame(pt@pntr$coordinates()) ) )
      ptmt4326 <- as.matrix( unname(as.data.frame(pt4326@pntr$coordinates()) ) )
      cr <- terra::cellFromXY( crr , ptmt )

      allig <- isolate(ignitionPointsCoords())
      df <- data.frame(Year=as.integer(1), Ncell=as.integer(cr), X=ptmt4326[1,1],  Y=ptmt4326[1,2])
      nn <- rbind(allig, df )

      ignitionPointsCoords( nn )

    }


    ## make sure map mode returns to null
    # runjs("Shiny.setInputValue('map_mode', '', {priority: 'event'});")

  })

  # pretty raster info ----
  output$raster_info <- renderUI({
    info <- rasterInfo()
    div(
      style = "
        background-color:#f8f9fa;
        border-left:6px solid #1f77b4;
        padding:12px;
        border-radius:6px;
        font-family:Arial;
      ",

      h4("FOREST Raster stack information", style = "margin-top:0;"),

      tags$b("Dimensions: "), info$nrow, " × ", info$ncol, tags$br(),
      tags$b("Resolution: "), info$res, tags$br(),
      tags$b("Extent: "), HTML(info$extent),
      tags$b("CRS: "), tags$code(info$crs), tags$br(),

      if (!is.null(info$classes)) {
        tagList(
          tags$hr(),
          tags$b("Classes:"),
          tags$ul(
            lapply(info$classes, tags$li)
          )
        )
      }
    )
  })


  ## delete dataset -----
  observeEvent(input$deletefolder, {
    if(input$inputfolder==""){
      showNotification("Please select a dataset from the drop down menu",
                       type="warning")
      return(NULL)
    }
    print("here")
        shinyWidgets::confirmSweetAlert(session=session,
                                        "confirmDelete",
                                        "Confirm",
     paste("Confirm you want to delete dataset", input$inputfolder ))
  })

  observeEvent(input$confirmDelete, {
    print(input$inputfolder)
      req(input$inputfolder)
      showNotification("Removing folder " )
      unlink( file.path("data", input$inputfolder) )
  })
  ## download dataset -----
  observeEvent(input$downloadfolder, {
    req(input$inputfolder)
    fp <- file.path("data", input$inputfolder )
    req(fp!="data")
    zip(sprintf("%s.zip", fp), list.files(fp, full.names = T) )
  })
  ## upload dataset -----
  observeEvent(input$zipfileload, {
    req(input$zipfileload)
 
    print(input$zipfileload$name)
   
    path <- file.path("data", tools::file_path_sans_ext(input$zipfileload$name) )
    if(dir.exists(path)){
      showNotification(paste("Directory ", input$zipfileload$name, " exists, please delete it first or change the name of the zip file!"),
                       type = "error",duration = 15
      )
      return(NULL)
    }
    fs <- file.size(input$zipfileload$datapath)
    showNotification(
      paste("Unzipping ", fs/1e6 ," MB in data folder to ", input$zipfileload$name)
      )

    unzip(input$zipfileload$datapath, exdir=path )
    showNotification(
      paste("Finished unzipping  to ", input$zipfileload$name)
    )
  })

  ## monitor DATA FOLDER -----
  folders <- reactivePoll(
    intervalMillis = 10000,   # check every 10 seconds
    session,

    # Check function (fast, lightweight)
    checkFunc = function() {
      if (!dir.exists(base_path)) return(NULL)
      file.info(base_path)$mtime   # modification time
    },


    # Value function (runs only if check changes)
    valueFunc = function() {
      if (!dir.exists(base_path)) return(character(0))
      dirs <- list.dirs("data",full.names = T,recursive = F)
      names(dirs) <- gsub("_", " ", basename(dirs))
      dirs
    }
  )
  observeEvent(folders(), {

    current <- input$inputfolder

    updateSelectInput(
      session,
      "inputfolder",
      choices = c("", folders()),
      selected = if (current %in% folders()) current else NULL
    )

  }, ignoreNULL = FALSE)

}
