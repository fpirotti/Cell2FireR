clean <- function(){
  # threshold: one week ago
  cutoff <- Sys.time() - 7 * 24 * 60 * 60 
  # list all subdirectories
  dirs <- list.dirs(outfolder, full.names = TRUE, recursive = FALSE) 
  # get modification times
  info <- file.info(dirs) 
  # select old folders
  old_dirs <- dirs[info$mtime < cutoff] 
  print(old_dirs)
  # remove them (recursively!)
  unlink(old_dirs, recursive = TRUE, force = TRUE)
}

proc <- function(dry = TRUE) {  
  # 1. Require essential Shiny inputs
  req(input$FUEL, input$FUEL_MODEL, input$inputfolder)
  
  if (!dry) {
    showNotification(
      text = c("
========================================
====== SIMULATION STARTING =============
========================================")
    )
  } else {
    showNotification(
      text = paste("
==========",    as.character(Sys.time())  ,"===============
===== DRY RUN OF SIMULATION STARTING === 
========================================")
    )
  }
  
  tryCatch({
    # 2. Define Paths (Adapting from your original script's context)
    # Using the same logic you had to locate the binary and templates
    template_dir <- file.path(this.path::this.dir(), "templates")
  
    c2f_bin <-  Cell2FireR::c2f_bin_pathEnv()
    
    slope = input$SLOPE
    saz = input$SAZ
    cur = input$CUR
    
    terrain <- list(
      slope = input$SLOPE,
      saz = input$SAZ,
      cur = input$CUR
    )
    # browser()
    if(isTruthy(input$ELEVATION)){
      
      elev <- rast(input$ELEVATION)
      for(tt in names(terrain)){
        pout <- file.path(input$inputfolder,   sprintf("%s.tif", tt) )
        if(!file.exists(terrain[[tt]]) && !file.exists(pout)){
        shiny::showNotification(ui =sprintf("%s raster NOT present, 
but elevation raster is - I will create it for you...
This is a one-time operation, please be patient.", tt), type = "warning")
        if(tt=="slope") ttt <- terra::terrain(elev, v = "slope", unit = "degrees")
        if(tt=="saz") ttt <- terra::terrain(elev, v = "aspect", unit = "degrees")
        if(tt=="cur") ttt <- spatialEco::curvature(elev, type = "total")
        
        
        writeRaster(ttt, pout )
        assign(tt, pout)  
       } 
     }
    } 
     
      # 3. Call the Agnostic Function
      # We map the Shiny input$ variables directly to the function arguments
      if(!dry) runjs("startSimlog();")
      
      sim_result <- run_cell2fire(
        fuel = input$FUEL,
        fuel_model = input$FUEL_MODEL,
        input_folder = input$inputfolder,
        out_folder = outfolder, # Assuming outfolder is still in the global/parent environment
        elevation = input$ELEVATION,
        slope = slope,
        saz = saz,
        cur = cur,
        crown = isTruthy(input$CROWN) && input$CROWN == TRUE,
        cbh = input$CBH,
        cbd = input$CBD,
        ccf = input$CCF,
        hm = input$CHM,
        ignition_mode = input$IGNITION_MODE,
        ignition_file = input$IGNITIONFILE,
        ignition_point = input$IGNIPOINT,
        ignition_radius = input$IGNIRADIUS,
        firebreaks = input$FIREBREAKS,
        weather_mode = input$WEATHER_MODE,
        wea_file = input$WEAFILE,
        weather_weights = input$WEATHERWEIGHTS,
        nsims = input$NSIM,
        rng_seed = input$RNG_SEED,
        sim_threads = input$SIM_THREADS,
        fmc = input$FMC,
        ldfmcs = input$LDFMCS,
        outputs = input$OUTPUTS,
        # c2f_bin_path = c2f_bin,
        template_dir = template_dir,
        dry = dry,
        verbose = FALSE #input$VERBOSE
      )  
 
    # 4. Handle Logging and UI Updates
    # If it's a dry run, run_cell2fire returns the command-line arguments.
    # We can print these to the log interface just like the original script.
    if (dry) {  
      shinyjs::runjs(paste0("$('#code1').text('", paste(sim_result, collapse = " "),
                     "')") ) 
      showNotification(
        text = c(paste(sim_result, collapse = "<BR>"), "<BR>
========================================
===== DRY RUN OF SIMULATION END  ======= 
========================================<BR><BR>")
      )
      return(NULL)
    } 
      
    # If not dry, run_cell2fire returns the processx object.
    # We update the tab and assign it to the global environment so the rest of your app can track it.
    updateTabsetPanel(session, "tabs", selected = "processLogTab")
    simProcess <<- sim_result 
    
  }, error = function(e) {   
    shiny::showNotification(ui = paste("Error in preparing simulation:", e$message), type = "error")
  }, warning = function(e) { 
    message("warning in proc run 2 ", e$message)
    shiny::showNotification(ui = paste("Warning in preparing simulation:", e$message), type = "warning")
  })
}