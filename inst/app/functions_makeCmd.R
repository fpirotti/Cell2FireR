# Helper function mapping the UI fuel choices to Cell2Fire's internal string keys
# (This replaces the NAME["fuel_model_key"] from the Python script)
# Helper function mapping the UI fuel choices to Cell2Fire's expected single-character flags
get_fuel_key <- function(fuel_string) {
  fuel_map <- c(
    "0. Scott & Burgan" = "S",
    "1. Kitral" = "K",
    "2. Canada FBP" = "C",
    "3. Portugal" = "P"
  )
  
  # Fallback to "S" if the input somehow doesn't match, mirroring the C++ default behavior
  mapped_val <- unname(fuel_map[fuel_string])
  if (is.null(mapped_val) || is.na(mapped_val)) {
    return("S") 
  }
  return(mapped_val)
}

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


proc <- function(dry=T) {  
    req(input$FUEL, input$FUEL_MODEL, input$inputfolder)
  
  if(!dry)  showNotification(text=c("
========================================
====== SIMULATION STARTING =============
========================================")  )
 

    tryCatch({
      # -------------------------------------------------------------
      # 1. SETUP DIRECTORIES
      # -------------------------------------------------------------
      # Create an instance directory similar to QGIS: firesim_yymmdd_HHMMSS
      timestamp <- format(Sys.time(), "%y%m%d_%H%M%S")
      instance_dir <- file.path(outfolder, paste0("firesim_", timestamp))
      results_dir <- file.path(instance_dir, "results")
      simLog <- logs$logfileSim
      dir.create(instance_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
      
      map2instance <- list()
      # Helper to copy and rename files to standard names expected by C2F
      copy_to_instance <- function(filepath, standard_name) {
        # map2instance <- list()
        if (!is.null(filepath) && filepath != "") {
          ext <- tools::file_ext(filepath)
          map2instance[[paste0(standard_name, ".", ext)]] <<- filepath
          # file.link()
          if(standard_name=="fuels"){
            tf <- terra::rast(filepath)
            if( as.integer(substr(terra::datatype(tf), 4, 4) ) < 4){
               
              if(!dry){
                showNotification(text = "Fuel is not in 32 or 64 bits, will have to copy it... please upload a dataset with fuel as 32 and 64 bits to avoid this warning",
                               type = "warning"  )
               terra::writeRaster(tf, datatype="INT4U",  
                                 file.path(instance_dir, paste0(standard_name, ".", ext)), 
                                 overwrite=T
                                 )
              }
            } 
          } else { 
            if(!dry)  file.link(filepath, file.path(instance_dir, paste0(standard_name, ".", ext)))
          }
        }
        # map2instance
      }
      
      # -------------------------------------------------------------
      # 2. COPY LANDSCAPE INPUTS
      # -------------------------------------------------------------
      copy_to_instance(input$FUEL, "fuels")
      copy_to_instance(input$ELEVATION, "elevation") 
      
      if (input$CROWN) { 
        if(!dry) showNotification(text=c("CROWN info available and used")  ) 
        copy_to_instance(input$CBH, "cbh")
        copy_to_instance(input$CBD, "cbd")
        copy_to_instance(input$CCF, "ccf")
        copy_to_instance(input$CHM, "hm")
      } else {
        if(!dry) showNotification(text=c("CROWN info not used")  )
      }
      
      
      Simulator <- get_fuel_key(input$FUEL_MODEL)
      if (Simulator == "K") {
        lookupTable = "kitral_lookup_table.csv";
      }
      else if (Simulator == "S") {
        lookupTable = "spain_lookup_table.csv";
      }
      else if (Simulator == "C") {
        lookupTable = "fbp_lookup_table.csv";
      }
      else if (Simulator == "P") {
        lookupTable =  "portugal_lookup_table.csv";
      }
      else { 
        showNotification(text= paste0("Simulation type not found. Type ", 
                                                  input$FUEL_MODEL),
                                     type = "warning" ) 
      }
      
      if(!dry) file.copy( file.path( this.path::this.dir(), "templates", lookupTable), 
                 file.path( instance_dir, lookupTable ) )
      
      if (input$IGNITION_MODE == "1. Probability map distributed random ignition") {
        copy_to_instance(input$IGNITIONFILE, "probabilityMap")
      }
      
      # Handling Firebreaks (Translating 'raster_layer_to_firebreak_csv')
      if (!is.null(input$FIREBREAKS) && input$FIREBREAKS != "") {
        # Using terra to read raster, find cells == 1, and write coordinates/ids to CSV
        fb_rast <- terra::rast(input$FIREBREAKS)
        fb_cells <- terra::cells(fb_rast, 1) # Get cell indices where value is 1
        # Cell2Fire requires 1-based indices or specific row/col CSVs depending on the version
        if(!dry)  write.csv(data.frame(Cell = fb_cells), file.path(instance_dir, "firebreaks.csv"), row.names = FALSE)
      }
      
      # -------------------------------------------------------------
      # 3. WEATHER & IGNITIONS
      # -------------------------------------------------------------
      
      showNotification(text=c("WEATHER_MODE: ", input$WEATHER_MODE)  ) 
      if (input$WEATHER_MODE == "0. Single weather file") { 
        if(!shiny::isTruthy(input$WEAFILE) || !file.exists(input$WEAFILE) ){
          showNotification(text = "No Weather file found or selected, I will use a generic weather file with 20° and 20 km/h wind from south direction blowing towards north (direction 180°).", 
                                       type = "warning")
           
          if(!dry) file.copy( file.path( this.path::this.dir(), "templates", "Weather.csv" ), file.path(instance_dir, "Weather.csv"))
        } else { 
          if(!dry) file.copy(input$WEAFILE, file.path(instance_dir, "Weather.csv"))
        }
      } else { 
        # Random draw from directory (Looks in the directory of the selected dataset)
        wea_dir <- input$inputfolder
        wea_out <- file.path(instance_dir, "Weathers")
        if(!dry)  dir.create(wea_out, showWarnings = FALSE)
         
        wea_files <- list.files(wea_dir, pattern = "^Weather[0-9]*\\.csv$", full.names = TRUE)
        if (length(wea_files) > 0) {
          if(!dry) file.copy(wea_files, wea_out)
        } else { 
          showNotification(text="Multiple weathers requires a directory with Weather[0-9]*.csv files!", 
                                  type = "Error" ) 
        }
      }
      
      if(!dry) showNotification(text=c("IGNITION_MODE: ", input$IGNITION_MODE)  ) 
      if (input$IGNITION_MODE == "2. Single points on a Layer") {
        req(input$IGNIPOINT)
        
        if(!shiny::isTruthy(input$IGNIPOINT) || !file.exists(input$IGNIPOINT) ){ 
          showNotification(text="No ignition points found!",   type = "Error" ) 
        } else {
          fuel_rast <- terra::rast(input$FUEL)
          ign_pt <- sf::st_read(input$IGNIPOINT, quiet = TRUE)
          if(is.na(sf::st_is_longlat(ign_pt))){ 
            if(is.data.frame(ign_pt)) { 
              if(!dry) {
                showNotification(text="Writing Ignition points!",type = "info" ) 
                write.csv(ign_pt, file.path(instance_dir, "Ignitions.csv"), 
                          row.names = F)
              }
            } else { 
              showNotification(text="Cannot read Ignition points anywhere - was a file provided?", 
                                           type = "Error" ) 
            }
          } else {
            # Match CRS
            if(!dry) {
              ign_pt <- sf::st_transform(ign_pt, terra::crs(fuel_rast)) 
              # Get cell index mapping (Cell2Fire uses 1-based indexing top-left to bottom-right)
              cell_id <- terra::cellFromXY(fuel_rast, sf::st_coordinates(ign_pt)[1, 1:2]) 
              # Write Ignitions.csv
              writeLines(c("Year,Ncell", paste0("1,", cell_id)), file.path(instance_dir, "Ignitions.csv")) 
            }
          }
        }
        # Assuming IGNIPOINT is a vector file. We extract its coordinate, 
        # map it to the fuel raster to get the cell ID.
        
      }
      
      # -------------------------------------------------------------
      # 4. BUILD COMMAND LINE ARGUMENTS
      # -------------------------------------------------------------
      # Core Arguments
      args <- list(
        "--sim" = Simulator,
        "--nsims" = input$NSIM,
        "--seed" = input$RNG_SEED,
        "--nthreads" = input$SIM_THREADS,
        "--fmc" = input$FMC,
        "--scenario" = input$LDFMCS
      )
      
      # Boolean / Mode Flags
      if (input$CROWN) args[["--cros"]] <- ""
      
      if (input$IGNITION_MODE == "2. Single points on a Layer") {
        args[["--ignitions"]] <- ""
        args[["--IgnitionRad"]] <- input$IGNIRADIUS
      }
      
      if (input$WEATHER_MODE == "0. Single weather file") {
        args[["--weather"]] <- "rows"
      } else {
        args[["--weather"]] <- "random"
      }
      
      if (!is.null(input$FIREBREAKS) && input$FIREBREAKS != "") {
        args[["--FirebreakCells"]] <- shQuote(file.path(instance_dir, "firebreaks.csv"))
      }
      
      # Append Selected Outputs
      if (!is.null(input$OUTPUTS)) {
        for (opt in input$OUTPUTS) {
          args[[paste0("--", opt)]] <- ""
        }
      }
      
      ## always make grids!
      args[[paste0("--grids")]] <- ""
      
      # Directories
      args[["--input-instance-folder"]] <-   file.path(this.path::this.dir(),  instance_dir) 
      args[["--output-folder"]] <-   file.path(this.path::this.dir(),  results_dir) 
      
      # Flatten list to a character vector for system2
      cli_args <- character()
      for (name in names(args)) {
        if (args[[name]] == "") {
          cli_args <- c(cli_args, name) # Append as a flag (no value)
        } else {
          cli_args <- c(cli_args, name, as.character(args[[name]]))
        }
      }
      
 
      # -------------------------------------------------------------
      # 5. EXECUTE CELL2FIRE
      # -------------------------------------------------------------
      # Cell2Fire is  in ../bin/C2F/
      c2f_bin <- if(.Platform$OS.type == "windows") file.path(dirname(this.path::this.dir()), 
                                                              "bin", "C2F", "Cell2Fire.exe") else file.path(dirname(this.path::this.dir()), 
                                                                                                            "bin", "C2F",
                                                                                                                              "Cell2Fire")
      flush(logs$log_con)
      # Optional: Print command to R console for debugging
      writeLines(paste0("INFO: ", c2f_bin, " ",  paste(cli_args, collapse = " "), " 
 <br>
 " ),  con =   logs$log_con)
      flush(logs$log_con)
      
      
       # append mode
      # Execute via system2. Output is pushed to LogFile.txt
      
      updateTabsetPanel(session, "tabs", selected = "processLogTab")
      
      if(dry){
        return(NULL)
      }
      # exit_code <- system2(
      p <- processx::process$new( c2f_bin,
        args = cli_args,
        stdout = "|",
        stderr = "|"
      )
       
      simProcess(p) 
  
    }, error = function(e) {  
      browser()
      shiny::showNotification(ui = paste("Error preparing simulation:", e$message) )
    }, warning = function(e) { 
      print(e$message) 
      browser()
      shiny::showNotification(ui = paste("Error preparing simulation:", e$message) )
    })
  }  