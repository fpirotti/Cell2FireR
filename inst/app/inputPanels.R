tr <- function(x) return(x)
library(shinyWidgets)

get_existing_files <- function(type="rast" , datasetpath="data") {
  if(exists("input")){
    datasetpath=input$inputfolder
  }
  pattern="dsfdf"
  if(type=="rast"){
    pattern = "\\.tif$|\\.asc$"
  }
  if(type=="vect"){
    pattern = "\\.gpkg$|\\.shp$"
  }
  if(type=="text"){
    pattern = "\\.csv$"
  }
  list.files(datasetpath, pattern=pattern, full.names = T)
}

SIM_INPUTS <- list(
  fuel = list(units = "categorical", description = tr("Fuel")),
  elevation = list(units = "m", description = tr("Elevation")),
  cbh = list(units = "m", description = paste0("cbh: ", tr("Canopy Base Height"))),
  cbd = list(units = "kg/m3", description = paste0("cbd: ", tr("Canopy Bulk Density"))),
  ccf = list(units = "0,1", description = paste0("ccf: ", tr("Canopy Cover Fraction"))),
  chm = list(units = "m", description = paste0("chm: ", tr("Canopy Height"))),
  firebreaks = list(units = "0,1", description = paste0("Fire breaks: ", tr(" Fire breaks"))),
  ignitionfile = list(
    units = "0,1",
    description = paste0(tr("Probability map"), tr(" (requires generation mode 1)"))
  )
)
 
STATS <- list(
  ros = list(
    name  = tr("Hit Rate Of Spread"),
    dir   = "RateOfSpread",
    title = "Generates the Rate of Spread to a asc file",
    file  = "ROSFile",
    ext   = "asc",
    arg   = "out-ros",
    unit  = "m/min",
    dtype = "float32"
  ),
  
  flamelen = list(
    name  = tr("Surface Flame Length"),
    title = "Generates the Flame Length to a asc file ",
    dir   = "SurfaceFlameLength",
    file  = "SurfaceFlameLength",
    ext   = "asc",
    arg   = "out-fl",
    unit  = "m",
    dtype = "float32"
  ),
  
  surfintensity = list(
    name  = tr("Byram Surface Intensity"),
    title = "Generates the Byram Intensity to a asc file", 
    dir   = "SurfaceIntensity",
    file  = "SurfaceIntensity",
    ext   = "asc",
    arg   = "out-intensity",
    unit  = "kW/m",
    dtype = "float32"
  ),
  
  crownscar = list(
    name  = tr("Crown Fire Scar"),
    title = "Generates the Crown behavior to a asc file ",
    dir   = "CrownFire",
    file  = "Crown",
    ext   = "asc",
    arg   = "out-crown",
    unit  = "bool",
    dtype = "int16"
  ),
  
  crownconsumptionratio = list(
    name  = tr("Crown Fire Fuel Consumption Ratio"),
    title = "Generates the Crown Fraction Burn to a Folder ",
    dir   = "CrownFractionBurn",
    file  = "Cfb",
    ext   = "asc",
    arg   = "out-cfb",
    unit  = "ratio",
    dtype = "float32"
  ),
  
  surfaceburnfraction = list(
    name   = tr("Surface Burn Fraction"),
    suffix = tr(" (only Canada FBP)"),
    title  = "Generates the Surface Fraction Burned to a asc file",
    dir    = "SurfFractionBurn",
    file   = "Sfb",
    ext    = "asc",
    arg    = "out-sfb",
    unit   = "ton",
    dtype  = "float32"
  ),
  
  crownintensity = list(
    name   = tr("Crown Intensity"),
    suffix = tr(" (only Spain S&B)"),
    title  = "Generates the Crown Fire Intensity to a asc file",
    dir    = "CrownIntensity",
    file   = "CrownIntensity",
    ext    = "asc",
    arg    = "out-intensity",
    unit   = "kW/m",
    dtype  = "float32"
  ),
  
  crownflamelen = list(
    name   = tr("Crown Flame Length"),
    suffix = tr(" (only Spain S&B)"),
    title  = "Generates the Crown Flame Length to a asc file",
    dir    = "CrownFlameLength",
    file   = "CrownFlameLength",
    ext    = "asc",
    arg    = "out-fl",
    unit   = "m",
    dtype  = "float32"
  ),
  
  maxflamelen = list(
    name   = tr("Max Flame Length"),
    suffix = tr(" (only Spain S&B)"),
    title  = "Generates the Maximum calculated Flame Length to a asc file",
    dir    = "MaxFlameLength",
    file   = "MaxFlameLength",
    ext    = "asc",
    arg    = "out-fl",
    unit   = "m",
    dtype  = "float32"
  ) 
)

SIM_OUTPUTS <- list(
  finalscar = list(
    name  = tr("Final Fire Scar"),
    dir   = file.path("Grids", "Grids"),
    title = tr("Generates the final state of the landscape grid (Burned vs. Unburned) at the end of the simulation"),
    file  = "ForestGrid",
    ext   = "csv",
    arg   = "final-grid",
    unit  = "bool",
    dtype = "int16"
  ),
  
  propagationscars = list(
    name  = tr("Propagation Fire Scars"),
    title = tr("Generates a series of grid snapshots for each time step to track fire growth over time"),
    dir   = file.path("Grids", "Grids"),
    file  = "ForestGrid",
    ext   = "csv",
    arg   = "grids",
    unit  = "bool",
    dtype = "int16"
  ),
  
  propagationdigraph = list(
    name  = tr("Propagation Directed Graph"),
    title = tr("Generates a detailed log of fire transmission events (who burned whom) between specific cells"),
    dir   = "Messages",
    file  = "MessagesFile",
    ext   = "csv",
    arg   = "output-messages",
    unit  = "simtime",
    dtype = "float32"
  ),
  
  ignitionpoints = list(
    name  = tr("Ignition Points"),
    title = tr("Logs the coordinates and timing of the initial fire start points and the weather conditions at ignition"),
    dir   = ".",
    file  = "ignition_and_weather_log",
    ext   = "csv",
    arg   = "ignitionsLog",
    unit  = "cell_id",
    dtype = "int32"
  )
)
 
SIM_OUTPUTS <- modifyList(SIM_OUTPUTS, STATS)

NAME <- list(
  fuel_models = c("0. Scott & Burgan", "1. Kitral", "2. Canada FBP", "3. Portugal"),
  ignition_modes = c(
    "0. Uniformly distributed random ignition",
    "1. Probability map distributed random ignition",
    "2. Single points on a Layer"
  ),
  weather_modes = c("0. Single weather file", "1. Random draw from directory")
)

PANELS <- list()

simout <- sapply(SIM_OUTPUTS, function(x) {
   x$name 
})
# simout <- sapply(SIM_OUTPUTS, function(x) {
#   if(!is.null(x$unit)) x$unit <- paste0(" [", x$unit , "]")
#   paste0(x$name, x$unit, x$suffix)
#   })
# browser()

# choices = c("a","b","c")
# 
# choicesOpt = list(
#   content = c(
#     '<span title="Info A">Option A</span>',
#     '<span title="Info B">Option B</span>',
#     '<span title="Info C">Option C</span>'
#   )
# )

simoutf <- sapply(SIM_OUTPUTS, function(x)  x$arg)
voc <- sapply(names(SIM_OUTPUTS), function(n) {
  x <- SIM_OUTPUTS[[n]]
  sprintf("'%s': '%s <br>unit: [%s]<br>argument: --%s'", x$name, x$title, x$unit, x$arg )
 
})

vocsJS <- sprintf("var cell2fireArgumentVoc = {%s};", 
               paste(unname(unlist(voc)), collapse=",
"))
## OUTPUTS & DIREsimout## OUTPUTS & DIRECTORIES ----
PANELS[["OUTPUTS OPTIONS"]] <- list(
  
  VERBOSE= div(title="Use with caution, provides MB of output messages! Disable as too many output messages not so useful in an interactive context - please use CLI to turn on verbose messaging", 
               disabled( shinyWidgets::prettySwitch("VERBOSE", "Verbose (!))", value = FALSE, status = "danger") )
               ),
 
  OUTPUTS= #div(title="Disabled as only advanced users have access - modifying outputs might jeopardize correct postprocessing",
    # disabled(
      # shiny::selectizeInput(
      #   inputId = "OUTPUTS", multiple=T,
      #   label = "Select desired outputs / options:",
      #   choices = simoutf,
      #   selected = simoutf
      #   ) 
      # 
    pickerInput(
      inputId = "OUTPUTS",
      label = "Choose output arguments",
      multiple = TRUE,
      
      choices = simoutf,
      selected = simoutf,
      choicesOpt = list(
        content = simout,
        style = sprintf('background:yellow;'),
        class =  rep("cell2fireOutputOptions", length(simout)) 
      ) 
    )
      # ) 
   #   )
   
  # shinyWidgets::materialSwitch("INSTANCE_IN_PROJECT", "Override instance directory", value = FALSE, status = "primary"),
  #
  # shinyWidgets::materialSwitch("RESULTS_IN_INSTANCE", "Results in instance folder", value = TRUE, status = "primary")

  # shiny::textInput("RESULTS_DIR", "Custom Results Directory", placeholder = "Leave empty for default...")

)

## LANDSCAPE ----
PANELS[["LANDSCAPE"]] <- list(
 
  FUEL_MODEL=shiny::selectInput("FUEL_MODEL", "Surface fuel model", choices = NAME$fuel_models),

  FUEL=div(title="Fuel raster - this file  is mandatory and should be available as 32 or 64 bit raster in your dataset. 
 This file will be soft-linked to the temporary input instance directory as fuel.asc or fuel.tif depending on the format.
 This is not an argument to cell2fire, it is implicitly present in the input instance directory (--input-instance-folder argument) ", shiny::selectInput("FUEL", SIM_INPUTS$fuel$description,
              choices = get_existing_files() )
           ),

  # prettyCheckbox("PAINTFUELS", "Style (paint) fuel raster", value = FALSE, status = "info"),

  ELEVATION=shiny::selectInput("ELEVATION", paste0(SIM_INPUTS$elevation$description, " [", SIM_INPUTS$elevation$units, "]"),
              choices = get_existing_files()),

  CBH=shiny::selectInput("CBH", paste0(SIM_INPUTS$cbh$description, " [", SIM_INPUTS$cbh$units, "]"),
              choices = get_existing_files()),

  CBD=shiny::selectInput("CBD", paste0(SIM_INPUTS$cbd$description, " [", SIM_INPUTS$cbd$units, "]"),
              choices = get_existing_files()),

  CCF=shiny::selectInput("CCF", paste0(SIM_INPUTS$ccf$description, " [", SIM_INPUTS$ccf$units, "]"),
              choices = get_existing_files()),

  CHM=shiny::selectInput("CHM", paste0(SIM_INPUTS$chm$description, " [", SIM_INPUTS$chm$units, "] (only Scott & Burgan)"),
              choices = get_existing_files()),

  CROWN=shinyWidgets::prettySwitch("CROWN", "Enable Crown Fire behavior", value = FALSE, status = "danger"),

  FIREBREAKS=shiny::selectInput("FIREBREAKS", "Firebreaks raster (1=firebreak)", choices = get_existing_files())
)


## IGNITION SECTION ----
PANELS[["IGNITION SECTION"]] <- list(
  NSIM=div(title="If generation mode is 0. (Uniformly distributed random ignition) then these are the number of random ignition points. 
If generation mode is 2. (Single points on a Layer.) then if 'Radius around single point' is set to 0 this will simulate the same point. If radius is not 0, it will randomly put points in the area around the area defined by the radius around each point.",
           shiny::numericInput("NSIM", "Number of simulations", value = 3, min = 1)
           ),
  
  IGNITION_MODE=shiny::selectInput("IGNITION_MODE", "Generation mode", choices = NAME$ignition_modes),
  
  IGNITIONFILE=shiny::selectInput("IGNITIONFILE", "Probability map [0,1]", 
                                  paste0(SIM_INPUTS$ignitionfile$description, " [", SIM_INPUTS$ignitionfile$units, "]"),
                                  choices = get_existing_files()),
  
  IGNIPOINT=shiny::div( title="Select one of the ignition points layer, usually named Ignitions.csv 
                        but user can create several scenarios using the interactive map and the ignitions panel 
                        <a href= >here</a>. Once simulation is run, the selected ignition file, whatever the name, will be converted to 
                        Ignitions.csv in the input instance. <hr>NB: the format requires either X and Y columns with coordinates in latitude and longitude, or NCell as ", enabled=FALSE,
                        shiny::selectInput("IGNIPOINT", "Single points  layer", choices = get_existing_files())
  ) ,
  
  IGNIRADIUS=shiny::sliderInput("IGNIRADIUS", "Radius around single point", min = 0, max = 11, value = 1)
  
  
)


## WEATHER & CONFIG ----
PANELS[["WEATHER & CONFIG"]] <- list(

  WEATHER_MODE = div(title="for Single Weather File you must pick a file, for random choice 
 from directory it will look into the directory of the selected dataset.",
      shiny::selectInput("WEATHER_MODE", "Source mode", choices = NAME$weather_modes) ),

  WEAFILE=shiny::selectInput("WEAFILE", "Single weather file (.csv)", choices = get_existing_files("\\.csv$")),

  # shiny::textInput("WEADIR", "Weather directory path", placeholder = "/path/to/weather/"),

  FMC=shiny::numericInput("FMC", "Foliar Moisture Content [40-200]", value = 66, min = 40, max = 200),

  LDFMCS= shiny::sliderInput("LDFMCS", "Fuel Moisture Scenario [1-4]", min = 1, max = 4, value = 2),
 
  SIM_THREADS=shiny::numericInput("SIM_THREADS", sprintf("CPU Threads (%d available)", Cell2FireR::detectCores()), 
                      value = ceiling(Cell2FireR::detectCores()/4), min = 1, max=Cell2FireR::detectCores()),
  RNG_SEED=shiny::numericInput("RNG_SEED", "Random Seed", value = runif(n=1, min=100, max=999))

)


