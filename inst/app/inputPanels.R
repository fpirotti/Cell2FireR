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
    file  = "ROSFile",
    ext   = "asc",
    arg   = "out-ros",
    unit  = "m/min",
    dtype = "float32"
  ),
  
  flamelen = list(
    name  = tr("Surface Flame Length"),
    dir   = "SurfaceFlameLength",
    file  = "SurfaceFlameLength",
    ext   = "asc",
    arg   = "out-fl",
    unit  = "m",
    dtype = "float32"
  ),
  
  surfintensity = list(
    name  = tr("Byram Surface Intensity"),
    dir   = "SurfaceIntensity",
    file  = "SurfaceIntensity",
    ext   = "asc",
    arg   = "out-intensity",
    unit  = "kW/m",
    dtype = "float32"
  ),
  
  crownscar = list(
    name  = tr("Crown Fire Scar"),
    dir   = "CrownFire",
    file  = "Crown",
    ext   = "asc",
    arg   = "out-crown",
    unit  = "bool",
    dtype = "int16"
  ),
  
  crownconsumptionratio = list(
    name  = tr("Crown Fire Fuel Consumption Ratio"),
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
    name = tr("Final Fire Scar"),
    dir  = file.path("Grids", "Grids"),
    file = "ForestGrid",
    ext  = "csv",
    arg  = "final-grid",
    unit = "bool"
  ),
  
  propagationscars = list(
    name = tr("Propagation Fire Scars"),
    dir  = file.path("Grids", "Grids"),
    file = "ForestGrid",
    ext  = "csv",
    arg  = "grids",
    unit = "bool"
  ),
  
  propagationdigraph = list(
    name = tr("Propagation Directed Graph"),
    dir  = "Messages",
    file = "MessagesFile",
    ext  = "csv",
    arg  = "output-messages",
    unit = "simtime"
  ),
  
  ignitionpoints = list(
    name = tr("Ignition Points"),
    dir  = ".",
    file = "ignition_and_weather_log",
    ext  = "csv",
    arg  = "ignitionsLog",
    unit = "cell_id"
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
  if(!is.null(x$unit)) x$unit <- paste0(" [", x$unit , "]")
  paste0(x$name, x$unit, x$suffix)
  })
simoutf <- names(simout)
names(simoutf) <- simout
## OUTPUTS & DIREsimout## OUTPUTS & DIRECTORIES ----
PANELS[["OUTPUTS OPTIONS"]] <- list(
  shinyWidgets::prettySwitch("VERBOSE", "Verbose", value = TRUE, status = "danger"),
 
  shiny::selectizeInput(
    inputId = "OUTPUTS", multiple=T,
    label = "Select desired outputs / options:",
    choices = simoutf,
    selected = c("finalscar", "ignitionpoints")
  )

  # shinyWidgets::materialSwitch("INSTANCE_IN_PROJECT", "Override instance directory", value = FALSE, status = "primary"),
  #
  # shinyWidgets::materialSwitch("RESULTS_IN_INSTANCE", "Results in instance folder", value = TRUE, status = "primary")

  # shiny::textInput("RESULTS_DIR", "Custom Results Directory", placeholder = "Leave empty for default...")

)

## landscape ----
PANELS[["LANDSCAPE"]] <- list(
 
  shiny::selectInput("FUEL_MODEL", "Surface fuel model", choices = NAME$fuel_models),

  shiny::selectInput("FUEL", SIM_INPUTS$fuel$description,
              choices = get_existing_files() ),

  # prettyCheckbox("PAINTFUELS", "Style (paint) fuel raster", value = FALSE, status = "info"),

  shiny::selectInput("ELEVATION", paste0(SIM_INPUTS$elevation$description, " [", SIM_INPUTS$elevation$units, "]"),
              choices = get_existing_files()),

  shiny::selectInput("CBH", paste0(SIM_INPUTS$cbh$description, " [", SIM_INPUTS$cbh$units, "]"),
              choices = get_existing_files()),

  shiny::selectInput("CBD", paste0(SIM_INPUTS$cbd$description, " [", SIM_INPUTS$cbd$units, "]"),
              choices = get_existing_files()),

  shiny::selectInput("CCF", paste0(SIM_INPUTS$ccf$description, " [", SIM_INPUTS$ccf$units, "]"),
              choices = get_existing_files()),

  shiny::selectInput("CHM", paste0(SIM_INPUTS$chm$description, " [", SIM_INPUTS$chm$units, "] (only Scott & Burgan)"),
              choices = get_existing_files()),

  shinyWidgets::prettySwitch("CROWN", "Enable Crown Fire behavior", value = FALSE, status = "danger"),

  shiny::selectInput("FIREBREAKS", "Firebreaks raster (1=firebreak)", choices = get_existing_files())
)


## IGNITION SECTION ----
PANELS[["IGNITION SECTION"]] <- list(
  shiny::numericInput("NSIM", "Number of simulations", value = 3, min = 1),

  shiny::selectInput("IGNITION_MODE", "Generation mode", choices = NAME$ignition_modes),

  shiny::selectInput("IGNITIONFILE", "Probability map [0,1]", paste0(SIM_INPUTS$ignitionfile$description, " [", SIM_INPUTS$ignitionfile$units, "]"),
                            choices = get_existing_files()),

  shiny::div( title="", enabled=FALSE,
    shiny::selectInput("IGNIPOINT", "Single points  layer", choices = get_existing_files())
  ) ,

  shiny::sliderInput("IGNIRADIUS", "Radius around single point", min = 0, max = 11, value = 1)


)


## WEATHER & CONFIG ----
PANELS[["WEATHER & CONFIG"]] <- list(

  div(title="for Single Weather File you must pick a file, for random choice 
 from directory it will look into the directory of the selected dataset.",
      shiny::selectInput("WEATHER_MODE", "Source mode", choices = NAME$weather_modes) ),

  shiny::selectInput("WEAFILE", "Single weather file (.csv)", choices = get_existing_files("\\.csv$")),

  # shiny::textInput("WEADIR", "Weather directory path", placeholder = "/path/to/weather/"),

  shiny::numericInput("FMC", "Foliar Moisture Content [40-200]", value = 66, min = 40, max = 200),

  shiny::sliderInput("LDFMCS", "Fuel Moisture Scenario [1-4]", min = 1, max = 4, value = 2),

  shiny::hr(),
  shiny::numericInput("SIM_THREADS", sprintf("CPU Threads (%d available)", Cell2FireR::detectCores()), 
                      value = ceiling(Cell2FireR::detectCores()/4), min = 1, max=Cell2FireR::detectCores()),
  shiny::numericInput("RNG_SEED", "Random Seed", value = 123)

)


