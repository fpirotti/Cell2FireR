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
  fuels = list(units = "categorical", description = tr("Fuel")),
  elevation = list(units = "m", description = tr("Elevation")),
  cbh = list(units = "m", description = paste0("cbh: ", tr("Canopy Base Height"))),
  cbd = list(units = "kg/m3", description = paste0("cbd: ", tr("Canopy Bulk Density"))),
  ccf = list(units = "0,1", description = paste0("ccf: ", tr("Canopy Cover Fraction"))),
  hm = list(units = "m", description = paste0("hm: ", tr("Canopy Height"))),
  probabilityMap = list(
    units = "0,1",
    description = paste0(tr("Probability map"), tr(" (requires generation mode 1)"))
  )
)

STATS <- list(
  ros = list(name = tr("Hit Rate Of Spread"), suffix = ""),
  flamelen = list(name = tr("Surface Flame Length"), suffix = ""),
  surfintensity = list(name = tr("Byram Surface Intensity"), suffix = ""),
  crownscar = list(name = tr("Crown Fire Scar"), suffix = ""),
  crownconsumptionratio = list(name = tr("Crown Fire Fuel Consumption Ratio"), suffix = ""),
  surfaceburnfraction = list(name = tr("Surface Burn Fraction"), suffix = tr(" (only Canada FBP)")),
  crownintensity = list(name = tr("Crown Intensity"), suffix = tr(" (only Spain S&B)")),
  crownflamelen = list(name = tr("Crown Flame Length"), suffix = tr(" (only Spain S&B)")),
  maxflamelen = list(name = tr("Max Flame Length"), suffix = tr(" (only Spain S&B)"))
)

SIM_OUTPUTS <- list(
  finalscar = list(name = tr("Final Fire Scar"), suffix = ""),
  propagationscars = list(name = tr("Propagation Fire Scars"), suffix = ""),
  propagationdigraph = list(name = tr("Propagation Directed Graph"), suffix = ""),
  ignitionpoints = list(name = tr("Ignition Points"), suffix = "")
)
SIM_OUTPUTS <- modifyList(SIM_OUTPUTS, STATS)

NAME <- list(
  fuel_models = c("0. Scott & Burgan", "1. Kitral", "2. Canada FBP", "3. Portugal"),
  ignition_modes = c(
    "0. Uniformly distributed random ignition",
    "1. Probability map distributed random ignition",
    "2. Single point on a Layer"
  ),
  weather_modes = c("0. Single weather file", "1. Random draw from directory")
)

PANELS <- list()
## landscape ----
PANELS[["LANDSCAPE"]] <- list(
  shinyWidgets::pickerInput("FUEL_MODEL", "Surface fuel model", choices = NAME$fuel_models),

  shinyWidgets::pickerInput("FUEL", SIM_INPUTS$fuels$description,
              choices = get_existing_files(), options = list(`live-search` = TRUE)),

  prettyCheckbox("PAINTFUELS", "Style (paint) fuel raster", value = FALSE, status = "info"),

  shinyWidgets::pickerInput("ELEVATION", paste0(SIM_INPUTS$elevation$description, " [", SIM_INPUTS$elevation$units, "]"),
              choices = get_existing_files()),

  shinyWidgets::pickerInput("CBH", paste0(SIM_INPUTS$cbh$description, " [", SIM_INPUTS$cbh$units, "]"),
              choices = get_existing_files()),

  shinyWidgets::pickerInput("CBD", paste0(SIM_INPUTS$cbd$description, " [", SIM_INPUTS$cbd$units, "]"),
              choices = get_existing_files()),

  shinyWidgets::pickerInput("CCF", paste0(SIM_INPUTS$ccf$description, " [", SIM_INPUTS$ccf$units, "]"),
              choices = get_existing_files()),

  shinyWidgets::pickerInput("HM", paste0(SIM_INPUTS$hm$description, " [", SIM_INPUTS$hm$units, "] (only Scott & Burgan)"),
              choices = get_existing_files()),

  shinyWidgets::prettySwitch("CROWN", "Enable Crown Fire behavior", value = FALSE, status = "danger"),

  shinyWidgets::pickerInput("FIREBREAKS", "Firebreaks raster (1=firebreak)", choices = get_existing_files())
)


## IGNITION SECTION ----
PANELS[["IGNITION SECTION"]] <- list(
  shiny::numericInput("NSIM", "Number of simulations", value = 3, min = 1),

  shinyWidgets::pickerInput("IGNITION_MODE", "Generation mode", choices = NAME$ignition_modes),

  shinyWidgets::pickerInput("IGNIPROBMAP", "Probability map [0,1]", choices = get_existing_files()),

  shiny::div( title="", enabled=FALSE,
    shinyWidgets::pickerInput("IGNIPOINT", "Single point vector layer", choices = get_existing_files())
  ) ,

  shiny::sliderInput("IGNIRADIUS", "Radius around single point", min = 0, max = 11, value = 0)


)


## WEATHER & CONFIG ----
PANELS[["WEATHER & CONFIG"]] <- list(

  div(title="for Single Weather File you must pick a file, for random choice 
 from directory it will look into the directory of the selected dataset.",
      shinyWidgets::pickerInput("WEATHER_MODE", "Source mode", choices = NAME$weather_modes) ),

  shinyWidgets::pickerInput("WEAFILE", "Single weather file (.csv)", choices = get_existing_files("\\.csv$")),

  # shiny::textInput("WEADIR", "Weather directory path", placeholder = "/path/to/weather/"),

  shiny::numericInput("FMC", "Foliar Moisture Content [40-200]", value = 66, min = 40, max = 200),

  shiny::sliderInput("LDFMCS", "Fuel Moisture Scenario [1-4]", min = 1, max = 4, value = 2),

  shiny::hr(),
  shiny::numericInput("SIM_THREADS", sprintf("CPU Threads (%d available)", Cell2FireR::detectCores()), 
                      value = ceiling(Cell2FireR::detectCores()/4), min = 1, max=Cell2FireR::detectCores()),
  shiny::numericInput("RNG_SEED", "Random Seed", value = 123)

)



## OUTPUTS & DIRECTORIES ----
PANELS[["OUTPUTS & DIRECTORIES"]] <- list(

  shinyWidgets::multiInput(
    inputId = "OUTPUTS",
    label = "Select desired outputs:",
    choices = sapply(SIM_OUTPUTS, function(x) paste0(x$name, x$suffix)),
    selected = c("Final Fire Scar", "Ignition Points")
  ) #,

  # shinyWidgets::materialSwitch("INSTANCE_IN_PROJECT", "Override instance directory", value = FALSE, status = "primary"),
  # 
  # shinyWidgets::materialSwitch("RESULTS_IN_INSTANCE", "Results in instance folder", value = TRUE, status = "primary") 

  # shiny::textInput("RESULTS_DIR", "Custom Results Directory", placeholder = "Leave empty for default...")

)
