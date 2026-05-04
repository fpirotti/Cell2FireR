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


NAME <- list(
  fuel_models = c("0. Scott & Burgan", "1. Kitral", "2. Canada FBP", "3. Portugal"),
  ignition_modes = c(
    "0. Uniformly distributed random ignition",
    "1. Probability map distributed random ignition",
    "2. Single points on a Layer"
  ),
  weather_modes = c("0. Single weather file", "1. Random draw from directory")
)

SIM_INPUTS <- list(
  fuel_model = list(
    units = "categorical", 
    description = tr("Surface fuel model system"), 
    title = tr("Defines the fire spread logic: 'Scott & Burgan' uses the Standard 40 US models, while 'FBP' uses the Canadian system. Selecting the wrong model for your raster values will result in zero spread or incorrect intensity."),
    choices = NAME$fuel_models
  ), 
  fuel = list(
    units = "categorical", 
    description = tr("Fuel"), 
    title = tr("A categorical raster where each value represents a specific Fuel Model (e.g., Scott & Burgan or Canadian FBP). It defines the fuel load, moisture of extinction, and surface-area-to-volume ratio, which are critical for determining the Rate of Spread (ROS) and Flame Length. 
<b>Has to be either in 16 or 32 bits, not 8 bit (byte)</b>")
  ),
  elevation = list(
    units = "m", 
    description = tr("Elevation"), 
    title = tr("Height above sea level")
  ),
  slope = list(
    units = "degrees", 
    description = tr("Slope"), 
    title = tr("Slope Gradient: determines the acceleration of the fire front. Fire spreads significantly faster uphill due to flame tilt and convective pre-heating of fuels. Affects spread rate and uphill acceleration.")
  ),
  saz = list(
    units = "degrees", 
    description = tr("Slope Azimuth (Aspect)"), 
    title = tr("The compass direction the slope faces. Affects solar radiation, surface temperature, and local wind alignment.
 Cell2Fire uses a standard compass bearing where 0 deg (or 360 deg. ) is true North, $90 deg.  is East, $180 deg.  is South, and $270 deg.  is West. The engine uses this angle to calculate solar radiation (which dries out fuels on South-facing slopes) and to align the local wind vectors with the terrain.")
  ),
  cur = list(
    units = "index", 
    description = tr("Curvature"), 
    title = tr("Surface Curvature: Represents convex (ridges) or concave (valleys) terrain. Influences micro-climates, wind turbulence, and spread vector convergence/divergence.")
  ),
  
  crown=shinyWidgets::prettySwitch("CROWN", "Enable Crown Fire behavior", value = FALSE, status = "danger"),
  
  cbh = list(
    units = "m", 
    description = paste0("cbh: ", tr("Canopy Base Height")),
    title = tr("The distance from the ground to the bottom of the live canopy. Critical threshold for surface fires transitioning into crown fires.")
  ),
  cbd = list(
    units = "kg/m3", 
    description = paste0("cbd: ", tr("Canopy Bulk Density")),
    title = tr("The mass of available canopy fuel per unit volume. High density allows active crown fires to spread from tree to tree.")
  ),
  ccf = list(
    units = "0-1", # or "%" depending on your specific raster scale
    description = paste0("ccf: ", tr("Canopy Cover Fraction")),
    title = tr("The fraction of the ground covered by the vertical projection of the tree canopy. Modifies wind reduction and surface shading.")
  ),
  chm = list(
    units = "m", 
    description = paste0("chm: ", tr("Canopy Height Model")),
    title = tr("The maximum height of the tree canopy. Used to calculate wind profiles and flame length interactions.")
  ),
  probabilityMap = list(
    units = "0-1", 
    description = tr("Ignition Probability"),
    title = tr("A raster representing the spatial likelihood of an ignition occurring. Used for distributed random ignition modes.")
  ), 
  firebreaks = list(units = "0,1", description = "Firebreaks raster (1=firebreak)",
                    title = tr("Firebreaks raster (1=firebreak)")
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



PANELS <- list()

simout <- sapply(SIM_OUTPUTS, function(x) {
   x$name 
})


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
PANELS[["LANDSCAPE"]] <- lapply(names(SIM_INPUTS), function(id) {

  # Extract the specific metadata for this item
  item <- SIM_INPUTS[[id]]
  if(inherits(item , "shiny.tag")){
    return(item)
  }
  # Create the formatted label with the help tooltip
  label_html <- sprintf(
    "%s [%s]<sup class='helpTitle' title='%s'>?</sup>",
    item$description,
    item$units,
    item$title
  )
  if(!is.null(item$choices)){ 
    shiny::selectInput(
      inputId = toupper(id), # e.g., "ELEVATION", "SLOPE"
      label = shiny::HTML(label_html),
      choices = item$choices
    )
  } else {
    shiny::selectInput(
      inputId = toupper(id), # e.g., "ELEVATION", "SLOPE"
      label = shiny::HTML(label_html),
      choices = get_existing_files()
    )
  }
  # Return the Shiny input object
  # We use HTML() so Shiny renders the <sup> tags correctly

})

names(PANELS[["LANDSCAPE"]]) <- toupper(names(SIM_INPUTS))
## IGNITION SECTION ----
PANELS[["IGNITION SECTION"]] <- list(
  NSIM=div(title="If generation mode is 0. (Uniformly distributed random ignition) then these are the number of random ignition points. 
If generation mode is 2. (Single points on a Layer.) then if 'Radius around single point' is set to 0 this will simulate the same point. If radius is not 0, it will randomly put points in the area around the area defined by the radius around each point.",
           shiny::numericInput("NSIM", "Number of simulations", value = 3, min = 1)
           ),
  
  IGNITION_MODE=shiny::selectInput("IGNITION_MODE", 
                                   HTML("Ignition Generation Mode<sup class='helpTitle' title='
<h6>Ignition Generation Mode</h6>                                  
   <b>0. Uniformly distributed random ignition</b> (stochastic)<br>
  Ignitions are generated randomly across the landscape with equal probability for every burnable cell. Used for general baseline risk assessment when specific ignition drivers are unknown.
   <br> <b>1. Probability map distributed random ignition</b> (spatial probability)<br>
  Ignitions are distributed randomly but weighted by a probability raster (probabilityMap.tif). Cells with higher values (closer to 1) are statistically more likely to be chosen as starting points.
   <br> <b>2. Single points on a Layer</b> (coordinates)<br>
  The simulation starts at specific geographic locations provided via a CSV file created interactively by clicking on the map using the button on the left top part of the map itself. 
The system transforms these coordinates into specific cell IDs to ensure the fire ignites exactly where specified.
           '>?</sup>"), choices = NAME$ignition_modes),
  
 
  IGNITIONFILE=shiny::selectInput("IGNITIONFILE", 
                                  HTML("Probability map 0 to 1 (requires ignition mode 1)<sup class='helpTitle' 
                                  title='The ignition_file (typically probabilityMap.tif) is a spatial raster where each cell value represents the relative probability of an ignition occurring, 
used specifically when the simulation is set to distributed random ignition mode.'>?</sup>"),  
                                  choices = get_existing_files()),
  

  IGNIPOINT=shiny::div(  enabled=FALSE,
                        shiny::selectInput("IGNIPOINT",    
                                           HTML("Single points  layer (requires ignition mode 2)<sup class='helpTitle' 
                                  title='  Select one of the ignition points layer, usually named Ignitions.csv 
  but user can create several scenarios using the interactive map and the ignitions panel 
  <a href= >here</a>. Once simulation is run, the selected ignition file, whatever the name, will be converted to 
  Ignitions.csv in the input instance. <hr>NB: the format requires either X and Y 
  columns with coordinates in latitude and longitude, or NCell which is the raster cell id.
  '>?</sup>"), choices = get_existing_files())
  ) ,
  IGNIRADIUS=shiny::sliderInput("IGNIRADIUS", 
                                HTML("Radius around single point (requires ignition mode 2)<sup class='helpTitle' 
                                  title='  The ignition_radius argument in Cell2Fire 
                                  is used only with Ignition Mode 2 (Single points on a Layer).
  When using specific ignition points, the simulator uses this radius to determine how many <b>cells</b> around the target coordinate should be set on fire at the start of the simulation.
  '>?</sup>"), min = 0, max = 11, value = 1)
  
  
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


