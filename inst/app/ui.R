# ui.R
ui <-  shinydashboardPlus::dashboardPage(
  options = list(sidebarExpandOnHover = TRUE),
  header = shinydashboardPlus::dashboardHeader(
    title = HTML(
      "<div \title='Wildfire-Sim: Learning Wildfire Behaviour with Interaction with Spatial Data'>🔥 Wildfire-Sim🔥</div>"
    ),
    leftUi = tagList(
      div(
        style = "margin-bottom:-15px;",
        title = "Do you want to keep seeing the help tooltips or not?",
        switchInput(
          size = "sm",
          inputId = "tooltips",
          value = FALSE,
          inline = T,
          
          label = "🎓Tips"
        )
      ),
      div(
        style = "margin-bottom:-15px;",
        title = "Size of tooltip text",
        numericInput(
          "tooltipsSize" ,
          width =
            "60px",
          value = 13,
          min = 10,
          max = 24,
          step = 1,
          label = NULL
        )
      ),
      div(
        # style="margin-bottom:-15px;",
        title = "Quick overview of functionalities (not yet active)",
        actionBttn(
          "runTooltips" ,
          size = "sm",
          # icon = icon("chalkboard-teacher"),
          inline = T,
          style = "simple",
          label =
            "🎓 overview"
        )
      ),
      div(
        # style="margin-bottom:-15px;",
        title = "Quick overview of functionalities (not yet active)",
        # shinydashboard::menuItem("Information", icon = icon("info"), tabName = "infoBox")
        actionBttn(
          "infoBoxButton" ,
          size = "sm",
          icon = icon("info"),
          inline = T,
          style = "simple",
          label = "Information"
        )
      )
      
      
    )
  ),
  # footer = shinydashboardPlus::dashboardFooter(
  #   left =  HTML("<a href='www.cirgeo.unipd.it' >CIRGEO - University of Padova</a> "),
  #   right = HTML("<a href='mailto:francesco.pirotti@unipd.it' >MAIL<span style='font-size:22px'>📧</span></a> ")
  # ),
  # SIDEBAR ----
  sidebar = shinydashboardPlus::dashboardSidebar(
    minified = FALSE,
    collapsed = FALSE,
    
    shiny::selectInput(
      "inputfolder",
      HTML(
        "DATASET<sup class='helpTitle' title='Choose dataset - you can upload your own ZIP file with rasters and weather files (click top right gear icon)'>?</sup>"
      ),
      choices = c("", basename(list.dirs("data", recursive = F)))
    ),
    
    shiny::selectInput(
      "outputInstanceFolder",
      HTML(
        "Simulation Output Instance<sup class='helpTitle' title='Simulation outputs will create a folder with timestamp so that it
can be accessed anytime to check and download logs and outputs'>?</sup>"
      ),
      choices = c("")
    ),
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Map", tabName = "dashboardMap", icon = icon("dashboard")),
      
      # div(style="display:none;",
      #     shinydashboard::menuItem("Information", icon = icon("info"), tabName = "infoBox")
      # ),
      
      shinydashboard::menuItem(
        HTML(
          "SIMULATION LOG<sup class='helpTitle' title='Check parsed logs of the simulator'>?</sup>"
        ),
        icon = icon("clipboard-list"),
        tabName = "processLogTab"
      ),
      # shinydashboard::menuItem("Inputs", icon = icon("table"),
      shinydashboard::menuItem(
        "Input Args",
        icon = icon("sliders-h"),
        tabName = "inputInstancesInputArgs"
      ),
      # shinydashboard::menuItem("Input Args2", icon = icon("sliders-h"), tabName = "inputInstancesInputArgs2"),
      shinydashboard::menuItem(
        "Weather Table",
        icon = icon("cloud-rain"),
        tabName = "inputInstancesWeather"
      ),
      # shinydashboard::menuItem("Ignitions Table", icon=icon("fire"), tabName="ignitionsTable" ),
      tags$li(
        tags$a(
          onclick = "Shiny.setInputValue('ignitionsTable', 1, {priority: 'event'});",
          href = "#",
          # Keeps the cursor as a pointer
          icon("fire"),
          # Add your icon
          tags$span("Ignitions Table") # Add your text
        )
      ),
      # shinydashboard::menuItem("LUT FUEL Model", icon = icon("fire"), tabName = "inputInstancesLUT")
      # ),
      shinydashboard::menuItem("Outputs", icon = icon("table"), tabName = "outputInstances")
    ),
    
    
    shiny::actionButton("runsim", "🔥 Run"),
    # shiny::actionButton("postsim", "PostProcessing",
    #                     title="After simulation is finished, you can run a <b>postprocess</b> to extract information from results - requires an output instance" ),
    #
    tags$div(
      class = "sidebar-logo",
      tags$img(src = "wildfireLogo.png", style = "width:100%; padding:10px;")
    )
  ),
  
  # BODY ----
  body = dashboardBody(
    useShinyjs(),
    # use_theme(mytheme),
    tags$head(
      tags$script(HTML(vocsJS)),
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = sprintf("log.css?%s", Sys.time())
      ),
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = sprintf("tippy.css?%s", Sys.time())
      ),
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = sprintf("cell2fire_log.css?%s", Sys.time())
      ),
      tags$script(src = sprintf("log.js?%s", Sys.time())),
      tags$script(src = sprintf("makeWindWidget.js?%s", Sys.time())),
      tags$link(rel = "stylesheet", href = "https://unpkg.com/tippy.js@6/dist/tippy.css"),
      tags$script(src = "https://unpkg.com/@popperjs/core@2"),
      tags$script(src = "https://unpkg.com/tippy.js@6")
    ),
    tags$div(id = "page-loader", tags$div(class = "loader-spinner")),
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "dashboardMap",
        #style="margin:-15px -30px;",
        shinydashboardPlus::box(
          id = "mapContainer",
          leafletOutput("map", height = 600),
          collapsible = TRUE,
          width = 12,
          title = "Map",
          solidHeader = TRUE,
          status = "black",
          sidebar = boxSidebar(
            icon = tags$div(tags$i("🔥"), "Toggle Ignition Table", title = "Ignition table") ,
            background = "white",
            id = "ignitionSideBar",
            # shinydashboardPlus::box(width=12, id = "inputInstancesIgnitions", title =
            tags$div(
              style = "display:flex; align-items:center; gap:6px;",
              span("Ignition File"),
              actionButton(
                "delete_table_ignition_row",
                HTML(
                  "<sup class='helpTitle'
            title='Delete selected rows'>?</sup>"
                ),
                icon = icon("cancel"),
                class = "btn-sm" 
              ),
              span("|", style =
                     "color:black;"),
              div( 
                style = "margin-bottom:-15px",
                shinyWidgets::pickerInput(
                  "chooseIgnitionFile",
                  HTML(
                    "<sup class='helpTitle'
            title='Select an ignition file to modify - this layer can be used as <u>ignitions points</u> input argument. To add rows, use the button <span>🔥 </span> in the map, coordinates will be converted to Ncell values.'>?</sup>"
                  ),
                  width = "100px",
                  choices = c()
                )
              ),
              actionButton(
                "save_table_ignition", 
                HTML(
                  "<sup class='helpTitle'
            title='Save this table!'>?</sup>"
                ),
                icon = icon("save"),
                class = "btn-sm" 
              ),
              actionButton(
                "delete_table_ignition",
                HTML(
                  "<sup class='helpTitle'
            title='Delete this table!'>?</sup>"
                ),
                icon = icon("trash"),
                class = "btn-sm" 
              ),
              downloadButton(
                "download_table_ignition",
                label = NULL,
                icon = icon("download"),
                class = "btn-sm",
                title =
                  "Download table in CSV file format"
              ),
              # actionButton("upload_table_ignition", label = NULL, icon = icon("upload"), class="btn-sm", title="Upload your table(make sure it is in the same format as the required input format for Cell2Fire)"),
              div(
                style = "margin-bottom: -40px; ",
                fileInput(
                  "upload_table_ignition_input",
                  label = NULL,
                  buttonLabel = icon("upload"),
                  width = 20,
                  accept = c(".csv", ".xlsx", ".xls")
                )
              )
            ),
            div(style = "overflow-x: auto;", DTOutput("ignitionInfo"))
            # ,# collapsible=TRUE, collapsed = TRUE,
            # solidHeader = TRUE,status = "primary")
          )
        ) #,
        
        # shinydashboardPlus::box( uiOutput("raster_info"),collapsible=TRUE,title="Rasters Info",solidHeader = TRUE )
      ),
      
      shinydashboard::tabItem(
        tabName = "processLogTab",
        tabsetPanel(
          tabPanel("Log of Wildfire-Sim", uiOutput("log")),
          tabPanel("Message Log of Simulator", div(id =
                                                     "logSim")) ,
          tabPanel("Errors/Warnings of Simulator", div(id =
                                                         "logSimErr"))
        )
        # shinydashboardPlus::box( uiOutput("log"), width=12, collapsible=TRUE,title="Log of process",solidHeader = TRUE,status = "primary"),
        
      ),
      
      shinydashboard::tabItem(
        tabName = "infoBox",
        box(
          title = "Information",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          collapsible = TRUE ,
          shiny::includeMarkdown(file.path(
            this.path::this.dir(), "../../README.md"
          ))
        )
      ),
      shinydashboard::tabItem(
        tabName = "inputInstancesWeather",
        # h3("Weather CSV File"),
         shinydashboardPlus::box(
         width = 12,
         tags$div(
            style = "display:flex; align-items:center; gap:6px;",
            
            div(
              style = "margin-bottom:-15px",
              shinyWidgets::pickerInput(
                "chooseWeatherFile",
                label =  NULL,
                # width = "200px",
                choices = c()
              )
            ),HTML(
              "<sup class='helpTitle'
            title='Select a weather file (or upload one if not present) - check format at <a href=https://github.com/fire2a/C2F-W/blob/main/data/ScottAndBurgan/Zona_60-tif/Weather.csv target=_blank>Link here</a>'>?</sup>"
            ),
            
            actionButton(
              "save_table_weather",
              HTML(
                "<sup class='helpTitle'
            title='Save weather  table!'>?</sup>"
              ),
              icon = icon("save"),
              class = "btn-sm" 
            ),
            actionButton(
              "delete_table_weather",
              HTML(
                "<sup class='helpTitle'
            title='Delete weather table!'>?</sup>"
              ),
              icon = icon("trash"),
              # width = "100%",
              class = "btn-danger"
            ),
            
            downloadButton(
              "download_table_weather", 
              label =  HTML(
                "<sup class='helpTitle'
            title='Download table in CSV format so you can modify and reload'>?</sup>"
              ),
              icon = icon("download"),
              class = "btn-sm"
            ),
            # actionButton("upload_table_weather", label = NULL, icon = icon("upload"), class="btn-sm", title="Upload your table(make sure it is in the same format as the required input format for Cell2Fire)"),
    
            
              actionButton(
                "create_table_weather",
                label =  HTML(
                  "<sup class='helpTitle'
            title='Click here to get values at center of map using  <b>open-meteo API and EFFIS Web Services</b> (NB this is experimental and not to be used for production environments or decision making!). The time steps are daily and  in GMT and at at hourly steps so make sure you remove lines that do not cover your desired fire time span.<br><a href=https://forest-fire.emergency.copernicus.eu/ target=_blank>👉 Learn more about EFFIS, the European Forest Fire Information System from Copernicus</a><br><a href=https://open-meteo.com/ target=_blank>👉 Learn more about meteo data and fire behaviour</a>'>?</sup>"
                ),
                icon = icon("cloud"),
                class = "btn-sm" 
            ),
            div( 
              style="margin-bottom:-35px;",
              fileInput(
                "upload_table_weather_input",
                label =  NULL,
                buttonLabel = HTML(
                  paste(icon("upload"), "<sup class='helpTitle'
            title='Upload your table(make sure it is in the same format as the required input format for Cell2Fire)'>?</sup>")
                  
                ),
                width = 10,
                accept = c(".csv", ".xlsx", ".xls")
              )
            )
          ),
          # status = "primary",
          # solidHeader = TRUE,
          # collapsible = TRUE,
          # headerBorder = TRUE,
          # enable_sidebar = FALSE,
          div(style = "overflow-x: auto; margin-top:10px;", id =
                "weatherTableOutputDIV", DTOutput("weatherTable"))
        )
      ),
      shinydashboard::tabItem(
        tabName = "inputInstancesLUT",
        shinydashboardPlus::box(
          width = 12,
          title = tags$div(
            style = "display:flex; align-items:center; gap:6px;",
            span("Look-up table FBP CSV File"),
            actionButton(
              "save_table_FBP",
              label = NULL,
              icon = icon("save"),
              class = "btn-sm",
              title = "Save changes to be used in the Cell2Fire process (only valid for this session)"
            ),
            downloadButton(
              "download_table_FBP",
              label = NULL,
              icon = icon("download"),
              class = "btn-sm",
              title = "Download table in CSV file format"
            ),
            actionButton(
              "upload_table_FBP",
              label = NULL,
              icon = icon("upload"),
              class = "btn-sm",
              title = "Upload your table (make sure it is in the same format as the required input format for Cell2Fire)"
            ),
            div(
              style = "display:none;",
              fileInput(
                "upload_table_FBP_input",
                label = NULL,
                buttonLabel = NULL,
                width = 10,
                accept = c(".csv", ".xlsx", ".xls")
              )
            )
          ),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          headerBorder = TRUE,
          enable_sidebar = FALSE,
          div(
            div(
              "Look up table between Fuel model codes in Forest raster and Canadian Fire Behaviour Prediction System"
            ),
            style = "overflow-x: auto;",
            DTOutput("FBP.table")
          )
        )
      ),
      
      shinydashboard::tabItem(
        tabName = "inputInstancesInputArgs",
        tags$div(
          class = "code-box",
          tags$span(class = "copy-btn", onclick = "copyToClipboard('code1')", "Copy"),
          tags$div(id = "code1")
        ),
        tags$div(class = "masonry", uiInputsArgs)
      )
      
    )
    
  ),
  
  
  
  # CONTROLBAR ----
  controlbar = dashboardControlbar(
    id = "my_controlbar",
    controlbarMenu(
      id = "controlbar_tabs",
      controlbarItem(
        title = NULL,
        icon = tags$div(title = "Manage Selected Dataset", icon("database")),
        h6("Manage selected dataset"),
        
        shiny::fileInput(
          "zipfileload_dataset",
          label = HTML(
            "Upload Dataset<sup class='helpTitle' title='add a zip file with all necessary files - please see <a href=https://github.com/fpirotti/Cell2FireR/blob/master/README.md target=_blank> 📖 documentation HERE</a> on how to prepare it'>?</sup>"
          ),
          buttonLabel =  icon("upload"),
          accept = c(".zip")
        ),
        
        div( 
          disabled( shiny::downloadButton(
            "download_landscape_flammap" ,
            label =tagList(
              tags$img(
                src = "flammap.png", 
                # height = "20px", 
                style = "vertical-align: middle; margin-right: 5px;"
              ) 
            ), 
            style = "width:70%; margin-bottom:10px;" 
          ) ),
          HTML(
          "<sup class='helpTitle' title='<table style=\"width:100%; border-collapse: collapse; font-family: sans-serif; margin: 20px 0;\">",
          "<thead>",
          "<tr style=\"background-color: #2c3e50; color: white; text-align: left;\">
              <th style=\"padding: 4px; border: 1px solid #dddddd;\">GeoTIFF Band</th>
              <th style=\"padding: 4px; border: 1px solid #dddddd;\">Data </th>
              <th style=\"padding: 4px; border: 1px solid #dddddd;\">Units</th>
              </tr>
              </thead>
              <tbody>
              <tr style=\"background-color: #f8f9fa;\">
              <td style=\"padding: 4px; border: 1px solid #dddddd; font-weight: bold;\">Band 1</td>
              <td style=\"padding: 4px; border: 1px solid #dddddd;\">Elevation</td>
              <td style=\"padding: 4px; border: 1px solid #dddddd;\">Meters</td>
              </tr>
              <tr>
              <td style=\"padding: 4px; border: 1px solid #dddddd; font-weight: bold;\">Band 2</td>
              <td style=\"padding: 4px; border: 1px solid #dddddd;\">Slope</td>
              <td style=\"padding: 4px; border: 1px solid #dddddd;\">Degrees</td>
              </tr>
              <tr style=\"background-color: #f8f9fa;\">
              <td style=\"padding: 4px; border: 1px solid #dddddd; font-weight: bold;\">Band 3</td>
              <td style=\"padding: 4px; border: 1px solid #dddddd;\">Aspect</td>
              <td style=\"padding: 4px; border: 1px solid #dddddd;\">Degrees</td>
              </tr>
              <tr>
              <td style=\"padding: 4px; border: 1px solid #dddddd; font-weight: bold;\">Band 4</td>
              <td style=\"padding: 4px; border: 1px solid #dddddd;\">Fire Behavior Fuel Model</td>
              <td style=\"padding: 4px; border: 1px solid #dddddd;\">Class (40 FBFM / 13 Anderson)</td>
              </tr>
              <tr style=\"background-color: #f8f9fa;\">
              <td style=\"padding: 4px; border: 1px solid #dddddd; font-weight: bold;\">Band 5</td>
              <td style=\"padding: 4px; border: 1px solid #dddddd;\">Canopy Cover</td>
              <td style=\"padding: 4px; border: 1px solid #dddddd;\">Percent (%)</td>
              </tr>
              <tr>
              <td style=\"padding: 4px; border: 1px solid #dddddd; font-weight: bold;\">Band 6</td>
              <td style=\"padding: 4px; border: 1px solid #dddddd;\">Canopy (Stand) Height</td>
              <td style=\"padding: 4px; border: 1px solid #dddddd;\">Meters</td>
              </tr>
              <tr style=\"background-color: #f8f9fa;\">
              <td style=\"padding: 4px; border: 1px solid #dddddd; font-weight: bold;\">Band 7</td>
              <td style=\"padding: 4px; border: 1px solid #dddddd;\">Canopy Base Height (CBH)</td>
              <td style=\"padding: 4px; border: 1px solid #dddddd;\">Meters</td> 
              </tr>
              <tr>
              <td style=\"padding: 4px; border: 1px solid #dddddd; font-weight: bold;\">Band 8</td>
              <td style=\"padding: 4px; border: 1px solid #dddddd;\">Canopy Bulk Density (CBD)</td>
              <td style=\"padding: 4px; border: 1px solid #dddddd;\">kg/m&sup3;</td>
              </tr>
              </tbody>
              </table>'>?</sup>")
          ),

        shiny::downloadButton(
          "downloadfolder_dataset" ,
          label = HTML(
            "Download Dataset<sup class='helpTitle' title='Downloads a ZIP file with dataset so you can run on your own cell2fire implementation.'>?</sup>"
          ),
          style = "width:100%; margin-bottom:10px;",
          
          icon = icon("download")
        ),
        
        shiny::actionButton(
          "deletefolder_dataset",
          label = HTML(
            "Delete Dataset<sup class='helpTitle' title='Delete active dataset'>?</sup>"
          ),
          style = "width:100%; margin-bottom:10px;",
          icon = icon("trash")
        )
      ),
      
      controlbarItem(
        title = NULL,
        icon = tags$div(title = "Simulation Output Instance", icon("square-poll-vertical")),
        
        div(
          class = "p-2",
          
          h4("Manage Selected Simulation Output Instance"),
          
          fluidRow(column(
            width = 12,
            checkboxInput(
              "processSimulationOutputInstance_force",
              label = HTML(
                "FORCE<sup class='helpTitle'
              title='Force re-postProcessing   the active simulation instance. '>?</sup>"
              ),
              value = F
            )
          )),
          fluidRow(column(
            width = 12, div(
              class = "d-grid gap-2",
              
              actionButton(
                "processSimulationOutputInstance",
                HTML(
                  "Process <sup class='helpTitle'
              title='Postprocess the active simulation instance.'>?</sup>"
                ),
                icon = icon("gear"),
                width = "100%",
                class = "btn-primary"
              )
            )
          )),
          
          br(),
          
          fluidRow(column(
            width = 12, div(class = "d-grid gap-2", downloadButton(
              "download.ros",
              HTML(
                "ROS <span></span>  ▦ <sup class='helpTitle'
            title='Download Rate of Spread raster, one for each simulation'>?</sup>"
              ),
              style = "font-size:small;  width:100%; margin-bottom:10px;"
            ))
          )),
          fluidRow(column(
            width = 12, div(class = "d-grid gap-4", downloadButton(
              "download.grids.tiff",
              HTML(
                "Burned Area <span></span> ▦ <sup class='helpTitle'
            title='Download raster grids with burnt pixels for each timestamp in the weather file and for each simulation.'>?</sup>"
              ),
              style = "font-size:small; width:100%; margin-bottom:10px;"
            ))
          )),
          
          fluidRow(column(
            width = 12, div(class = "d-grid gap-4", downloadButton(
              "download.grids.gpkg",
              HTML(
                "Burned Area <span></span> ⭔ <sup class='helpTitle'
            title='Download vector models in Geopackage format with with burnt areas for each timestamp in the weather file and for each simulation.'>?</sup>"
              ),
              style = "font-size:small; width:100%; margin-bottom:10px;"
            ))
          )),
          tags$hr(),
          
          fluidRow(column(
            width = 12,
            
            actionButton(
              "deleteSimulationOutputInstance",
              HTML(
                "Delete <sup class='helpTitle'
            title='Delete active simulation instance.'>?</sup>"
              ),
              icon = icon("trash"),
              width = "100%",
              class = "btn-danger"
            )
          ))
          # ,
          
          # tags$hr(),
          # 
          # fluidRow(column(
          #   width = 12,
          #   
          #   selectInput(
          #     "simulator",
          #     "Fire Spread Simulator",
          #     choices = c(
          #       "Cell2Fire" = "Cell2Fire",
          #       "FlamMap" = "FlamMap",
          #       "..." = "others"
          #     ),
          #     width = "100%"
          #   )
          # )
          # )
        )
      ),
    )
  ),
  
  title = "Cell2Fire R bindings Demo App"
)
