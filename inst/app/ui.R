# ui.R
ui <-  shinydashboardPlus::dashboardPage(
  options = list(sidebarExpandOnHover = TRUE),
  header = shinydashboardPlus::dashboardHeader(title = "Cell2Fire R bindings App" ),
  footer = shinydashboardPlus::dashboardFooter(
    left =  HTML("<a href='www.cirgeo.unipd.it' >CIRGEO - University of Padova</a> "),
    right = HTML("<a href='mailto:francesco.pirotti@unipd.it' >MAIL<span style='font-size:22px'>📧</span></a> ")
  ),
  sidebar = shinydashboardPlus::dashboardSidebar(minified = FALSE, collapsed = FALSE,
                                                 shiny::selectInput(
                                                   "inputfolder",
                                                   NULL,
                                                   choices = c("")
                                                 ),
                                                 shiny::fluidRow(
                                                   
                                                   shiny::column(4,    div(
                                                     shiny::fileInput("zipfileload", label=NULL, buttonLabel =  icon("upload") ), 
                                                     title="add a zip file with all necessary files - please see documentation on how to prepare it"
                                                   ) ), 
                                                    shiny::column(3,   div(
                                                     shiny::actionButton("downloadfolder" ,   NULL,
                                                                               icon = icon("download") ),  title="Download dataset") ),
                                                   shiny::column(3, div(
                                                     shiny::actionButton("deletefolder", NULL,
                                                                              icon=icon("trash") ), title="Delete dataset") ) 
                                                  ), 
                                                 shiny::actionButton("run", "Run Cell2Fire", icon=icon("fire") ),
                                                 div(onclick="$('#inputInstancesIgnitions').toggle()", shiny::actionButton("run", "Ignitions Table", icon=tags$i("🔥") ) ), 
                                                 shinydashboard::sidebarMenu(
                                                   shinydashboard::menuItem("Map", tabName = "dashboardMap", icon = icon("dashboard")),
                                                   shinydashboard::menuItem("Process Log", icon = icon("gear"), tabName = "widgets"),
                                                   shinydashboard::menuItem("Inputs", icon = icon("table"),
                                                                            shinydashboard::menuItem("Input Args", icon = icon("sliders-h"), tabName = "inputInstancesInputArgs"),
                                                                           # shinydashboard::menuItem("Input Args2", icon = icon("sliders-h"), tabName = "inputInstancesInputArgs2"),
                                                                            shinydashboard::menuItem("Weather File", icon = icon("cloud-rain"), tabName = "inputInstancesWeather"),
                                                                            
                                                                            shinydashboard::menuItem("LUT FUEL Model", icon = icon("fire"), tabName = "inputInstancesLUT")
                                                                            ),
                                                   shinydashboard::menuItem("Outputs", icon = icon("table"), tabName = "outputInstances"),
                                                   shinydashboard::menuItem("Information", icon = icon("info"), tabName = "infoBox")
                                                 )
                                              ),
  body = dashboardBody(
    useShinyjs(),
    # use_theme(mytheme),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = sprintf("log.css?%s", Sys.time()) ),
      tags$script(src = sprintf("log.js?%s", Sys.time())),
        tags$link(rel="stylesheet", href="https://unpkg.com/tippy.js@6/dist/tippy.css"),
        tags$script(src="https://unpkg.com/@popperjs/core@2"),
        tags$script(src="https://unpkg.com/tippy.js@6")
    ),

    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "dashboardMap",
              shinydashboardPlus::box(  leafletOutput("map", height = 600), 
                                        collapsible=TRUE, width = 12, 
                                        title="Map", solidHeader = TRUE,
                                        status = "black",         sidebar = boxSidebar(icon=tags$div(tags$i("🔥"), title="Ignition table") ,
                                                                                       background = "white",
                                          id = "ignitionSideBar",
                                          # shinydashboardPlus::box(width=12, id = "inputInstancesIgnitions", title = 
                                                                  tags$div(
                                                                    style = "display:flex; align-items:center; gap:6px;",
                                                                    span("Ignition File"),
                                                                    div(title="Select an ignition file", shinyWidgets::dropdown("chooseIgnitionFile", NULL, size = "sm",  choices = c()) ),
                                                                    actionButton("save_table_ignition", label = NULL, icon = icon("save"), class="btn-sm", title="Save changes to be used in the Cell2Fire process (only valid for this session)"),
                                                                    downloadButton("download_table_ignition", label = NULL, icon = icon("download"), class="btn-sm", 
                                                                                   title="Download table in CSV file format"),
                                                                    actionButton("upload_table_ignition", label = NULL, icon = icon("upload"), class="btn-sm", title="Upload your table(make sure it is in the same format as the required input format for Cell2Fire)"),
                                                                    div(style="display:none;", fileInput("upload_table_ignition_input", label = NULL, buttonLabel = NULL, width = 10,  accept = c(".csv", ".xlsx", ".xls")  ) )
                                                                  ),
                                                                  div( style = "overflow-x: auto;",  DTOutput("ignitionInfo")  )
                                                                  # ,# collapsible=TRUE, collapsed = TRUE,
                                                                  # solidHeader = TRUE,status = "primary")
                                        ) ),
              
              shinydashboardPlus::box( uiOutput("raster_info"),collapsible=TRUE,title="Rasters Info",solidHeader = TRUE )
      ),

      shinydashboard::tabItem(tabName = "widgets",
              shinydashboardPlus::box( uiOutput("log"), width=12, collapsible=TRUE,title="Log of process",solidHeader = TRUE,status = "primary"),

      ),

      shinydashboard::tabItem(tabName = "infoBox",

        box(
          title = "Information",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          collapsible = TRUE,
          shiny::includeMarkdown("../../README.md")
        )
      ),
      shinydashboard::tabItem(tabName = "inputInstancesWeather",
          shinydashboardPlus::box(width=12,
                                  title = tags$div(
                                    style = "display:flex; align-items:center; gap:6px;",
                                    span("Weather CSV File"),
                                    actionButton("save_table_weather", label = NULL, icon = icon("save"), class="btn-sm", title="Save changes to be used in the Cell2Fire process (only valid for this session)"),
                                    downloadButton("download_table_weather", label = NULL, icon = icon("download"), class="btn-sm", title="Download table in CSV file format"),
                                    actionButton("upload_table_weather", label = NULL, icon = icon("upload"), class="btn-sm", title="Upload your table(make sure it is in the same format as the required input format for Cell2Fire)"),
                                    actionButton("create_table_weather", label = NULL, icon = icon("cloud"), class="btn-sm", title="This will upload the weather scenario from the current day using online APIs such as JRC's jrc-effis@ec.europa.eu services that provide FWI indices..."),
                                    div(style="display:none;", fileInput("upload_table_weather_input", label = NULL, buttonLabel = NULL, width = 10,  accept = c(".csv", ".xlsx", ".xls")  ) )
                                  ),
                                  status = "primary",
                                  solidHeader = TRUE, collapsible=TRUE,
                                  headerBorder = TRUE,
                                  enable_sidebar = FALSE,
                                  div(
                                    style = "overflow-x: auto;",
                                    DTOutput("weather.table")
                                  )
          )
      ),
      shinydashboard::tabItem(tabName = "inputInstancesLUT",
        shinydashboardPlus::box(width=12,
          title = tags$div(
            style = "display:flex; align-items:center; gap:6px;",
            span("Look-up table FBP CSV File"),
            actionButton("save_table_FBP", label = NULL, icon = icon("save"), class="btn-sm", title="Save changes to be used in the Cell2Fire process (only valid for this session)"),
            downloadButton("download_table_FBP", label = NULL, icon = icon("download"), class="btn-sm", title="Download table in CSV file format"),
            actionButton("upload_table_FBP", label = NULL, icon = icon("upload"), class="btn-sm", title="Upload your table(make sure it is in the same format as the required input format for Cell2Fire)"),
            div(style="display:none;", fileInput("upload_table_FBP_input", label = NULL, buttonLabel = NULL, width = 10,  accept = c(".csv", ".xlsx", ".xls")  ) )
          ),
          status = "primary",
          solidHeader = TRUE, collapsible=TRUE,
          headerBorder = TRUE,
          enable_sidebar = FALSE,
          div(
            div("Look up table between Fuel model codes in Forest raster and Canadian Fire Behaviour Prediction System"),
            style = "overflow-x: auto;",
            DTOutput("FBP.table")
          )
        )
      ),

      shinydashboard::tabItem(tabName = "inputInstancesInputArgs",
                              uiInputsArgs
      )

     )

  ),
  controlbar = shinydashboardPlus::dashboardControlbar(),
  title = "Cell2Fire R bindings Demo App"
)

