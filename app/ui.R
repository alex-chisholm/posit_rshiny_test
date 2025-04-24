# packages ===================================================================
# basic shiny packages
library(shiny)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(shinysky)
library(shinyvalidate)
library(bslib)
library(shinyfullscreen)
# data manupulation packages 
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(readr)
library(tidyr)
library(cli)
library(rjson)
# map packages
library(leaflet)
library(leaflet.esri)
library(leaflet.extras)
library(leafgl)
library(sf)
library(cartography)
# db management packages
library(pool)
library(DBI)
library(RPostgres)
# viz packages
library(reactable)
library(plotly)
# others
library(emayili)

# default config =============================================================
options(stringsAsFactors = FALSE)
options(scipen=999) # avoids scientific notation
options(dplyr.summarise.inform = FALSE)
sf_use_s2(FALSE)

# global reactive vars
vals <- reactiveValues(count=0)

# db =========================================================================
readRenviron(".Renviron")

con <- pool::dbPool(
  RPostgres::Postgres(),
  dbname = Sys.getenv("dbname"),
  host =  Sys.getenv("host"),
  user = Sys.getenv("user"),
  password =  Sys.getenv("password"),
  port = Sys.getenv("port")
)

# load functions =============================================================
purrr::walk(
  list.files("functions", full.names = T),
  ~source(.x)
)

navbarPage(  
  title = span(
    img(src = 'img/TFT_logo_icon.png', height = 32),
    "PROJECT NAME",
    class = "app-title",
    
  ),
  windowTitle = "PROJECT NAME",
  theme = bs_theme(version = 5),
  collapsible = TRUE,
  header = div(
    id = "dev_status",
    style = "
      position: fixed;
      top: 10px;
      right: 10px;
      z-index: 4000;
      color: red;
    ",
    class = "shinyjs-hide",
    "dev"
  ),
  footer = div(
    class = "container app-footer",
    "© 2024 The Freshwater Trust"
  ),
  
  id = "pages", 
  
  
  # Home tab ==================================================================
  tabPanel(
    'Home',
    tags$head(
      includeCSS("www/styles_project_name.css")
    ),
    tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
    tags$head(
      tags$script(
        'window.onbeforeunload = function() { return "Please use the button on the webpage"; };'
      )
    ),
    tags$head(
      tags$script(
        src ="https://code.highcharts.com/modules/full-screen.js"
      )
    ),
    useShinyjs(),
    ui_home_page("home"),
  ),
  
  # Exploratory Map tab =======================================================
  tabPanel('Exploratory Map', ui_exploratory_map("exploratory_map")),
  
  # Tabular Data tab ==========================================================
  tabPanel('Tabular Data', ui_tabular_data("tabular_data")),
  
  # Quarto Reports tab ========================================================
  tabPanel('Reports', ui_reports("reports")),
  
  # Contact tab ===============================================================
  #tabPanel('Contact', ui_contact("contact"))
  
  
  
)


