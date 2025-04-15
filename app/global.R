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

