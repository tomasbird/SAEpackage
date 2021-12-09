#library(dplyr)
#library(leaflet)
##library(shiny)
#library(RColorBrewer)
#library(lattice)
#library(scales)
#library(sp)
#library(rgdal)
#library(shinycssloaders)
#library(ggplot2)
#library(cvms)
#library(car)
#library(Rcpp)
#library(Metrics)
#library(merTools)
#library(leaps)
#library(sf)
#library(tidyr)
#library(groupdata2)
#library(pROC)
#library(DT)

options(shiny.maxRequestSize=200*1024^2)

packages=c("DT", "pROC", "groupdata2", "tidyr", "sf", "leaps", "merTools", "Metrics", "Rcpp", "car", 
           "cvms", "shinycssloaders", "ggplot2", "rgdal", "sp", "lattice", "scales", "RColorBrewer", 
           "leaflet", "shiny", "dplyr", "plotly", "widgetframe")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


tags$style(type="text/css", "
           #loadmessage {
             position: fixed;
             top: 0px;
             left: 0px;
             width: 100%;
             padding: 5px 0px 5px 0px;
             text-align: center;
             font-weight: bold;
             font-size: 100%;
             color: #000000;
             background-color: #CCFF66;
             z-index: 105;
           }
  ")


