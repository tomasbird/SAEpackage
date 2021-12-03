library(dplyr)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(sp)
library(rgdal)
library(ggplot2)
library(shinycssloaders)
library(cvms)
library(car)
library(Rcpp)
library(Metrics)
library(merTools)
library(leaps)
library(sf)
library(tidyr)
library(groupdata2)
library(pROC)
library(DT)
options(shiny.maxRequestSize=200*1024^2)


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


