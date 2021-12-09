


### Conditional panel if loading data
surveyloadPanel=conditionalPanel(
  condition='input.datatoload == "Survey"', 
  fileInput("survey.file", "Upload .csv for survey data",
            multiple = TRUE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  
  #Input: Select a survey shapefile ----
  fileInput("survey.shp.file", "Upload survey shapefile",
            multiple = TRUE, 
            accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg', '.xml', ".DBF"))
) 

### Conditional panel if loading data
censusloadPanel=conditionalPanel(
  condition='input.datatoload == "Census"', 
  fileInput("census.file", "Upload .csv for census data",
            multiple = TRUE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  
  #Input: Select a survey shapefile ----
  fileInput("census.shp.file", "Upload census shapefile",
            multiple = TRUE,
            accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg', '.xml', ".DBF"))
) 

## Function for data file upload
uploadDF <- function(usedemo, loadfile, localfile){
  if(usedemo==FALSE) {path = loadfile$datapath}
  else {path = localfile}
  read.csv(path) 
}

# load survey DF
surveyDF <- reactive({
  uploadDF(usedemo=input$usedemo, loadfile=input$survey.file$datapath, localfile="R/data/DHS_formatted.csv")
})

## load census DF 
censusDF <- reactive({
  uploadDF(usedemo=input$usedemo, loadfile=input$census.file$datapath, localfile="R/data/census_formatted.csv")
})



## Reactive UI for choosing survey variables to use
# indicator
output$choose_survey_indicator <- renderUI({
  req(input$usedemo)
  selectInput("indicator", "Choose an indicator to model", names(surveyDF()))
})

# survey spatial identifier
output$choose_survey_spatial <- renderUI({
  req(input$usedemo)
  selectInput("survey_spatial", "Choose column for survey areas", names(surveyDF()))
})

# response variables
output$choose_census_vars <- renderUI({
  req(input$usedemo)
  #req(input$survey_spatial)
  rem= which(names(censusDF()) %in% c(input$survey_spatial, input$census_spatial))
  checkboxGroupInput("Predictors", "Choose predictor variables", 
                     choices=names(censusDF())[-c(rem)], 
                     selected=names(censusDF())[-c(rem)])
})

# census spatial identifier
output$choose_census_spatial <- renderUI({
  req(input$usedemo)
  req(input$survey_spatial)
  remspat= which(names(censusDF()) %in% c(input$survey_spatial))
  selectInput("census_spatial", "Choose column for census areas", names(censusDF())[-remspat])
})
  
## Tabular output for viewing data
# Survey
# preview 
output$survey_preview <- DT::renderDataTable({
  dat=head(surveyDF())
  DT::datatable(dat, rownames=FALSE) %>%
    formatSignif(columns=  which(sapply(dat, class) %in% c("numeric")), digits=2)
  
})

# selected data
output$surveyselect_table <- DT::renderDataTable({
  req(input$usedemo)
  dat=head(subset(surveyDF(),select=c(input$indicator, input$survey_spatial)), n=100)
  DT::datatable(format(dat, 2), rownames=FALSE) %>%
    formatSignif(columns=  which(sapply(dat, class) %in% c( "numeric")), digits=2)
})

# Census
# Preview
output$census_preview <- DT::renderDataTable({
  req(input$usedemo)
  dat=head(censusDF(), n=100)
  DT::datatable(dat) #%>%
    #formatSignif(columns= which(sapply(dat, class) %in% c( "numeric")), digits=2)
})

# preview selected variables
output$censusselect_table <- DT::renderDataTable({
  req(input$usedemo)
  DT::datatable(head(subset(censusDF(), 
                            select=c(input$Predictors, input$census_spatial, input$survey_spatial))), n=100)
})



## Load shapefiles function

uploadShpfn=function(usedemo, uploadfile, localfile) {
  if(usedemo==FALSE) {
    if (!is.null(uploadfile)){
      shpDF <- uploadfile
      prevWD <- getwd()
      uploadDirectory <- dirname(shpDF$datapath[1])
      setwd(uploadDirectory)
      for (i in 1:nrow(shpDF)){
        file.rename(shpDF$datapath[i], shpDF$name[i])
      }
      shpName <- shpDF$name[grep(x=shpDF$name, pattern="*.shp")]
      if(length(shpName)>1) shpName=shpName[-(grep(".xml", shpName))]
      shpPath <- paste(uploadDirectory, shpName, sep="/")
      setwd(prevWD)
      survey_shp <- readOGR(shpPath)
      return(survey_shp)
    } else {
      return()
    }
  } else {
    survey_shp = readOGR(localfile)
    return(survey_shp)}  
}


surveyShp <- reactive({
  req(input$usedemo)
  uploadShpfn(usedemo=input$usedemo, uploadfile=input$survey.shp.file, 
              localfile="R/Shapefiles/sdr_subnational_boundaries3.shp")
})

#surveyShp <- reactive({
  #req(input$survey.shp.file)
#  if(input$usedemo==FALSE) {
#  if (!is.null(input$survey.shp.file)){
#    shpDF <- input$survey.shp.file
#    prevWD <- getwd()
#    uploadDirectory <- dirname(shpDF$datapath[1])
#    setwd(uploadDirectory)
#    for (i in 1:nrow(shpDF)){
#      file.rename(shpDF$datapath[i], shpDF$name[i])
#    }
#    shpName <- shpDF$name[grep(x=shpDF$name, pattern="*.shp")]
#      if(length(shpName)>1) shpName=shpName[-(grep(".xml", shpName))]
#    shpPath <- paste(uploadDirectory, shpName, sep="/")
#    setwd(prevWD)
#    survey_shp <- readOGR(shpPath)
#    return(survey_shp)
#  } else {
#    return()
#  }
#  } else {
#      survey_shp = readOGR("R/Shapefiles/sdr_subnational_boundaries3.shp")
#      return(survey_shp)}
#})

# map of survey areas
output$surveyMap <- renderPlot({
  surveyShp() %>% 
    st_as_sf()  %>%
    ggplot() +
    geom_sf() +
    theme_void() 
  
  #ggplotly(p) %>%
  #  highlight(
  #    "plotly_hover",
  #    selected = attrs_selected(line = list(color = "black"))
  #  ) %>%
  #  widgetframe::frameWidget()
})

output$surveyshpchk <- renderText({
  print(class(surveyShp()))
})

# census
censusShp <- reactive({
  uploadShpfn(usedemo=input$usedemo, uploadfile=input$census.shp.file, 
              localfile="R/Shapefiles/nepal_districts.shp")
})


#censusShp <- reactive({
  #req(input$survey.shp.file)
#  if(input$usedemo==FALSE) {
#      shpDF <- input$census.shp.file
##    if (!is.null(input$census.shp.file)){
#      prevWD <- getwd()
#      setwd(uploadDirectory)
##      uploadDirectory <- dirname(shpDF$datapath[1])
#      for (i in 1:nrow(shpDF)){
#        file.rename(shpDF$datapath[i], shpDF$name[i])
#      }
#      shpName <- shpDF$name[grep(x=shpDF$name, pattern="*.shp")]
#      if(length(shpName)>1) shpName=shpName[-(grep(".xml", shpName))]
#      shpPath <- paste(uploadDirectory, shpName, sep="/")
#      setwd(prevWD)
#      census_shp <- readOGR(shpPath)
#      return(census_shp)
#    } else {
#      return()
#    }
#  } else {
#    census_shp = readOGR("R/Shapefiles/nepal_districts.shp")
#    return(census_shp)}
#})


## map of census units
output$censusMap <- renderPlot({
 censusShp() %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf() +
    theme_void() 
})

