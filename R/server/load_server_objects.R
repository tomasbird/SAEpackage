


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
#uploadDF <- function(usedemo, loadfile, localfile){
#  req(input$usedemo)
#  path=ifelse(usedemo==TRUE, localfile, loadfile$datapath )
  #if(usedemo==FALSE) {path = loadfile$datapath}
  #else {path = localfile}
#  read.csv(path) 
#}

#uploadDFpath <- function(usedemo, loadfile, localfile){
#  path=ifelse(usedemo==TRUE, localfile, loadfile$datapath[1] )
#  #if(usedemo==FALSE) {path = loadfile$datapath}
  #else {path = localfile}
#  print(path)
#  print(input$usedemo)
#  print(head(read.csv(path)))
#  print(class(surveyDF()))
#}

#output$pathprint=renderPrint({uploadDFpath(usedemo=input$usedemo, loadfile=input$survey.file, localfile="R/data/DHS_formatted.csv")})

# load survey DF
rawsurveyDF <- reactive({
  shiny::validate(
    need(input$usedemo | !is.null(input$survey.file), "Please load survey dataset")
    )
  
  path=ifelse(input$usedemo==TRUE, "R/data/Nepal/tables/NPL_DHS_formatted.csv", input$survey.file$datapath )
  read.csv(path)
  #uploadDF(usedemo=input$usedemo, loadfile=input$survey.file, localfile="R/data/DHS_formatted.csv")
})

## load census DF 
rawcensusDF <- reactive({
  shiny::validate(
    need(input$usedemo | !is.null(input$census.file), "Please load census dataset")
  )
  #req(input$usedemo)
  cenpath=ifelse(input$usedemo==TRUE, "R/data/Nepal/tables/NPL_census_formatted_VDC.csv", input$census.file$datapath )
  read.csv(cenpath)
  #uploadDF(usedemo=input$usedemo, loadfile=input$census.file$datapath, localfile="R/data/census_formatted_VDC.csv")
})



## Reactive UI for choosing survey variables to use
# indicator
output$choose_survey_indicator <- renderUI({
  req(rawsurveyDF())
  selectInput("indicator", "Indicator to model", names(rawsurveyDF()))
})

# survey spatial identifier
output$choose_survey_spatial <- renderUI({
  req(rawsurveyDF())
  selectInput("survey_spatial", "Survey areas", names(rawsurveyDF()))
})

# choose response variables
output$choose_census_vars <- renderUI({
  req(rawcensusDF())
  rem= which(names(rawcensusDF()) %in% c(input$survey_spatial, input$census_spatial))
  checkboxGroupInput("Predictors", "Predictor variables", 
                     choices=names(rawcensusDF()[-c(rem)]), 
                     selected=names(rawcensusDF())[-c(rem)])
})

# census spatial identifier
output$choose_census_spatial <- renderUI({
  #req(surveyShp())
  req(rawcensusDF(), input$survey_spatial)
  remspat= which(names(censusDF()) %in% c(input$survey_spatial))
  selectInput("census_spatial", "Census area names", names(rawcensusDF())[-remspat])
})


### resulting survey table
# 
censusDF=reactive({
  subset(rawcensusDF(), select=c(input$Predictors, input$census_spatial, input$survey_spatial))
})

surveyDF=reactive({rawsurveyDF()})
  
## Tabular output for viewing data
# Survey
# preview 
output$survey_preview <- DT::renderDataTable({
  req(surveyDF())
  shiny::validate(
    need(!(input$survey_spatial == input$indicator), "Spatial and indicator variables should be different", label="surveydfmissing")
  )
  
  dat=head(surveyDF(), n=100)
  DT::datatable(surveyDF(), rownames=FALSE) %>%
    formatSignif(columns =  which(sapply(dat, class) %in% c("numeric")), digits=2)
})



output$census_preview <- DT::renderDataTable({
  req(censusDF())
  shiny::validate(
   need(censusDF(), "Please load census dataset", label="surveydfmissing"),
   need(input$survey_spatial %in% names(censusDF()), "Survey spatial variable not found in census data. Please check your selection")
  )
  
  dat=head(censusDF(), n=100)
  DT::datatable(dat, rownames=FALSE) #%>%
    #formatSignif(columns=  which(sapply(dat, class) %in% c("numeric")), digits=2)
})


## Load shapefiles function

#uploadShpfn=function(usedemo, uploadfile, localfile) {
#  if(usedemo==FALSE) {
#    if (!is.null(uploadfile)){
#      shpDF <- uploadfile
#      prevWD <- getwd()
#      setwd(uploadDirectory)
#      uploadDirectory <- dirname(shpDF$datapath[1])
#      for (i in 1:nrow(shpDF)){
#        file.rename(shpDF$datapath[i], shpDF$name[i])
#      }
#      shpName <- shpDF$name[grep(x=shpDF$name, pattern="*.shp")]
#      if(length(shpName)>1) shpName=shpName[-(grep(".xml", shpName))]
#      shpPath <- paste(uploadDirectory, shpName, sep="/")
#      setwd(prevWD)
##      survey_shp <- readOGR(shpPath)
#      return(survey_shp)
#    } else {
#      return()
#    }
#  } else {
#    survey_shp = readOGR(localfile)
#    return(survey_shp)}  
#}


surveyShp <- reactive({
 ## Validation
  shiny::validate(
    need(input$usedemo | !is.null(input$survey.shp.file), "Please load survey shapefile")
  )
  
  localfile="R/data/Nepal/shapefiles/NPL_DHS_Regions.shp"
  if(input$usedemo==FALSE) {
    if (!is.null(input$survey.shp.file)){
      shpDF <- input$survey.shp.file
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
      survey_shp <- try(readOGR(shpPath), silent=T)
      return(survey_shp)
    } else {
      return()
    }
  } else {
    survey_shp = try(readOGR(localfile), silent=T)
    return(survey_shp)}  
})


# map of survey areas
output$surveyMap <- renderPlot({
  shiny::validate(
    need(class(surveyShp())=="SpatialPolygonsDataFrame", "No survey shapefile detected. Did you load all associated files?")
  )
  
  #req(surveyShp())
  surveyShp() %>% 
    st_as_sf()  %>%
    ggplot() +
    geom_sf() +
    theme_void() 
})

# census
censusShp <- reactive({
  shiny::validate(
    need(input$usedemo | !is.null(input$census.shp.file), "Please load census shapefile")
  )
  
  localfile="R/data/Nepal/shapefiles/NPL_census_districts.shp"
  if(input$usedemo==FALSE) {
    if (!is.null(input$census.shp.file)){
      shpDF <- input$census.shp.file
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
      census_shp <- try(readOGR(shpPath), silent=T)
      return(census_shp)
    } else {
      return()
    }
  } else {
    census_shp = try(readOGR(localfile), silent=T)
    return(census_shp)}
  
})


## map of census units
output$censusMap <- renderPlot({
  shiny::validate(
    need(class(censusShp())=="SpatialPolygonsDataFrame", "No census shapefile detected. Did you load all associated files?")
  )
  
  req(censusShp())
  censusShp() %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf() +
    theme_void() 
})

