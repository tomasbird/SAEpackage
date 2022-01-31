


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
surveyDF <- reactive({
  path=ifelse(input$usedemo==TRUE, "R/data/DHS_formatted.csv", input$survey.file$datapath )
  read.csv(path)
  #uploadDF(usedemo=input$usedemo, loadfile=input$survey.file, localfile="R/data/DHS_formatted.csv")
})

## load census DF 
censusDF <- reactive({
  #req(input$usedemo)
  cenpath=ifelse(input$usedemo==TRUE, "R/data/census_formatted_VDC.csv", input$census.file$datapath )
  read.csv(cenpath)
  #uploadDF(usedemo=input$usedemo, loadfile=input$census.file$datapath, localfile="R/data/census_formatted_VDC.csv")
})



## Reactive UI for choosing survey variables to use
# indicator
output$choose_survey_indicator <- renderUI({
  req(surveyDF())
  selectInput("indicator", "Choose an indicator to model", names(surveyDF()))
})

# survey spatial identifier
output$choose_survey_spatial <- renderUI({
  req(surveyShp())
  req(surveyDF())
  selectInput("survey_spatial", "Choose column for survey areas", names(surveyDF()))
})

# choose response variables
output$choose_census_vars <- renderUI({
  req(censusDF())
  #req(censusShp())
  #req(surveyShp())
  #req(input$survey_spatial)
  rem= which(names(censusDF()) %in% c(input$survey_spatial, input$census_spatial))
  checkboxGroupInput("Predictors", "Choose predictor variables", 
                     choices=names(censusDF()[-c(rem)]), 
                     selected=names(censusDF())[-c(rem)])
})

# census spatial identifier
output$choose_census_spatial <- renderUI({
  #req(surveyShp())
  req(censusDF(), input$survey_spatial)
  #remspat= which(names(censusDF()) %in% c(input$survey_spatial))
  selectInput("census_spatial", "Choose column for census areas", names(censusDF()))#[-remspat])
})
  
## Tabular output for viewing data
# Survey
# preview 
output$survey_preview <- DT::renderDataTable({
  #req(input$usedemo)
  req(surveyDF())
  dat=head(surveyDF(), n=100)
  DT::datatable(surveyDF(), rownames=FALSE) %>%
    formatSignif(columns=  which(sapply(dat, class) %in% c("numeric")), digits=2)
})


#output$census_exists <- renderPrint({print(head(censusDF()))})
#output$censusprint=renderPrint({
#  print(head(censusDF()))
#  })

#subcensus=reactive({
#  req(censusDF())
#  subset(censusDF(), select=c(input$Predictors, input$census_spatial, input$survey_spatial))
#})


output$census_preview <- DT::renderDataTable({
  req(censusDF)
  #req(subcensus())
  dat=head(subset(censusDF(), select=c(input$Predictors, input$census_spatial, input$survey_spatial)), n=100)
  DT::datatable(censusDF(), rownames=FALSE) #%>%
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
  #req(input$survey.shp.file)
  #uploadShpfn(usedemo=input$usedemo, uploadfile=input$survey.shp.file, 
              localfile="R/Shapefiles/sdr_subnational_boundaries3.shp"
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
      survey_shp <- readOGR(shpPath)
      return(survey_shp)
    } else {
      return()
    }
  } else {
    survey_shp = readOGR(localfile)
    return(survey_shp)}  
})


# map of survey areas
output$surveyMap <- renderPlot({
  #req(input$survey.shp.file)
  req(surveyShp())
  surveyShp() %>% 
    st_as_sf()  %>%
    ggplot() +
    geom_sf() +
    theme_void() 
})

# census
censusShp <- reactive({
  #req(input$census.shp.file)
  #uploadShpfn(usedemo=input$usedemo, uploadfile=input$census.shp.file, 
  localfile="R/Shapefiles/nepal_districts.shp"
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
      census_shp <- readOGR(shpPath)
      return(census_shp)
    } else {
      return()
    }
  } else {
    census_shp = readOGR(localfile)
    return(census_shp)}
  
})


## map of census units
output$censusMap <- renderPlot({
  req(censusShp())
  censusShp() %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf() +
    theme_void() 
})

