



### Conditional panel if loading data
surveyloadPanel=conditionalPanel(
  condition='input.datatoload == "Survey"', 
  p("Choose survey files"),
  fileInput("survey.file", "Choose file for survey data",
            multiple = TRUE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  
  #Input: Select a survey shapefile ----
  fileInput("survey.shp.file", "Choose file for survey spatial data",
            multiple = TRUE, 
            accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg', '.xml', ".DBF"))
) 

### Conditional panel if loading data
censusloadPanel=conditionalPanel(
  condition='input.datatoload == "Census"', 
  p("Choose census files"),
  fileInput("census.file", "Choose file for census data",
            multiple = TRUE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  
  #Input: Select a survey shapefile ----
  fileInput("census.shp.file", "Choose file for census spatial data",
            multiple = TRUE,
            accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg', '.xml', ".DBF"))
) 