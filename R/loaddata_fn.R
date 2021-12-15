



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
  condition='(input.datatoload == "Census")', 
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