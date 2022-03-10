library(shiny)


SAEapp=function(...){
#library(leaflet)
#source("R/global.R", local=TRUE)
#source("R/UI_functions/loaddata_fn.R",local=TRUE)

ui <- fluidPage(
        
    # Define UI for application that draws a histogram
    navbarPage(
        title = 'SAE App',
        
        tabPanel("Introduction",
                 titlePanel("The UNFPA Small Area Estimation Training Application"),
                 fluidRow(column(12, 
                                 br(),
                                 h2("Introduction"),
                                 p("This application provides an introduction to the steps involved in 
                                   conducting small area estimation, based on the methods described in [SAE paper].
                                   It is intended to provide a teaching example based on binary data, but may also be
                                   used on users own data.", 
                                   style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                 br(),
                                 h4("Overview"),
                                 p("The analysis proceeds in several sections, starting with choosing the data files and 
                                   selecting the relevant indicators and predictor variables. When the data are chosen, variables
                                   to be used in models are compared and assessed for their suitability in modeling. When the data are 
                                   ready and the model run, the user has the option to include a spatial random effect.
                                   Following model creation, the user may assess the model fit in a number of ways, followed by prediction
                                   onto census data.", 
                                   style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                 br(),
                                 h4("Available Data"),
                                 p("For learning purposes, survey, census and spatial data are available for the Nepal 2015 DHS
                                   and 2015 census. Users can also load their own data, if they are properly formatted. 
                                   Data from Sierra Leone are also included for training pursposes.", 
                                   style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                 br(),
                                 h4("How to use this App"),
                                 p("Each Tab at the top of the screen walks the user through a different stage of analysis.  Results at 
                                   each stage are presented as tables or figures, which themselves can be downloaded. Error messages are 
                                   displayed, if some part of the analysis goes wrong or data are not formatted correctly.  A full description
                                   of the app is available in the user guide, and an overview of the full methods is present in the working
                                   SAE paper.", 
                                   style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px"),
            
                                 #fluidRow(column(6, img(src="www/surveyheader.png", height=140,width=240))),
                                 ))),
                 
        
        ## Data load
        # App title ----
        tabPanel("Select Data",
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
            # Sidebar panel for inputs ----
            sidebarPanel(
                
                h3("Load data source"),
                p("Choose spatial and numerical data files.
                                       Or use demonstration data by checking the box below."), 
                  #style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                # Input: Choose a data source
                checkboxInput("usedemo", label = "Check to use Nepal data", value = TRUE),
          
                #Choose survey data files 
                surveyloadPanel,
                
                #Choose census data files 
                censusloadPanel,
                
            h3("Select variables"),
            
            conditionalPanel('input.datatoload === "Survey"',),
              #p("Choose the indicator to model from survey data.")), 
              #style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px")),
            
            conditionalPanel('input.datatoload === "Census"',),
              #p("Choose a column for census area names. This should be present in the census shapefile. 
               # Choose variables to be used as predictors in the model.")), 
                             # style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px")),
           
           # Choose indicator and spatial data
           conditionalPanel(
                           'input.datatoload === "Survey"',
                           p("Choose the indicator to model from survey data."),
                           uiOutput("choose_survey_indicator"),
                           
                           p("Choose the column in the survey data that contain the DHS spatial area names"),
                           uiOutput("choose_survey_spatial")
                           ),
               
           conditionalPanel(
                           'input.datatoload === "Census"',
                          p("Choose the column in the census data representing census spatial areas."),
                          uiOutput("choose_census_spatial"),
                          
                          p("Choose variables to be used as predictors. 
                            Please exclude any variables (such as spatial data) 
                            that won't be used in the model"),
                          uiOutput("choose_census_vars"))
           ),
           
            # Display Data from census and survey ----
            mainPanel(
                tabsetPanel(
                id='datatoload',
                tabPanel("Survey", 
                         conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                          tags$div("Loading survey data...",id="loadmessage")
                         ),
                         
                         fluidRow(plotOutput("surveyMap")),
                         fluidRow(DT::dataTableOutput("survey_preview")),
                         fluidRow(verbatimTextOutput("pathprint"))
                         ),
                
                tabPanel("Census", 
                         conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                          tags$div("Loading census data...",id="loadmessage")
                         ),
                         
                         #conditionalPanel(condition="input.indicator === undefined", 
                        #                  h3("Please choose a survey indicator")
                         #),
                         #conditionalPanel(condition="input.indicator !in names(output.survey_preview)", 
                         #                  h3("Please choose a survey indicator")
                         #),
                         fluidRow(plotOutput("censusMap") %>% withSpinner(color="#0dc5c1")),
                         #fluidRow(verbatimTextOutput("censusprint")),
                         #fluidRow(verbatimTextOutput("census_exists")%>% withSpinner(color="#0dc5c1")))
                         fluidRow(DT::dataTableOutput("census_preview"))) #%>% withSpinner(color="#0dc5c1")))
                         )))),
        

        
        ## Tab for Comparing Data
        tabPanel('Compare Data', #sidebarLayout(
            #sidebarPanel(
                titlePanel("Data distributions"),
                
                conditionalPanel('input.comparison=="Correlations"',
                p("Before building a model, it's important to check whether the data are suitable.  
                  One way to do this is to check whether the variables in survey data are correlated 
                  with those in census data.")),
                
                conditionalPanel('input.comparison=="Distributions"',
                    p("For each predictive variable, we check whether the distributions 
                      in each category is similar between regions")),
                
                conditionalPanel('input.comparison=="Spatial"',
                                 p("We also check to see whether the relative sampling effort was 
                                   similar between regions.")),
              #  ),
                                 
            #mainPanel(
                tabsetPanel(
                    id = 'comparison',
                    tabPanel("Correlations", #titlePanel("Correlations"),
                             conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                              tags$div("Loading correlations...",id="loadmessage")),
                             fluidRow(plotOutput("VarsR2plot") %>% withSpinner(color="#0dc5c1")),
                             downloadButton("VarsR2plot_down", "Download comparisons")),
                    
                    tabPanel("Distributions", #titlePanel("Numerical"),
                             conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                              tags$div("Loading plots",id="loadmessage")),
                             uiOutput("show_survey_vars"),
                             fluidRow(column(6, plotOutput("compare_vars_barplot")),
                                      column(6, plotOutput("compare_vars_scatterplot"))),
                             fluidRow(column(6,downloadButton("compare_vars_barplot_down", "Download Barplot")),
                                      column(6,downloadButton("compare_vars_scatterplot_down", "Download Scatterplot"))),
                              ),
                    tabPanel("Spatial",
                             fluidRow(h3("Frequency of observations by survey area")),
                             conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                              tags$div("Loading maps...",id="loadmessage")),
                             fluidRow(plotOutput("survey_freq_plot")), 
                             fluidRow(plotOutput("census_freq_plot"))
                             )
                    )
            ), #)),

                
        ## Tab for Setting up Model
        tabPanel('Model Setup', sidebarLayout(
            sidebarPanel(
                titlePanel("Build the Model"),
                conditionalPanel('input.modelbuild=="Parameter Tests"',
                    br(),
                    h3("Aliasing"),
                    p("Aliasing refers to when two variables are perfectly correlated. "),
                    actionButton("checkalias", "Check for Aliasing"),             
                    br(),
                    h3("Collinearity"),
                    p("Collinearity occurs when two variables are closely correlated.  We can
                      check for collinearity by calculating the variance inflation factor (vif)
                      for all variables in a model."),
                    actionButton("checkvif", "Variance Inflation Factors"),
                    br()
                    ),
                conditionalPanel('input.modelbuild=="Build the Model"',
                    h3("Stepwise regression"),
                    p("Stepwise regression selects a best subset of the available variables."),
                    br(),
                    actionButton("runstepwise", "Stepwise variable selection", icon=icon("shoe-prints")),             
                    uiOutput("include_rfx")
                    ),
                h4("Included Variables"),
                p("Variables to include in the final model. Uncheck those that you 
                      would like to exclude"),
                uiOutput("choose_model_params")
                ),
              
            
            mainPanel(
                tabsetPanel(
                    id = 'modelbuild',
                    tabPanel("Parameter Tests", 
                             p("While we may have data available for many parameters, not all should be used 
                               at the same time in the model. In this panel, we test for Aliasing and Multicollinearity
                               to determine which variables should be excluded."),
                             h3("Alias Report"),
                             p("Aliasing exists when two variables are perfectly correlated."),
                             fluidRow(textOutput("Aliasreport")),
                             br(),
                             h3("Variance Inflation Table"),
                             p("Collinearity occurs when multiple variables are highly correlated with one another.  
                               Such correlations can make it difficult to estimate the coefficients for those variables.  Variance
                                inflation factors help estimate the severity of multicollinearity in individual variables. Variables 
                               with a VIF score of greater than 2 should be excluded."),
                             fluidRow(DT::dataTableOutput("VIFtable")),
                             br(),
                             fluidRow(downloadButton("vif_down", "Download VIF table"))
                     ),
                   
                     tabPanel("Build the Model", 
                              h4("Model Formula"),
                              conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                               tags$div("Loading formula",id="loadmessage")),
                              fluidRow(textOutput("print_formula")),
                              br(),
                              h4("Model Summary"),
                              fluidRow(verbatimTextOutput("model_summary") %>% withSpinner(color="#0dc5c1"))
                              )
                )
            ))),
        
        ## Tab for Assessing up Model
        tabPanel('Model Assessment', 
            tabsetPanel(
              id = 'validation',
              
              tabPanel("Assess", titlePanel("Assess model fit"),
                       fluidRow(
                         column(6, h3("Residual Plot")), 
                         column(6, h3("Observed versus Predicted"))),
                       fluidRow(
                         column(6, p("A residual plot shows the difference between observed values and those 
                                    expected by the model, or how much the model result varies from reality.")), 
                         column(6, p("This plot shows the relationship between observed and expected values when 
                                    aggregated at regional level. A closer correlation indicates a better model fit.
                                    However it is also important to run independent assessments through cross-validation."))),
                       fluidRow(column(6,plotOutput("resid_plot")%>% withSpinner(color="#0dc5c1")),
                                column(6,plotOutput("r2plot")%>% withSpinner(color="#0dc5c1"))),
                       fluidRow(
                         column(6, downloadButton("residplot_down", "Download Residuals plot")),
                         column(6, downloadButton("r2plot_down", "Download correlation plot")))
                       ),
              
              tabPanel("Validation", 
                       fluidRow(
                         column(6,h3("Confusion matrix")), 
                         column(6,h3("ROC Curve"))),
                       fluidRow(
                         column(6,p("A confusion matrix is another way to assess the difference between 
                                    observed and expected values. The top-left panel indicates the percentage 
                                    of true positive observations the model predicted correctly, while the bottom-right 
                                    shows true negatives.  The top-right and bottom left indicate false positive and 
                                    false negatives.")), 
                         column(6,p("The Receiver-Operating Curve shows the rate at which the model distinguishes true 
                                    positives relative to the false positive rate. "))),
                       fluidRow(
                         column(6,plotOutput("conf.mat") %>% withSpinner(color="#0dc5c1")),
                         column(6,plotOutput("roc.plot")%>% withSpinner(color="#0dc5c1") )),
                       fluidRow(
                         column(6, downloadButton("conf.mat_down", "Download Confusion Matrix")),
                         column(6, downloadButton("roc.plot_down", "Download ROC curve")))
              ),
              
              tabPanel("Cross Validation", titlePanel("Cross-validation"),
                       p("Cross-validation takes a sample of the data to build the model, 
                         then uses that model to predict into the remainder of the data. The true values are 
                         calculated compared to the predicted values to get an assessment of the model's predictive power. 
                         This process is repeated multiple times to determine how repeatable this predictive power is."),
                       fluidRow(column(6, numericInput("numberfolds", "Number of Folds", 5))),
                       conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                        tags$div("Running Cross Validation...",id="loadmessage")),
                       DT::dataTableOutput("xval_table") %>% withSpinner(color="#0dc5c1")     
              )
            )
          ),
        
      
        ## Tab for prediction
        tabPanel('Prediction', 
                 
                tabsetPanel(
                    id = 'pred',
                    tabPanel("Map output", titlePanel("Spatial predictions"),
                             conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                              tags$div("Loading prediction maps...",id="loadmessage")),
                             fluidRow(
                               column(4, fluidRow(h3("Census-level predictions")),
                                          fluidRow(br()),
                                          fluidRow(downloadButton("predicted_census_down", "Download Census Predictions"))),
                               column(8, plotOutput("predicted_census_map")%>% withSpinner(color="#0dc5c1"))),
                               #column(2, downloadButton("predicted_census_down", "Download Direct Estimates"))),
                             #fluidRow(
                             #   column(4,p("Predictions aggregated at census area level")),
                             #  column(4,p("Predictions aggregated at survey area level")),
                              # column(4,p("Direct estimates from survey data"))),
                             fluidRow(
                               column(4, fluidRow(h3("Survey-level predictions")),
                                        fluidRow(br()),
                                        fluidRow(downloadButton("predicted_survey_down", "Download Survey Predictions"))),
                               column(8, plotOutput("predicted_survey_map")%>% withSpinner(color="#0dc5c1"))),
                               #column(2, downloadButton("predicted_survey_map_down", "Predictions at survey area"))),
                               fluidRow(
                               column(4, fluidRow(h3("Direct Estimates")),
                                         fluidRow(br()),
                                         fluidRow(downloadButton("direct_estimate_map_down", "Estimates"))),
                               column(8, plotOutput("direct_plot")%>% withSpinner(color="#0dc5c1"))),
                               #column(2, downloadButton("direct_estimate_map_down", "Direct Estimates at survey area")))
                    ),
                    tabPanel("Table output", titlePanel("Tabular predictions"),
                             p("Predicted outputs are presented here as tabular outputs, with mean, lower and upper 
                               95% confidence intervals."),
                             br(),
                             h3("Results within survey regions"),
                             fluidRow(DT::dataTableOutput("predicted_survey_table")%>% withSpinner(color="#0dc5c1")),
                             fluidRow(downloadButton("pred_survey_table_down", "Download survey level predictions")),
                             br(),
                             h3("Results within census regions"),
                             fluidRow(DT::dataTableOutput("predicted_census_table")%>% withSpinner(color="#0dc5c1")),
                             downloadButton("pred_census_table_down", "Download census level predictions")
                    )
                )
            )))#))##,
        

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    ## Objects for Data load Tab
    source("R/server/load_server_objects.R", local=TRUE)
    
    # Data Select Tab
    source("R/server/load_server_maps.R", local=TRUE)
    
    # Data Compare Tab
    source("R/server/load_compare_objects.R", local=TRUE)
    
    # Model Build Tab
    source("R/server/load_model_objects.R", local=TRUE)
    
    #  Assessments Tab
    source("R/server/load_assessment_objects.R", local=TRUE)
  
    # Cross Validation Tab
    #source("R/server/load_Xval_objects.R", local=TRUE)

    # Prediction Tab
    source("R/server/load_prediction_objects.R", local=TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)

}
