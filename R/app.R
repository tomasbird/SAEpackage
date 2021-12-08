library(shiny)


SAEapp=function(...){
#library(leaflet)
#source("global.R", local=TRUE)
#source("R/UI_functions/loaddata_fn.R",local=TRUE)

ui <- fluidPage(
        
    # Define UI for application that draws a histogram
    navbarPage(
        title = 'SAE pages',
        
        tabPanel("Introduction",
                 titlePanel("Introduction to the UNFPA Small Area Estimation Training Tool"),
                 fluidRow(column(10, 
                                 br(),
                                 p("This application provides an introduction to the steps involved in 
                                   conducting small area estimation, based on the methods described in [SAE paper].
                                   It is intended to provide a teaching example based on binary data, but may also be
                                   used on properly formatted data.", 
                                   style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                 br(),
                                 p("The analysis proceeds in several sections, starting with choosing the data files and 
                                   selecting the relevant indicators and predictor variables. When the data are chosen, variables
                                   to be used in models are compared and assessed for their suitability in modeling. When the data are 
                                   ready and the model run, the user has the option to include a spatial random effect.
                                   Following model creation, the user may assess the model fit in a number of ways, followed by prediction
                                   onto census data.", 
                                   style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                 br())),
                 fluidRow( column(2, titlePanel("Select Files"), 
                                     br(),
                                     p("This panel allows the user to choose what spatial and numerical data files to use.
                                       If desired, the user can also use demonstration data.", 
                                       style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                         # column(2, titlePanel("Choose Data"), 
                         #        br(),
                         #        p("Here the user selects which columns are to be modeled as the indicator, and which
                         #          are variables to be used as predictors.",
                         #          style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                          column(2, titlePanel("Compare Data"), 
                                 br(), 
                                 p("The census and survey data must be compared in terms of their spatial and numerical
                                   distributions, to ensure that they are suitable for prediction.",
                                   style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                          column(2, titlePanel("Model Setup"), 
                                 br(),
                                 p("The model setup step allows the user to test different model scenarios, before addding a
                                   spatial random effect.",
                                   style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                          column(2, titlePanel("Power Analysis"),
                                 br(),
                                 p("In this step, the user can assess the fit of the chosen model through a number of tests.",
                                   style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                          column(2, titlePanel("Cross Validation"), 
                                 br(),
                                 p("Cross validation allows the user to check the predictive power of the 
                                   model through repeated subsampling", style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                          column(2, titlePanel("Prediction"),
                                 br(),
                                 p("Finally, the user is able to use the derived model to predict onto the census data.", 
                                   style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px")
                                 ))
                                
                 ),
        
        
        ## Data load
        # App title ----
        tabPanel("Select Data",
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
            # Sidebar panel for inputs ----
            sidebarPanel(
                
                titlePanel("Load data source"),
                
                # Input: Choose a data source
                checkboxInput("usedemo", label = "Check this box to use demo data", value = TRUE),
           
           #Choose survey data files [from loaddata_fn.R]
           surveyloadPanel,
           
           #Choose census data files [from loaddata_fn.R]
           censusloadPanel,
           
           # Choose indicator and spatial data
           conditionalPanel(
                           'input.datatoload === "Survey"',
                           uiOutput("choose_survey_indicator"),
                           uiOutput("choose_survey_spatial")
                           ),
               
                       conditionalPanel(
                           'input.datatoload === "Census"',
                          uiOutput("choose_census_spatial"),
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
                         fluidRow(DT::dataTableOutput("survey_preview"))
                         ),
                
                tabPanel("Census", 
                         conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                          tags$div("Loading census data...",id="loadmessage")
                         ),
                         fluidRow(plotOutput("censusMap")%>% withSpinner(color="#0dc5c1")),
                         fluidRow(DT::dataTableOutput("census_preview")%>% withSpinner(color="#0dc5c1")))
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
                      in each category is similar between regions"),
                    uiOutput("show_survey_vars")),
                
                conditionalPanel('input.comparison=="Spatial"',
                                 p("We also check to see whether the relative sampling effort was 
                                   similar between regions.")),
              #  ),
                                 
            #mainPanel(
                tabsetPanel(
                    id = 'comparison',
                    tabPanel("Correlations", titlePanel("Correlations"),
                             conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                              tags$div("Loading correlations...",id="loadmessage")),
                             fluidRow(plotOutput("VarsR2plot") %>% withSpinner(color="#0dc5c1"))),
                    
                    tabPanel("Distributions", titlePanel("Numerical"),
                             fluidRow(column(6, plotOutput("compare_vars_barplot")),
                                      column(6, plotOutput("compare_vars_scatterplot"))),
                             fluidRow(column(6,downloadButton("vars_barplot_down", "Download Barplot")),
                                      column(6,downloadButton("vars_scatter_down", "Download Scatterplot"))),
                              ),
                    tabPanel("Spatial",
                             fluidRow(titlePanel("Frequency of observations by survey area")),
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
                p(),
                conditionalPanel('input.modelbuild=="Parameter Tests"',
                    br(),
                    titlePanel("Aliasing"),
                    p("Aliasing refers to when two variables are perfectly correlated. "),
                    actionButton("checkalias", "Check for Aliasing"),             
                    br(),
                    titlePanel("Collinearity"),
                    p("Collinearity occurs when two variables are closely correlated.  We can
                      check for collinearity by calculating the variance inflation factor (vif)
                      for all variables in a model."),
                    actionButton("checkvif", "Variance Inflation Factors"),
                    br()
                    ),
                conditionalPanel('input.modelbuild=="Build the Model"',
                    titlePanel("Stepwise regression"),
                    p("We can use stepwise regression to select a best subset of the available variables"),
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
                             titlePanel("Alias Report"),
                             p("Aliasing exists when two variables are perfectly correlated."),
                             fluidRow(textOutput("Aliasreport")),
                             p(),
                             titlePanel("Variance Inflation Table"),
                             p("Collinearity occurs when multiple variables are highly correlated with one another.  
                               Such correlations can make it difficult to estimate the coefficients for those variables.  Variance
                                inflation factors help estimate the severity of multicollinearity in individual variables. Variables 
                               with a VIF score of greater than 2 should be excluded."),
                             fluidRow(DT::dataTableOutput("VIFtable")),
                             fluidRow(downloadButton("vif_down", "Download VIF table"))
                     ),
                   
                     tabPanel("Build the Model", 
                              p("Build the model"),
                              fluidRow(textOutput("print_formula")),
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
                         column(6, titlePanel("Residual Plot")), 
                         column(6, titlePanel("Observed versus Predicted"))),
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
                         column(6,titlePanel("Confusion matrix")), 
                         column(6,titlePanel("ROC Curve"))),
                       fluidRow(
                         column(6,p("A confusion matrix is another way to assess the difference between 
                                    observed and expected values. The top-left panel indicates the percentage 
                                    of true positive observations the model predicted correctly, while the bottom-right 
                                    shows true negatives.  The top-left and bottom right indicate false positive and 
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
                       fluidRow(column(6, numericInput("numberfolds", "Number of Folds", 10))),
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
                               column(4,titlePanel("Census-level predictions")),
                               column(4,titlePanel("Survey-level predictions")),
                               column(4,titlePanel("Direct Estimates"))),
                             fluidRow(
                               column(4,p("Predictions aggregated at census area level")),
                               column(4,p("Predictions aggregated at survey area level")),
                               column(4,p("Direct estimates from survey data"))),
                             fluidRow(
                               column(4,plotOutput("predicted_census_map")%>% withSpinner(color="#0dc5c1")),
                               column(4,plotOutput("predicted_survey_map")%>% withSpinner(color="#0dc5c1")),
                               column(4,plotOutput("direct_plot") %>% withSpinner(color="#0dc5c1"))),
                             fluidRow(
                               column(4, downloadButton("predicted_census_down", "Predictions at census area")),
                               column(4, downloadButton("predicted_survey_map_down", "Predictions at survey area")),
                               column(4, downloadButton("direct_plot_down", "Download Direct Estimates")))
                    ),
                    tabPanel("Table output", titlePanel("Tabular predictions"),
                             fluidRow(DT::dataTableOutput("predicted_survey_table")%>% withSpinner(color="#0dc5c1")),
                             fluidRow(DT::dataTableOutput("predicted_census_table")%>% withSpinner(color="#0dc5c1"))
                    )
                )
            )))#))##,
        
        ## Tab for Map
  #      tabPanel("Map View",  div(class="outer",
                    #              tags$head(
                   #                   # Include our custom CSS
                  #                    includeCSS("styles.css"),
                 #                     includeScript("gomap.js")),
                                  
                #                  # If not using custom CSS, set height of leafletOutput to a number instead of percent
               #                   leafletOutput("map", width="100%", height="100%"),
              #                    
             #                     # Shiny versions prior to 0.11 should use class = "modal" instead.
            #                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
           #                                     draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
          #                                      width = 330, height = "auto",
         #                                       h2("Sample mapped output")
        #                          )
       # ), tags$div(id="cite",
      #              'Data compiled for ', tags$em('SAE analysis for Nepal'))
     #   )
    #)
   # )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    ## Objects for Data load Tab
    #source("R/server/load_server_objects.R", local=TRUE)
    
    # Data Select Tab
    #source("R/server/load_server_maps.R", local=TRUE)
    
    # Data Compare Tab
    #source("R/server/load_compare_objects.R", local=TRUE)
    
    # Model Build Tab
    #source("R/server/load_model_objects.R", local=TRUE)
    
    # Cross Assessments Tab
    #source("R/server/load_assessment_objects.R", local=TRUE)
  
    # Cross Validation Tab
    #source("R/server/load_Xval_objects.R", local=TRUE)

    # Prediction Tab
    #source("R/server/load_prediction_objects.R", local=TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)

}
