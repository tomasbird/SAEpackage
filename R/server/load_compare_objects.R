

## objects used for merging shape and df data sources
mergeshpfn=function(shp,df, select.vars, select.spatial){
  shp=shp %>% st_as_sf
  
  freqdat=subset(df, 
                    select=c(select.vars, select.spatial)) %>%
    as.data.frame() %>%
    group_by_at(select.spatial) %>%
    summarize(n=n()) %>%
    mutate(freq=n/sum(n)) 
  
  srvshpmerge=merge(shp, freqdat, by=select.spatial)
}

srvshpmerge=reactive({
  mergeshpfn(shp=surveyShp(), df=surveyDF(), select.vars=input$Predictors, select.spatial=input$survey_spatial)
  })

rangefun=function(cen, surv){
  cenrng=range(cen)
  survrng=range(surv)
  minlims=min(c(cenrng, survrng))
  maxlims=max(c(cenrng, survrng))
  ranges=c(minlims,maxlims)
  return(ranges)
}

#####Spatial tab
# plot frequency of observations at survey scale 
output$survey_freq_plot <- renderPlot({
  ranges=rangefun(censhpmerge()$freq, srvshpmerge()$freq)
  ggplot(data=srvshpmerge(), aes(fill=freq)) + 
    geom_sf() +
    theme_void() +
    scale_fill_gradientn(colors=c("black", "yellow", "blue"), limits=ranges) +
    labs(title="Survey Data")
})

# merge census summaries with spatial data
censhpmerge=reactive({
  mergeshpfn(shp=surveyShp(), df=censusDF(), 
             select.vars=input$show_survey_vars, select.spatial=input$survey_spatial)
})

## plot census frequencies on same scale as surveys
output$census_freq_plot <- renderPlot({
  cenrng=range(censhpmerge()$freq)
  survrng=range(srvshpmerge()$freq)
  minlims=min(c(cenrng, survrng))
  maxlims=max(c(cenrng, survrng))
  ggplot(censhpmerge(), aes(fill=freq)) +
    geom_sf() +
    theme_void() +
    scale_fill_gradientn(colors=c("black", "yellow", "blue")) + #, limits=c(minlims, maxlims)) +
    labs(title="Census Data")
})


### Reactive UI to choose what survey variable to compare
output$show_survey_vars <- renderUI({
  selectInput("show_survey_vars", "Choose variable to compare", input$Predictors)
})

#output$select_correlation_vars <- renderUI({
#  selectInput("show_correlation_vars", "Choose variables to keep", input$Predictors)
#})

surveyvars=reactive({
  names(surveyDF())
})

## Reactive function aggregating survey data
Agg_fn=function(df, datatype){

    df %>%
    group_by_all(.groups=c(input$show_survey_vars, input$survey_spatial)) %>%
    summarise(n=n()) %>%
    group_by(input$survey_spatial) %>%
    mutate(nreg=sum(n),
           freqreg=n/nreg,
           data=datatype) %>%
    ungroup()
}

# reactive for survey and census aggregated data
Surv_Agg=reactive( {
  Agg_fn(df=subset(surveyDF(), select=c(input$show_survey_vars, input$survey_spatial)),datatype="Survey")
  })

Cen_Agg=reactive( {
  Agg_fn(df=subset(censusDF(), select=c(input$show_survey_vars, input$survey_spatial)), datatype="Census")
  })


### bar plot to compare census and survey distributions

# Correlations Tab
## Tab that shows all variables correlated against each other
### stuff for correlations scatterplot display


compare_vars_scatterplot_fn <- function(cen, surv){
  
  SURV= surv %>% 
    mutate(freqdatsurv=freqreg) %>%
    select_all(.vars=c(input$select_survey_spatial, input$show_survey_vars, "freqdatsurv"))
  
  CEN= cen %>%
    mutate(freqdatcen=freqreg) %>%
    select_all(.vars=c(input$survey_spatial, input$show_survey_vars, "freqdatcen"))
  
    ## make plot
    merge(SURV,CEN, by=c(input$survey_spatial, input$show_survey_vars)) %>%
      ggplot(aes(x=freqdatsurv, y=freqdatcen)) + 
      geom_point(position="dodge") + 
      facet_wrap(as.formula(paste("~", input$show_survey_vars )), ncol=2) +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
}

# render scatterplot
output$compare_vars_scatterplot=renderPlot({
  
  req(Surv_Agg(), Cen_Agg())
  compare_vars_scatterplot_fn(cen=Cen_Agg(), surv=Surv_Agg())
})

# download scatterplot
output$compare_vars_scatterplot_down<-downloadHandler(
  filename = function() {
    paste0("scatterplot", ".jpg")
  },
  content = function(file) {
    ggsave(file, compare_vars_scatterplot_fn(surv=Surv_Agg(), cen=Cen_Agg()))
})


## Barplot for comparing variables
# barplot function
compare_vars_barplot_fn<-function(SURV, CEN){
  ## make plot
  rbind(SURV,CEN) %>%
    ggplot(aes_string(x=input$survey_spatial, y="freqreg",  fill="data")) + 
    geom_col(position="dodge") + 
    facet_wrap(as.formula(paste("~", input$show_survey_vars )), ncol=2) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
  #rm(SURV, CEN)
}


# render barplot
output$compare_vars_barplot <- renderPlot({
  req(Surv_Agg(), Cen_Agg())
  compare_vars_barplot_fn(SURV=Surv_Agg(), CEN=Cen_Agg())
})


# download barplot
output$compare_vars_barplot_down <-downloadHandler(
  
   filename = function() {
    paste0("barplot", ".jpg")
  },
  content = function(file) {
    ggsave(file, compare_vars_barplot_fn(SURV=Surv_Agg(), CEN=Cen_Agg()))
  })


### Multi-panel correlation plot between census and survey data for all variables
### 
cen_agg_long <- reactive({
  subset(censusDF(), 
              select=c(input$Predictors,  input$survey_spatial)) %>%
              pivot_longer(cols=input$Predictors, 
                            names_to="Variable", values_to="outcome") %>%
                  group_by_all(.groups=c(input$survey_spatial, "Variable", "outcome")) %>%
                  summarize(n.cen=n())
  })


surv_agg_long <- reactive({
  shiny::validate(
    need(sum(1-(input$Predictors %in% names(surveyDF())))==0 , "Predictor missing in survey data.  
         Check whether unnecessary census spatial variables have been excluded.")
  )
  subset(surveyDF(), 
                select=c(input$Predictors,  input$survey_spatial)) %>%
    pivot_longer(cols=input$Predictors, 
                 names_to="Variable", values_to="outcome") %>%
    group_by_all(.groups=c(input$survey_spatial, "Variable", "outcome")) %>%
    summarize(n.surv=n())
})



### Things for the R2 plots

# R2 function 
VarsR2Fun=function(cen,surv){
  shiny::validate(
    need(sum(1-(input$Predictors %in% names(surveyDF())))==0 , "Predictor missing in survey data.  
         Check whether unnecessary census spatial variables have been excluded.")
  )
  left_join(cen,surv) %>%
    ggplot(aes(x=n.surv, y=n.cen, colour=outcome)) + 
    geom_point() +
    facet_wrap(~Variable)
}


# render R2 plot
output$VarsR2plot <-renderPlot({
  shiny::validate(
    need(sum(1-(input$Predictors %in% names(surveyDF())))==0 , "Predictor missing in survey data.  
         Check whether unnecessary census spatial variables have been excluded.")
  )
  req(Surv_Agg(), Cen_Agg())
  VarsR2Fun(cen=cen_agg_long(), surv=surv_agg_long())
    })


# Download R2 plot
output$VarsR2plot_down<-downloadHandler(
  filename = function() {
    paste0("R2_plot", ".jpg")
  },
  content = function(file) {
    ggsave(file,   VarsR2Fun(cen=cen_agg_long(), surv=surv_agg_long()))
  })
  
  
