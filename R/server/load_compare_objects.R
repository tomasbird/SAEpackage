

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
             select.vars=input$Predictors, select.spatial=input$survey_spatial)
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

output$select_correlation_vars <- renderUI({
  selectInput("show_correlation_vars", "Choose variables to keep", input$Predictors)
})


## Reactive function aggregating survey data
Agg_fn=function(df, datatype){
subset(df, select=c(input$show_survey_vars, input$survey_spatial)) %>%
    group_by_all(.groups=c(input$show_survey_vars, input$survey_spatial)) %>%
    summarise(n=n()) %>%
    group_by(input$survey_spatial) %>%
    mutate(nreg=sum(n),
           freqreg=n/nreg,
           data=datatype) %>%
    ungroup()
}

# reactive for survey and census
Surv_Agg=reactive( {
  Agg_fn(df=surveyDF(),datatype="Survey")
  })

Cen_Agg=reactive( {
  Agg_fn(df=censusDF(), datatype="Census")
  })


## bar plot to compare census and survey distributions
output$compare_vars_scatterplot <- renderPlot({
  SURV= Surv_Agg() %>% 
    mutate(freqdatsurv=freqreg) %>%
    select_all(.vars=c(input$select_suvey_spatial, input$show_survey_vars, "freqdatsurv"))
  
  CEN= Cen_Agg() %>%
    mutate(freqdatcen=freqreg) %>%
    select_all(.vars=c(input$survey_spatial, input$show_survey_vars, "freqdatcen"))
  
    ## make plot
    merge(SURV,CEN, by=c(input$survey_spatial, input$show_survey_vars)) %>%
      ggplot(aes(x=freqdatsurv, y=freqdatcen)) + 
      geom_point(position="dodge") + 
      facet_wrap(as.formula(paste("~", input$show_survey_vars )), ncol=2) +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
})



## R2 table for comparing variables
output$compare_vars_barplot <- renderPlot({
  SURV=Surv_Agg()
  
  CEN=Cen_Agg()
  
  ## make plot
  rbind(SURV,CEN) %>%
    ggplot(aes_string(x=input$survey_spatial, y="freqreg",  fill="data")) + 
    geom_col(position="dodge") + 
    facet_wrap(as.formula(paste("~", input$show_survey_vars )), ncol=2) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
   #rm(SURV, CEN)
})


### correlation between census and survey data
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
  subset(surveyDF(), 
                select=c(input$Predictors,  input$survey_spatial)) %>%
    pivot_longer(cols=input$Predictors, 
                 names_to="Variable", values_to="outcome") %>%
    group_by_all(.groups=c(input$survey_spatial, "Variable", "outcome")) %>%
    summarize(n.surv=n())
})


output$VarsR2plot <-renderPlot({
  left_join(cen_agg_long(),surv_agg_long()) %>%
      ggplot(aes(x=n.surv, y=n.cen, colour=outcome)) + 
      geom_point() +
      facet_wrap(~Variable)
    })
