### Prediction objects

## Get prediction and interval
predicted_census=reactive({
  if(input$rfx_yes_no==F){
    predicted=predict(mod(), type="response", se.fit=T, newdata=censusDF()) %>% 
      data.frame() %>%
      mutate(lwr=fit-se.fit, upr=fit+se.fit) %>%
      subset(select=c(fit, lwr, upr)) 
    predicted[[input$survey_spatial]]=censusDF()[[input$survey_spatial]]
    predicted[[input$census_spatial]]=censusDF()[[input$census_spatial]]
    predicted
    
  } else {
  predicted=predictInterval(mod(), newdata=censusDF(),which="full", type="probability", n.sims=100) 
  predicted[[input$survey_spatial]]=censusDF()[[input$survey_spatial]]
  predicted[[input$census_spatial]]=censusDF()[[input$census_spatial]]
  predicted
  }
})

## Prediction aggregation function
prediction_agg_fn=function(data, agglevel){
    data %>% 
    group_by_at(.vars=c(agglevel)) %>%
    summarize(mn=mean(fit),
              lwr=mean(lwr),
              upr=mean(upr))
}


# predicted to survey area
census_predicted_at_survey=reactive({
   prediction_agg_fn(predicted_census(),input$survey_spatial )
})


## Prediction aggregated to census area
census_predicted_at_census=reactive({
  prediction_agg_fn(predicted_census(),input$census_spatial )
})


## Direct estimates 
Direct_Estimate=reactive({
  surveyDF() %>%
    group_by_at(.vars=c(input$survey_spatial)) %>%
    summarise(n=n(),
              n.ind=sum(.data[[input$indicator]]),
              mn=n.ind/n) %>%
    ungroup()
})


# function for plotting survey results

predicted_map_fn=function(shp, data, spatscale, title){
    shp %>% 
    st_as_sf()  %>%
    left_join(data, by=spatscale) %>%
    ggplot(aes(fill=mn)) +
    geom_sf() +
    theme_void() +
    scale_fill_gradientn(colors=c("black", "yellow", "blue"))+
    labs(title=title)
    
}

## plot of predictions from census at survey scale
output$predicted_survey_map=renderPlot({
  predicted_map_fn(surveyShp(), census_predicted_at_survey(), input$survey_spatial, "Predicted at survey scale")
})


# Download mapped prediction at survey area
output$predicted_survey_map_down<-downloadHandler(
  filename = function() {
    paste0("Predictions_at_survey_scale", ".jpg")
  },
  content = function(file) {
    ggsave(file, predicted_map_fn(surveyShp(), census_predicted_at_survey(), input$survey_spatial, "Predicted at survey scale"))
  })


## plot of predictions from census at survey scale
output$predicted_census_map=renderPlot({
  predicted_map_fn(censusShp(), census_predicted_at_census(), input$census_spatial, "Predicted at census scale")
})

# Download mapped prediction at census area
output$predicted_census_map_down<-downloadHandler(
  filename = function() {
    paste0("Predictions_at_census_scale", ".jpg")
  },
  content = function(file) {
    ggsave(file, predicted_map_fn(censusShp(), census_predicted_at_census(), input$census_spatial, "Predicted at census scale"))
  })

## Direct estimates at survey scale
output$direct_plot=renderPlot({
  predicted_map_fn(surveyShp(), Direct_Estimate(), input$survey_spatial, "Direct Estimates")
})

# Download mapped direct estimates at survey area
output$direct_plot_down<-downloadHandler(
  filename = function() {
    paste0("Direct_Estimates", ".jpg")
  },
  content = function(file) {
    ggsave(file, predicted_map_fn(surveyShp(), Direct_Estimate(), input$survey_spatial, "Direct Estimates"))
  })



# function for predictions
pred_output_table_fn=function(dat, digits=2){
  DT::datatable(dat) %>%
    formatSignif(columns= which(sapply(census_predicted_at_survey(), class) %in% c("integer", "numeric")), digits=digits)  
}


## display table of predicted at survey
output$predicted_survey_table=DT::renderDataTable({
  pred_output_table_fn(dat=census_predicted_at_survey(), digits=2)
})


## display table of predicted at census
output$predicted_census_table=DT::renderDataTable({
  pred_output_table_fn(dat=census_predicted_at_census(), digits=2)
})


# download table predicted at surveylevel
output$pred_survey_table_down<-downloadHandler(
  filename = function() {
    paste0("Tabular_predictions_at_survey_scale", ".csv")
  },
  content = function(file) {
    write.csv(file,  pred_output_table_fn(dat=census_predicted_at_survey(), digits=4))
  })

# download table predicted at census level
output$pred_census_table_down<-downloadHandler(
  filename = function() {
    paste0("Tabular_predictions_at_census_scale", ".csv")
  },
  content = function(file) {
    write.csv(file,  pred_output_table_fn(dat=census_predicted_at_census(), digits=4))
  })
