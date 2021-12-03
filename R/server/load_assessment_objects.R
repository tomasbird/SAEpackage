#### Assessment of model


## residual plot
output$resid_plot <- renderPlot({
  plot(mod())
})

# download residual plot
output$resid_plot_down<-downloadHandler(
  filename = function() {
    paste0("residual_plot", ".jpg")
  },
  content = function(file) {
    ggsave(file, plot(mod()))
  })

## model prediction
predicted <- reactive({
  req(mod())
  predict(mod(), type="response")
})

# predicted data
pred_df <- reactive({
  req(predicted())
  surveyDF() %>%
    mutate(predicted=predicted(), #predict(mod(), type="response"),
           observed=surveyDF()[,input$indicator])
})

# R2 plot
r2data <- reactive({
    req(pred_df())
    pred_df() %>%
    group_by_at(input$survey_spatial) %>%
    summarize(predicted=mean(predicted, na.rm=T),
              observed=mean(observed, na.rm=T)) 
})

r2plotfun=function(data) {
ggplot(data, aes(x=observed, 
           y=predicted)) + 
  geom_point() + 
  geom_abline()
}

output$r2plot=renderPlot({
  req(r2data())
  r2plotfun(r2data())
})

# download R2plot
output$r2plot_down<-downloadHandler(
  filename = function() {
    paste0("r2plot", ".jpg")
  },
  content = function(file) {
    ggsave(file, r2plotfun(r2data()))
  })


### Confusion matrix
conf.mat_fun=function(data, model){
  evaluate(pred_df(), target_col="observed", prediction_cols="predicted", type="binomial" )$`Confusion Matrix`[[1]]
}

# make the plot
output$conf.mat=renderPlot({
  req(pred_df())
  plot_confusion_matrix(conf.mat_fun())
})

# confusion matrix downloader
output$conf.mat_down<- downloadHandler(
  filename = function() {
    paste("Confusion_Matrix", ".jpg", sep = "")
  },
  content = function(file) {
    plot_confusion_matrix(conf.mat_fun())  }
)


### ROC curve          
surveyroc=reactive({
  roc(data=pred_df(), response=observed, predictor=predicted)
  })

# plot roc
output$roc.plot=renderPlot({ plot.roc(surveyroc()) })

# download ROC curve
output$roc.plot_down<- downloadHandler(
  filename = function() {
    paste("ROC_Curve", ".jpg", sep = "")
  },
  content = function(file) {
    plot.roc(surveyroc())  }
)


### Cross Validation
### cross validate
xval=reactive({
folded_surv <-  fold(pred_df(), k = input$numberfolds)      

xval=cross_validate(folded_surv, formulas=form(), family="binomial")

preds=(xval$Predictions[[1]]) %>%
  arrange(Observation)

preds[[input$survey_spatial]] =folded_DHS[[input$survey_spatial]] 

preds %>% group_by_at(.vars=c("Fold", input$survey_spatial)) %>%
  summarise(nTarget=sum(Target),
            nPred=sum(Prediction)) %>%
  group_by(Fold) %>%
  summarise(R2=cor(nTarget,nPred)^2,
            RMSE=Metrics::rmse(nTarget,nPred))
})


output$xval_table=DT::renderDataTable({
  req(xval())
  DT::datatable(xval(), rownames=FALSE) %>%
    formatSignif(columns= which(sapply(xval(), class) %in% c("numeric")), digits=2)
})
