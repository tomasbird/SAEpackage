### Objects for building and checking models
library(lme4)
library(leaps)
library(ggplot2)
#setwd("~/Google Drive/Business/Customers/UNFPA/SAE contract/Working_SAE_App/R/data")

#### Check Aliasing
# Button to run AliasTest
Aliastest <- eventReactive(input$checkalias, {
  alias(formula(paste(input$indicator, "~" , 
                      paste0(input$model_params, collapse=" + "))), 
        data=surveyDF())
})

# Alias report output
output$Aliasreport <- renderPrint({
  #req(Aliastest())
  aliased=Aliastest()
  al_all=data.frame(aliased$Complete)
  al=names(al_all)[which(al_all<0)]
  if(length(al)>0) {print(paste("The variable(s) [", paste(al, collapse=", "), "] are perfectly correlated with other variables and should be removed."))
    } else {print("No aliasing detected")}
  })


### Check Variance Inflation Factors
## VIF test on all variables
VIFtest <- eventReactive(input$checkvif, {
  fullform=formula(paste(input$indicator, "~" , 
                         paste0(input$model_params, collapse=" + ")))
 mod=glm(fullform, data=surveyDF(), family="binomial")
 data.frame(vif(mod))
})

## download VIF test
output$vif_down<-downloadHandler(
  filename = function() {
    paste0("Variance inflation test", ".csv")
  },
  content = function(file) {
    ggsave(file, VIFtest())
  })


# VIF tabular output
output$VIFtable=DT::renderDataTable({
  #req(VIFtest())
  DT::datatable(VIFtest())  %>%
    formatSignif(columns= which(sapply(VIFtest(), class) %in% c("numeric")), digits=2)
})



### stepwise selection
# Stepwise AIC

# run multi-model inference
swfun=function(mod, scope=list(~.), dir="both") {
  stepAIC(object=mod, scope=scope, direction=dir)
}


r=reactiveValues()
r$runstepwise=0
#r$resetvals=0
r$stepwise=NULL
#r$vars=NULL

observeEvent(input$runstepwise, {
  r$runstepwise=1
 # r$resetvals=0
  r$stepwise=swfun(mod=mod(), scope=list(~.), dir="both")
})

## switch for resetting variables
vars=reactive({
  if(r$runstepwise==0)
    input$Predictors
  else if(r$runstepwise==1)
    labels(r$stepwise$terms)
})


#### Build model
# Model Terms
output$choose_model_params <- renderUI({
  checkboxGroupInput("model_params", "Choose parameters to include",
                     input$Predictors, vars())
})


# UI for Random Effects
output$include_rfx=renderUI({
  checkboxInput("rfx_yes_no", "Include a Regional random effect", value=FALSE)
})


## build formula
form_rfx<-reactive({
  #if(input$rfx_yes_no==T & input$modelbuild=="Random Effects")
    paste(input$indicator, "~" ,paste0(input$model_params, collapse=" + "), "+ (1|", input$survey_spatial, ")") 
})


form=reactive({ 
    paste(input$indicator, "~" , paste0(input$model_params, collapse=" + "), "+", input$survey_spatial)
  })

## print formula as text
output$print_formula <- renderText({
  if(input$rfx_yes_no==T)
    form_rfx()
  else
    form()
  })


## create reactive model object
mod=reactive({ 
    if(input$rfx_yes_no==T)
      glmer(form_rfx(), data=surveyDF(), family="binomial")
    else
      glm(form(), data=surveyDF(), family="binomial")
})

## Model Summary
output$model_summary <- renderPrint({
    summary(mod())
})

# Model Summary Download
#output$model_summary_down<-downloadHandler(
#  filename = function() {
#    paste0("model_summary_table", ".csv")
#  },
#  content = function(file) {
#    write.csv(file, plot(mod()))
#  })


