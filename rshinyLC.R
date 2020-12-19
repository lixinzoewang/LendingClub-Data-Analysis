
if (!require(shiny)){install.packages(shiny); require(shiny)}
if(!require(ggplot2)){install.packages("ggplot2")}

load('LCdata.RData')

ui= fluidPage(titlePanel("LendingClub Statistical Model"),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "model", label="Select a Model", choices = c("Elastic Net", "Kappa","Boost","ALL")),
                  sliderInput(inputId = "threshold1", label = "threshold", min=0, max=1, value=0.2)
                ),
                mainPanel(plotOutput(outputId = "AucPlot"))
              )
              )

server = function(input, output){
    output$AucPlot = renderPlot(
      if (input$model =="Elastic Net"){
        plot(rocOut.glmnet,print.thres = input$threshold1, legacy.axes = TRUE, 
             main = "Elastic Net ROC Curve")
        legend("topleft",legend=paste("AUC = ",round(rocOut.glmnet$auc,3)))
        
      }
      else if(input$model =="Kappa"){
        plot(rocOutKappa, print.thres = input$threshold1, legacy.axes = TRUE, 
             main = "Boosting Using Kappa ROC Curve")
        legend("topleft",legend=paste("AUC = ",round(rocOutKappa$auc,3)))
      }
      else if(input$model =="Boost"){
        plot(rocOutBoost,print.thres = input$threshold1, legacy.axes = TRUE,
             main = "Boosting ROC Curve")
        legend("topleft",legend=paste("AUC = ",round(rocOutBoost$auc,3)))
      }
      else if (input$model =="ALL"){
        plot(rocOut.glmnet,print.thres = input$threshold1, legacy.axes = TRUE, 
             main = "Elastic Net ROC Curve", col="black")
        lines(rocOutKappa, print.thres = input$threshold1, legacy.axes = TRUE, 
              main = "Boosting Using Kappa ROC Curve", col="red")
        #points(print.thres = input$threshold1)
        lines(rocOutBoost,print.thres = input$threshold1, legacy.axes = TRUE,
             main = "Boosting ROC Curve", col="blue")
        legend("topleft", col=c("black", "red", "blue"), lty=1,legend=c("Elastic Net", "boosting with kappa", "boosting"))
      }
      
    )
}
  
shinyApp(ui = ui, server = server)
  
