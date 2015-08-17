suppressWarnings(suppressMessages(library(shiny)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(caret)))
suppressWarnings(suppressMessages(library(randomForest)))

library(shiny)
library(datasets)
library(ggplot2)
library(caret)
library(randomForest)

data(diamonds)

set.seed(1578)
inTrain <- createDataPartition(y=diamonds$price,
                               p=0.4, list=FALSE)

diamonds2 <- diamonds[inTrain,]

set.seed(45678)
inTrain <- createDataPartition(y=diamonds2$price,
                               p=0.75, list=FALSE)

training <- diamonds2[inTrain,]
testing  <- diamonds2[-inTrain,]

# modelFit <- train(price ~ ., method="rf", data=training,proximity=TRUE,importance=TRUE)
# save(modelFit, file="./diamondTrain.rf.rda")                              importance=TRUE)

# load(file="./diamondsFit.rf.rda")

# Define server logic required to plot various variables against diamond price
shinyServer(function(input, output) {
    
        # Generate plots to examine variability of price vs independent variables
        output$diamondPricePlot <- renderPlot({
            diamondXY <- data.frame(price=diamonds$price, xvar=diamonds[[input$variable]], cut=diamonds$cut, color=diamonds$color, clarity=diamonds$clarity)
            mytitle <- paste("price ~", input$variable)
            varname <- names(diamondXY)[2]
            
            # Generate box plots of outcome (price) vs the requested covariate
            # when the covariate is a factored variable
            
            if(input$variable %in% c("cut","color","clarity")) {
                if(input$variable == "cut") {
                    p <- ggplot(diamondXY, aes(xvar, price, color=cut)) + geom_boxplot() +
                            scale_color_discrete(name="cut")
                }
                else {
                    if(input$variable == "color") {
                        p <- ggplot(diamondXY, aes(xvar, price, color=color)) + geom_boxplot() +
                                scale_color_discrete(name="color")           
                    }
                    else {
                        p <- ggplot(diamondXY, aes(xvar, price, color=clarity)) + geom_boxplot() +
                                 scale_color_discrete(name="clarity")                     
                    }
                }
            }
            else {
                
                # Generate scatter plots of outcome (price) vs the requested covariate
                # when the covariate is a non-factored variable
                
                p <- ggplot(diamondXY, aes(xvar, price, color=cut)) + 
                                geom_point() + 
                                scale_color_discrete(name="cut")
            }
                
            # Add x-axis label and title for all the plots from above
            p <- p +  xlab(input$variable) + ggtitle(mytitle) + 
                    theme(plot.title = element_text(lineheight=.8, face="bold"))
            
            print(p)
        })
        
        # Each diamond cut has a characteristic price distribution. The
        # following call renders price density function for each cut separately
        
        output$priceDensityPlot <- renderPlot({
            p <- ggplot(diamonds,aes(price, fill=cut)) + 
                    geom_density(alpha=0.2) + 
                    ggtitle("Price Density Plots for Each Diamond Cut") +
                    theme(plot.title = element_text(lineheight=.8, face="bold"))
            
            print(p)
        })
        
        # Generate summary of the diamond data
        output$summary <- renderDataTable({
            
            summary(diamonds)
        })
        
        output$rfPredictionAccuracy <- renderDataTable({
            
            load(file="./diamondTrain.rf.rda")
            
            modelFitp <<- modelFit
            modelPred <<- predict(modelFit, testing)
            
            modelFit$results
        })
        
        output$testPredictionMSE <- renderPlot({
            
            load(file="./diamondTrain.rf.rda")
            
            modelPred <<- predict(modelFit, testing)
            predComparison <- rbind(data.frame(id=seq(1,length(modelPred)),price=modelPred,Type=rep("Prediction",length(modelPred))),
                                    data.frame(id=seq(1,length(modelPred)),price=testing$price,Type=rep("ActualValue",length(modelPred))))
            
            p <- ggplot(predComparison,aes(x=id, y=price, color = Type)) + geom_point() +
                        scale_color_discrete(name="") + theme(legend.position="right") + 
                        ggtitle("Diamond Price Prediction") +
                        theme(plot.title = element_text(lineheight=.8, face="bold"))
            
            print(p)
            
        })
        
        output$variableImportancePlot <- renderPlot({
            
            load(file="./diamondTrain.rf.rda")
            
            p <- varImpPlot(modelFit$finalModel,main="Ranking of Predictors by Descending Importance")
            
            print(p)
            
        })
        
        output$MSE_vs_NumTrees <- renderPlot({
            
            load(file="./diamondTrain.rf.rda")
            
            plot(modelFit$finalModel,col="purple",lwd=5,main="RF Regression MSE vs number of trees")
        })

})



