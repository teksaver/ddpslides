---
title: "Power Plant Energy Output"
author: "Sylvain Tenier"
highlighter: highlight.js
output: pdf_document
job: Esigelec
knit: slidify::knit2slides
mode: selfcontained
hitheme: tomorrow
subtitle: A shiny and slidify assignment for the DDP course
framework: io2012
widgets: mathjax
---

##  Combined Cycle Power Plant Data Set 

The shiny application uses the  Combined Cycle Power Plant Data Set  available at [the UCI web site](http://archive.ics.uci.edu/ml/datasets/Combined+Cycle+Power+Plant).

It displays the predicted amount of power provided by 3 machine learning algorithms : linear regression, random forests and generalized linear model. Other models could be added to the resulting graph but shiny.io puts a strong constraint on calculation resources for deployment, at least on the basic plan.

The application is available at [shiny.io](https://teksaver.shinyapps.io/DDPassignment/)

--- .class #id 

## Model calculation

At startup the application sources the *prediction.R* files that fits 3 models


```r
suppressMessages(library(xlsx))
suppressMessages(library(caret))

# load file from data folder
# original data available at http://archive.ics.uci.edu/ml/datasets/Combined+Cycle+Power+Plant
data <- read.xlsx2("data/Folds5x2_pp.xlsx",1,
                   colClasses = c("numeric","numeric","numeric","numeric","numeric"))

# fit 3 fast models (longer training not working with shiny.io basic plan)
lmFit <- train(PE ~.,data=data,method="lm",preProc=c("center","scale"))
dtFit <- train(PE ~.,data=data,method="rpart",preProc=c("center","scale"))
glmFit  <- train(PE ~.,data=data,method="glm",preProc=c("center","scale"))
```

---

## Prediction function

This function receives the dynamic values and returns a dataframe with the predicted values


```r
pred_data <- function(temp, vac, press, humid ) {
  dfTest<-data.frame(AT=temp,V=vac,AP=press,RH=humid)
  lmPred <- predict(lmFit,dfTest)
  dtPred <- predict(dtFit,dfTest)
  glmPred <- predict(glmFit,dfTest)
  res<- data.frame(method="lm",value=lmPred)
  res<-rbind(res,data.frame(method="dt",value=dtPred))
  res<-rbind(res,data.frame(method="glm",value=glmPred))
  res
}
pred_data(30,50,1000,60) # example call for illustration
```

```
##    method    value
## 1      lm 436.1878
## 11     dt 438.1841
## 12    glm 436.1878
```

---

## Reactive behaviour

The *server.R* file defines a reactive function and 2 renderings that are displayed in the [running shiny application](https://teksaver.shinyapps.io/DDPassignment/)

```r
dataPred <- reactive({
    pred_data(input$temperature,input$vacuum,input$pressure,input$humidity)
  })

  output$meanVal <- renderText({
    paste("The mean predicted value is: ",round(mean(dataPred()$value),digits=2), " MW")
  })

  output$predPlot <- renderPlot({
    g <- ggplot(dataPred(),aes(x=method,y=value))
    g + geom_bar(stat="identity") + 
      coord_cartesian(ylim=c(400, 500)) +
      geom_hline(yintercept=mean(dataPred()$value),linetype="dotted") +
      ggtitle("Predicted power output according to prediction models")
  })
```

