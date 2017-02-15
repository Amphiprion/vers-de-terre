# Author: Amphiprion

library(shiny) ; library(ggplot2)

shinyUI(fluidPage(
  titlePanel(title="evolution de la biomasse des vers de terre sous different traitement"),
  sidebarLayout(
    sidebarPanel(
      selectInput("varX","Variable X",choices=c("time"=4)),
      selectInput("varY","Variable Y",choices=c("bm"=5)),
      selectInput("Precision","precision",choices=c("all_publie","by_publie")),      
      selectInput("Species","species",choices=unique(my_data$species.raw)),
      selectInput("Publie","publie",choices="",selected = ""),
      selectInput("Figure","figure",choices="",selected = ""),
      numericInput("Min","min",0)),
    
    mainPanel(tabsetPanel(type="tab",
                          tabPanel("plot",plotOutput("plot1")),
                          tabPanel("data_publie",tableOutput("data_publie")),
                          tabPanel("summary_publie",verbatimTextOutput("summary_publie")),
                          tabPanel("data",tableOutput("data"))
    ),"Pour Lofs-Holmin_1980, bien penser a supprimer son nom de 'publie' avant de le reselectionner,
    sinon le changement d'espece ne fonctionne pas"
    )
    
    
  )
  
  
))


