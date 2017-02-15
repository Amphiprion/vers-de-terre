# Author: Amphiprion

library(shiny) ; library(ggplot2) ; library(shinyapps) ; library(nlme)

function(session,input,output){
  
  observeEvent(input$Species,
               updateSelectInput(session,"Publie","publie",
               choices = my_data$auteur[my_data$species.raw==input$Species]))
  
  observeEvent(input$Publie,
               updateSelectInput(session,"Figure","figure",
               choices = my_data$figure[my_data$species.raw==input$Species & my_data$auteur==input$Publie ]))
  
  my_data2 <- reactive({
    my_data=my_data[my_data$auteur==input$Publie & my_data$figure==input$Figure,]
  })
  
  output$plot1 <- renderPlot({
    axeX=as.numeric(input$varX)
    axeY=as.numeric(input$varY)
    selec=input$Precision
    
  })
  output$data <- renderTable({
    my_data
  })
  output$summary_publie <- renderPrint({
    summary(my_data2())
  })
  output$data_publie <- renderTable({
    my_data2()
  })
  
  
  output$plot1 <- renderPlot({
    colmX=as.numeric(input$varX)
    colmY=as.numeric(input$varY)
    autor=input$Publie
    figure=input$Figure
    species=input$Species
    select=input$Precision
    min=input$Min
    
    if(select=="by_publie"){
      if(species=="Aporrectodea.caliginosa" & autor=="Vercesi_2006" |
         species=="Aporrectodea.longa" |
         autor=="Lofs-Holmin_1980" &  figure=="figure4"){
        linear(my_data2())
      }else{
        if(species=="Dendrobaena.octaedra" & autor=="Addison_1995" &  figure=="figure1" |
           species=="Dendrobaena.octaedra" & autor=="Addison_1995" &  figure=="figure3"|
           species=="Dendrobaena.octaedra" & autor=="Fisker_2011"){
          brain_cousen(my_data2(),min,400,70,5,2)
        }else{
          if(species=="Dendrobaena.octaedra" & autor!="Fisker_2011"){
            brain_cousen_sans_horm(my_data2(),min,400,70,5)
          }else{
            if(species=="drawida.willsi"){
              logistique(my_data2(),min,20,45,5)
            }else{
              if((autor=="Lofs-Holmin_1980" & figure=="figure5")){
                gompertz(my_data2(),min,250,35,0.05)
              }else{
                if(species=="Eisenia.andrei" & autor=="Van-gestal_1991"){
                  brain_cousen_sans_horm(my_data2(),min,500,30,2)
                }else{
                  if(autor=="Brunninger_1994" & figure=="figure1"){
                    double_brain_cousen(my_data2(),min,480,200,24,1,400)
                  }else{
                    if(autor=="Brunninger_1994" & figure=="figure2"){
                      brain_cousen(my_data2(),min,600,200,24,1)
                    }else{
                      if(species=="Eisenia.fetida" & autor=="Fernandez_Gomez_2011" & figure=="figure1"){
                        brain_cousen_sans_horm(my_data2(),min,500,40,3)
                      }else{
                        if(species=="Eisenia.fetida" & autor=="Fernandez_Gomez_2011" & figure=="figure2"){
                          brain_cousen_sans_horm(my_data2()[-c(12:14),],min,700,60,5)+
                            ggtitle("la figure est inverse par rapport a la realite ;\nles 3 derniers points de la figure 1 sont supprimes")
                        }else{
                          if(species=="Eisenia.fetida" & autor!="Helling_2000"){
                            brain_cousen_sans_horm(my_data2(),min,500,40,3)
                          }else{
                            if(species=="Eisenia.fetida" & autor=="Helling_2000"){
                              brain_cousen(my_data2(),min,500,40,5,1)
                            }else{
                              if(species=="Lumbricus.rubellus"){
                                gompertz(my_data2(),min,1500,100,0.01)
                              }else{
                                if(species=="Pontoscolex.corethrurus"){
                                  brain_cousen(my_data2(),min,1000,100,5,10)
                                }else{
                                  if(species=="Perionyx.sansibaricus"){
                                    logistique(my_data2(),min,60,15,15)
                                  }else{
                                    if(autor=="Lofs-Holmin_1980" & figure=="figure3"){
                                      logistique(my_data2(),min,350,35,5)
                                    }else{
                                      if(species=="Lumbricus.terrestris"){
                                        logistique(my_data2(),min ,200,8,5)
                                      }else{
                                        if(species=="Allolobophora.caliginosa"){
                                          logistique(my_data2(),min ,500,70,5)
                                        }else{
                                          if(autor=="Springett_1992"){
                                            non_analyse("donnees non analyse car curieuse",5)
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }else{ggplot(my_data,aes(my_data$time,my_data$bm,col=my_data$treatment))+
        geom_point()+theme_bw()+theme(legend.position='none')+ggtitle("all data")}
  }
  )
}



