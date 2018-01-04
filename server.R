library(shiny)
library(leaflet)
library(rgdal)
library(data.table)
library(dplyr)
library(RColorBrewer)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  Mex <- rgdal::readOGR(dsn="C:/DISCO D/Bienvenu/Voitures/Individuel/PT/Models/Spatial Smooth/Mapas/02.-Nivel Estatal", layer="MEX_adm1") 
  centr <- gCentroid(Mex)@coords
  data_inegi<-fread("C:/DISCO D/Bienvenu/Voitures/Individuel/PT/Academic/Coursera/Data Products/Shiny/Autos_2016.csv")
  data_inegi<-data_inegi%>%dplyr::select(-Nombre,-Clave)
  Mex@data<-left_join(Mex@data,data_inegi, by="HASC_1")
  
  
  
  map_type<-reactive({input$Veh})
  
  output$distPlot<-renderLeaflet({
    if (map_type()=="Total"){
      bins <- quantile(Mex@data$Total, c(0, .25, .5, .75, 1))
      pal <- colorBin("YlOrRd", domain = Mex@data$Total, bins = bins,
                      na.color = "grey40", reverse = FALSE)
      
      leaflet(options = leafletOptions(minZoom = 5, maxZoom = 5)) %>% 
          addTiles() %>% 
          setView(centr[1], centr[2], zoom = 5) %>% 
          leaflet::addLegend(pal = pal, values =Mex@data$Total, 
                             opacity = 0.4, position = "topright", title = "Total Vehicles")%>% 
          addPolygons(data=Mex, weight = 1, 
                      fill = ~Mex@data$Total, fillColor = ~pal(Total),
                      opacity=1, fillOpacity = 0.6, color=grey(0.5),
                      highlight = highlightOptions(
                        weight = 1,
                        color = "white",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                      popup  = ~as.character(
                        paste(as.character(NAME_1),"<br>",
                              "Total vehicles: ",format(Total,big.mark = ","))))    
    }
    else if (map_type()=="Cars"){
      
      bins <- quantile(Mex@data$Auto, c(0, .25, .5, .75, 1))
      pal <- colorBin("YlOrRd", domain = Mex@data$Auto, bins = bins,
                      na.color = "grey40", reverse = FALSE)
      
      leaflet(options = leafletOptions(minZoom = 5, maxZoom = 5)) %>% 
        addTiles() %>% 
        setView(centr[1], centr[2], zoom = 5) %>% 
        leaflet::addLegend(pal = pal, values =Mex@data$Auto, 
                           opacity = 0.4, position = "topright", title = "Cars")%>% 
        addPolygons(data=Mex, weight = 1, 
                    fill = ~Mex@data$Auto, fillColor = ~pal(Auto),
                    opacity=1, fillOpacity = 0.6, color=grey(0.5),
                    highlight = highlightOptions(
                      weight = 1,
                      color = "white",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    popup  = ~as.character(
                      paste(as.character(NAME_1),"<br>",
                            "Cars: ",format(Auto,big.mark = ","))))
    }
    else if (map_type()=="Passenger Trucks"){
      bins <- quantile(Mex@data$Cam_Pas, c(0, .25, .5, .75, 1))
      pal <- colorBin("YlOrRd", domain = Mex@data$Cam_Pas, bins = bins,
                      na.color = "grey40", reverse = FALSE)
      
      leaflet(options = leafletOptions(minZoom = 5, maxZoom = 5)) %>% 
        addTiles() %>% 
        setView(centr[1], centr[2], zoom = 5) %>% 
        leaflet::addLegend(pal = pal, values =Mex@data$Cam_Pas, 
                           opacity = 0.4, position = "topright", title = "Cars")%>% 
        addPolygons(data=Mex, weight = 1, 
                    fill = ~Mex@data$Cam_Pas, fillColor = ~pal(Cam_Pas),
                    opacity=1, fillOpacity = 0.6, color=grey(0.5),
                    highlight = highlightOptions(
                      weight = 1,
                      color = "white",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    popup  = ~as.character(
                      paste(as.character(NAME_1),"<br>",
                            "Passenger Trucks: ",format(Cam_Pas,big.mark = ","))))
    }
    else if (map_type()=="Cargo Trucks"){
    
      bins <- quantile(Mex@data$Cam_Carg, c(0, .25, .5, .75, 1))
      pal <- colorBin("YlOrRd", domain = Mex@data$Cam_Carg, bins = bins,
                      na.color = "grey40", reverse = FALSE)
      
      leaflet(options = leafletOptions(minZoom = 5, maxZoom = 5)) %>% 
        addTiles() %>% 
        setView(centr[1], centr[2], zoom = 5) %>% 
        leaflet::addLegend(pal = pal, values =Mex@data$Cam_Carg, 
                           opacity = 0.4, position = "topright", title = "Cars")%>% 
        addPolygons(data=Mex, weight = 1, 
                    fill = ~Mex@data$Cam_Carg, fillColor = ~pal(Cam_Carg),
                    opacity=1, fillOpacity = 0.6, color=grey(0.5),
                    highlight = highlightOptions(
                      weight = 1,
                      color = "white",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    popup  = ~as.character(
                      paste(as.character(NAME_1),"<br>",
                            "Cargo Trucks: ",format(Cam_Carg,big.mark = ","))))
    }
    else if (map_type()=="Motorcycles"){
      bins <- quantile(Mex@data$Moto, c(0, .25, .5, .75, 1))
      pal <- colorBin("YlOrRd", domain = Mex@data$Moto, bins = bins,
                      na.color = "grey40", reverse = FALSE)
      
      leaflet(options = leafletOptions(minZoom = 5, maxZoom = 5)) %>% 
        addTiles() %>% 
        setView(centr[1], centr[2], zoom = 5) %>% 
        leaflet::addLegend(pal = pal, values =Mex@data$Moto, 
                           opacity = 0.4, position = "topright", title = "Cars")%>% 
        addPolygons(data=Mex, weight = 1, 
                    fill = ~Mex@data$Moto, fillColor = ~pal(Moto),
                    opacity=1, fillOpacity = 0.6, color=grey(0.5),
                    highlight = highlightOptions(
                      weight = 1,
                      color = "white",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    popup  = ~as.character(
                      paste(as.character(NAME_1),"<br>",
                            "Motorcycles: ",format(Moto,big.mark = ","))))
      }
    })
  output$selected_var <- renderText({ 
    paste("You have selected:", input$Veh)
  })
  
})
