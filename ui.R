library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Vehicles in Mexico"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("Select the vehicle type of which you want to see its geographical distribution."),
      selectInput('Veh','Type of Vehicule',c("Total","Cars","Passenger Trucks","Cargo Trucks","Motorcycles")),
      helpText("The maps are based on Data from INEGI (2016).")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("selected_var"),
      leafletOutput("distPlot", width = "100%", height = "500px")
    )
  )
))
