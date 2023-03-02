#this app will take CORINE2018 land cover map data and allow the user to assign 
#different values to the different land covers 

#load packages
library(shiny)
library(terra)

#load data pre prepared data
#lc_b <- readRDS("data/lc_b.rds") #Corine land cover map 2018 of Belgium classified to its 
#CORINE level 2 land cover classes (5 classes)


lc_b <- terra::rast("data/lc_b.tif") #Corine land cover map 2018 of Belgium classified to its 
#CORINE level 2 land cover classes (5 classes)


#defin user interface (ui)
ui <- fluidPage(
  
  #App title ----
  titlePanel("Valuing land in Belgium"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # 
    sidebarPanel(
      
      
      # Input: slider for weighting Artificial surfaces land cover class----
      
      sliderInput(inputId = "art", #the id assigned to this input that is used by the server
                  label = "Artificial surfaces:", #the label of this slider that is displayed in the ui 
                  min = 0, max = 1, #the min and max values of the slider
                  value = 0.5), #the default value of the slider (the value of the slider when the app first loads)
      
      # Input: slider for weighting Agricultural areas land cover class----
      #same as above but ID and label are changed to another land cover class
      sliderInput(inputId = "ag", #the id assigned to this input that is used by the server
                  label = "Agricultural areas:", 
                  min = 0, max = 1, 
                  value = 0.5),
      
      # Input: slider for weighting Forest and semi natural areas land cover class----
      #same as above but ID and label are changed to another land cover class
      sliderInput(inputId = "forest", #the id assigned to this input that is used by the server
                  label = "Forest and semi natural areas:", 
                  min = 0, max = 1, 
                  value = 0.5), 
      
      # Input: slider for weighting Wetlands land cover class----
      #same as above but ID and label are changed to another land cover class
      sliderInput(inputId = "wet", #the id assigned to this input that is used by the server
                  label = "Wetlands:", 
                  min = 0, max = 1, 
                  value = 0.5), 
      
      # Input: slider for weighting Water bodies land cover class----
      #same as above but ID and label are changed to another land cover class
      sliderInput(inputId = "wat", #the id assigned to this input that is used by the server
                  label = "Water bodies:", 
                  min = 0, max = 1, 
                  value = 0.5)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("lc_map_w"),
      plotOutput("lc_map")
    )

  )
  

)

# Define server logic for slider examples ----
server <- function(input, output) {
  
  #create a variable that react to changes in the ui (changes in the slide values) 
  #and weights lc_b by the users land cover slider weights
  w_lc <- reactive({ 
    
    #create look up table to weight by
    lkp_w = data.frame(
      #id of the land cover in the land cover map for Belgium
      lc_id = 1:5,
      
      #weights assigned to each land cover from the users input (slider values)
      lc_weight = c(input$art,
                    input$ag,
                    input$forest,
                    input$wet,
                    input$wat
                    )
    )
  })
    
    #classify lc_b to lkp_w
    output$lc_map_w = renderPlot({ #land cover map weighted 

      lkp_w <- w_lc()

      lc_b = terra::classify(lc_b,
                             lkp_w,
                             others = NA)
      terra::plot(lc_b)
    
  })
    
    #land use map
    output$lc_map <- renderPlot(terra::plot(lc_b,
                                            col = RColorBrewer::brewer.pal(n = 5,
                                                                           name = "RdBu")))
}

shinyApp(ui, server)