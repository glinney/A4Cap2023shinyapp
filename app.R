#This app allows the user to assign a "value" to the land cover types in Belgium.
#It uses data from the CORINE2018 classified to its broad level 2 land classifications. 

#load packages
#install.packages("shiny")
#install.packages("terra")
#install.packages("RColorBrewer")
library(shiny)
#packages can also be called inline by packagename::function e.g terra::plot()

#load data pre prepared data
#This a raster of the CORINE2018 land cover map classified to its broad level 2 land classifications cropped to Belgium
#where the values
#1 = Artificial surfaces
#2 = Agricultural areas
#3 = Forest and semi natural areas
#4 = Wetlands
#5 = Water bodies

lc_b <- terra::rast("data/lc_b.tif") 


#defin user interface (ui)
ui <- fluidPage( 
  
  #App title ----
  titlePanel("Valuing land in Belgium"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
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
      
      column(6, #creates a column on width 6 to output the below map into (max = 12)
             #this is so that the two maps can be viewed side by side
      #outputs the weighted land cover map created in the server
      plotOutput("lc_map_w")
      ),
      
      column(6, 
      #outputs the land cover map created in the server
      plotOutput("lc_map")
      )
    )

  )
  

)

# Define server logic for slider examples ----
server <- function(input, output) {
  
  #create a variable that react to changes in the ui (changes in the slide values) 
  #and weights lc_b by the users land cover slider weights
  w_lc <- reactive({ 
    
    #create look up table to weight the values in the land cover map (lc_b) by 
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
    output$lc_map_w <-  renderPlot({ #land cover map weighted 
      
      #classifying lc_b by the lookup table of the user assigned weights lkp_w
      lc_b = terra::classify(lc_b,
                             w_lc(), #to use data from a reactive function you must call it as a function hence the brackets
                             others = NA)
      
      #plot the weighted land cover map using a blue color palette 
      terra::plot(lc_b,
                  col = RColorBrewer::brewer.pal(n = 5,
                                                 name = "Blues"))
    
  })
    
    #plot the land cover map lc_b with a custome pallet
    #this map is for reference and does not react to the slider input
    output$lc_map <- renderPlot(
      terra::plot(lc_b,
                  col = c("gray", "orange", "darkgreen", "pink", "blue"),
                  plg=list(legend=c("Artificial surfaces", 
                                    "Agricultural areas", 
                                    "Forest and semi natural areas", 
                                    "Wetlands",
                                    "Water bodies")) #legend labels
    )
    )
}

#run the app
shinyApp(ui, server)