# Interactive Shiny + Leaflet Dashboard Application
# Los Angeles Urban Heat Island and Environmental Justice Analysis

# this dashboard visualizes urban heat islands in Los Angeles
# and the relationship between heat and demographic patterns
# highlighting environmental justice concerns



#####################
### load packages ###
#####################

library(dplyr)     # data manipulation
library(leaflet)   # interactive maps
library(sf)        # simple features for spatial data
library(shiny)     # web application framework
library(viridis)   # viridis color palettes



#############################
### load application data ###
#############################

# previously generated simulated temperature and demographic data
# for the city of Los Angeles region
temperature_data <- readRDS("temperature_data.rds")
demographic_data <- readRDS("demographic_data.rds")



#################################
### user interface definition ###
#################################

ui <- fluidPage(
  
  # custom css for stlying
  tags$head(
    tags$style(HTML("
      body {
        overflow-x: hidden;
      }
      .container-fluid {
        padding-left: 15px;
        padding-right: 15px.
      }
      /* reduce padding on the main panel */
      .col-sm-9 {
        padding-left: 10px
      }
      .title-panel {
        background: white;
        color:black;
        padding: 15px;
        margin-bottom: 15px;
        border-radius: 8px;
        border: 2px solid #e0e0e0;
      }
      .info-box {
        background-color: #f8f9fa;
        margin-bottom: 15px;
        border-radius: 4px;
      }
      .stat-box {
        background-color: white;
        color: black;
        padding: 10px;
        border-radius: 8px;
        border: 2px solid black;
        text-align: center;
        margin: 5px 0;
      }
      .stat-number {
        font-size: 24px;
        font-weight: bold;
        display: block;
      }
      .stat-label {
        font-size: 13px;
        opacity: 0.8;
      }
   "))
  ),
  
  # title
  div(class = "title-panel",
      h1("Urban Heat Island Explorer",
         style = "margin: 0;"),
      p("Exploring heat island distribution and environmental justice in Los Angeles", 
         style = "margin: 5px 0 0 0; font-size: 16px;")
  ),
  
  # main layout - sidebar + map
  sidebarLayout(
    
    ######################################
    ### sidebar panel - user interface ###
    ######################################
    
    sidebarPanel(
      width = 3,
      
      # information box
      div(class = "info-box",
          h4("About Urban Heat Islands",
             style = "margin-top: 0;"),
          p("Urban heat islands occur when cities replace natural land with 
             pavement and buildings that absorb heat. These islands 
             disproportionately affect vulnerable communities.")
          ),
      
      # time of day selection
      selectInput(
        inputId = "time_period",
        label = "Time of Day",
        choices = c("Morning (7 AM)",
                    "Afternoon (2 PM)",
                    "Evening (7 PM)"
        ),
        selected = "Afternoon (2 PM)"
      ),
      
      # demographic overlay data selection
      selectInput(
        inputId = "demographic_var",
        label = "Demographic Overlay",
        choices = c("None" = "none",
                    "Median Income" = "median_income",
                    "Percent People of Color" = "pct_poc",
                    "Percent Over 65" = "pct_over_65",
                    "Percent in Poverty" = "pct_poverty"
        ),
        selected = "none"
      ),

      hr(),
      
      # temperature display options
      h4("Temperature Display"),
      
      # show/hide temperature point data checkbox
      checkboxInput(
        inputId = "show_temp_points",
        label = "Show Temperature Point Data",
        value = T # default
      ),
      
      # temperature point data opacity slider
      sliderInput(
        inputId = "point_opacity",
        label = "Point Opacity:",
        min = 0.3,
        max = 1.0,
        value = 0.3,
        step = 0.1
      ),
      
    ),

    ##################
    ### main panel ###
    ##################
    
    mainPanel(
      width = 9,
      
      # summary statistics
      fluidRow(
        
        column(4, div(class = "stat-box",
                      span(class = "stat-number",
                           textOutput("avg_temp")),
                      span(class = "stat-label",
                           "Average Temperature")
                      )
               ),
        
        
        column(4, div(class = "stat-box",
                      span(class = "stat-number",
                           textOutput("max_temp")),
                      span(class = "stat-label",
                           "Maximum Temperature")
                      )
               ),
        
        column(4, div(class = "stat-box",
                      span(class = "stat-number",
                           textOutput("temp_range")),
                      span(class = "stat-label",
                           "Temperature Range")
                      )
               )
      ),
      
      br(),
      
      # tab panel to select different views
      tabsetPanel(
        
        # main map tab
        tabPanel(
          "Heat Island Map",
          br(),
          leafletOutput("heat_map", height = 450)
        ),
        
        # methodology tab
        tabPanel(
          "About & Methodology",
          br(),
          div(class = "info-box",
              h3("Urban Heat Islands & Environmental Justice"),
              
              p("Urban heat islands (UHIs) are metropolitan areas significantly 
                 warmer than surrounding rural areas. This temperature difference 
                 is primarily due to human activities and modifications to land surfaces."),
              
              h4("Why This Matters"),
              p("Heat islands contribute to:"),
              tags$ul(
                tags$li("Increased energy consumption and costs"),
                tags$li("Elevated emissions of air pollutants and greenhouse gases"),
                tags$li("Compromised human health and comfort"),
                tags$li("Impaired water quality")
              ),
              
              h4("Environmental Justice Concerns"),
              p("Heat islands disproportionately affect vulnerable populations:"),
              tags$ul(
                tags$li("Low-income communities often have less tree canopy"),
                tags$li("Communities of color experience higher exposure to heat"),
                tags$li("Elderly residents face increased health risks"),
                tags$li("Limited access to cooling resources compounds the problem")
              ),
              
              h4("Data & Methods"),
              p(strong("Temperature Data:"), " Simulated based on known urban heat 
                 island patterns in Los Angeles. Real applications would use NASA 
                 LANDSAT thermal imagery or NOAA climate data."),
              
              p(strong("Demographic Data:"), " Census tract-level data showing 
                 income, race/ethnicity, age, and poverty rates. Real applications 
                 would integrate IPUMS or Census Bureau data."),
              
              p(strong("Analysis Approach:"), " Spatial overlay of temperature 
                 patterns with demographic characteristics to identify environmental 
                 justice concerns."),
              
              h4("Applications for Research"),
              p("This type of analysis supports:"),
              tags$ul(
                tags$li("Climate adaptation planning"),
                tags$li("Environmental justice assessments"),
                tags$li("Public health interventions"),
                tags$li("Urban greening initiatives"),
                tags$li("Policy development and evaluation")
              ),
              
              h4("Connections to My Research"),
              p("My PhD work in Population, Health, and Place examined spatial 
                 patterns of health disparities. This dashboard applies those same 
                 analytical approaches to climate-related health risks, demonstrating 
                 how geospatial methods can illuminate environmental justice issues.")
          )
        )
      )
    )
  )
)



#########################
### server definition ###
#########################

server <- function(input, output, session) {
  
  ###############################
  ### reactive data filtering ###
  ###############################
  
  # filter temperature data based on the user-selected time period
  filtered_temp <- reactive({
    temperature_data %>%
      filter(time_of_day == input$time_period)
  })
  
  ##########################
  ### summary statistics ###
  ##########################
  
  # calculate average temperature
  output$avg_temp <- renderText({
    paste0(round(mean(filtered_temp()$temperature_f), 1), "°F")
  })
  
  # calculate maximum temperature
  output$max_temp  <- renderText({
    paste0(round(max(filtered_temp()$temperature_f), 1), "°F")
  })

  # calculate temperature range
  output$temp_range <- renderText({
    temps <- filtered_temp()$temperature_f
    paste0(round(max(temps) - min(temps), 1), "°F")
  })
  
  
  
  ################
  ### main map ###
  ################
  
  # render interactive map
  output$heat_map <- renderLeaflet({
    
    # basemap
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -118.25, lat = 34.05, zoom = 10)
    
    # add demographic layer if selected
    if (input$demographic_var != "none") {
      
      # create color palette for the selected demographic variable
      if (input$demographic_var == "median_income") {
        pal <- colorNumeric(
          palette = "YlGnBu",
          domain = demographic_data$median_income
        )
        var_values <- demographic_data$median_income
        legend_title <- "Median Income"
        
      } else if (input$demographic_var == "pct_poc") {
        pal <- colorNumeric(
          palette = "Purples",
          domain = demographic_data$pct_poc
        )
        var_values <- demographic_data$pct_poc  # ← DON'T multiply here
        legend_title <- "% People of Color"
        
      } else if (input$demographic_var == "pct_over_65") {
        pal <- colorNumeric(
          palette = "Oranges",
          domain = demographic_data$pct_over_65
        )
        var_values <- demographic_data$pct_over_65  # ← DON'T multiply here
        legend_title <- "% Over 65"
        
      } else {  # pct_poverty
        pal <- colorNumeric(
          palette = "Reds",
          domain = demographic_data$pct_poverty
        )
        var_values <- demographic_data$pct_poverty  # ← DON'T multiply here
        legend_title <- "% in Poverty"
      }
      
      demographic_data_temp <- demographic_data
      demographic_data_temp$display_value <- var_values
      
      # add demographic polygons to map  
      map <- map %>%
        addPolygons(
          data = demographic_data_temp,
          fillColor = ~pal(display_value),
          fillOpacity = 0.4,
          color = "transparent",
          weight = 0,
          popup = ~paste0(
            "<strong>Census Tract:</strong> ", tract_id, "<br>",
            "<strong>Median Income:</strong> $", format(median_income, big.mark = ","), "<br>",
            "<strong>% POC:</strong> ", round(pct_poc * 100, 1), "%<br>",
            "<strong>% Over 65:</strong> ", round(pct_over_65 * 100, 1), "%<br>",
            "<strong>% Poverty:</strong> ", round(pct_poverty * 100, 1), "%"
          )
        ) %>%
        addLegend(
          position = "bottomleft",
          pal = pal,
          values = demographic_data_temp$display_value,
          title = legend_title,
          opacity = 0.8,
          labFormat = labelFormat(
            transform = function(x) {
              # Only transform for percentage variables
              if (input$demographic_var != "median_income") {
                return(x * 100)  # ← Multiply by 100 for DISPLAY only
              } else {
                return(x)
              }
            }
          )
        )
    }
    
    # add temperature points if checkbox is checked
    if (input$show_temp_points) {
      
      # color palette for temperature (hot = red, cool = blue)
      temp_pal <- colorNumeric(
        palette = "RdYlBu",  # Red-Yellow-Blue
        domain = filtered_temp()$temperature_f,
        reverse = TRUE  # Reverse so red = hot
      )
      
      map <- map %>%
        addCircleMarkers(
          data = filtered_temp(),
          lng = ~longitude,
          lat = ~latitude,
          radius = 4,
          fillColor = ~temp_pal(temperature_f),
          fillOpacity = input$point_opacity,
          color = "white",
          weight = 1,
          popup = ~paste0(
            "<strong>Temperature:</strong> ", temperature_f, "°F<br>",
            "<strong>Location:</strong> ", round(latitude, 3), ", ", round(longitude, 3)
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = temp_pal,
          values = filtered_temp()$temperature_f,
          title = "Temperature (°F)",
          opacity = 0.8
        )
    }
    
    # returns the map
    map
    
  })
}



#######################
### run application ###
#######################

shinyApp(ui = ui, server = server)