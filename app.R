require(shinyWidgets)
require(shiny)
require(tidyverse)
require(httr)
require(sf)
require(rgdal)
require(leaflet)
require(leaflet.extras)
require(maps)
require(shinycssloaders)
require(shinyjs)
require(rmapshaper)
#replace data scripts in both ui and server


###AIDR data
aidr <- read.csv("Education insecurity tweet counts - Weekly by language and country.csv", na.strings = "")

aidr_content <- aidr %>%  filter(Week.starting != '#date+week_start')

aidr_content$Week.starting<- as.Date(aidr_content$Week.starting, format = '%d/%m/%Y')

aidr_content$Tweets <- as.numeric(as.character(aidr_content$Tweets))

aidr_content$Week.starting <- lubridate::floor_date(aidr_content$Week.starting, "week")

aidr_content <- aidr_content %>% filter(Week.starting >= as.Date('2019-07-01'))

aidr_content$group <- as.factor(1)

aidr_content <- na.omit(aidr_content)



#shapefiles
world <- sf::read_sf(dsn ='TM_WORLD_BORDERS-0.3') %>% rmapshaper::ms_simplify()

aidr_map_data <- world %>% full_join(aidr_content, by = c('ISO3' = 'Country.code'))

#aidr_map_data$Tweets[is.na(aidr_map_data$Tweets)] <- 0

##AcledData from API to csv
acledurl <- "https://api.acleddata.com/acled/read?terms=accept&limit=0&notes=school:OR:notes=college:OR:notes=university:OR:actor1=student:OR:actor1=teacher:OR:actor2=student:OR:actor2=teacher&year=2019"
acleddata <- GET(acledurl)
accleddata <- content(acleddata)
accleddata <- accleddata$data
accleddata <- bind_rows(accleddata)

#format all vars then to week on line 25
accleddata$event_date <- as.Date(accleddata$event_date, format = "%Y-%m-%d")
accleddata$latitude <- as.numeric(as.character(accleddata$latitude))
accleddata$longitude <- as.numeric(as.character(accleddata$longitude))
accleddata$fatalities <- as.numeric(as.character(accleddata$fatalities))
accleddata$event_date <- lubridate::floor_date(accleddata$event_date, "week")
accleddata <- accleddata %>% filter(event_date >= as.Date('2019-07-01'))

#centre data

# centers <- read.csv('country_centroid.csv')
# 
# aidr_map_data <- aidr_map_data %>% left_join(centers, centers, by = 'NAME')

#aidr_map_data <- aidr_map_data %>% select(NAME, LON, LAT, Week.starting, Language, Tweets, group)
                               


#_________________________________app
#bootstrap and div(class outer) make it full screen


ui <- fillPage(navbarPage("AIDR and ACLED Map", id ="nav",
                          tabPanel("Interactive Map",
                                   bootstrapPage(
                                     div(class = "outer",
                                         tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                         #titlePanel(title = "Attacks Against Education - Data from AIDR and ACLED"),
                                         leafletOutput("myheatmap", height = "100%", width = "100%"),
                                         absolutePanel(bottom = 15,
                                                       left = 5,
                                                       height = 300,
                                                       width = 200,
                                                       id = 'logop',
                                                       id="controls",
                                                       style="z-index:500; background-color: rgba(0,0,0,0); border:0;;",
                                                       class = "panel panel-default",
                                                       draggable = FALSE,
                                                       h5("Powered by:"),
                                                       img(src = 'hdx.png', height = 50, width = 150), 
                                                       img(src = 'aidr_logo_300h.png', height = 100, width = 190),
                                                       img(src = 'eaa.png', height = 80, width = 160)),
                                         fillPage(bootstrap = TRUE, absolutePanel(top = 45, 
                                                                                  right = 5,
                                                                                  height = 500,
                                                                                  width = 400,
                                                                                  id="controls",
                                                                                  style="z-index:500; background-color: rgba(255,255,255,1); border:0",
                                                                                  class = "panel panel-default",
                                                                                  draggable = FALSE,
                                                                                  pickerInput(
                                                                                    inputId = "dropdown", 
                                                                                    # label = "Select/deselect all + format selected", 
                                                                                    choices = c("No Country Selected", names_sorted <- unique(aidr_map_data$NAME) %>% sort()), 
                                                                                    options = list(`actions-box` = TRUE), 
                                                                                    multiple = F),
                                                                                  
                                                                                  h4("Week and Tweet Breakdowns", size = 16, align = 'center'),
                                                                                  # switchInput(inputId = "switch", value = TRUE),
                                                                                  h6("AIDR Education Insecurity Tweets"),
                                                                                  textOutput(if("ctweets" == 0) 
                                                                                    {"No Country Selected"} else {"ctweets"}),
                                                                                  sliderInput(ticks = TRUE, "daterange", "",
                                                                                              as.Date(min(accleddata$event_date)), 
                                                                                              as.Date(max(accleddata$event_date)),
                                                                                              value = min(accleddata$event_date), 
                                                                                              step = 7,
                                                                                              animate = animationOptions(interval = 1000, loop = TRUE),
                                                                                              width = "100%"),
                                                                                  
                                                                                  plotOutput("plot", width = 400, height = 200),
                                                                                  
                                                                                  plotOutput("Barplot", width = 400, height = 400)
                                                                                  
                                                                                  
                                                                                  
                                                                                  
                                         ))
                                     ))),
                          tabPanel("AIDR Data", dataTableOutput("AIDR_dt")),
                          tabPanel("ACLED Data", dataTableOutput("ACLED_dt"))
))




# server
server <- function(input, output, session) {
  ##reactive statements
  
  reactive_plot_data <- reactive({
    aidr_map_data %>%
      filter(Week.starting == input$daterange[1])
  })
  
  
  reactive_bar <- reactive({
    aidr_map_data %>% 
      filter(Week.starting <= input$daterange[1],
             NAME == input$dropdown)
  })
  
  reactive_data_chrono <- reactive({
    accleddata %>% 
      filter(event_date == input$daterange[1]) 
  })

  reactive_tweet_country <- reactive({
    aidr_map_data %>% 
      filter(Week.starting == input$daterange[1],
             NAME == input$dropdown)
  })

  
  rc1 <- colorRampPalette(colors = c("red", "orange"), space = "Lab")(10)
  
  rc2 <- colorRampPalette(colors = c("orange", "yellow"), space = "Lab")(30)
  
  ## Make vector of colors for values larger than 0 (180 colors)
  rc3 <- colorRampPalette(colors = c("yellow", "green"), space = "Lab")(75000)
  
  ## Combine the two color palettes
  rampcols <- c(rc1, rc2, rc3)
  

  
  
 
  #palette
  # l
  pal <- colorNumeric(palette = rampcols, 
                      domain = aidr_map_data$Tweets, 
                 #na.color = "#EA3546",
                 alpha = TRUE)
  
  # sort(dexp(x = aidr_map_data$Tweets, log = TRUE))
    
  #  observe statement needs session
  observe({
    leafletProxy("myheatmap", session, data = reactive_plot_data()) %>%
      clearShapes() %>%
      addPolygons(
        popup = ~NAME,
        color = "#000",
        weight = 1,
        opacity = 0.2,
        options = pathOptions(pane = "polygons"),
        fillColor = ~pal(reactive_plot_data()$Tweets))
    
    leafletProxy("myheatmap", session, data = reactive_data_chrono()) %>%
      clearHeatmap() %>%
      addHeatmap(radius = 25, blur = 35)
    
  })
  
  proxy <- leafletProxy("myheatmap")
  
  #
  
  observe({
    if(input$dropdown!="No Country Selected"){
      #get the selected polygon and extract the label point 
      selected_polygon <- subset(aidr_map_data, aidr_map_data$NAME==input$dropdown & aidr_map_data$Week.starting == input$daterange[1])
      polygon_labelPt <- selected_polygon$geometry
      
      #remove any previously highlighted polygon
      proxy %>% removeShape("highlighted_polygon")
      
      proxy %>% removeShape("highlighted_polygon_base")
      
      #center the view on the polygon 
      
      proxy %>% addPolygons(color="yellow",data=selected_polygon$geometry, layerId = "highlighted_polygon_base")
      
      #add a slightly thicker red polygon on top of the selected one
      proxy %>% addPolylines(stroke=TRUE, weight = 3,color="red",data=selected_polygon, layerId = "highlighted_polygon")
    }
  })
  
  
  # observeEvent({ # update the map markers and view on map clicks
  #   if(input$dropdown !="No Country Selected"){
  #     proxy %>% setView(lng = selected_polygon$Longitude,lat=selected_polygon$Latitude, zoom=5)
  #   } else {
  #     proxy %>% setView(lng = 30.7382679, lat =  15.3489054, zoom = 3)
  #   }
  # })
  
  
  
 # leaflet render
  output$myheatmap <- renderLeaflet({
    leaflet() %>%
      enableTileCaching() %>%
      addTiles(options = tileOptions(useCache = TRUE, crossOrigin = TRUE)) %>% 
      #addProviderTiles(provider = "OpenStreetMap.HOT") %>%
      addMapPane("polygons", zIndex = 399) %>%
      setView(lng = 30.7382679, lat =  15.3489054, zoom = 3) %>% 
      addLegend(pal = pal,
                values = aidr_map_data$Tweets,
                position = "topleft",
                title = "Tweets")
      
  })
  


  # observe({
  #   leafletProxy("myheatmap", session, data = reactive_data_chrono()) %>%
  #       setView(lat = input$dropdown$centlat,
  #               lng = input$dropdown$centlong,
  #               zoom = input$dropdown$Level)
  #   })


  #mybarplots   
  output$plot <- renderPlot({
    ggplot(reactive_tweet_country(), 
           aes(group, Tweets, fill = Language)) + 
      geom_col(position = "fill", width = 0.2) + 
      scale_fill_manual(values = c('#47A025', '#9A031E', '#064789'), 
                        labels = c('Arabic', 'English', 'French')
      ) +
      labs(title = '% Breakdown of Tweet language', x = 'Language Breakdown', y = "") + 
      coord_flip() + 
      theme(
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(family = 'Gotham', size = 14, vjust = -5, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        legend.spacing.x = unit(0.2, 'cm'),
        #axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8, family = 'Gotham', color = 'black')
        
      ) + guides(fill = guide_legend(reverse = TRUE))
    
  })
  
  output$Barplot <- renderPlot({
    ggplot(reactive_bar(), aes(Week.starting, Tweets, fill = Language)) + geom_col() +
      scale_fill_manual(values = c('#47A025', '#9A031E', '#064789'),
                        labels = c('Arabic', 'English', 'French')) +
      labs(title = "Number of Tweets by Date", x = "Week Starting", y = "Tweet Frequency") +
      theme(
        plot.background = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(family = 'Gotham', size = 14, hjust = 0.5),
        legend.position = "bottom",
        legend.spacing.x = unit(0.2, 'cm'),
        axis.text.x = element_text(size = 8, family = 'Gotham'),
        axis.title = element_text(size = 8, family = 'Gotham')
        
      ) + guides(fill = guide_legend(reverse = TRUE))
  })
  
  
  
  output$AIDR_dt <- renderDataTable({aidr_map_data})
  
  output$ACLED_dt <- renderDataTable({accleddata})
  
  output$aidrimg <- renderImage({
    outfile <- tempfile(fileext = 'aidr_logo_300h.png')
  })
  
  #rendered text
  output$ctweets <- renderText({
    paste('Country:', input$dropdown,
          sum(reactive_tweet_country()$Tweets))
  })
}


# Run the application 

shinyAppDir(".")

shinyApp(ui = ui, server = server)

#runGitHub( "AIDRv4", "emcbride09")

