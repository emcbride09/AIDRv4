require(shinyWidgets)
library(shiny)
require(tidyverse)
require(httr)
library(sf)
require(rgdal)
require(leaflet)
library(leaflet.extras)
library(maps)

#replace data scripts in both ui and server

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


###AIDR data
aidr <- read.csv("Education insecurity tweet counts - Weekly by language.csv")

aidr_content <- aidr %>% 
    filter(Tweet.date != '#date+week_start')

aidr_content$Tweet.date <- as.Date(aidr_content$Tweet.date)

aidr_content$Relevant.tweets <- as.numeric(aidr_content$Relevant.tweets)

aidr_content$Tweet.date <- lubridate::floor_date(aidr_content$Tweet.date, "week")

aidr_content <- aidr_content %>% filter(Tweet.date >= as.Date('2019-07-01'))

aidr_content$group <- as.factor(1)



#language data

# langs <- read.csv("2019-07-29 education insecurity tweet counts - Sheet1.csv")  %>% 
#     filter(Tweet.date != '#date+week_start')

#shapefiles
# world <- sf::read_sf(dsn ='TM_WORLD_BORDERS-0.3')
# 
# aidr_map_data <- world %>% left_join(aidr_content, by = 'ISO3')

#_________________________________app
#bootstrap and div(class outer) make it full screen

ui <- bootstrapPage(div(class = "outer",
    tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
    #titlePanel(title = "Attacks Against Education - Data from AIDR and ACLED"),
    leafletOutput("myheatmap", height = "100%", width = "100%"),
    absolutePanel(top = 10, 
                  right = 5,
                  height = 500,
                  width = 400,
                  id="controls",
                  style="z-index:500;",
                  class = "panel panel-default",
                  draggable = TRUE,
                  
                  h3("Week and Tweet Breakdowns", size = 16),
                  
                  
                  
                  sliderInput("daterange", "Select Week Starting", 
                              as.Date(min(accleddata$event_date)), 
                              as.Date(max(accleddata$event_date)),
                              value = min(accleddata$event_date), 
                              step = 7,
                              animate = animationOptions(interval = 1000, loop = TRUE)),
                  plotOutput("plot", width = 400, height = 200),
                  plotOutput("Barplot", width = 400, height = 400))))


# server
server <- function(input, output, session) {
    
    ##reactive statements
    
    reactive_data_chrono <- reactive({
        accleddata %>% 
        filter(event_date == input$daterange[1]) 
        })
    
    reactive_plot_data <- reactive({
        aidr_content %>%
            filter(Tweet.date == input$daterange[1])
    })
    
    
    reactive_bar <- reactive({
        aidr_content %>% 
            filter(Tweet.date <= input$daterange[1])
    })

    
#palette
# pal <- colorBin(palette = "Blues", domain = aidr_content$Relevant.tweets, na.color = "grey3", 
#                 bins = 4, pretty = TRUE
#                     )

#leaflet render
    output$myheatmap <- renderLeaflet({
        leaflet() %>% 
        addProviderTiles(provider = "OpenStreetMap.HOT") %>% 
            setView(9.7382679, 40.3489054, zoom = 3)
        #%>% 
        # addHeatmap(data = accleddata, 
        #            lat = latitude,
        #            lng = longitude,
        #            radius = 15, 
        #            blur = 25) %>%
        #     popup = ~paste("Country:", country,
        #                    "<br/>",
        #                    "Event:", notes)
    })
#observe statement needs session    
    observe({
        leafletProxy("myheatmap", session, data = reactive_data_chrono()) %>%
            clearHeatmap() %>%
            addHeatmap(radius = 25, blur = 35) 
        
        # %>% 
            # popup = ~paste("Country:", country,
            # "<br/>",
            # "Event:", notes) %>% 
             # fitBounds(lng1 = ~min(longitude), lat1 = ~min(latitude),
             #           lng2 = ~max(longitude), lat2 = ~max(latitude))
        
    })
    

#mybarplots   
    output$plot <- renderPlot({
        ggplot(reactive_plot_data(), aes(group, fill = Language)) + 
            geom_bar(position = "fill", width = 0.5) + 
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
                plot.title = element_text(family = 'Gotham', size = 20, vjust = -5),
                legend.title = element_blank(),
                legend.position = "none",
                legend.spacing.x = unit(0.2, 'cm'),
                #axis.title.x = element_blank(),
                axis.text.x = element_text(size = 12, family = 'Gotham')
                
            ) + guides(fill = guide_legend(reverse = TRUE))
        

       
    })
    
    output$Barplot <- renderPlot({
        ggplot(reactive_bar(), aes(Tweet.date, Relevant.tweets, fill = Language)) + geom_col() +
            scale_fill_manual(values = c('#47A025', '#9A031E', '#064789'),
                              labels = c('Arabic', 'English', 'French')) +
            labs(title = "Number of Tweets by Date", x = "Week Starting", y = "Tweet Frequency") +
            theme(
                plot.background = element_blank(),
                panel.background = element_blank(),
                plot.title = element_text(family = 'Gotham', size = 20),
                legend.position = "bottom",
                legend.spacing.x = unit(0.2, 'cm'),
                axis.text.x = element_text(size = 12, family = 'Gotham'),
                axis.title = element_text(size = 16, family = 'Gotham')

            ) + guides(fill = guide_legend(reverse = TRUE))
    })
    
    

    
    
    
    
    
    
    
}


# Run the application 
# shinyApp(ui = ui, server = server)

runGitHub( "<AIDRv4>", "<emcbride09>")
