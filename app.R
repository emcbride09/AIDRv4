require(shinyWidgets)
library(shiny)
require(tidyverse)
require(httr)
library(sf)
require(rgdal)
require(leaflet)
library(leaflet.extras)
library(maps)

?req()

#replace data scripts in both ui and server
acledurl <- "https://api.acleddata.com/acled/read?terms=accept&limit=0&notes=school:OR:notes=college:OR:notes=university:OR:actor1=student:OR:actor1=teacher:OR:actor2=student:OR:actor2=teacher&year=2019"

acleddata <- GET(acledurl)

accleddata <- content(acleddata)

accleddata <- accleddata$data

accleddata <- bind_rows(accleddata)

accleddata$event_date <- as.Date(accleddata$event_date, format = "%Y-%m-%d")
accleddata$latitude <- as.numeric(as.character(accleddata$latitude))
accleddata$longitude <- as.numeric(as.character(accleddata$longitude))
accleddata$fatalities <- as.numeric(as.character(accleddata$fatalities))
accleddata$event_date <- lubridate::floor_date(accleddata$event_date, "week")


aidr <- read.csv("2019-07-21 English tweets by country - Sheet1.csv")

aidr_content <- aidr %>% rename('NAME' = 'Country.or.area.name')



#language data

langs <- read.csv("2019-07-29 education insecurity tweet counts - Sheet1.csv")  %>% 
    filter(Tweet.date != '#date +posted')

langs$Tweet.date <- as.Date(langs$Tweet.date)

langs$Tweet.date <- lubridate::floor_date(langs$Tweet.date, "week")

langs$group <- as.factor(1)

#shapefiles

world <- sf::read_sf(dsn ='TM_WORLD_BORDERS-0.3')

aidr_map_data <- world %>% left_join(aidr_content, by = 'ISO3')


#_________________________________app
ui <- fluidPage(
    titlePanel("Attacks Against Education - Data from AIDR and ACLED"),
    sidebarLayout(
        sidebarPanel(top = 10, right = 5,
                     
                     
                     
                     sliderInput("daterange", "Select Week Starting", 
                                 as.Date(min(accleddata$event_date)), 
                                 as.Date(max(accleddata$event_date)),
                                 value = min(accleddata$event_date), 
                                 step = 7,
                                 animate = animationOptions(interval = 1000, loop = TRUE)
                     )
                     
        ), 
        mainPanel(
            leafletOutput("myheatmap"),
            br(), br(),
            
            plotOutput("plot"))
        
    )
)


# server
server <- function(input, output, session) {
    
    ###data clean
    
    acledurl <- "https://api.acleddata.com/acled/read?terms=accept&limit=0&notes=school:OR:notes=college:OR:notes=university:OR:actor1=student:OR:actor1=teacher:OR:actor2=student:OR:actor2=teacher&year=2019"
    
    acleddata <- GET(acledurl)
    
    accleddata <- content(acleddata)
    
    accleddata <- accleddata$data
    
    accleddata <- bind_rows(accleddata)
    
    
    accleddata$event_date <- as.Date(accleddata$event_date, format = "%Y-%m-%d")
    accleddata$latitude <- as.numeric(as.character(accleddata$latitude))
    accleddata$longitude <- as.numeric(as.character(accleddata$longitude))
    accleddata$fatalities <- as.numeric(as.character(accleddata$fatalities))
    accleddata$event_date <- lubridate::floor_date(accleddata$event_date, "week")
    
    
    aidrdat <- read.csv("2019-07-29 education insecurity tweet counts - Sheet1.csv")
    
    aidrdat <- aidrdat %>% filter(Tweet.date != '#date +posted')
    
    aidrdat$Tweet.date <- as.Date(aidrdat$Tweet.date, origin = "1960-10-01")
    
    aidrdat$Tweet.date <- lubridate::floor_date(aidrdat$Tweet.date, "week")
    
    aidrdat$Relevant.tweets <- as.numeric(aidrdat$Relevant.tweets)
    
    ##reactive statements
    
    reactive_data_chrono <- reactive({
        accleddata %>% 
        filter(event_date == input$daterange[1]) 
        })
    
    reactive_plot_data <- reactive({
        langs %>%
            filter(Tweet.date == input$daterange[1])
    })
?colorNumeric
    
#palette
pal <- colorBin(palette = "Blues", domain = aidr_map_data$Tweets, na.color = "grey3", 
                bins = 4, pretty = TRUE
                    )

#leaflet render
    output$myheatmap <- renderLeaflet({
        leaflet(height = 700) %>% 
            addProviderTiles(provider = "OpenStreetMap.HOT") %>%
            setView(lng = 3-12.422889, lat = 22.232698, zoom = 1) %>% 
            addPolygons(data = aidr_map_data,
                        color = "#000",
                        weight = 1,
                        popup = ~paste("Country:", aidr_map_data$NAME.x,
                                       "<br/>",
                                       "Number of Tweets:", Tweets), 
                         fillColor = ~pal(aidr_map_data$Tweets)) %>%
            
                             addHeatmap(data = accleddata, radius = 15, blur = 25) %>% 
                             addLegend(pal = pal,
                                       values = aidr_map_data$Tweets,
                                       position = "bottomright",
                                       title = "Tweets",
                                       )
        
       
        
    })
    
    observe({
        leafletProxy("myheatmap", session, data = reactive_data_chrono()) %>%
            clearHeatmap() %>%
            addHeatmap(radius = 15, blur = 25) %>% 
            fitBounds(lng1 = ~min(longitude), lat1 = ~min(latitude),
                      lng2 = ~max(longitude), lat2 = ~max(latitude))
        
    })
    
######the observe statement messing up everything    
    # observe({
    #     req(input$myheatmap)
    #     leafletProxy("myheatmap" %>%
    #                      clearHeatmap() %>%
    #                      addHeatmap(radius = 15, blur = 25, data = reactive_plot_data())) %>%
    #         fitBounds(fitBounds(lng1 = ~min(longitude), lat1 = ~min(latitude),
    #                             lng2 = ~max(latitude), lat2 = ~max(latitude)))
    #     
    # })
            # fitBounds(fitBounds(lng1 = ~min(longitude), lat1 = ~min(latitude),
            #                     lng2 = ~max(latitude), lat2 = ~max(latitude)))
        
 

    

#mybarplot   
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
                plot.title = element_text(family = 'Gotham', size = 18, hjust = 0.5, vjust = -5),
                legend.title = element_blank(),
                legend.position = "bottom",
                legend.spacing.x = unit(0.2, 'cm'),
                #axis.title.x = element_blank(),
                axis.text.x = element_text(size = 16, family = 'Gotham')
                
            ) + guides(fill = guide_legend(reverse = TRUE))
        

       
    }, height = 200)
    
    

    
    
    
    
    
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
