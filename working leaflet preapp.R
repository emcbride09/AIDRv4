pal <- colorNumeric("Blues", domain = aidr_map_data$Tweets)

world <- geojsonio::geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", what = 'sp')

aidr_map_data <- world %>% left_join(aidr_content, by = c('ISO3'= 'Country.code'))

mlm <- leaflet(world) %>% 
  setView(30.7382679, 15.3489054, zoom = 2) %>% 
  addProviderTiles(provider = "OpenStreetMap.HOT")

mlm %>% 
  addPolygons(data = aidr_map_data,
              weight = 1,
              color = "grey",
              fillOpacity = 0.7,
              fillColor = ~pal(aidr_map_data$Tweets),
              popup = ~paste("Country:", aidr_map_data$NAME,
                                            "<br/>",
                                            "Number of Tweets:", aidr_map_data$Tweets),
              highlight = highlightOptions(sendToBack = TRUE)) %>% 
  addHeatmap(data = accleddata,
             lat = ~latitude,
             lng = ~longitude,
             radius = 15, 
             blur = 25)





%>% 
  addLegend(pal = pal,
            values = aidr_map_data$Tweets,
            position = "topleft",
            title = "Tweets")
 






  
  
 %>% 
  # addMapPane("heatmap", zIndex = 420) %>% 
  
              %>% 
 


              # weight = 1,
              # popup = ~paste("Country:", aidr_map_data$NAME,
              #                "<br/>",
              #                "Number of Tweets:", Tweets), 
              # fillColor = ~pal(aidr_map_data$Tweets),
              # setView(30.7382679, 15.3489054, zoom = 3))
