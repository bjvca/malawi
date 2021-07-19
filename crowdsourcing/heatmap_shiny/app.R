### dynamic choropleth for malawi
#to deploy this on shiny.io, in the directory of App.R, just type rsconnect::deployApp()
rm(list=ls())
library(shiny)
library(dplyr)
library(leaflet)
library(sf)
library(rmapshaper)
library(stringr)
library(lubridate)

data.p <- sf::st_read("input/Malawi_TA_2018.shp") %>% 
    st_transform(4326) %>%
    rmapshaper::ms_simplify()
    
  lng.center <- 34
  lat.center <- -13
  zoom.def <- 7.5
  
dta <- read.csv("input/soja_data.csv")[c("Timestamp","Quantity","Price","TAcode")]
dta2 <- read.csv("input/maize_data.csv")[c("Timestamp","Quantity","Price","TAcode")]
dta21 <- read.csv("input/data_21.csv")[c("date","quantity_sold","price","TAcode","crop")]
names(dta21) <- c("Timestamp","Quantity","Price","TAcode","crop")
dta21$crop[dta21$crop == "Soya"] <- "soy_bean" 
dta$crop <- "soy_bean"
dta2$crop <- "maize"

dta <- rbind(dta,dta2,dta21)

#merge in centroids
dta <- merge(dta,read.csv("input/Malawi_TA_2018_centroids.csv")[c("TA_CODE","lon_centroid", "lat_centroid")], by.x="TAcode", by.y="TA_CODE")


dta$day <- str_split_fixed(dta$Timestamp," ",2)[,1]

dta$year <- substr(dta$Timestamp,6,9)
dta$month <- substr(dta$Timestamp,3,5)
dta$pos <- NA
dta$pos[dta$month %in% c("apr","may")] <- 1
dta$pos[dta$month %in% c("jun")] <- 2
dta$pos[dta$month %in% c("jul")] <- 3

dta$crop <- tolower(dta$crop)

dta$Price[dta$Price >200 & dta$crop=="maize"] <- NA
dta$Price[dta$Price < 50 & dta$crop=="maize"] <- NA
dta$Price[dta$Price > 450 & dta$crop=="soy_bean"] <- NA
dta$Price[dta$Price < 180 & dta$crop=="soy_bean"] <- NA


dta$Quantity[dta$Quantity >2000 & dta$crop=="maize"] <- NA
dta$Quantity[dta$Quantity < 50 & dta$crop=="maize"] <- NA
dta$Quantity[dta$Quantity > 1400 & dta$crop=="soy_bean"] <- NA
dta$Quantity[dta$Quantity < 45 & dta$crop=="soy_bean"] <- NA

dta$kg <- dta$Quantity

dta$Quantity <- dta$Quantity/max(dta$Quantity, na.rm=T)*25


data.p$TA_CODE <- as.numeric(as.character(data.p$TA_CODE))
dta$TAcode <- as.numeric(as.character(dta$TAcode))

data <- left_join(data.p, dta, by = c("TA_CODE"= "TAcode"))


ui <- fluidPage(  tags$style(type = "text/css", "html, body {width:100%;height:100%}",
      ".leaflet .legend i{
      border-radius: 50%;
      width: 10px;
      height: 10px;
      margin-top: 4px;
      }
    "),
tags$img(height=200, width=1200, src="banner.png"),
  tabsetPanel(
tabPanel("Prices", fluid = TRUE,
  # Sidebar with a slider input for year of interest
  sidebarLayout(
    sidebarPanel( p("These interactive maps show the prices that farmers in Malawi report receiving for their maize/soybeans during the main 2020 and 2021 marketing seasons in a crowdsourcing exercise conducted by IFPRI and Farm Radio Trust. Prices are stated in MWK/kg at the TA level, with green colors showing higher prices. TAs where no farmers have reported sales during the month are shaded in grey."),
      sliderInput("period_p",h4("Select period or push play button:"),
                  min = as.Date("2019-05-01"),max =as.Date("2019-07-01"),value=as.Date("2019-07-01"),step = 31, timeFormat="%b",animate =
                   animationOptions(interval = 5000, loop = TRUE)),
                   radioButtons("crop_select_p","Crop", c("maize" = "maize", "soybean" = "soy_bean"), inline=T),
                   radioButtons("year_select_p","Use data from", c("2020" = "2020", "2021" = "2021","2020 + 2021" = "both"), inline=T)
                   
    ),

    # Output of the map
    mainPanel(
      leafletOutput("prices",width=600, height=1200)
    )
  )
),
tabPanel("Volumes", fluid = TRUE,
  # Sidebar with a slider input for year of interest
  sidebarLayout(
    sidebarPanel( p("These interactive maps show the volumes of maize and soybeans that farmers in Malawi report selling during the main 2020 and 2021 marketing seasons in a crowdsourcing exercise conducted by IFPRI and Farm Radio Trust. Volumes are in kg, with larger circles showing larger sales volumes. TAs where no farmers have reported sales during the month are shaded in grey."),
     sliderInput("period_q",h4("Select period or push play button:"),
                   min = as.Date("2019-05-01"),max =as.Date("2019-07-01"),value=as.Date("2019-07-01"),step = 31, timeFormat="%b",animate =
                   animationOptions(interval = 2000, loop = TRUE)),
                   radioButtons("crop_select_q","Crop", c("maize" = "maize", "soybean" = "soy_bean"), inline=T),
                  radioButtons("year_select_q","Use data from", c("2020" = "2020", "2021" = "2021","2020 + 2021" =  "both"), inline=T)
    ),

    # Output of the map
    mainPanel(
      leafletOutput("volumes",width=600, height=1200)
    )
  )

)

)
)

server <- function(input, output) {
 addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
      colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
      labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
    
      return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
    }

  output$prices <- renderLeaflet({
    leaflet(data = data.p) %>%
        addProviderTiles("OpenStreetMap.Mapnik", options = providerTileOptions(opacity = 1), group = "Open Street Map") %>%
      setView(lng = lng.center, lat = lat.center, zoom = zoom.def) %>%
      addPolygons(group = 'base_p', 
                  fillColor = 'transparent', 
                  color = 'black',
                  weight = 1.5) 
  })
  
    output$volumes <- renderLeaflet({
    leaflet(data = data.p)  %>% 
      addProviderTiles("OpenStreetMap.Mapnik", options = providerTileOptions(opacity = 1), group = "Open Street Map") %>% 
      setView(lng = lng.center, lat = lat.center, zoom = zoom.def) %>%
      addPolygons(group = 'base_q', 
                  fillColor = 'transparent', 
                  color = 'black',
                  weight = 1.5) 
  })

  get_data_p <- reactive({
  if (input$year_select_p  == "both") {
    aggregate(data[which(data$pos == (month(as.POSIXct(input$period_p, tz="GMT")) -4)  & data$crop == input$crop_select_p)  ,][c("Price","lon_centroid","lat_centroid","kg","Quantity","TA_NAME")],list(data[which(data$pos == (month(as.POSIXct(input$period_p, tz="GMT")) -4)  & data$crop == input$crop_select_p)  ,]$TA_NAME), mean, na.rm=T)
    } else {
     aggregate(data[which(data$pos == (month(as.POSIXct(input$period_p, tz="GMT")) -4) & data$year == input$year_select_p & data$crop == input$crop_select_p)  ,][c("Price","lon_centroid","lat_centroid","kg","Quantity","TA_NAME")],list(data[which(data$pos == (month(as.POSIXct(input$period_p, tz="GMT")) -4) & data$year == input$year_select_p & data$crop == input$crop_select_p)  ,]$TA_NAME), mean, na.rm=T)  
    }
    
  })

  get_data_q <- reactive({

     if (input$year_select_q  == "both") {
   aggregate( data[which(data$pos == (month(as.POSIXct(input$period_q, tz="GMT")) -4)  & data$crop == input$crop_select_q)  ,][c("Price","lon_centroid","lat_centroid","kg","Quantity","TA_NAME")],list(data[which(data$pos == (month(as.POSIXct(input$period_q, tz="GMT")) -4)  & data$crop == input$crop_select_q)  ,]$TA_NAME), mean, na.rm=T)
    } else {
      aggregate(data[which(data$pos == (month(as.POSIXct(input$period_q, tz="GMT")) -4) & data$year == input$year_select_q & data$crop == input$crop_select_q)  ,][c("Price","lon_centroid","lat_centroid","kg","Quantity","TA_NAME")],list(data[which(data$pos == (month(as.POSIXct(input$period_q, tz="GMT")) -4) & data$year == input$year_select_q & data$crop == input$crop_select_q)  ,]$TA_NAME), mean, na.rm=T)  
    }
  })




  observe({
    data <- get_data_p()
     leafletProxy('prices', data = data) %>%
      clearGroup('base_p') %>%
clearControls() %>%
      addPolygons(group = 'base_p', 
                  fillColor = ~colorNumeric(c("#008000","#FF0000"), domain =  data$Price , reverse = TRUE)(Price), 
                  fillOpacity = 0.9,
                  color = 'black',
                  weight = 1.5, label = ~as.character(Group.1), popup = ~as.character(round(Price)))  %>%
      addLegend(pal = colorNumeric(c("#008000","#FF0000"), domain =  data$Price , reverse = TRUE), values = data$Price, opacity = 0.7, title = NULL,
                position = "topright") 
  })
  
    observe({
    data <- get_data_q()
     leafletProxy('volumes', data = data) %>%
  clearGroup('dots')     %>% clearControls() %>%
          addCircleMarkers(group='dots',data=data, lng=~lon_centroid, lat=~lat_centroid,radius=~Quantity,  stroke = FALSE,color="red", label = ~as.character(Group.1), popup = ~as.character(round(kg)),fillOpacity=.5)  %>% addLegendCustom(colors = c("red", "red", "red","red"), labels = c("400 kg", "800 kg", "1200 kg","1600 kg"), sizes = c(10, 20,30, 40))
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)    


                  
                  
 
