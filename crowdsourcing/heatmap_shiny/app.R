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
  zoom.def <- 7.4
  
dta <- read.csv("input/soja_data.csv")
dta2 <- read.csv("input/maize_data.csv")
dta2[c("TAname","Date","Time")] <- NULL 
dta$crop <- "soy_bean"
dta2$crop <- "maize"

dta <- rbind(dta,dta2)

dta$day <- str_split_fixed(dta$Timestamp," ",2)[,1]


dta$month <- substr(dta$Timestamp,3,5)
dta$pos <- NA
dta$pos[dta$month %in% c("apr","may")] <- 1
dta$pos[dta$month %in% c("jun")] <- 2
dta$pos[dta$month %in% c("jul")] <- 3


TA_price_av_1_s <- data.frame(tapply(dta$Price[dta$pos==1 & dta$crop =="soy_bean"], dta$TAcode[dta$pos==1  & dta$crop =="soy_bean"], mean))
names(TA_price_av_1_s) <- "price"
TA_price_av_1_s$period <- 1 
TA_price_av_1_s$TA <- rownames(TA_price_av_1_s) 
TA_price_av_1_s$price_dev <-  (TA_price_av_1_s$price - mean(TA_price_av_1_s$price ))/ mean(TA_price_av_1_s$price )

TA_price_av_1_m <- data.frame(tapply(dta$Price[dta$pos==1 & dta$crop =="maize"], dta$TAcode[dta$pos==1  & dta$crop =="maize"], mean))
names(TA_price_av_1_m) <- "price"
TA_price_av_1_m$period <- 1 
TA_price_av_1_m$TA <- rownames(TA_price_av_1_m) 
TA_price_av_1_m$price_dev <-  (TA_price_av_1_m$price - mean(TA_price_av_1_m$price ))/ mean(TA_price_av_1_m$price )

TA_price_av_2_s <- data.frame(tapply(dta$Price[dta$pos==2 & dta$crop =="soy_bean"], dta$TAcode[dta$pos==2 & dta$crop =="soy_bean"], mean))
names(TA_price_av_2_s) <- "price"
TA_price_av_2_s$period <- 2
TA_price_av_2_s$TA <- rownames(TA_price_av_2_s)
TA_price_av_2_s$price_dev <-  (TA_price_av_2_s$price - mean(TA_price_av_2_s$price ))/ mean(TA_price_av_2_s$price )

TA_price_av_2_m <- data.frame(tapply(dta$Price[dta$pos==2 & dta$crop =="maize"], dta$TAcode[dta$pos==2 & dta$crop =="maize"], mean))
names(TA_price_av_2_m) <- "price"
TA_price_av_2_m$period <- 2
TA_price_av_2_m$TA <- rownames(TA_price_av_2_m)
TA_price_av_2_m$price_dev <-  (TA_price_av_2_m$price - mean(TA_price_av_2_m$price ))/ mean(TA_price_av_2_m$price )

TA_price_av_3_s <- data.frame(tapply(dta$Price[dta$pos==3 & dta$crop =="soy_bean"], dta$TAcode[dta$pos==3 & dta$crop =="soy_bean"], mean))
names(TA_price_av_3_s) <- "price"
TA_price_av_3_s$period <- 3
TA_price_av_3_s$TA <- rownames(TA_price_av_3_s)
TA_price_av_3_s$price_dev <-  (TA_price_av_3_s$price - mean(TA_price_av_3_s$price ))/ mean(TA_price_av_3_s$price )

TA_price_av_3_m <- data.frame(tapply(dta$Price[dta$pos==3 & dta$crop =="maize"], dta$TAcode[dta$pos==3 & dta$crop =="maize"], mean))
names(TA_price_av_3_m) <- "price"
TA_price_av_3_m$period <- 3
TA_price_av_3_m$TA <- rownames(TA_price_av_3_m)
TA_price_av_3_m$price_dev <-  (TA_price_av_3_m$price - mean(TA_price_av_3_m$price ))/ mean(TA_price_av_3_m$price )

##now for quantities

TA_quant_av_1_s <- data.frame(tapply(dta$Quantity[dta$pos==1 & dta$crop =="soy_bean"] , dta$TAcode[dta$pos==1 & dta$crop =="soy_bean"], mean))
names(TA_quant_av_1_s) <- "quantity"
TA_quant_av_1_s$period <- 1 
TA_quant_av_1_s$TA <- rownames(TA_quant_av_1_s) 
TA_quant_av_1_s$quantity_dev <-  (TA_quant_av_1_s$quantity - mean(TA_quant_av_1_s$quantity ))/ mean(TA_quant_av_1_s$quantity )

TA_quant_av_1_m <- data.frame(tapply(dta$Quantity[dta$pos==1 & dta$crop =="maize"] , dta$TAcode[dta$pos==1 & dta$crop =="maize"], mean))
names(TA_quant_av_1_m) <- "quantity"
TA_quant_av_1_m$period <- 1 
TA_quant_av_1_m$TA <- rownames(TA_quant_av_1_m) 
TA_quant_av_1_m$quantity_dev <-  (TA_quant_av_1_m$quantity - mean(TA_quant_av_1_m$quantity ))/ mean(TA_quant_av_1_m$quantity )

TA_quant_av_2_s <- data.frame(tapply(dta$Quantity[dta$pos==2 & dta$crop =="soy_bean"] , dta$TAcode[dta$pos==2 & dta$crop =="soy_bean"], mean))
names(TA_quant_av_2_s) <- "quantity"
TA_quant_av_2_s$period <- 2 
TA_quant_av_2_s$TA <- rownames(TA_quant_av_2_s) 
TA_quant_av_2_s$quantity_dev <-  (TA_quant_av_2_s$quantity - mean(TA_quant_av_2_s$quantity ))/ mean(TA_quant_av_2_s$quantity )

TA_quant_av_2_m <- data.frame(tapply(dta$Quantity[dta$pos==2 & dta$crop =="maize"] , dta$TAcode[dta$pos==2 & dta$crop =="maize"], mean))
names(TA_quant_av_2_m) <- "quantity"
TA_quant_av_2_m$period <- 2 
TA_quant_av_2_m$TA <- rownames(TA_quant_av_2_m) 
TA_quant_av_2_m$quantity_dev <-  (TA_quant_av_2_m$quantity - mean(TA_quant_av_2_m$quantity ))/ mean(TA_quant_av_2_m$quantity )

TA_quant_av_3_s <- data.frame(tapply(dta$Quantity[dta$pos==2 & dta$crop =="soy_bean"] , dta$TAcode[dta$pos==2 & dta$crop =="soy_bean"], mean))
names(TA_quant_av_3_s) <- "quantity"
TA_quant_av_3_s$period <- 3 
TA_quant_av_3_s$TA <- rownames(TA_quant_av_3_s) 
TA_quant_av_3_s$quantity_dev <-  (TA_quant_av_3_s$quantity - mean(TA_quant_av_3_s$quantity ))/ mean(TA_quant_av_3_s$quantity )

TA_quant_av_3_m <- data.frame(tapply(dta$Quantity[dta$pos==2 & dta$crop =="maize"] , dta$TAcode[dta$pos==2 & dta$crop =="maize"], mean))
names(TA_quant_av_3_m) <- "quantity"
TA_quant_av_3_m$period <- 3 
TA_quant_av_3_m$TA <- rownames(TA_quant_av_3_m) 
TA_quant_av_3_m$quantity_dev <-  (TA_quant_av_3_m$quantity - mean(TA_quant_av_3_m$quantity ))/ mean(TA_quant_av_3_m$quantity )



TA_price_m <- rbind(TA_price_av_1_m,TA_price_av_2_m, TA_price_av_3_m)
TA_quant_m <- rbind(TA_quant_av_1_m,TA_quant_av_2_m, TA_quant_av_3_m)
TA_price_s <- rbind(TA_price_av_1_s,TA_price_av_2_s, TA_price_av_3_s)
TA_quant_s <- rbind(TA_quant_av_1_s,TA_quant_av_2_s, TA_quant_av_3_s)
TA_price_m$crop <- "maize"
TA_price_s$crop <- "soy_bean" 
TA_price <- rbind(TA_price_m,TA_price_s)

TA_quant_m$crop <- "maize"
TA_quant_s$crop <- "soy_bean" 
TA_quant <- rbind(TA_quant_m,TA_quant_s)

TA_price <- merge(TA_price, TA_quant)



data.p$TA_CODE <- as.numeric(as.character(data.p$TA_CODE))
TA_price$TA <- as.numeric(as.character(TA_price$TA))

data <- left_join(data.p, TA_price, by = c("TA_CODE"= "TA"))


ui <- fluidPage(

  titlePanel("heatmap"),
  tabsetPanel(
tabPanel("Prices", fluid = TRUE,
  # Sidebar with a slider input for year of interest
  sidebarLayout(
    sidebarPanel(
      sliderInput("period_p",h3("Select period or push play button"),
                  min = as.Date("2019-05-01"),max =as.Date("2019-07-01"),value=as.Date("2019-07-01"),step = 31, timeFormat="%b",animate =
                   animationOptions(interval = 2000, loop = TRUE)),
                   radioButtons("crop_select_p","Crop", c("maize" = "maize", "soy bean" = "soy_bean"), inline=T),
                   radioButtons("stat_select_p","Statistic", c("levels" = "levels", "deviation from period mean" = "dev_mean"), inline=T)
                   
    ),

    # Output of the map
    mainPanel(
      leafletOutput("prices",width=400, height=800)
    )
  )
),
tabPanel("Volumes", fluid = TRUE,
  # Sidebar with a slider input for year of interest
  sidebarLayout(
    sidebarPanel(
     sliderInput("period_q",h3("Select period or push play button"),
                   min = as.Date("2019-05-01"),max =as.Date("2019-07-01"),value=as.Date("2019-07-01"),step = 31, timeFormat="%b",animate =
                   animationOptions(interval = 2000, loop = TRUE)),
                   radioButtons("crop_select_q","Crop", c("maize" = "maize", "soy bean" = "soy_bean"), inline=T),
                   radioButtons("stat_select_q","Statistic", c("levels" = "levels", "deviation from period mean" = "dev_mean"), inline=T)
    ),

    # Output of the map
    mainPanel(
      leafletOutput("volumes",width=400, height=800)
    )
  )

)

)
)

server <- function(input, output) {


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
    leaflet(data = data.p) %>%
      addProviderTiles("OpenStreetMap.Mapnik", options = providerTileOptions(opacity = 1), group = "Open Street Map") %>%
      setView(lng = lng.center, lat = lat.center, zoom = zoom.def) %>%
      addPolygons(group = 'base_q', 
                  fillColor = 'transparent', 
                  color = 'black',
                  weight = 1.5) 
  })

  get_data_p <- reactive({
    data[which(data$period == (month(as.POSIXct(input$period_p, tz="GMT")) -4) & data$crop == input$crop_select_p)  ,]
  })

  get_data_q <- reactive({
    data[which(data$period == (month(as.POSIXct(input$period_q, tz="GMT")) -4) & data$crop == input$crop_select_q ),]
  })

  pal_p <- reactive({
    colorNumeric(c("#008000","#FF0000"), domain =  data$price[data$crop == input$crop_select_p] , reverse = TRUE)
  })
  
  pal_q <- reactive({
    colorNumeric(c("#008000","#FF0000"), domain = data$quantity[data$crop == input$crop_select_q] , reverse = TRUE)
  })

  observe({
    data <- get_data_p()
      ifelse(input$stat_select_p == "levels",leafletProxy('prices', data = data) %>%
      clearGroup('base_p') %>%
clearControls() %>%
      addPolygons(group = 'base_p', 
                  fillColor = ~pal_p()(price), 
                  fillOpacity = 0.9,
                  color = 'black',
                  weight = 1.5)  %>%
      addLegend(pal = pal_p(), values = data$price, opacity = 0.7, title = NULL,
                position = "topright") ,leafletProxy('prices', data = data) %>%
      clearGroup('base_p') %>%
clearControls() %>%
      addPolygons(group = 'base_p', 
                  fillColor = ~pal_p()(price_dev), 
                  fillOpacity = 0.9,
                  color = 'black',
                  weight = 1.5) %>%
      addLegend(pal = pal_p(), values = data$price_dev, opacity = 0.7, title = NULL,
                position = "topright"))
  })
  
    observe({
    data <- get_data_q()
      ifelse(input$stat_select_q == "levels", leafletProxy('volumes', data = data) %>%
      clearGroup('base_q') %>%
clearControls() %>%
      addPolygons(group = 'base_q', 
                  fillColor = ~pal_q()(quantity), 
                  fillOpacity = 0.9,
                  color = 'black',
                  weight = 1.5) %>%
      addLegend(pal = pal_p(), values = data$quantity, opacity = 0.7, title = NULL,
                position = "topright"), leafletProxy('volumes', data = data) %>%
      clearGroup('base_q') %>%
clearControls() %>%
      addPolygons(group = 'base_q', 
                  fillColor = ~pal_q()(quantity_dev), 
                  fillOpacity = 0.9,
                  color = 'black',
                  weight = 1.5) %>%
      addLegend(pal = pal_q(), values = data$quantity_dev, opacity = 0.7, title = NULL,
                position = "topright"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)    


                  
                  
 
