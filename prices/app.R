#to deploy this on shiny.io, in the directory of App.R, just type rsconnect::deployApp()


library(shiny)
library(ggplot2)
library(reshape2)
library(lubridate)
library(flexdashboard)
library(plotly)



# Loading data ----
prices <- read.csv("data/latest_prices.csv")[,1:5]

#returns difined as log(t)- log(t-1)
returns <- prices
for (i in 2:dim(prices)[1]) {
	returns[i,2:dim(prices)[2]] <- log(prices[i,2:dim(prices)[2]]) - log(prices[i-1,2:dim(prices)[2]])
}
#drop first observation 
returns <- returns[-1,]

#data probably needs to be in long form

prices <- melt(prices, id.vars = c("Date"))
names(prices) <- c("Date","market","price")
prices$Date <- dmy(prices$Date)

returns <- melt(returns, id.vars = c("Date"))
names(returns) <- c("Date","market","return")
returns$Date <- dmy(returns$Date)


# ui.R
ui <- fluidPage( 
tags$img(height=200, width=1200, src="banner.png"),
	titlePanel("Maize prices in Malawi"),
tabsetPanel(
tabPanel("Levels", fluid = TRUE,
dateRangeInput('dateRange',
      label = 'Date range input: yyyy-mm-dd',
      start = "2018-04-18", end = Sys.Date() + 2
    ),
      checkboxGroupInput("market", "Choose market to show:",
                     c("Chitipa" = "Chitipa", "Karonga"="Karonga", "Rumphi"="Rumphi", "Mzuzu"="Mzuzu"), selected= "Chitipa"),
                     column(8, plotOutput("price"))
	), 
	tabPanel("Returns", fluid = TRUE, 
	dateRangeInput('dateRange_ret',
      label = 'Date range input: yyyy-mm-dd',
      start = "2018-04-18", end = Sys.Date() + 2
    ),
    radioButtons("market_ret", "Choose market to show:",
               c("Chitipa" = "Chitipa", "Karonga"="Karonga", "Rumphi"="Rumphi", "Mzuzu"="Mzuzu")),
	column(8, gaugeOutput("gauge")), column(8, plotOutput("firstdif"))
	
	)
	))

# Define server logic required to draw a histogram
server <- function(input, output) {
	output$price<- renderPlot(
		ggplot(data = prices[prices$market %in% input$market & prices$Date > input$dateRange[1]  & prices$Date < input$dateRange[2],], 
					 aes(x=Date, y=price, group=market)) +
geom_line(aes(color=market))+
  geom_point(aes(color=market))
	)
	
	output$firstdif <- renderPlot(
		ggplot(data = returns[returns$market %in% input$market_ret & returns$Date > input$dateRange_ret[1]  & returns$Date < input$dateRange_ret[2],], 
					 aes(x=Date, y=return)) +
geom_line()+
  geom_point()
	)
	
	 output$gauge = renderGauge({
    gauge(
    round(sd(
    prices[prices$market %in% input$market_ret & prices$Date > input$dateRange_ret[1]  & prices$Date < input$dateRange_ret[2],3])/mean(
    prices[prices$market %in% input$market_ret & prices$Date > input$dateRange_ret[1]  & prices$Date < input$dateRange_ret[2],3]), digits=2)
  
    
    , abbreviateDecimals =0, abbreviate = TRUE,
          min = 0, 
          max = 1, 
          sectors = gaugeSectors(danger = c(0.5, 1), 
                                 warning = c(0.3, 0.5),
                                 success = c(0, 0.3)))
  })
	}

# Run the application 
shinyApp(ui = ui, server = server)
