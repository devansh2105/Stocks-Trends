library (shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(quantmod)
library(stringr)


#Reading in stocks in the basic insdutries sector
basic <- read.csv("https://raw.githubusercontent.com/devansh2105/Stocks_Trends/master/Data/companylist_basic.csv")

#Reading in stocks in the capital goods sector
capital_goods <- read.csv("https://raw.githubusercontent.com/devansh2105/Stocks_Trends/master/Data/companylist_capital_goods.csv")

#Reading in stocks in the consumer non-durables sector
con_non_durables <- read.csv("https://raw.githubusercontent.com/devansh2105/Stocks_Trends/master/Data/companylist_con_non-durables.csv")

#Reading in stocks in the consumer services sector
con_services <- read.csv("https://raw.githubusercontent.com/devansh2105/Stocks_Trends/master/Data/companylist_con_services.csv")

#Reading in stocks in the enegry sector
energy <- read.csv("https://raw.githubusercontent.com/devansh2105/Stocks_Trends/master/Data/companylist_energy.csv")

#Reading in stocks in the finance sector
finance <- read.csv("https://raw.githubusercontent.com/devansh2105/Stocks_Trends/master/Data/companylist_finance.csv")

#Reading in stocks in the healthcare sector
healthcare <- read.csv("https://raw.githubusercontent.com/devansh2105/Stocks_Trends/master/Data/companylist_healthcare.csv")

#Reading in stocks in the miscellaneous sector
miscellaneous <- read.csv("https://raw.githubusercontent.com/devansh2105/Stocks_Trends/master/Data/companylist_miscellaneous.csv")

#Reading in stocks in the public utilities sector
utilities <- read.csv("https://raw.githubusercontent.com/devansh2105/Stocks_Trends/master/Data/companylist_utilities.csv")

#Reading in stocks in the technology sector
technology <- read.csv("https://raw.githubusercontent.com/devansh2105/Stocks_Trends/master/Data/companylist_technology.csv")

#Reading in stocks in the transportation sector
transportation <- read.csv("https://raw.githubusercontent.com/devansh2105/Stocks_Trends/master/Data/companylist_transportation.csv")

#Combining all the datasets of the stocks in different sectors to one complete dataset 
sectors <- rbind(basic, capital_goods, con_non_durables, con_services, energy, finance, healthcare, miscellaneous, utilities, technology, transportation)
dim(sectors)

#Reducing the complete dataset of sectors to only the stock symbol and the respective sector
sectors_reduced <- select(sectors, c(1, 7))

#Convering Sector variable from factor to character
sectors_reduced$Sector <- as.character(sectors_reduced$Sector)
sectors_reduced$Symbol <- as.character(sectors_reduced$Symbol)

#Reading in the daily historical data of 8,032 stock tickers on AMEX, NYSE, and NASDAQ 
#from January 2, 2019, to March 1, 2019
stocks <-read.csv("/Users/devansh/Downloads/stocks.csv")

#Creating the valueChange variable that is the difference between the closing and opening prices of a stock 
stocks <- mutate(stocks, valueChange = close - open)

#Adding the sector information to the 'stocks' dataset
stocks1 <- left_join(stocks, sectors_reduced, by= c("symbol" = "Symbol"))
dim(stocks1)

#Removing the stocks that do not have the sector information
stocks2 <- filter(stocks1, Sector!="<NA>") 
dim(stocks2)
colnames(stocks2)[colnames(stocks2)=="date"] <- "Date"

#Changing the "Date" column from factor to character and converting it into the correct date format
stocks2$Date <- as.Date(as.character(stocks2$Date), '%Y-%m-%d')

#Writing a custom function that appends the sectors column
appending_sectors <- function(stocks2_mean_x) {
  stocks2_mean_x2 <- left_join(stocks2_mean_x, sectors_reduced, by= c("symbol" = "Symbol"))
  return(stocks2_mean_x2)
}

#Grouping stocks2 by symbol (i.e, the company) and then finding the mean adjusted closing price of each symbol
stock2_mean_adjclose <- stocks2 %>% group_by(symbol) %>% summarise(mean(adjclose))

#Appending the sector variable to 'stock2_mean_ajdclose'
stock2_mean_adjclose <- appending_sectors(stock2_mean_adjclose)

#Changing the column name from "mean(adjclose)" to "mean_adjclose" for convinience
colnames(stock2_mean_adjclose)[colnames(stock2_mean_adjclose)=="mean(adjclose)"] <- "mean_adjclose"

#Repeating the three steps above for opening price
stock2_mean_open <- stocks2 %>% group_by(symbol) %>% summarise(mean(open))
stock2_mean_open <- appending_sectors(stock2_mean_open)
colnames(stock2_mean_open)[colnames(stock2_mean_open)=="mean(open)"] <- "mean_open"

#Repeating the three steps above for closing price
stock2_mean_close <- stocks2 %>% group_by(symbol) %>% summarise(mean(close))
stock2_mean_close <- appending_sectors(stock2_mean_close)
colnames(stock2_mean_close)[colnames(stock2_mean_close)=="mean(close)"] <- "mean_close"

#Repeating the three steps above for high price
stock2_mean_high <- stocks2 %>% group_by(symbol) %>% summarise(mean(high))
stock2_mean_high <- appending_sectors(stock2_mean_high)
colnames(stock2_mean_high)[colnames(stock2_mean_high)=="mean(high)"] <- "mean_high"

#Repeating the three steps above for low price
stock2_mean_low <- stocks2 %>% group_by(symbol) %>% summarise(mean(low))
stock2_mean_low <- appending_sectors(stock2_mean_low)
colnames(stock2_mean_low)[colnames(stock2_mean_low)=="mean(low)"] <- "mean_low"

#Repeating the three steps above for volume
stock2_mean_volume <- stocks2 %>% group_by(symbol) %>% summarise(mean(volume))
stock2_mean_volume <- appending_sectors(stock2_mean_volume)
colnames(stock2_mean_volume)[colnames(stock2_mean_volume)=="mean(volume)"] <- "mean_volume"

#Repeating the three steps above for valueChange
stock2_mean_valueChange <- stocks2 %>% group_by(symbol) %>% summarise(mean(valueChange))
stock2_mean_valueChange <- appending_sectors(stock2_mean_valueChange)
colnames(stock2_mean_valueChange)[colnames(stock2_mean_valueChange)=="mean(valueChange)"] <- "mean_valueChange"

ui <- dashboardPage(
  dashboardHeader(title = "Visualizing Trends in Stock Prices",
                  titleWidth = 450),
  
  dashboardSidebar(
    # Sidebar for user inputs
    selectInput("type", 
                label = "Choose a type of price",
                choices = c("open", 
                            "close",
                            "high", 
                            "low",
                            "adjclose", 
                            "volume",
                            "valueChange"),
                selected = "adjclose"),
    
    selectInput("sector", 
                label = "Choose a sector to display",
                choices = c(
                  "All",
                  "Basic Industries",
                  "Capital Goods", 
                  "Finance",
                  "Health Care",
                  "Technology",
                  "Transportation",
                  "Energy",
                  "Consumer Non-Durables",
                  "Consumer Services",
                  "Miscellaneous",
                  "Public Utilities"),
                selected = "All"),
    
    sliderInput(inputId = "total_companies",
                label = "Choose number of companies",
                min = 1,
                max = 10,
                value = 5),
    
    uiOutput("conditionalInput"),
    #checkboxInput("checkbox", "Sector-wise smoothened summary", FALSE),
  
    helpText("Note: Sector-wise smoothened summary is only applicable when 'All' sector is selected")
  ),
  dashboardBody(
    #Body for output
    plotOutput("plot1"),
    tableOutput("table1"))
)

server <- function(input, output) {
  
  #Switching input$type from string to value that can be used for the filter function
  datasetInput <- reactive ({
    switch (input$type,
            "open" = stock2_mean_open,
            "close" = stock2_mean_close,
            "high" = stock2_mean_high,
            "adjclose" = stock2_mean_adjclose,
            "low" = stock2_mean_low,
            "volume" = stock2_mean_volume,
            "valueChange" = stock2_mean_valueChange)
  })
  
  #If "All" sector is selected then display checkbox input
  output$conditionalInput <- renderUI({
    if(input$sector == "All"){
      checkboxInput("checkbox", "Sector-wise smoothened summary", FALSE)
    }
  })
  
  #Displaying main plot based on various inputs
  output$plot1 <- renderPlot({
    
    if(input$sector == "All")
    {
      #If "All" sector is selected then no need to filter
      stocks2_price_sector <- datasetInput()}
    else{
      stocks2_price_sector <- filter(datasetInput(), Sector == input$sector)
    }
    
    #Choosing the top n companies according to a certain price from the stocks2_price_sector
    stocks2_price_sector_top_n <- top_n(stocks2_price_sector, n = input$total_companies, wt = as.data.frame(stocks2_price_sector[,2]))
    
    #Filtering all the data for these top n companies from the original dataset
    stock2_sector_top_n <- filter(stocks2, symbol %in% stocks2_price_sector_top_n$symbol)
    
    if(input$sector == "All" && input$checkbox)
    {
      #Plotting the time-series graph of smoothened lines of stocks in each of the sectors' adjusted 
      #closing prices from Jan 2-March 1
      ggplot(stocks2) + 
        geom_smooth(aes(x=Date, y=stocks2[,which(colnames(stocks2)==input$type)], color=Sector), se=FALSE) +
        scale_x_date(NULL, date_labels = "%d-%b") + 
        ylab(input$type) +
        ggtitle(paste("Sector-wise smoothened summary"))
    }
    else{
      ggplot(stock2_sector_top_n) + 
        geom_line(aes(x=Date, y= stock2_sector_top_n[,which(colnames(stock2_sector_top_n)==input$type)], color=symbol)) +
        scale_x_date(NULL, date_labels = "%d-%b") + 
        ylab(input$type) +
        ggtitle(paste("Top",input$total_companies, "stocks in", input$sector, "sector"))
    }
  })
  
  output$table1 <- renderTable({
    
    if(input$sector == "All")
    {
      #If "All" sector is selected then no need to filter
      stocks2_price_sector <- datasetInput()}
    else{
      stocks2_price_sector <- filter(datasetInput(), Sector == input$sector)
    }
    
    if(!(input$sector == "All" && input$checkbox))
    {
      stocks2_price_sector_top_n <- top_n(stocks2_price_sector, n = input$total_companies, wt = as.data.frame(stocks2_price_sector[,2]))
    }
  })
}

shinyApp(ui,server)
