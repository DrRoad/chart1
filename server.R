library(shiny)
library(lubridate)

shinyServer(function(input, output) {
    output$distPlot <- renderPlot({
        library(quantmod)
        # load historical prices from Yahoo Finance
        #data = getSymbols(input$symbol, src = 'yahoo', from = '1980-01-01', auto.assign = F)
        data <- getSymbols(input$symbol, src = 'yahoo', from='1900-01-01')
        #print(get(input$symbol))
        gdata <<- get(input$symbol)

        if (input$span == "1M"){
            strDate = paste0(Sys.Date() %m-% months(1),"::")
        }
        else if (input$span == "3M"){
            strDate = paste0(Sys.Date() %m-% months(3),"::")
        }
        else if (input$span == "6M"){
            strDate = paste0(Sys.Date() %m-% months(6),"::")
        }
        else if (input$span == "YTD"){
            startyr = today()
            month(startyr) = 1
            day(startyr) = 1
            strDate = paste0(as.character(startyr),"::")
        }
        else if (input$span == "1Y"){
            strDate = paste0(Sys.Date() - years(1),"::")
        }
        else if (input$span == "2Y"){
            strDate = paste0(Sys.Date() - years(2),"::")
        }
        else if (input$span == "5Y"){
            strDate = paste0(Sys.Date() - years(5),"::")
        }
        else if (input$span == "10Y"){
            strDate = paste0(Sys.Date() - years(10),"::")
        }
        else if (input$span == "MAX"){
            strDate = "::"
        }
        else{
            strDate = paste0(as.character(input$dateRange[1]),"::",as.character(input$dateRange[2]))
        }
        #chartSeries(SPY, TA=c(addVo(), addBBands(),addEMA(n = 200)))
        chartSeries(get(input$symbol), name = input$symbol, type = input$type, subset = strDate, theme = input$theme, multi.col = input$multicol)
        #print(strDate)
        date1 = '2010-01-01::2016-12-02'
        tas = NULL
        if (input$volume == TRUE){
            tas = c(tas, addVo())
        }
        if (input$bollinger == TRUE){
            tas = c(tas, addBBands())
        }
        if (input$ilab1 == "EMA"){
            if (input$ival1 > 0){
                num1 <<- input$ival1
                col1 <<- input$icol1
                tas = c(tas, addEMA(n = num1, col = col1))
            }
        }
        else if (input$ilab1 == "SMA"){
            if (input$ival1 > 0){
                num1 <<- input$ival1
                col1 <<- input$icol1
                tas = c(tas, addSMA(n = num1, col = col1))
            }
        }
        if (input$ilab2 == "EMA"){
            if (input$ival2 > 0){
                num2 <<- input$ival2
                col2 <<- input$icol2
                tas = c(tas, addEMA(n = num2, col = col2))
            }
        }
        else if (input$ilab2 == "SMA"){
            if (input$ival2 > 0){
                num2 <<- input$ival2
                col2 <<- input$icol2
                tas = c(tas, addSMA(n = num2, col = col2))
            }
        }
        #print(paste0("chartSeries(", input$symbol, ", ", strDate, ")"))
        cat(file = stderr(), paste0("chartSeries(", input$symbol, ", ", strDate, ")\n"))
        if (is.null(tas)){
            chartSeries(get(input$symbol), name = input$symbol, type = input$type, TA = NULL, subset = strDate, theme = input$theme, multi.col = input$multicol)
        }
        else{
            chartSeries(get(input$symbol), name = input$symbol, type = input$type, TA = tas, subset = strDate, theme = input$theme, multi.col = input$multicol)
        }
        #chartSeries(get(input$symbol), name = input$symbol, subset = '2010-01-01::2016-12-02', theme = "white")
    })
    output$myText <- renderPrint({
        cat(paste0(input$symbol,"\n"))
        #print(get("SPY"))
        print(head(gdata))
        print(tail(gdata))
    })
})