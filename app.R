library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)
library(lubridate)

library(data.table)
library(dplyr)
library(formattable)
library(tidyr)

#library("DT")

lastused <- "FV"
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    theme = shinytheme("flatly"),

    navbarPage("Financial Tool Dashboard",
               # Application title
               tabPanel("Future Value",
                        # Sidebar with a slider input for number of bins
                        sidebarLayout(
                            sidebarPanel(
                              selectInput("choice", "Choose which variable to solve for",
                                          choices = c("Future Value",
                                                      "Periods",
                                                      "Interest",
                                                      "Present Value")),
                              numericInput("n_periods", "Periods",10, min =0),
                              numericInput("interest", "Interest (%)",2),
                              numericInput("PV", "Present Value",5000),
                              numericInput("FV", "Future Value", value = 0),
                              submitButton("Submit")

                            ),
                            # Show a plot of the generated distribution
                            mainPanel(
                                plotOutput("distPlot")
                            )
                        )
               ),
               tabPanel("PMT",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("choice_pmt", "Choose which variable to solve for",
                                        choices = c("Future Value",
                                                    "Periods",
                                                    "Interest",
                                                    "Present Value",
                                                    "Payment")),
                            numericInput("n_periods2", "Periods",5, min =0),
                            numericInput("interest2", "Interest (%)",5),
                            numericInput("PV2", "Present Value",0, min=0),
                            numericInput("FV2", "Future Value", value = 0),
                            numericInput("PMT2", "Payment", value = 500),
                            submitButton("Submit")
                          ),
                          mainPanel(
                            plotOutput("distPlot2")
                          )
                          
                        )
               ),
               tabPanel("Insurance Comparator",
                        sidebarPanel(
                          textInput("p1", "Plan 1 Name:", "general"),
                          numericInput("p1Monthly", "Monthly Premium", value=0),
                          numericInput("p1Deduct", "Deductable", value=0),
                          numericInput("p1Coin", "Coinsurance coverage (% covered by provider)", value=0),
                          numericInput("p1Copay", "Copayment", value=0),
                          numericInput("p1Max", "Max Out of Pocket", value=0),
                          submitButton("update")
                        ),
                        sidebarPanel(
                          textInput("p2", "Plan 2 Name:", "general"),
                          numericInput("p2Monthly", "Monthly Premium", value=0),
                          numericInput("p2Deduct", "Deductable", value=0),
                          numericInput("p2Co", "Coinsurance coverage (% covered by provider)", value=0),
                          numericInput("p2Copay", "Copayment", value=0),
                          numericInput("p2Max", "Max Out of Pocket", value=0),
                          submitButton("update")
                        ),
                        sidebarPanel(
                          h4("Recuring Costs"),
                          h6("For example, monthly prescriptions or annual doctors visits"),
                          numericInput("Recuring", "Estimated Cost", value=0),
                          numericInput("Charges", "Number of Charges", value=0),
                          selectInput("Frequency", "Per:", c("day", "week", "month", "year")),
                          submitButton("update")
                        ),
                        mainPanel(
                         # h4("Possible one-time incident"),
                         # sliderInput("incidentCost", "Cost:", 1, 15000, 0),
                         # h4("One Year Cost Summary"),
                          h3(textOutput("p1summary")),
                          h3(textOutput("p2summary"))

                        )
               ), #close insurance panel
               
               tabPanel("Car Buying Calculator",
                        sidebarPanel(
                          numericInput("down", "Down Payment", value=0, min = 0),
                          numericInput("trade", "Trade In Value", value=0, min = 0),
                          numericInput("price", "Price of Car", value=0, min = 0),
                          radioButtons("term", "Term (Months)",
                                       c("36" = "36",
                                         "48" = "48",
                                         "60" = "60",
                                         "72" = "72")),
                          numericInput("apr", "Interest Rate (% APR)", value=0, min = 0, max = 100),
                          submitButton("Calculate Payment")
                        ),
                        mainPanel(
                          plotOutput("cashVsCarLoan"),
                          h3(textOutput("payment"))
                        )
                        
               ), #close car purchase calculator
               
               tabPanel("Mortgage Manager",
                        sidebarPanel(
                          numericInput("mortgage", "Loan Amount", value=100000, min = 0),
                          numericInput("mortgageLength", "Loan Length", value=1, min = 0),
                          radioButtons("mortgageTerm", "Term",
                                       c("years" = "Years",
                                         "months" = "Months")),
                          numericInput("mortgageRate", "Interest Rate (% APR)", value=5, min = 0, max = 100),
                          submitButton("Generate Schedule")
                        ),
                        mainPanel(
                          #summary at the top
                          h3(textOutput("mortgageMonthly")),
                          h3(textOutput("accumInterest")),
                          h3(textOutput("totalPaid")),
                          h3(textOutput("mortgageEnd"))
                          #table with specifics
                          #tableOutput("amoritizationTable")
                        )
                        
               ) #close mortgage calculator

    ) #close main tab panel
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  choiceFV <- reactive({
    switch(input$choice,
           "Present Value" = "pv",
           "Periods" = "n",
           "Interest" = "i",
           "Future Value" = "fv")
  })
  choicePMT <- reactive({
    switch(input$choice_pmt,
           "Present Value" = "pv",
           "Periods" = "n",
           "Interest" = "i",
           "Future Value" = "fv",
           "Payment" = "pmt")
  })
    observe({ })

    output$distPlot <- renderPlot({
      choice <- choiceFV()
      if(choice == "pv"){
        #f <- function(temp) (temp * (1 + (input$interest)/100)^(input$n_periods))
        #val <- uniroot(f, lower=0.1, upper=100000000)$root
        temp <- (input$FV) / ( (1+input$interest/100)^(input$n_periods))
        updateNumericInput(session, "PV", value = temp)
      }
      else if(choice == "fv"){
        temp <- (input$PV) * (1 + (input$interest)/100)^(input$n_periods)
        updateNumericInput(session, "FV", value = temp)
      }
      else if(choice == "n"){
        temp <- log(input$FV/input$PV) / log(1+input$interest/100)
        updateNumericInput(session, "n_periods", value = temp)
      }
      else if(choice == "i"){
        temp <- log(input$FV/input$PV)/ (input$n_periods) * 100
        updateNumericInput(session, "interest", value = temp)
      }
      
      x <- c(0:input$n_periods)
      result <- input$PV * (1 + (input$interest)/100)^(x)
      plot(x,result)
      lines(x,result)
    })
    
    # PMT Tab calculations     
    output$distPlot2 <- renderPlot({
      # source: http://www.tvmcalcs.com/tvm/formulas/regular_annuity_formulas
      choice <- choicePMT()
      # Calculate some shit
      
      if(choice == "pv"){
        temp <- input$PMT2 * ( ( 1 -(1/(1+(input$interest2/100))^input$n_periods2) ) / input$interest2 )
        updateNumericInput(session, "PV2", value = temp)
      }
      else if(choice == "fv"){
        cat("here\n")
        temp <- input$PMT2 * ( ( ( 1 +(input$interest2/100))^(input$n_periods2) - 1) / (input$interest2/100))
        updateNumericInput(session, "FV2", value = temp)
      }
      else if(choice == "n"){
        
        if(input$PV2 == 0 || is.null(input$PV2) ){
          temp <- (log(1 + (input$FV2/input$PMT2) * (input$interest2/100)) )/ log(1+(input$interest2/100))
          
        }
        else if(input$FV2 == 0 || is.null(input$FV2)){
          temp <- (-log(1 - (input$PV2/input$PMT2) * (input$interest2/100)) )/ log(1+(input$interest2/100))
        }
        else{
          # IF YOU HAVE BOTH PV AND FV ENTERED IN, DEFAULT TO NULLING OUT FV AND SOLVE FOR N
          temp <- (log(1 + (input$FV2/input$PMT2) * (input$interest2/100)) )/ log(1+(input$interest2/100))
          updateNumericInput(session, "FV2", value = 0)
        }
        updateNumericInput(session, "n_periods2", value = temp)
        
      }
      else if(choice == "i"){
        
        if(input$PV2 == 0 || is.null(input$PV2)){
          f <- function(interest) (input$FV2/( ( (1 + interest/100)^input$n_periods2 - 1 )/ (interest/100) ) - input$PMT)
          temp <- uniroot(f, lower=0.1, upper=100)#$root
          
          # TBD hard math
        }
        else if(input$FV2 == 0 || is.null(input$FV2)){
          f <- function(interest)(input$PV2 / ( ( 1 - ( 1/ (1+input$interest2/100)^input$n_periods2 ) )/ (input$interest2/100) ) - input$PMT) 
          temp <- uniroot(f, lower=0.1, upper=100)#$root
          # TBD hard math
        }
        else{
          # IF YOU HAVE BOTH PV AND FV ENTERED IN, DEFAULT TO NULLING OUT FV AND SOLVE FOR N
          
          updateNumericInput(session, "FV2", value = 0)
          f <- function(interest)(input$PV2 / ( ( 1 - ( 1/ (1+input$interest2/100)^input$n_periods2 ) )/ (input$interest2/100) ) - input$PMT) 
          temp <- uniroot(f, lower=0.1, upper=100)$root
        }
        
        updateNumericInput(session, "interest2", value = temp)
      }
      else if(choice == "pmt"){
        
        if(input$PV2 == 0 ||  is.null(input$PV2) ){
          temp <- input$FV2/( ( (1 + input$interest2/100)^input$n_periods2 - 1 )/ (input$interest2/100) )
        }
        else if(input$FV == 0 || is.null(input$FV)){
          temp <- input$PV2 / ( ( 1 - ( 1/ (1+input$interest2/100)^input$n_periods2 ) )/ (input$interest2/100) )
        }
        else{
          # IF YOU HAVE BOTH PV AND FV ENTERED IN, DEFAULT TO NULLING OUT FV AND SOLVE FOR N
          temp <- input$PV2 / ( ( 1 - ( 1/ (1+input$interest2/100)^input$n_periods2 ) )/ (input$interest2/100) )
          updateNumericInput(session, "FV2", value = 0)
        }
        
        updateNumericInput(session, "PMT2", value = temp)
      }
      
      x <- c(0:input$n_periods2)
      result <- input$PMT2 * ( ( ( 1 +input$interest2/100)^(x) - 1) / (input$interest2/100))
      plot(x, result)#, xlim = max(x), ylim = max(result))
      lines(x,result)
    })
    
    ############### Insurance Tab Functions ###############
    
    output$p1summary <- renderText({
      
      if(input$Recuring == 0 || is.null(input$Recuring)){
        total <- input$p1Monthly*12
      }
      else{
        #do some complicated equations factoring in the recuring costs...
        if(input$Frequency == "day"){
          extra <- input$Charges*input$Recuring*365 #total charges per year
        }
        else if(input$Frequency == "week"){
          extra <- input$Charges*input$Recuring*52 #total charges per year
        }
        else if(input$Frequency == "month"){
          extra <- input$Charges*input$Recuring*12 #total charges per year
        }
        else{
          extra <- input$Charges*input$Recuring
        }
        
        total <- (input$p1Monthly*12 + extra)
      }
      
      ##add logic here to check if total is over the deductable
      #and check other input info...
      
      paste("Your annual total would be $", total)
    })
    
    output$p2summary <- renderText({
      
      if(input$Recuring == 0 || is.null(input$Recuring)){
        total <- input$p2Monthly *12
      }
      else{
        #do some complicated equations factoring in the recuring costs...
        if(input$Frequency == "day"){
          extra <- input$Charges*input$Recuring*365 #total charges per year
        }
        else if(input$Frequency == "week"){
          extra <- input$Charges*input$Recuring*52 #total charges per year
        }
        else if(input$Frequency == "month"){
          extra <- input$Charges*input$Recuring*12 #total charges per year
        }
        else{
          extra <- input$Charges*input$Recuring
        }
        
        total <- input$p2Monthly *12 + extra
        if(total > input$p2Deduct){
          if(input$coin != 0 ){
            #if they have coinsurance, multiply remaining costs by % they pay
            total <- (total- input$p2Deduct)*(1-(input$coin/100)) + input$p2Deduct
          }
          else{
            #otherwise, just take the deductable
            total <- input$p2Deduct
          }
        }
      }
      paste("Your annual total would be $", total)
    })
    
    ############### Car Buying Tab Functions ###############
    output$payment <- renderText({
      
      dollar_format(prefix = "$", suffix = "", largest_with_cents = 1e+05, big.mark = ",", negative_parens = FALSE)
      
      rate <- (input$apr/100) / 12
      amount <- input$price - input$down - input$trade
      term <- as.integer(input$term)
      top <- rate * ((1 + rate)^term)
      bottom <- ((1 + rate)^term) - 1
      total <- amount * (top/bottom)
      total <- dollar(total)
      
      if(is.na(total)) {
        total <- dollar(0)
      }
      #h1(paste("Your payment is $", total, " per month.", sep = ""))
      paste("Your payment is ", total, " per month for ", term, " months.", sep="")
    })
    
    output$cashVsCarLoan <- renderPlot({
      
      dollar_format(prefix = "$", suffix = "", largest_with_cents = 1e+05, big.mark = ",", negative_parens = FALSE)
      
      rate <- (input$apr/100) / 12
      amount <- input$price - input$down - input$trade
      term <- as.integer(input$term)
      top <- rate * ((1 + rate)^term)
      bottom <- ((1 + rate)^term) - 1
      total <- amount * (top/bottom)
      total <- total * term
      total <- round(total, digits = 2)
      
      if(is.nan(total)) {
        total <- 0
      }
      
      primary <- "#61ABF6"
      secondary <- "#1E4398"
      
      df <- data.frame(PaymentType=c("Loan", "Cash"),
                       Amount=c(total, amount))
      ggplot(data=df, aes(x=PaymentType, y=Amount)) +
        geom_bar(
          stat="identity",
          fill = primary,
          colour = secondary) +
        labs(title = "Cash vs. Loan",
             x = "Payment Type",
             y = "Amount") +
        geom_text(
          size = 5,
          label = dollar(df$Amount),
          size = 3,
          vjust = 1.3,
          color = "white"
        ) +
        theme(
          axis.text = element_text(color = secondary, size = 12),
          axis.line = element_line(
            color = secondary,
            linetype = "solid"
          ),
          axis.title = element_text(color = secondary, size = 16),
          plot.title = element_text(
            color = secondary,
            size = 20,
            hjust = 0.5
          ),
          panel.background = element_rect(fill = "#ECF0F1", colour = "#DCE4EC"),
          panel.grid = element_line(colour = alpha("white", 0.5))
        )
      
    })
    
    output$timeValue <- renderPlot ({
      
      rate <- (input$apr/100) / 12
      amount <- input$price - input$down - input$trade
      term <- as.integer(input$term)
      top <- rate * ((1 + rate)^term)
      bottom <- ((1 + rate)^term) - 1
      total <- amount * (top/bottom)
      total <- round(total, digits = 2)
      
      if(is.nan(total)) {
        total <- 0
      }
      
      df1 <- data.frame(matrix(0, ncol = 7, nrow = term))
      ggplot()
      
      
    })
    
    #render a line chart to visualize cost over time for each plan
    output$insuranceCostPlot <- renderPlot({
      x <- c(0:input$planTimeline)
      result <- input$monthly*(x)*12
      plot(x,result)
      lines(x,result)
      
    })
    
    ############### Mortgage Tab Functions ###############
   
    output$accumInterest <- renderText({
      if (input$mortgageTerm == "Years"){
        n <- input$mortgageLength*12
      }
      else{  
        n <- input$mortgageLength
      }
      r <- r <- (input$mortgageRate/100)/12
      paid <- (r* input$mortgage * n)/(1-((1+r)^-n))
      accum <- paid - input$mortgage
      
      paste("Total Interest Accumulated: ", format(round(accum, 2), nsmall = 2))
      })
    
    output$totalPaid <- renderText({
      if (input$mortgageTerm == "Years"){
        n <- input$mortgageLength*12
      }
      else{  
        n <- input$mortgageLength
      }
      r <- r <- (input$mortgageRate/100)/12
      paid <- (r* input$mortgage * n)/(1-((1+r)^-n))
      
      paste("Total Amount Paid: ", format(round(paid, 2), nsmall = 2))
      })
    
    output$mortgageMonthly <- renderText({
      if (input$mortgageTerm == "Years"){
        n <- input$mortgageLength*12
        }
      else{  
        n <- input$mortgageLength
      }
      
      r <- (input$mortgageRate/100)/12
      numerator <- r*((1+r)^n)
      denominator <- ((1+r)^n) - 1
      monthly <- input$mortgage*(numerator/denominator)
      
      paste("Monthly Payment: ", format(round(monthly, 2), nsmall = 2))
    })
    
    output$mortgageEnd <- renderText({
      currentDate <-today()
      endDate <-Sys.Date()
      if (input$mortgageTerm == "Years"){ 
        year(endDate) <- year(currentDate)+input$mortgageLength
      }
      else{  
        month(endDate) <- month(currentDate)+input$mortgageLength
      }
      paste("Final Payment Date: ", endDate)
      })
    

}

# Run the application
shinyApp(ui = ui, server = server)

