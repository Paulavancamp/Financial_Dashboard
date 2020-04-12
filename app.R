library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)
library(lubridate)
#library("DT")

lastused <- "FV"

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    theme = shinytheme("flatly"),

    navbarPage("Financial Tool Dashboard",
               # Application title
               tabPanel("Future Value",
                        # Sidebar with a slider input for number of bins
                        sidebarLayout(
                            sidebarPanel(
                                numericInput("n_periods", "Periods",0, min =0),
                                numericInput("interest", "Interest (%)",0),
                                numericInput("PV", "Present Value",0),
                                numericInput("FV", "Future Value", value = 0),
                                #numericInput("PMT", "Payment", value = 0),
                                submitButton("controller","Submit")

                            ),
                            # Show a plot of the generated distribution
                            mainPanel(
                                plotOutput("distPlot")
                            )
                        )
               ),
               tabPanel("Insurance Comparator",
                        sidebarPanel(
                          h4("Plan Specific Information"),
                          textInput("plan1", "Plan 1 Name:", "general"),
                          numericInput("monthly", "Monthly Premium", value=0),
                          
                          h4("Related Recurring Costs"),
                          numericInput("recuring", "Estimated Cost", value=0),
                          numericInput("number", "Number of Charges", value=0),
                          selectInput("rate", "Each:", c("day", "week", "month", "year")),
                          
                          h4("One Time Accident Costs"),
                          numericInput("singleExpense", "Total Estimated Cost", value=0),
                          
                          submitButton("update graph")
                        ),
                        mainPanel(
                          sliderInput("planTimeline", "Years:", 1, 50, 10),
                          plotOutput("insuranceCostPlot")

                        )
                        #this needs serious work...............................
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
                          # h3(textOutput("vals"))
                          #plotOutput("timeValue")
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



    observe({

        # figure out what to calculate
        if(input$PV == 0 || input$PV.isNull)
            {
            lastused <- "PV"
        }
        else if(input$FV == 0 || input$FV.isNull)
        {
            lastused <- "FV"
        }

        # Calculate some shit
        if(lastused == "FV"){
            temp <- (input$PV) * (1 + (input$interest)/100)^(input$n_periods)
            updateNumericInput(session, "FV", value = temp)
        }
        else if(lastused == "PV") {
            temp = input$FV / (1+ input$interest)^(input$n_periods)
            updateNumericInput(session, "PV", value = temp)
        }

    })

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x <- c(0:input$n_periods)

        #bins <- seq(min(0), max(input$n_periods), length.out = input$n_periods + 1)
        result <- input$PV * (1 + (input$interest)/100)^(x)
        plot(x,result)
        lines(x,result)
    })
    
    # output$vals <- renderText({
    #   
    #   rate <- (input$apr/100) / 12
    #   amount <- input$price - input$down - input$trade
    #   term <- as.integer(input$term)
    #   
    #   top <- rate * ((1 + rate)^term)
    #   bottom <- ((1 + rate)^term) - 1
    #   
    #   total <- amount * (top/bottom)
    #   
    #   overallTotal <- total * term
    #   
    #   
    #   paste("Rate:", rate, 
    #         "Amount:", amount, 
    #         "Term:", term, 
    #         "Top:", top, 
    #         "Bottom:", bottom, 
    #         "Total Per Month:", total,
    #         "Overall Total:", overallTotal, sep=" ")
    #   
    #   
    # })
    
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
      if (input$mortgageTerm == "years"){
        accum <- input$mortgageRate/input$mortgageLength*input$mortgage
      }
      else{  accum <- input$mortgageRate/(input$mortgageLength*12)*input$mortgage}
      
      paste("Total Interest Accumulated: ", format(round(accum, 2), nsmall = 2))
      })
    
    output$totalPaid <- renderText({
      if (input$mortgageTerm == "years"){
        accum <- input$mortgageRate/input$mortgageLength*input$mortgage
      }
      else{  accum <- input$mortgageRate/(input$mortgageLength*12)*input$mortgage}
      paid <- accum+input$mortgage
      
      paste("Total Amount Paid: ", format(round(paid, 2), nsmall = 2))
      })
    
    output$mortgageMonthly <- renderText({
      if (input$mortgageTerm == "years"){
        accum <- input$mortgageRate/input$mortgageLength*input$mortgage
        totalCost <- accum+input$mortgage
        monthly <- totalCost/input$mortgageLength/12
      }
      else{  
        accum <- input$mortgageRate/(input$mortgageLength*12)*input$mortgage
        totalCost <- accum+input$mortgage
        monthly <- totalCost/input$mortgageLength
      }
      
      paste("Monthly Payment: ", format(round(monthly, 2), nsmall = 2))
    })
    
    ##NEeds work...
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
    
    #output$amoritizationTable <- renderTable(    )
    

}

# Run the application
shinyApp(ui = ui, server = server)

