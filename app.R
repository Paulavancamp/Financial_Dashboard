library("shiny")
library("shinythemes")
library(ggplot2)
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
               ), #close insurance panel
               
               tabPanel("Car Buying Calculator",
                        sidebarPanel(
                          numericInput("down", "Down Payment", value=0),
                          numericInput("trade", "Trade In Value", value=0),
                          numericInput("price", "Price of Car", value=0),
                          radioButtons("term", "Term",
                                       c("36" = "36",
                                         "48" = "48",
                                         "60" = "60",
                                         "72" = "72")),
                          numericInput("apr", "Interest Rate (% APR)", value=0),
                          submitButton("Calculate Payment")
                        ),
                        mainPanel(
                          textOutput("payment"),
                          plotOutput("cashVsCarLoan")
                          
                        )
                        
               ), #close car purchase calculator
               
               tabPanel("Mortgage Manager",
                        h3("placeholder"),
                        h4("https://www.excel-easy.com/examples/loan-amortization-schedule.html")
                        
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
    
    output$payment <- renderText({
      
      total <- 0
      
      rate <- (input$apr/100) / 12
      amount <- input$price - input$down - input$trade
      term <- as.integer(input$term)
      
      top <- rate * ((1 + rate)^term)
      bottom <- ((1 + rate)^term) - 1
      
      total <- amount * (top/bottom)
    
      
      paste("Your payment is: $", total)
    })
    
    #render a line chart to visualize cost over time for each plan
    output$insuranceCostPlot <- renderPlot({
      x <- c(0:input$planTimeline)
      result <- input$monthly*(x)*12
      plot(x,result)
      lines(x,result, )
      
    })
    

}

# Run the application
shinyApp(ui = ui, server = server)

