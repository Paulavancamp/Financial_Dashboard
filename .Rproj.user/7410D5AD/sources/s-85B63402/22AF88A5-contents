library("shiny")
#library("DT")

lastused <- "FV"

# Define UI for application that draws a histogram
ui <- fluidPage(

    navbarPage("Paula and Louis's tool" ,
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
               tabPanel("Other"

               )

    )
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

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })

}

# Run the application
shinyApp(ui = ui, server = server)

