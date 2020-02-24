library(datasets)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Shiny Text"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId="dataset",
                        label="choose a dataset",
                        choices = c("abalone","heart","human","Iris")),
            
            numericInput(inputId = "obs",
                         label = "Number of observations to view:",
                         value = 10 )
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Summary", verbatimTextOutput("summary")),
                tabPanel("Table", tableOutput ("view"),
                         tabPanel("Histogram",plotOutput("plot")))
            )
            
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    abalone <- read.csv('abalone.csv',stringsAsFactors = FALSE)
    heart <- read.csv('heart.csv',stringsAsFactors = FALSE)
    human <- read.csv('human.csv',stringsAsFactors = FALSE)
    Iris <- read.csv('Iris.csv',stringsAsFactors = FALSE)
    
    datasetInput <- reactive({
        switch(input$dataset,
               "abalone" = abalone,
               "heart" = heart,
               "human" = human,
               "Iris"= Iris )
        
        dist <- switch(input$dist,
                       norm = rnorm,
                       unif = runif,
                       lnorm = rlnorm,
                       exp = rexp,
                       rnorm)
        
        dist(input$n)
    })
    
    output$plot <- renderPlot({
        dist <- input$dist
        n <- input$n
        
        hist(d(),
             main = paste("r", dist, "(", n, ")", sep = ""),
             col = "#75AADB", border = "white")
    })
    
    output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
    })
    
    output$view <- renderTable({
        head(datasetInput(), n = input$obs)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

