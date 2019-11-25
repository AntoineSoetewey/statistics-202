#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Statistics 202 - Simple linear regression"),
    h4(tags$a(href="https://www.antoinesoetewey.com/", "Antoine Soetewey")),
    withMathJax(),

    sidebarLayout(
        sidebarPanel(
            textInput("x", "x", value = "80, 90, 80, 70, 77, 65", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
            textInput("y", "y", value = "850, 1000, 750, 650, 850, 675", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
            hr(),
            checkboxInput("se", "Add confidence interval around the regression line", TRUE),
            hr(),
            # textInput("pred", "Prediction of y when x = ", value = "100, 110, 100, 90, 97, 85", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
            # hr(),
            HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/statistics-202/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/statistics-202/blob/master/app.R">code</a>. Back to <a href="https://www.antoinesoetewey.com/">www.antoinesoetewey.com</a>.</p>'),
        ),

        mainPanel(
            tags$b("Your data:"),
            DT::dataTableOutput('tbl'),
            br(),
            uiOutput("data"),
            br(),
            tags$b("Compute parameters by hand:"),
            uiOutput("by_hand"),
            br(),
            tags$b("Compute parameters in R:"),
            verbatimTextOutput("summary"),
            br(),
            tags$b("Regression plot:"),
            uiOutput("results"),
            plotlyOutput("plot"),
            br(),
            br()
        )
    )
)

server <- function(input, output) {
    extract <- function(text) {
        text <- gsub(" ", "", text)
        split <- strsplit(text, ",", fixed = FALSE)[[1]]
        as.numeric(split)
    }
    
    # Data output
    output$tbl = DT::renderDataTable({
        y <- extract(input$y)
        x <- extract(input$x)
        DT::datatable(data.frame(x, y), options = list(lengthChange = FALSE))
    })
    
    output$data <- renderUI({
        y <- extract(input$y)
        x <- extract(input$x)
        if (anyNA(x) | length(x) < 2 | anyNA(y) | length(y) < 2) {
            "Invalid input or not enough observations"
        } else if (length(x) != length(y)) {
            "Number of observations must be equal for x and y"
        } else {
            withMathJax(
                paste0("\\(\\bar{x} =\\) ", round(mean(x), 3)),
                br(),
                paste0("\\(\\bar{y} =\\) ", round(mean(y), 3)),
                br(),
                paste0("\\(n =\\) ", length(x))
            )
        }
    })
    
    output$by_hand <- renderUI({
        y <- extract(input$y)
        x <- extract(input$x)
        fit <- lm(y ~ x)
        withMathJax(
            paste0("\\(\\hat{\\beta}_1 = \\dfrac{\\big(\\sum^n_{i = 1} x_i y_i \\big) - n \\bar{x} \\bar{y}}{\\sum^n_{i = 1} (x_i - \\bar{x})^2} = \\) ", round(fit$coef[[2]], 3)),
            br(),
            paste0("\\(\\hat{\\beta}_0 = \\bar{y} - \\hat{\\beta}_1 \\bar{x} = \\) ", round(fit$coef[[1]], 3)),
            br(),
            br(),
            paste0("\\( \\Rightarrow y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x = \\) ", round(fit$coef[[1]], 3), " + ", round(fit$coef[[2]], 3), "\\( x \\)")
            )
    })
    
    output$summary <- renderPrint({
        y <- extract(input$y)
        x <- extract(input$x)
        fit <- lm(y ~ x)
        summary(fit)
    })
    
    output$results <- renderUI({
        y <- extract(input$y)
        x <- extract(input$x)
        fit <- lm(y ~ x)
        withMathJax(
            paste0("Adj. \\( R^2 = \\) ", round(summary(fit)$adj.r.squared, 3),
                   ", \\( \\beta_0 = \\) ", round(fit$coef[[1]], 3),
                   ", \\( \\beta_1 = \\) ", round(fit$coef[[2]], 3),
                   ", P-value ", "\\( = \\) ", signif(summary(fit)$coef[2,4], 3))
        )
    })
    
    output$plot <- renderPlotly({
        y <- extract(input$y)
        x <- extract(input$x)
        fit <- lm(y ~ x)
        dat <- data.frame(x, y)
        p <- ggplot(dat, aes(x = x, y = y)) +
            geom_point() +
            stat_smooth(method = "lm", se = input$se) +
            theme_minimal()
        ggplotly(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
