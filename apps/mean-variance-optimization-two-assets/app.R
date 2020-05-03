library(shiny)
library(plotly)
library(DT)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Mean-variance demonstration"),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "return1Input", label = "Return of Asset 1", value = 0.2, step = 0.01),
      numericInput(inputId = "sd1Input", label = "Standard deviation of Asset 1", value = 0.3, step = 0.01),
      numericInput(inputId = "return2Input", label = "Return of Asset 2", value = 0.1, step = 0.01),
      numericInput(inputId = "sd2Input", label = "Standard deviation of Asset 2", value = 0.25, step = 0.01),
      sliderInput(inputId = "corInput", label = "Correlation", min = -1, max = 1, value = 0, step = 0.01),
      numericInput(inputId = "rfInput", label = "Risk-free rate", value = 0, step = 0.01),
    ),
    mainPanel(plotlyOutput(outputId = "efFrontOutput")
              )
    
    # br(),
    # DTOutput(outputId = "portfolioOutput")
    
  )
)

server <- function(input, output) {

  portfolio <- reactive({
    mus <- c(input$return1Input, input$return2Input)
    sds <- c(input$sd1Input, input$sd2Input)
    cor12 <- input$corInput
    cov12 <- cor12 * sds[1] * sds[2]
    
    rf <- 0
    tibble(w1 = seq(0, 1, by = 0.001),
           w2 = 1 - w1,
           return = w1 * mus[1] + w2 * mus[2],
           sd = sqrt(w1^2 * sds[1]^2 + w2^2 * sds[2]^2 + 2 * w1 * w2 * cov12),
           sharpe_ratio = (return - rf) / sd)
  })
  
  output$efFrontOutput <- renderPlotly({
    p <- portfolio() %>% 
      ggplot(aes(x = sd, y = return, label = w1)) + 
      geom_path() + 
      geom_point(data = portfolio() %>% filter(sharpe_ratio == max(sharpe_ratio) | sd == min(sd) | w1 %in% c(0, 1))) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme_minimal()
    ggplotly(p)
  })
  
  # output$portfolioOutput <- renderDT(
  #   portfolio(),
  #   filter = "top", 
  #   rownames = FALSE,
  #   options = list(searching = FALSE)
  # )
}

shinyApp(ui, server)
