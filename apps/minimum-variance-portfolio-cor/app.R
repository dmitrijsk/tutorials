
#
# This Shiny app was written by Dmitrijs Kass (dmitrijs.kass@gmail.com)
# as a complimentary material for describing the mechanics 
# of investment portfolio mean-variance optimization.
#

library(shiny)
library(plotly)
library(dplyr)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "corInput", label = "Correlation", min = -1, max = 1, value = 0, step = 0.01)
    ),
    mainPanel(plotlyOutput(outputId = "efFrontOutput"))
  )
)

server <- function(input, output) {

  portfolio <- reactive({
    mus <- c(0.1, 0.2)
    sds <- c(0.25, 0.3)
    cor12 <- input$corInput
    cov12 <- cor12 * sds[1] * sds[2]
    
    rf <- 0
    tibble(w1 = seq(0, 1, by = 0.001),
           w2 = 1 - w1,
           return = w1 * mus[1] + w2 * mus[2],
           sd = sqrt(w1^2 * sds[1]^2 + w2^2 * sds[2]^2 + 2 * w1 * w2 * cov12),
           sharpe_ratio = (return - rf) / sd) %>% 
      mutate(return = round(return, 6),
             sd = round(sd, 6)) %>% 
      rename(`Weight of asset A` = w1,
             `Weight of asset B` = w2,
             `Portfolio return` = return,
             `Portfolio standard deviation` = sd)
  })
  
  output$efFrontOutput <- renderPlotly({
    
    points_df <- portfolio() %>% 
      filter(`Portfolio standard deviation` == min(`Portfolio standard deviation`) | `Weight of asset A` %in% c(0, 1))
    
    min_variance_df <- portfolio() %>% 
      filter(`Portfolio standard deviation` == min(`Portfolio standard deviation`))
    
    p <- portfolio() %>% 
      ggplot(aes(x = `Portfolio standard deviation`, 
                 y = `Portfolio return`, 
                 label = `Weight of asset A`)) + 
      geom_path() + 
      geom_point(data = points_df) +
      geom_text(data = min_variance_df, mapping = aes(label = "Minimum\nvariance\nportfolio"), nudge_x = -0.025) +
      annotate(geom = "text", x = 0.3, y = 0.2-0.01, label = "Asset B") +
      annotate(geom = "text", x = 0.25+0.02, y = 0.1, label = "Asset A") +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme_minimal()
    
    ggplotly(p)
    
  })

}

shinyApp(ui, server)
