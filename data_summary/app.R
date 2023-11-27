library(shiny)
library(tidyverse)
library(DT)

df <- datasets::ChickWeight %>%
  as_tibble() |>
  mutate(title = "what is this", Diet = as.character(Diet))

diets <- c(
  "kauraa" = 1,
  "kiviä" = 2,
  "ohraa" = 3,
  "heinää" = 4
)

ui <- fluidPage(
  fluidRow(
    selectInput("Diet", "Chicken diet number", choices = diets)
  ),
  fluidRow(column(5, dataTableOutput("dietsCount"))),
  fluidRow(column(10, plotOutput("viz")))
)




server <- function(input, output, session) {
  selected <- reactive(df %>% filter(Diet == input$Diet))

  output$dietsCount <- DT::renderDataTable(
    summary()
  )

  summary <- reactive({
    selected() |>
      group_by(Diet, Time) |>
      summarise(avg_weigt = mean(weight))
  })

  output$viz <- renderPlot(
    {
      summary() |>
        ggplot(aes(Time, avg_weigt, color = Diet)) +
        geom_line() +
        geom_smooth(method = "lm") +
        labs("Diets for Chicken")
    }
  )
}


shinyApp(ui, server)
