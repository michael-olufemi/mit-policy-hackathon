library(shiny)
library(ggplot2)
library(dplyr)

# Load Data
load_data <- function() {
  demographics <- read.csv("demographics.csv")
  prior_commitments <- read.csv("prior_commitments.csv")
  current_commitments <- read.csv("current_commitments.csv")

  demographics <- demographics %>%
    rename(cdc_no = CDCNo, ethnicity = Ethnicity, controlling_offense = Controlling.Offense,
           description = Description, sentence_type = Sentence.Type,
           aggregate_sentence_in_months = Aggregate.Sentence.in.Months,
           offense_category = Offense.Category)

  prior_commitments <- prior_commitments %>%
    rename(cdc_no = CDCNo)

  current_commitments <- current_commitments %>%
    rename(cdc_no = CDCNo)

  demographics <- demographics %>%
    left_join(prior_commitments %>% count(cdc_no, name = "prior_offense_count"), by = "cdc_no") %>%
    mutate(prior_offense_count = replace_na(prior_offense_count, 0))

  # Remove outliers: Sentence length greater than 5000
  demographics <- demographics %>%
    filter(aggregate_sentence_in_months <= 5000)

  demographics
}

merged_data <- load_data()

# UI
ui <- fluidPage(
  titlePanel("Offender Data Visualizations"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "plot_type",
        "Select Plot Type:",
        choices = c(
          "Histogram of Variable" = "histogram",
          "Barplot of Two Variables" = "barplot",
          "Boxplot of Sentence Length by Variable" = "boxplot"
        )
      ),
      conditionalPanel(
        condition = "input.plot_type == 'histogram'",
        selectInput(
          "hist_variable",
          "Select Variable:",
          choices = c("ethnicity", "offense_category", "prior_offense_count"),
          selected = "ethnicity"
        )
      ),
      conditionalPanel(
        condition = "input.plot_type == 'barplot'",
        selectInput(
          "x_variable",
          "Select X Variable:",
          choices = c("ethnicity", "offense_category", "prior_offense_count"),
          selected = "ethnicity"
        ),
        selectInput(
          "fill_variable",
          "Select Fill Variable:",
          choices = c("ethnicity", "offense_category", "prior_offense_count"),
          selected = "offense_category"
        )
      ),
      conditionalPanel(
        condition = "input.plot_type == 'boxplot'",
        selectInput(
          "box_variable",
          "Select Grouping Variable:",
          choices = c("ethnicity", "offense_category", "prior_offense_count"),
          selected = "ethnicity"
        )
      )
    ),
    mainPanel(
      plotOutput("main_plot")
    )
  )
)

# Server
server <- function(input, output) {
  output$main_plot <- renderPlot({
    data <- merged_data

    if (input$plot_type == "histogram") {
      ggplot(data, aes_string(x = input$hist_variable)) +
        geom_bar(fill = "steelblue", color = "white") +
        labs(title = paste("Histogram of", input$hist_variable), x = input$hist_variable, y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$plot_type == "barplot") {
      ggplot(data, aes_string(x = input$x_variable, fill = input$fill_variable)) +
        geom_bar(position = "dodge", color = "black") +
        scale_fill_brewer(palette = "Set2") +
        labs(
          title = paste("Barplot of", input$x_variable, "by", input$fill_variable),
          x = input$x_variable, y = "Count", fill = input$fill_variable
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$plot_type == "boxplot") {
      ggplot(data, aes_string(x = input$box_variable, y = "aggregate_sentence_in_months")) +
        geom_boxplot(outlier.color = "red", fill = "lightblue", color = "darkblue", alpha = 0.7) +
        labs(
          title = paste("Boxplot of Sentence Length by", input$box_variable),
          x = input$box_variable, y = "Sentence Length (Months)"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}

# Run App
shinyApp(ui = ui, server = server)
