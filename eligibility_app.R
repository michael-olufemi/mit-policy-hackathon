library(shiny)
library(tidyverse)

# Function to load and process datasets
load_and_process_data <- function() {
  demographics <- read_csv("demographics.csv") %>%
    rename(cdc_no = CDCNo, aggregate_sentence_in_months = `Aggregate Sentence in Months`)

  prior_commitments <- read_csv("prior_commitments.csv") %>%
    rename(cdc_no = CDCNo)

  current_commitments <- read_csv("current_commitments.csv") %>%
    rename(cdc_no = CDCNo, description = `Offense Description`, offense_category = `Offense Category`)

  # Merge datasets
  merged_data <- demographics %>%
    left_join(
      prior_commitments %>% count(cdc_no, name = "prior_count"),
      by = "cdc_no"
    ) %>%
    left_join(
      current_commitments %>% select(cdc_no, description, offense_category),
      by = "cdc_no"
    ) %>%
    mutate(
      repeat_offender = ifelse(!is.na(prior_count) & prior_count > 0, "Yes", "No"),
      aggregate_sentence_in_months = as.numeric(aggregate_sentence_in_months),
      offense_severity = case_when(
        str_detect(description, regex("petty theft|vandalism|trespass|shoplifting", ignore_case = TRUE)) ~ "Low Severity",
        str_detect(description, regex("drug|firearm|burglary|robbery", ignore_case = TRUE)) ~ "Medium Severity",
        str_detect(description, regex("murder|rape|kidnap|torture", ignore_case = TRUE)) ~ "High Severity",
        TRUE ~ "Unknown"
      ),
      has_enhancements = ifelse(is.na(offense_category), "No", "Yes")
    )

  return(merged_data)
}

calculate_eligibility_score <- function(data) {
  data %>%
    mutate(
      # Severity Points
      severity_points = case_when(
        offense_severity == "Low Severity" ~ 3,
        offense_severity == "Medium Severity" ~ 1,
        offense_severity == "High Severity" ~ -3,
        TRUE ~ 0
      ),
      # Repeat Offender Points
      repeat_points = case_when(
        repeat_offender == "No" ~ 5,
        prior_count > 0 & prior_count <= 3 ~ -2,
        prior_count > 3 ~ -5,
        TRUE ~ 0
      ),
      # Sentence Points
      sentence_points = case_when(
        aggregate_sentence_in_months <= 12 ~ 4,
        aggregate_sentence_in_months > 12 & aggregate_sentence_in_months <= 60 ~ 2,
        aggregate_sentence_in_months > 60 & aggregate_sentence_in_months <= 120 ~ 0,
        aggregate_sentence_in_months > 120 ~ -3,
        TRUE ~ 0
      ),
      # Enhancements Points
      enhancement_points = case_when(
        has_enhancements == "No" ~ 2,
        has_enhancements == "Yes" & prior_count <= 2 ~ -1,
        has_enhancements == "Yes" & prior_count > 2 ~ -3,
        TRUE ~ 0
      ),
      # Total Score
      total_score = severity_points + repeat_points + sentence_points + enhancement_points,

      # Eligibility Status
      eligibility_status = case_when(
        total_score >= 7 ~ "Eligible",
        total_score >= 3 & total_score < 7 ~ "Borderline",
        total_score < 3 ~ "Ineligible",
        TRUE ~ "Uncertain"
      )
    )
}


# Load and preprocess data
merged_data <- load_and_process_data()
merged_data <- calculate_eligibility_score(merged_data)

# Define UI
ui <- fluidPage(
  titlePanel("Eligibility Determination System"),

  sidebarLayout(
    sidebarPanel(
      textInput("cdc_no", "Enter CDC Number:", placeholder = "e.g., A12345"),
      actionButton("check_eligibility", "Check Eligibility")
    ),

    mainPanel(
      h3("Eligibility Results"),
      textOutput("eligibility_result"),
      h4("Score Breakdown"),
      tableOutput("score_breakdown"),
      h4("Eligibility Explanation"),
      textOutput("eligibility_reason")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive data filtered by CDC number
  offender_data <- eventReactive(input$check_eligibility, {
    req(input$cdc_no)
    merged_data %>% filter(cdc_no == input$cdc_no)
  })

  # Eligibility result
  output$eligibility_result <- renderText({
    data <- offender_data()
    if (nrow(data) == 0) {
      return("No offender found with the provided CDC Number.")
    }
    # Return the eligibility status once
    paste("Eligibility Status:", data$eligibility_status[1])
  })

  # Score breakdown
  output$score_breakdown <- renderTable({
    data <- offender_data()
    if (nrow(data) == 0) {
      return(NULL) # Return NULL if no data
    }
    data %>%
      select(cdc_no, severity_points, repeat_points, sentence_points, enhancement_points, total_score) %>%
      head(1) # Ensure only one row is shown
  })

  # Eligibility rationale
  output$eligibility_reason <- renderText({
    data <- offender_data()
    if (nrow(data) == 0) {
      return("")
    }
    reasons <- c()
    if (data$offense_severity[1] == "High Severity") reasons <- c(reasons, "High-severity offense")
    if (data$repeat_offender[1] == "Yes") reasons <- c(reasons, "Repeat offender")
    if (data$aggregate_sentence_in_months[1] > 120) reasons <- c(reasons, "Sentence exceeds 120 months")
    if (data$has_enhancements[1] == "Yes") reasons <- c(reasons, "Sentence enhancements applied")

    if (length(reasons) == 0) {
      return("Reason: Meets all criteria for eligibility.")
    }
    paste("Reason:", paste(reasons, collapse = "; "))
  })
}

# Run the App
shinyApp(ui = ui, server = server)
