# Load required libraries
library(tidyverse)
library(janitor)

# Load the datasets
demographics <- read_csv("demographics.csv") %>%
  clean_names()

prior_commitments <- read_csv("prior_commitments.csv") %>%
  clean_names()

current_commitments <- read_csv("current_commitments.csv") %>%
  clean_names()

# Merge data
merged_data <- demographics %>%
  left_join(
    prior_commitments %>% count(cdc_no, name = "prior_count"),
    by = "cdc_no"
  ) %>%
  mutate(
    repeat_offender = ifelse(!is.na(prior_count) & prior_count > 0, "Yes", "No")
  )

# View basic structure
glimpse(merged_data)


# Count offense categories
offense_category_stats <- merged_data %>%
  group_by(offense_category) %>%
  summarise(
    count = n(),
    avg_sentence_length = mean(aggregate_sentence_in_months, na.rm = TRUE),
    median_sentence_length = median(aggregate_sentence_in_months, na.rm = TRUE),
    repeat_offender_rate = mean(repeat_offender == "Yes", na.rm = TRUE)
  ) %>%
  arrange(desc(count))

# Print summary
print(offense_category_stats)

# Count by ethnicity
ethnicity_stats <- merged_data %>%
  group_by(ethnicity) %>%
  summarise(
    count = n(),
    avg_sentence_length = mean(aggregate_sentence_in_months, na.rm = TRUE),
    median_sentence_length = median(aggregate_sentence_in_months, na.rm = TRUE),
    repeat_offender_rate = mean(repeat_offender == "Yes", na.rm = TRUE)
  ) %>%
  arrange(desc(count))

# Print summary
print(ethnicity_stats)

# Repeat offender stats
repeat_offender_stats <- merged_data %>%
  group_by(repeat_offender) %>%
  summarise(
    count = n(),
    avg_sentence_length = mean(aggregate_sentence_in_months, na.rm = TRUE),
    median_sentence_length = median(aggregate_sentence_in_months, na.rm = TRUE)
  )

# Print summary
print(repeat_offender_stats)


# Sentence length distribution
sentence_length_stats <- merged_data %>%
  summarise(
    min_sentence = min(aggregate_sentence_in_months, na.rm = TRUE),
    max_sentence = max(aggregate_sentence_in_months, na.rm = TRUE),
    avg_sentence = mean(aggregate_sentence_in_months, na.rm = TRUE),
    median_sentence = median(aggregate_sentence_in_months, na.rm = TRUE)
  )

# Print summary
print(sentence_length_stats)


# Define cohorts based on offense category
merged_data <- merged_data %>%
  mutate(
    cohort_offense = case_when(
      offense_category == "Crimes Against Persons" ~ "High Severity",
      offense_category %in% c("Property Crimes", "Drug Crimes") ~ "Low Severity",
      TRUE ~ "Other"
    ),
    cohort_ethnicity = case_when(
      ethnicity %in% c("Mexican", "Black", "White", "American Indian") ~ "High Sentencing",
      ethnicity %in% c("Filipino", "Pacific Islander") ~ "Low Sentencing",
      TRUE ~ "Moderate Sentencing"
    ),
    cohort_repeat = ifelse(repeat_offender == "Yes", "Repeat Offender", "Non-Repeat Offender"),
    cohort_sentence_length = case_when(
      aggregate_sentence_in_months <= 12 ~ "Short Sentence",
      aggregate_sentence_in_months > 12 & aggregate_sentence_in_months <= 60 ~ "Moderate Sentence",
      aggregate_sentence_in_months > 60 ~ "Long Sentence"
    )
  )

# View cohort summary
cohort_summary <- merged_data %>%
  group_by(cohort_offense, cohort_ethnicity, cohort_repeat, cohort_sentence_length) %>%
  summarise(
    count = n(),
    avg_sentence = mean(aggregate_sentence_in_months, na.rm = TRUE),
    repeat_rate = mean(repeat_offender == "Yes", na.rm = TRUE)
  ) %>%
  arrange(desc(count))

print(cohort_summary)


# Create a contingency table
repeat_ethnicity_table <- table(merged_data$repeat_offender, merged_data$ethnicity)

# Perform the chi-square test
chi_sq_test <- chisq.test(repeat_ethnicity_table)

# Print results
print(chi_sq_test)

# Expected counts (for reference)
print(chi_sq_test$expected)


# Perform ANOVA
anova_result <- aov(aggregate_sentence_in_months ~ ethnicity, data = merged_data)

# Print summary of ANOVA
summary(anova_result)

# Tukey HSD for pairwise comparisons
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Perform ANOVA
anova_offense <- aov(aggregate_sentence_in_months ~ offense_category, data = merged_data)

# Print summary of ANOVA
summary(anova_offense)

# Tukey HSD for pairwise comparisons
tukey_offense <- TukeyHSD(anova_offense)
print(tukey_offense)


# Create a contingency table
repeat_offense_table <- table(merged_data$repeat_offender, merged_data$offense_category)

# Perform the chi-square test
chi_sq_offense <- chisq.test(repeat_offense_table)

# Print results
print(chi_sq_offense)

# Expected counts (for reference)
print(chi_sq_offense$expected)

# Cohort-specific summaries
cohort_insights <- merged_data %>%
  group_by(cohort_offense, cohort_ethnicity, cohort_repeat, cohort_sentence_length) %>%
  summarise(
    count = n(),
    avg_sentence = mean(aggregate_sentence_in_months, na.rm = TRUE),
    median_sentence = median(aggregate_sentence_in_months, na.rm = TRUE),
    repeat_rate = mean(repeat_offender == "Yes", na.rm = TRUE)
  ) %>%
  arrange(desc(count))

# Print cohort-specific insights
print(cohort_insights)
############




ggplot(merged_data, aes(x = ethnicity, y = aggregate_sentence_in_months, fill = ethnicity)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +
  labs(
    title = "Sentence Length Distribution by Ethnicity",
    x = "Ethnicity",
    y = "Sentence Length (Months)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")


library(reshape2)
heatmap_data <- merged_data %>%
  group_by(ethnicity, offense_category) %>%
  summarise(MeanSentence = mean(aggregate_sentence_in_months, na.rm = TRUE)) %>%
  dcast(ethnicity ~ offense_category, value.var = "MeanSentence")

library(ggplot2)
ggplot(melt(heatmap_data), aes(x = variable, y = ethnicity, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    title = "Heatmap of Sentence Length Disparities",
    x = "Offense Category",
    y = "Ethnicity",
    fill = "Mean Sentence"
  ) +
  theme_minimal()


ggplot(merged_data, aes(x = ethnicity, y = aggregate_sentence_in_months, fill = ethnicity)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1, outlier.color = "red") +
  labs(
    title = "Distribution of Sentence Lengths by Ethnicity",
    x = "Ethnicity",
    y = "Sentence Length (Months)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

ggplot(merged_data, aes(x = ethnicity, y = aggregate_sentence_in_months, fill = ethnicity)) +
  geom_violin(trim = TRUE, alpha = 0.7) +
  geom_boxplot(width = 0.1, outlier.color = "red") +
  labs(
    title = "Violin Plot of Sentence Length by Ethnicity",
    x = "Ethnicity",
    y = "Sentence Length (Months)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")




