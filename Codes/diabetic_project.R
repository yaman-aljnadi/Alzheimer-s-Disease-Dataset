# Load libraries
library(dplyr)
library(ggplot2)
library(DataExplorer)



# Load data
data <- read.csv("diabetic_data_project.csv")

# ------------------------
# Data Exploration
# ------------------------

# Convert "?" into NA
data[data == "?"] <- NA

# 1. Sample size
sample_size <- nrow(data)
cat("1. Sample size:", sample_size, "\n")

# 2. Number of predictors (drop response var)
response_var <- "readmitted"
num_predictors <- ncol(data) - 1  #removing the readmission column
# 3. Number of response variables
num_response <- 1

cat("2. Number of predictors:", num_predictors, "\n")
cat("3. Number of response variables:", num_response, "\n")

# 4. Type of data (exclude response variable)
response_var <- "readmitted"

categorical_cols <- setdiff(names(Filter(is.character, data_subset)), response_var)
numeric_cols <- setdiff(names(Filter(is.numeric, data_subset)), response_var)

num_categorical <- length(categorical_cols)
num_numeric <- length(numeric_cols)

cat("4. Data types -> Categorical:", num_categorical, ", Numeric:", num_numeric, "\n\n")


# 5. Balanced or unbalanced (distribution of response)
# Calculate class counts and percentages
response_df <- data %>%
  group_by(!!sym(response_var)) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Plot
ggplot(response_df, aes(x = !!sym(response_var), y = Count, fill = Percentage)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(Count, " (", round(Percentage, 1), "%)")),
            vjust = -0.5, size = 4) +
  scale_fill_gradient(low = "#99CCFF", high = "#0066CC") +  # optional gradient
  labs(title = "Class Distribution of Response Variable",
       x = "Class",
       y = "Count") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.position = "none"
  ) +
  ylim(0, max(response_df$Count) * 1.1)  # add space for labels

# Optional: save high-resolution plot
ggsave("class_distribution_plot.png", width = 7, height = 5, dpi = 300)

#Missing Value Calcualtion 
# Convert "?" to NA
data[data == "?"] <- NA
# Calculate missing percentage per column
missing_pct <- colSums(is.na(data)) / nrow(data) * 100

# Keep only columns with missing values and sort descending
missing_df <- data.frame(
  Column = names(missing_pct),
  MissingPercent = as.numeric(missing_pct)
) %>%
  filter(MissingPercent > 0) %>%
  arrange(desc(MissingPercent))

# ----------------------------
# Plot Missing Values
# ----------------------------
ggplot(missing_df, aes(x = reorder(Column, MissingPercent), y = MissingPercent)) +
  geom_col(fill = "#FF6347", width = 0.7) +  # Tomato color
  geom_text(aes(label = paste0(round(MissingPercent, 1), "%")),
            hjust = -0.2, size = 4, color = "black") +  # Label outside bars
  coord_flip() +
  labs(
    title = "Missing Values by Column",
    subtitle = "Only columns with missing values are shown",
    x = "Columns",
    y = "Missing Percentage (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))  # Add space for labels

# Save high-resolution image for slides
ggsave("missing_values_plot.png", width = 8, height = 6, dpi = 300)



