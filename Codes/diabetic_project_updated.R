#install.packages("fastDummies")
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

categorical_cols <- setdiff(names(Filter(is.character, data)), response_var)
numeric_cols <- setdiff(names(Filter(is.numeric, data)), response_var)

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


# Identify categorical predictors (excluding response variable)
response_var <- "readmitted"
categorical_cols <- names(Filter(is.character, data))
categorical_cols <- setdiff(categorical_cols, response_var)

# Convert them to numeric using factor encoding
data_numeric <- data %>%
  mutate(across(all_of(categorical_cols), ~ as.numeric(factor(.))))

# Check number of numeric predictors
numeric_cols <- names(Filter(is.numeric, data_numeric))
numeric_cols <- setdiff(numeric_cols, response_var)  # exclude response
num_numeric <- length(numeric_cols)
cat("Number of numeric predictors after conversion:", num_numeric, "\n")


# ----------------------------
# 3. Plot using ggplot2
# ----------------------------

library(reshape2)  # or use tidyr::pivot_longer

# ----------------------------
# 1. Subset numeric predictors
# ----------------------------
cor_data <- data_numeric %>% select(all_of(numeric_cols))

# 2. Compute correlation matrix
# ----------------------------
cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

# Convert correlation matrix to long format
cor_long <- melt(cor_matrix, varnames = c("Var1", "Var2"), value.name = "Correlation")
ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  labs(title = "Correlation Heatmap of Numeric Predictors") +
  coord_fixed()

# ----------------------------
# 4. Save high-resolution image
# ----------------------------
ggsave("correlation_heatmap_ggplot.png", width = 10, height = 8, dpi = 300)

library(ggplot2)
library(dplyr)
library(tidyr)

# Subset numeric predictors (exclude response)
numeric_data <- data_numeric %>% select(all_of(numeric_cols))

# Convert to long format
numeric_long <- numeric_data %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Boxplot: one box per variable using facet_wrap
ggplot(numeric_long, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "#1f77b4", outlier.color = "red", outlier.shape = 16) +
  facet_wrap(~Variable, scales = "free_y", ncol = 6) +  # each variable gets its own box
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_blank(),  # hide x-axis text
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  labs(title = "Boxplots of Numeric Predictors (Outliers)", y = "Values")

# Save high-resolution image
ggsave("numeric_boxplots_facet.png", width = 14, height = 10, dpi = 300)




# Subset numeric predictors (exclude response)
numeric_data <- data_numeric %>% select(all_of(numeric_cols))

# Convert to long format for ggplot
numeric_long <- numeric_data %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Histogram + density for each numeric predictor
ggplot(numeric_long, aes(x = Value)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#2ca02c", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  facet_wrap(~Variable, scales = "free") +  # each variable gets its own subplot
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  labs(title = "Distribution and Skewness of Numeric Predictors", x = "Value", y = "Density")

# Save high-resolution image
ggsave("numeric_skewness_plot.png", width = 14, height = 10, dpi = 300)


library(e1071)
# Subset numeric predictors (exclude response)
numeric_data <- data_numeric %>% select(all_of(numeric_cols))

# Calculate skewness for each numeric variable
skew_values <- sapply(numeric_data, function(x) {
  skewness(x, na.rm = TRUE)  # ignore missing values
})

# Convert to data frame for easy viewing
skew_df <- data.frame(
  Variable = names(skew_values),
  Skewness = as.numeric(skew_values)
) %>%
  arrange(desc(abs(Skewness)))  # sort by magnitude of skewness

# Display skewness values
print(skew_df)


# ----------------------------
# KNN Imputation (k = 5)
# ----------------------------
# ----------------------------
# Apply KNN Imputation (k = 5)
# ----------------------------
# ----------------------------
# Ensure categorical converted to numeric
# ----------------------------
response_var <- "readmitted"

categorical_cols <- names(Filter(is.character, data))
categorical_cols <- setdiff(categorical_cols, response_var)

# Convert categorical to numeric (factor encoding)
data_numeric <- data %>%
  mutate(across(all_of(categorical_cols), ~ as.numeric(factor(.))))

# Missing Value Handling
library(caret)

# Only numeric predictors (caret::preProcess requires numeric)
numeric_cols <- names(Filter(is.numeric, data_numeric))
data_numeric_only <- data_numeric[, numeric_cols]

# Create preProcess object with kNN imputation
preproc <- preProcess(data_numeric_only, method = "knnImpute", k = 5)

# Apply imputation
data_imputed <- predict(preproc, newdata = data_numeric_only)

# Replace original numeric columns
data_numeric[, numeric_cols] <- data_imputed

# Check missing values
cat("Total missing values after KNN imputation:", sum(is.na(data_numeric)), "\n")


library(reshape2)

# Apply spatial sign transformation
preproc_ss <- preProcess(data_imputed, method = "spatialSign")
data_ss <- predict(preproc_ss, newdata = data_imputed)


# Reshape to long format for ggplot
data_long <- melt(data_ss)


# Plot boxplots with red outliers
ggplot(data_long, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightgreen", outlier.colour = "red", outlier.size = 0.8) +
  facet_wrap(~ variable, scales = "free", ncol = 6) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 8)
  ) +
  labs(title = "Boxplots of All 46 Predictors After Spatial Sign Transformation",
       x = "Predictors",
       y = "Values")