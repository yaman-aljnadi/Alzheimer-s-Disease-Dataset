
#install.packages("naniar")
#install.packages("Amelia")
#install.packages("VIM")


# Load necessary libraries
library(tidyverse)   # for data manipulation and visualization
library(skimr)       # for quick data summaries

# 1. Load the data
data <- read.csv("alzheimers_disease_data_with_outliers.csv")

# 2. Basic checks
# View the first few rows
head(data)

# Check structure: column names, types, etc.
str(data)

# Overview of dataset: rows, columns
dim(data)


# 4. Check missing values
colSums(is.na(data))   # count of missing values per column
# or visualize:
library(Amelia)
missmap(data, main = "Missing Data Map")

# 5. Summary statistics for numeric variables
summary(data)

# 6. Identify categorical vs continuous variables
# Convert all character columns to factor
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)

# Now check again
categorical_vars <- names(Filter(is.factor, data))
numeric_vars <- names(Filter(is.numeric, data))

cat("Categorical variables:\n")
print(categorical_vars)

cat("Numeric variables:\n")
print(numeric_vars)

cat("Number of categorical variables:", length(categorical_vars), "\n")
cat("Number of numeric variables:", length(numeric_vars), "\n")

# Select numeric columns
numeric_data <- data[, numeric_vars]

# Function to check if all values are integers
is_integer_var <- sapply(numeric_data, function(x) all(x == floor(x), na.rm = TRUE))

# Continuous variables: numeric but not integer
continuous_vars <- names(is_integer_var)[!is_integer_var]
# Integer variables: numeric and all values are integers
integer_vars <- names(is_integer_var)[is_integer_var]

# Print counts
cat("Number of continuous numeric variables:", length(continuous_vars), "\n")
cat("Number of integer numeric variables:", length(integer_vars), "\n")

# Optionally, see the variable names
cat("Continuous variables:\n"); print(continuous_vars)
cat("Integer variables:\n"); print(integer_vars)


# Check class distribution
table(data$Diagnosis)

# Relative percentages
prop.table(table(data$Diagnosis))

# Set up plotting grid (adjust rows/cols if needed)
par(mfrow=c(5,6), mar=c(2,2,2,1))  # 5 rows, 6 cols = 30 slots
# Remove PatientID from numeric predictors
numeric_vars_model <- setdiff(numeric_vars, "PatientID")


for (var in numeric_vars_model) {
  hist(data[[var]],
       main = var,
       xlab = "",
       col = "lightblue",
       breaks = 20)
}


# Reset plotting grid
par(mfrow=c(3,3), mar=c(4,4,2,1))  # 3 rows, 3 cols = 9 slots

for (var in categorical_vars) {
  counts <- table(data[[var]])
  barplot(counts,
          main = var,
          col = "lightgreen",
          las = 2)  # vertical labels
}

# Set up plotting grid for boxplots
par(mfrow=c(5,6), mar=c(3,3,2,1))  # adjust rows/cols and margins

for (var in numeric_vars_model) {
  boxplot(data[[var]],
          main = var,
          col = "lightblue",
          horizontal = TRUE,
          outcol = "red",      # outlier color
          pch = 16,            # solid circle
          cex = 1.2)           # size of outlier points
}

#Missing Data Handling with KNN (k=5)

library(VIM)

# Exclude PatientID from imputation (identifier)
data_knn <- data[, setdiff(names(data), "PatientID")]

# KNN imputation (k = 5 neighbors)
set.seed(123)
data_imputed <- kNN(data_knn, k = 5, imp_var = FALSE)  # imp_var=FALSE prevents extra columns

# Now data_imputed has no missing values
# Verify
sapply(data_imputed, function(x) sum(is.na(x)))

# Check missing values per column
missing_counts <- sapply(data_imputed, function(x) sum(is.na(x)))
print(missing_counts)

# Total missing values in the dataset
total_missing <- sum(missing_counts)
cat("Total missing values in the dataset:", total_missing, "\n")

#Conversion using one hot encoding 
#Convert the categorical into numerical predictors
library(caret)
dummies <- dummyVars(~ ., data = data[, categorical_vars])
categorical_numeric <- predict(dummies, newdata = data[, categorical_vars])

# Combine numeric predictors and dummy-coded categorical variables
processed_data <- cbind(data_imputed[, numeric_vars_model], categorical_numeric)

# Check data types of all variables
var_types <- sapply(processed_data, class)
print(var_types)

summary(processed_data)

print(table(processed_data$Diagnosis))

# Correlation Matrix 
library(corrplot)

# Compute correlation matrix
predictors <- processed_data[, setdiff(names(processed_data), "Diagnosis")]
cor_matrix <- cor(predictors, use = "pairwise.complete.obs")

# Open a plotting window (RStudio does this automatically)
windows()   # for Windows
# quartz() # for macOS
# X11()    # for Linux

# Plot
corrplot(cor_matrix, method = "color", type = "full",
         tl.cex = 0.7, tl.col = "black",
         addCoef.col = "black", number.cex = 0.6, diag = FALSE)


# 7. Correlation matrix 
library(corrplot)

png("correlation_before_processing.png", width=1200, height=1000)
corrplot(cor_matrix_original, method = "color", type = "full",
         tl.cex = 0.7, tl.col = "black",
         addCoef.col = "black", number.cex = 0.6)
dev.off()

cat("Plot saved as correlation_before_processing.png in your working directory.\n")



#Box-cox Tarnsformation for reducing skewness 
# Load required libraries

library(e1071)   
library(ggplot2)
library(gridExtra)

# Select numeric predictors (exclude PatientID and Diagnosis if needed)
numeric_vars_model <- setdiff(numeric_vars, c("PatientID", "Diagnosis"))
numeric_data_original <- data[, numeric_vars_model]

# Check skewness before BoxCox
skew_before <- apply(numeric_data_original, 2, skewness, na.rm = TRUE)
print("Skewness before Box-Cox:")
print(skew_before)

# Preprocessing with BoxCox
preProc <- preProcess(numeric_data_original, method = "BoxCox")
numeric_data_boxcox <- predict(preProc, numeric_data_original)

# Check skewness after BoxCox
skew_after <- apply(numeric_data_boxcox, 2, skewness, na.rm = TRUE)
print("Skewness after Box-Cox:")windows()
print(skew_after)

windows()
# ---- Plot before vs after for a few variables ----
plot_list <- list()
vars_to_plot <- names(numeric_data_original)[1:6]   # first 6 variables (adjust as needed)

for (var in vars_to_plot) {
  p1 <- ggplot(data.frame(x = numeric_data_original[[var]]), aes(x)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    ggtitle(paste(var, "- Before BoxCox"))
  
  p2 <- ggplot(data.frame(x = numeric_data_boxcox[[var]]), aes(x)) +
    geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
    ggtitle(paste(var, "- After BoxCox"))
  
  plot_list[[length(plot_list)+1]] <- gridExtra::arrangeGrob(p1, p2, ncol = 2)
}

# Display all plots
gridExtra::grid.arrange(grobs = plot_list, ncol = 1)


