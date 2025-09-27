#install.packages("RANN")
# Load libraries
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(caret)
library(reshape2)


# Load data
data <- read.csv("../Datasets/diabetic_data_project.csv")

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





library(dplyr)
library(ggplot2)
library(caret)
library(reshape2)

set.seed(123)

response_var <- "readmitted"

# Use only predictors
pred_data <- dplyr::select(data, -all_of(response_var))

# 1) Drop columns that can't form contrasts: <2 distinct non-NA values
is_usable <- vapply(pred_data, function(x) dplyr::n_distinct(x[!is.na(x)]) >= 2, logical(1))
pred_data <- pred_data[, is_usable, drop = FALSE]

if (ncol(pred_data) < 2) {
  stop("After removing single-level/constant columns, fewer than 2 predictors remain. Check your data.")
}

# 2) One-hot encode (numeric stay numeric; character/factor become dummies)
dv <- dummyVars(~ ., data = pred_data, sep = "_", fullRank = TRUE)
X  <- as.data.frame(predict(dv, newdata = pred_data))   # <-- no contrasts error now

# 3) (Optional) Remove near-zero-variance columns (faster, clearer heatmap)
nzv <- nearZeroVar(X)
if (length(nzv)) X <- X[, -nzv, drop = FALSE]

# 4) (Optional) kNN impute if needed
if (anyNA(X)) {
  pp <- preProcess(X, method = "knnImpute", k = 5)
  X  <- predict(pp, X)
}

# 5) Correlation matrix
cor_mat <- cor(X, use = "pairwise.complete.obs")

# 6) Keep variables that show some signal (absolute correlation > 0.2 with anything else)
abs_max <- apply(abs(cor_mat), 1, function(r) {
  r <- r[is.finite(r) & !is.na(r)]
  if (!length(r)) 0 else max(r[r < 0.999], na.rm = TRUE)
})
keep <- abs_max >= 0.2
if (sum(keep) >= 2) cor_mat <- cor_mat[keep, keep, drop = FALSE]

# 7) Order by clustering for structure
d   <- as.dist(1 - abs(cor_mat))
hc  <- hclust(d, method = "average")
ord <- hc$order
cor_ord <- cor_mat[ord, ord, drop = FALSE]

# 8) Plot (upper triangle, fast raster, no text labels = smooth in RStudio)
cor_plot <- cor_ord
# cor_plot[lower.tri(cor_plot, diag = TRUE)] <- NA
cor_long <- melt(cor_plot, varnames = c("Var1", "Var2"), value.name = "Correlation")

ggplot(cor_long, aes(Var1, Var2, fill = Correlation)) +
  geom_raster(na.rm = TRUE) +
  scale_fill_gradient2(limits = c(-1, 1), midpoint = 0, low = "blue", mid = "white", high = "red") +
  coord_fixed() +
  labs(title = "Correlation Heatmap (after one-hot encoding)", x = NULL, y = NULL, fill = "r") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.grid = element_blank()
  )





################################################################################################# 

set.seed(42)

response_var <- "readmitted"

# --- 0) Identify the *original* numeric predictors only (avoid encoded categoricals)
orig_numeric <- setdiff(names(Filter(is.numeric, data)), response_var)

# We'll use your imputed frame `data_numeric` (created earlier) so there are no NAs.
# Keep response as factor for stratified splitting & resampling
data_numeric[[response_var]] <- factor(data[[response_var]])

# --- 1) Train/Test split (stratified)
idx <- createDataPartition(y = data_numeric[[response_var]], p = 0.80, list = FALSE)
train_df <- data_numeric[idx, , drop = FALSE]
test_df  <- data_numeric[-idx, , drop = FALSE]

cat("Train rows:", nrow(train_df), " | Test rows:", nrow(test_df), "\n\n")



skew_before <- sapply(train_df[, orig_numeric], function(x) e1071::skewness(x, na.rm = TRUE))
skew_before_df <- tibble(Variable = names(skew_before), Skewness = as.numeric(skew_before))


yj_vars <- orig_numeric

pp_yj <- preProcess(train_df[, yj_vars], method = "YeoJohnson")
train_df[, yj_vars] <- predict(pp_yj, train_df[, yj_vars])
test_df[,  yj_vars] <- predict(pp_yj, test_df[,  yj_vars])

skew_after <- sapply(train_df[, yj_vars], function(x) e1071::skewness(x, na.rm = TRUE))
skew_after_df <- tibble(Variable = names(skew_after), Skewness = as.numeric(skew_after))


skew_compare <- skew_before_df %>%
  rename(Before = Skewness) %>%
  inner_join(rename(skew_after_df, After = Skewness), by = "Variable") %>%
  mutate(AbsBefore = abs(Before)) %>%
  arrange(desc(AbsBefore))

print(skew_compare)


skew_long <- skew_compare %>%
  select(Variable, Before, After) %>%
  pivot_longer(cols = c(Before, After), names_to = "Stage", values_to = "Skewness")



skew_long$Variable <- factor(skew_long$Variable,
                             levels = skew_compare$Variable[order(-skew_compare$AbsBefore)])


ggplot(skew_long, aes(x = Variable, y = Skewness, fill = Stage)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  coord_flip() +
  labs(title = "Skewness Before vs After Yeo–Johnson (Training Set)",
       x = "Variable", y = "Skewness") +
  theme_minimal(base_size = 13) +
  theme(axis.text = element_text(color = "black"))
ggsave("skewness_before_after_bar.png", width = 9, height = 8, dpi = 300)




top_vars <- head(skew_compare$Variable, 12)

dens_long <- bind_rows(
  train_df[top_vars, drop = FALSE] %>% mutate(Stage = "After"),
  data_numeric[idx, top_vars, drop = FALSE]    %>% mutate(Stage = "Before")  # use the pre-transform values
) %>%
  pivot_longer(cols = all_of(top_vars), names_to = "Variable", values_to = "Value")

ggplot(dens_long, aes(x = Value)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", alpha = 0.7) +
  geom_density(size = 1) +
  facet_grid(Variable ~ Stage, scales = "free") +
  labs(title = "Distributions Before vs After Yeo–Johnson (Top-Skewed Variables)",
       x = "Value", y = "Density") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(color = "black"))
ggsave("skewness_before_after_hist.png", width = 12, height = 10, dpi = 300)


X_train <- train_df %>% select(-all_of(response_var))
y_train <- train_df[[response_var]]


train_up   <- upSample(x = X_train, y = y_train, yname = response_var)
train_down <- downSample(x = X_train, y = y_train, yname = response_var)


count_classes <- function(df, setname) {
  df %>%
    count(!!sym(response_var), name = "Count") %>%
    mutate(Percentage = round(100 * Count / sum(Count), 1),
           Set = setname,
           Class = !!sym(response_var)) %>%
    select(Set, Class, Count, Percentage)
}


dist_train <- count_classes(train_df,   "Train (original)")
dist_up    <- count_classes(train_up,   "Train (upsampled)")
dist_down  <- count_classes(train_down, "Train (downsampled)")

class_dist_all <- bind_rows(dist_train, dist_up, dist_down)


# Plot side-by-side panels for slides
ggplot(class_dist_all, aes(x = Class, y = Count, fill = Class)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(Count, " (", Percentage, "%)")),
            vjust = -0.4, size = 3.6) +
  facet_wrap(~ Set, ncol = 1, scales = "free_y") +
  labs(title = "Class Distribution: Train vs. Resampled",
       x = "Class", y = "Count") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    plot.title = element_text(face = "bold")
  ) +
  ylim(0, max(class_dist_all$Count) * 1.15)
ggsave("class_distribution_resampling.png", width = 8, height = 10, dpi = 300)



write.csv(train_df,   "train_processed_yj.csv", row.names = FALSE)
write.csv(test_df,    "test_processed_yj.csv",  row.names = FALSE)
write.csv(train_up,   "train_upsampled.csv",    row.names = FALSE)
write.csv(train_down, "train_downsampled.csv",  row.names = FALSE)


# Also keep a compact table of skewness deltas
skew_compare %>%
  transmute(Variable, Before, After, Delta = After - Before) %>%
  arrange(desc(abs(Before))) %>%
  write.csv("skewness_before_after_table.csv", row.names = FALSE)

cat("Files written:\n",
    "- skewness_before_after_bar.png\n",
    "- skewness_before_after_hist.png\n",
    "- class_distribution_resampling.png\n",
    "- train_processed_yj.csv, test_processed_yj.csv\n",
    "- train_upsampled.csv, train_downsampled.csv\n",
    "- skewness_before_after_table.csv\n")




rank_gauss_fit <- function(x_train) {
  x_train <- x_train[!is.na(x_train)]
  n <- length(x_train)
  eps <- 1/(2*n)
  Fhat <- ecdf(x_train)
  list(
    train = function(x) {
      r <- rank(x, na.last = "keep", ties.method = "average")
      qnorm((r - 3/8) / (n + 1/4))
    },
    test  = function(x) {
      p <- Fhat(x)
      p <- pmin(pmax(p, eps), 1 - eps)  # avoid +/- Inf
      qnorm(p)
    }
  )
}

train_raw <- data_numeric[idx, , drop = FALSE]
test_raw  <- data_numeric[-idx, , drop = FALSE]
train_raw[[response_var]] <- factor(data[[response_var]][idx])
test_raw[[response_var]]  <- factor(data[[response_var]][-idx])


orig_numeric <- setdiff(names(Filter(is.numeric, data)), response_var)
sk_before <- sapply(train_raw[, orig_numeric], function(x) skewness(x, na.rm = TRUE))


severe_vars <- intersect(names(which(abs(sk_before) >= 8)), orig_numeric)
if (length(severe_vars) == 0) severe_vars <- c("number_emergency", "number_outpatient")  # fallback


yj_vars <- setdiff(orig_numeric, severe_vars)

train_fix <- train_raw
test_fix  <- test_raw


if (length(yj_vars)) {
  pp_yj2 <- preProcess(train_raw[, yj_vars], method = "YeoJohnson")
  train_fix[, yj_vars] <- predict(pp_yj2, train_raw[, yj_vars])
  test_fix[,  yj_vars] <- predict(pp_yj2, test_raw[,  yj_vars])
}



for (v in severe_vars) {
  rg <- rank_gauss_fit(train_raw[[v]])
  train_fix[[v]] <- rg$train(train_raw[[v]])
  test_fix[[v]]  <- rg$test(test_raw[[v]])
}



sk_after <- sapply(train_fix[, orig_numeric], function(x) skewness(x, na.rm = TRUE))

sk_compare2 <- tibble(
  Variable = orig_numeric,
  Before = as.numeric(sk_before[orig_numeric]),
  After  = as.numeric(sk_after[orig_numeric])
) %>% mutate(Delta = After - Before) %>%
  arrange(desc(abs(Before)))

print(sk_compare2)
write.csv(sk_compare2, "skewness_before_after_table_updated.csv", row.names = FALSE)



for (v in severe_vars) {
  p0 <- mean(train_raw[[v]] == 0, na.rm = TRUE) * 100
  cat(sprintf("[%s] zeros in train: %.1f%%\n", v, p0))
}



top_vars <- head(sk_compare2$Variable[order(-abs(sk_compare2$Before))], 15)

dens_before <- train_raw[, top_vars, drop = FALSE]  %>% mutate(Stage = "Before")
dens_after  <- train_fix[, top_vars, drop = FALSE]  %>% mutate(Stage = "After")

dens_long <- bind_rows(dens_before, dens_after) %>%
  pivot_longer(cols = all_of(top_vars), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable, Stage) %>%
  mutate(
    # per-facet zoom: trim to 99th percentile to prevent stretched axes
    Value_zoom = pmin(Value, quantile(Value, 0.99, na.rm = TRUE))
  ) %>% ungroup()




ggplot(dens_long, aes(x = Value_zoom)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", alpha = 0.7) +
  geom_density(size = 1) +
  facet_grid(Variable ~ Stage, scales = "free") +
  labs(title = "Distributions Before vs After (per-facet 99th percentile trim)",
       x = "Value (trimmed at 99th pct)", y = "Density") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(color = "black"))
ggsave("skewness_before_after_hist_zoom.png", width = 12, height = 10, dpi = 300)



# Bar chart (unchanged idea)
sk_long2 <- sk_compare2 %>%
  select(Variable, Before, After) %>%
  pivot_longer(cols = c(Before, After), names_to = "Stage", values_to = "Skewness") %>%
  mutate(Variable = factor(Variable, levels = sk_compare2$Variable[order(-abs(sk_compare2$Before))]))

ggplot(sk_long2, aes(x = Variable, y = Skewness, fill = Stage)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  coord_flip() +
  labs(title = "Skewness Before vs After (RankGauss for severe; YJ for others)",
       x = "Variable", y = "Skewness") +
  theme_minimal(base_size = 13) +
  theme(axis.text = element_text(color = "black"))
ggsave("skewness_before_after_bar_updated.png", width = 9, height = 8, dpi = 300)





# ============================================
# OPTIONAL: Add Spatial Sign and compare results
# ============================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(e1071)

# 1) Fit spatial sign on *numeric* predictors using the TRAIN set
pp_ss <- preProcess(train_fix[, orig_numeric], method = "spatialSign")

# 2) Apply to train/test (keep response unchanged)
train_ss <- train_fix
test_ss  <- test_fix
train_ss[, orig_numeric] <- predict(pp_ss, train_fix[, orig_numeric])
test_ss[,  orig_numeric] <- predict(pp_ss,  test_fix[,  orig_numeric])

# 3) Skewness comparison: Hybrid vs Hybrid+SpatialSign (on TRAIN only)
sk_hybrid <- sapply(train_fix[, orig_numeric], function(x) skewness(x, na.rm = TRUE))
sk_ss     <- sapply(train_ss[,  orig_numeric], function(x) skewness(x, na.rm = TRUE))

sk_compare_ss <- tibble(
  Variable = orig_numeric,
  Before   = as.numeric(sk_hybrid[orig_numeric]),
  After    = as.numeric(sk_ss[orig_numeric])
) %>%
  mutate(Delta = After - Before) %>%
  arrange(desc(abs(Before)))

print(sk_compare_ss)
write.csv(sk_compare_ss, "skewness_hybrid_vs_spatial.csv", row.names = FALSE)

# 4) Readable histograms with per-facet 99th percentile trimming
top_vars_ss <- head(sk_compare_ss$Variable[order(-abs(sk_compare_ss$Before))], 12)

dens_before <- train_fix[, top_vars_ss, drop = FALSE] %>% mutate(Stage = "Hybrid")
dens_after  <- train_ss[,  top_vars_ss, drop = FALSE] %>% mutate(Stage = "Hybrid + SpatialSign")

dens_long_ss <- bind_rows(dens_before, dens_after) %>%
  pivot_longer(cols = all_of(top_vars_ss), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable, Stage) %>%
  mutate(Value_zoom = pmin(Value, quantile(Value, 0.99, na.rm = TRUE))) %>%
  ungroup()

ggplot(dens_long_ss, aes(x = Value_zoom)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", alpha = 0.7) +
  geom_density(size = 1) +
  facet_grid(Variable ~ Stage, scales = "free") +
  labs(title = "Hybrid vs Hybrid+SpatialSign (trimmed at 99th pct)",
       x = "Value (trimmed)", y = "Density") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(color = "black"))
ggsave("skewness_hybrid_vs_spatial_hist_zoom.png", width = 12, height = 10, dpi = 300)

# 5) Save transformed sets for presentation / optional modeling
write.csv(train_ss, "train_processed_hybrid_spatial.csv", row.names = FALSE)
write.csv(test_ss,  "test_processed_hybrid_spatial.csv",  row.names = FALSE)










































# ---- Helper: univariate outliers + plots ----
diag_outliers_univariate <- function(df, vars, method = c("IQR","MAD"),
                                     k = 1.5, c = 3.5, suffix = "hybrid_train") {
  method <- match.arg(method)
  
  long <- df[, vars, drop = FALSE] |>
    mutate(.row = dplyr::row_number()) |>
    pivot_longer(-.row, names_to = "Variable", values_to = "Value")
  
  # thresholds for each variable
  thresh <- long |>
    group_by(Variable) |>
    summarize(
      Q1  = quantile(Value, 0.25, na.rm = TRUE),
      Q3  = quantile(Value, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      Low_IQR  = Q1 - k*IQR,
      High_IQR = Q3 + k*IQR,
      Med = median(Value, na.rm = TRUE),
      MAD = mad(Value, constant = 1.4826, na.rm = TRUE),
      .groups = "drop"
    )
  
  long2 <- long |>
    left_join(thresh, by = "Variable") |>
    mutate(
      is_out = if (method == "IQR") {
        (Value < Low_IQR | Value > High_IQR)
      } else {
        z = abs(Value - Med) / ifelse(MAD == 0, NA_real_, MAD)
        z > c
      }
    )
  
  # counts table (save for slides)
  counts <- long2 |>
    group_by(Variable) |>
    summarize(Outliers = sum(is_out, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(Outliers))
  write.csv(counts, paste0("outliers_table_", tolower(method), "_", suffix, ".csv"), row.names = FALSE)
  
  # bar plot: outlier counts
  p_bar <- ggplot(counts, aes(x = reorder(Variable, Outliers), y = Outliers)) +
    geom_col(width = 0.7, fill = "#d62728") +
    coord_flip() +
    labs(title = paste0("Outliers per Variable (", method, ", ", suffix, ")"),
         x = "Variable", y = "# Outliers") +
    theme_minimal(base_size = 12) +
    theme(axis.text = element_text(color = "black"))
  ggsave(paste0("outliers_count_", tolower(method), "_", suffix, ".png"),
         p_bar, width = 8, height = 10, dpi = 300)
  
  # faceted boxplots with outliers highlighted
  p_box <- ggplot(long2, aes(x = Variable, y = Value)) +
    geom_boxplot(outlier.shape = NA, fill = "#1f77b4", alpha = 0.5) +
    geom_point(data = subset(long2, is_out %in% TRUE),
               aes(x = Variable, y = Value),
               color = "red", size = 0.6, alpha = 0.7,
               position = position_jitter(width = 0.15, height = 0)) +
    facet_wrap(~ Variable, scales = "free_y", ncol = 6) +
    labs(title = paste0("Outliers Highlighted (", method, ", ", suffix, ")"),
         x = NULL, y = "Value") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  ggsave(paste0("outliers_facets_", tolower(method), "_", suffix, ".png"),
         p_box, width = 14, height = 10, dpi = 300)
  
  invisible(list(data = long2, counts = counts))
}

# ---- Run univariate diagnostics on your Hybrid-transformed TRAIN set ----
# (uses `train_fix` and `orig_numeric` from your previous block)
diag_outliers_univariate(train_fix, orig_numeric, method = "IQR", suffix = "hybrid_train")
diag_outliers_univariate(train_fix, orig_numeric, method = "MAD", suffix = "hybrid_train")

# If you also created the spatial-sign version (`train_ss`), run the same:
if (exists("train_ss")) {
  diag_outliers_univariate(train_ss, orig_numeric, method = "IQR", suffix = "hybrid_spatial_train")
  diag_outliers_univariate(train_ss, orig_numeric, method = "MAD", suffix = "hybrid_spatial_train")
}

# ============================================
# Multivariate outliers (Mahalanobis distance)
# ============================================
# Robust if 'robustbase' is available; otherwise falls back to classical covariance.

if (!requireNamespace("robustbase", quietly = TRUE)) {
  message("Package 'robustbase' not installed; using classical covariance for Mahalanobis.")
  X <- as.matrix(train_fix[, orig_numeric])
  center <- colMeans(X, na.rm = TRUE)
  covmat <- cov(X, use = "pairwise.complete.obs")
} else {
  X <- as.matrix(train_fix[, orig_numeric])
  mcd <- robustbase::covMcd(X)  # Minimum Covariance Determinant
  center <- mcd$center
  covmat <- mcd$cov
}

d2 <- mahalanobis(X, center = center, cov = covmat)
df <- length(orig_numeric)
cut <- qchisq(0.999, df = df)        # 99.9% threshold (tighter -> fewer outliers)
flag <- d2 > cut
cat("Multivariate outliers flagged (99.9% rule):", sum(flag), "\n")

# QQ-style diagnostic vs Chi-square
qq_df <- data.frame(
  quantile = sort(d2),
  chisq_q  = qchisq(ppoints(length(d2)), df = df),
  outlier  = sort(flag)
)

p_qq <- ggplot(qq_df, aes(x = chisq_q, y = quantile)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.6) +
  geom_point(data = subset(qq_df, outlier), color = "red", alpha = 0.9, size = 1) +
  labs(title = "Robust Mahalanobis Distances vs Chi-square",
       x = expression(chi^2~quantiles), y = expression(D^2)) +
  theme_minimal(base_size = 12) +
  theme(axis.text = element_text(color = "black"))
ggsave("mahalanobis_chisq_qq.png", p_qq, width = 7, height = 5, dpi = 300)

# PC1–PC2 view with outliers highlighted
pc <- prcomp(train_fix[, orig_numeric], center = TRUE, scale. = TRUE)
pc_df <- data.frame(PC1 = pc$x[,1], PC2 = pc$x[,2], outlier = flag)

p_pc <- ggplot(pc_df, aes(PC1, PC2, color = outlier)) +
  geom_point(alpha = 0.7, size = 1) +
  scale_color_manual(values = c("FALSE" = "gray55", "TRUE" = "red")) +
  labs(title = "Multivariate Outliers in PC Space (99.9% threshold)",
       color = "Outlier") +
  theme_minimal(base_size = 12) +
  theme(axis.text = element_text(color = "black"))
ggsave("mahalanobis_pc_outliers.png", p_pc, width = 7, height = 5, dpi = 300)