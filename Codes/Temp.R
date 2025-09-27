# ===============================
# Diabetes project: clean + EDA + ML
# Missing values • Skewness • Resampling (SMOTE)
# ===============================

# ---------- 0) Packages ----------
need <- c(
  "tidyverse","tidymodels","themis","janitor","reshape2","e1071","vip", "ranger"
)
new <- need[!(need %in% installed.packages()[,"Package"])]
if(length(new)) install.packages(new, dependencies = TRUE)

library(tidyverse)
library(tidymodels)
library(themis)     # SMOTE
library(janitor)
library(reshape2)   # for correlation heatmap
library(e1071)      # skewness
library(vip)        # variable importance (RF/XGB)
theme_set(theme_minimal())

set.seed(123)

# ---------- 1) Load data ----------
# Try a few likely paths; change as needed.
csv_path <- "../Datasets/diabetic_data_project.csv"
if (!file.exists(csv_path)) {
  if (file.exists("../Datasets/diabetic_data_project.csv")) {
    csv_path <- "../Datasets/diabetic_data_project.csv"
  } else if (file.exists("/mnt/data/diabetic_data_project.csv")) {
    csv_path <- "/mnt/data/diabetic_data_project.csv"
  }
}

data <- read.csv(csv_path, na.strings = c("", "NA", "?"), stringsAsFactors = FALSE)
data <- janitor::clean_names(data)

# Outcome column (adjust if your target is named differently)
response_var <- "readmitted"
stopifnot(response_var %in% names(data))

# Make outcome a factor
data[[response_var]] <- factor(data[[response_var]])

# Optionally drop obvious ID columns if present
id_cols <- intersect(c("encounter_id","patient_nbr","patient_id","id"), names(data))
if (length(id_cols)) data <- data %>% select(-all_of(id_cols))

# ---------- 2) Quick dataset facts ----------
cat("Sample size:", nrow(data), "\n")
cat("Num predictors (excl. response):", ncol(data) - 1, "\n")
cat("Response levels:", paste(levels(data[[response_var]]), collapse=", "), "\n")

# ---------- 3) EDA ----------
# 3.1 Class distribution
resp_df <- data %>%
  count(!!sym(response_var), name = "Count") %>%
  mutate(Percentage = 100*Count/sum(Count))

p_resp <- ggplot(resp_df, aes(x = !!sym(response_var), y = Count, fill = Percentage)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(Count, " (", round(Percentage,1), "%)")), vjust = -0.4) +
  scale_fill_gradient(low = "#99CCFF", high = "#0066CC") +
  labs(title = "Class Distribution of Response", x = "Class", y = "Count") +
  theme(legend.position = "none") +
  ylim(0, max(resp_df$Count)*1.1)
ggsave("class_distribution.png", p_resp, width = 7, height = 5, dpi = 300)

# 3.2 Missingness per column
miss_rate <- sapply(data, function(x) mean(is.na(x)))*100
missing_df <- tibble(Column = names(miss_rate), MissingPercent = as.numeric(miss_rate)) %>%
  filter(MissingPercent > 0) %>% arrange(desc(MissingPercent))

if (nrow(missing_df) > 0) {
  p_miss <- ggplot(missing_df, aes(x = reorder(Column, MissingPercent), y = MissingPercent)) +
    geom_col(fill = "#FF6347", width = 0.7) +
    geom_text(aes(label = paste0(round(MissingPercent,1), "%")), hjust = -0.15, size = 3) +
    coord_flip() + labs(title = "Missing Values by Column", x = "Column", y = "Missing %") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  ggsave("missing_values.png", p_miss, width = 8, height = 6, dpi = 300)
}

# 3.3 Correlation heatmap (numeric only)
num_cols <- names(Filter(is.numeric, data %>% select(-all_of(response_var))))
if (length(num_cols) >= 2) {
  cor_data <- data %>% select(all_of(num_cols))
  cor_matrix <- suppressWarnings(cor(cor_data, use = "pairwise.complete.obs"))
  cor_long <- melt(cor_matrix, varnames = c("Var1","Var2"), value.name = "Correlation")
  p_cor <- ggplot(cor_long, aes(Var1, Var2, fill = Correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Correlation, 2)), size = 2.8) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    labs(title = "Correlation Heatmap (Numeric Predictors)", x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave("correlation_heatmap.png", p_cor, width = 10, height = 8, dpi = 300)
}

# 3.4 Boxplots + Hist/Density for numeric predictors
if (length(num_cols) >= 1) {
  numeric_long <- data %>% select(all_of(num_cols)) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Value")
  
  p_box <- ggplot(numeric_long, aes(x = Variable, y = Value)) +
    geom_boxplot(fill = "#1f77b4", outlier.color = "red") +
    facet_wrap(~Variable, scales = "free_y", ncol = 6) +
    labs(title = "Boxplots of Numeric Predictors", x = NULL, y = "Value") +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  ggsave("numeric_boxplots.png", p_box, width = 14, height = 10, dpi = 300)
  
  p_hist <- ggplot(numeric_long, aes(x = Value)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "#2ca02c", color = "black", alpha = 0.7) +
    geom_density(color = "red", linewidth = 0.8) +
    facet_wrap(~Variable, scales = "free") +
    labs(title = "Distributions (Histogram + Density)", x = "Value", y = "Density")
  ggsave("numeric_hist_density.png", p_hist, width = 14, height = 10, dpi = 300)
  
  # Skewness table
  skew_values <- sapply(data[num_cols], function(x) e1071::skewness(x, na.rm = TRUE))
  skew_df <- tibble(Variable = names(skew_values), Skewness = as.numeric(skew_values)) %>%
    arrange(desc(abs(Skewness)))
  write.csv(skew_df, "skewness_table.csv", row.names = FALSE)
  print(head(skew_df, 10))
}

# ---------- 4) Missingness triage: drop ultra-high-missing ----------
drop_threshold <- 40  # percent; adjust to taste
drop_cols <- names(miss_rate[miss_rate > drop_threshold])
if (length(drop_cols)) {
  message("Dropping high-missing columns (> ", drop_threshold, "%): ",
          paste(drop_cols, collapse = ", "))
  data <- data %>% select(-all_of(drop_cols))
}

# ---------- 5) Split (stratified) ----------
set.seed(123)
spl  <- initial_split(data, strata = !!sym(response_var))
train <- training(spl)
test  <- testing(spl)

# ---------- 6) Preprocessing recipe ----------
# Steps:
# - string -> factor
# - lump rare categories
# - drop zero-variance
# - impute categorical (mode)
# - impute numeric (kNN)
# - Yeo-Johnson to tame skewness
# - normalize numeric
# - one-hot encode categoricals
# - SMOTE for imbalance (applied within resamples/train only)
rec <- recipe(as.formula(paste(response_var, "~ .")), data = train) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_zv(all_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_impute_knn(all_numeric_predictors(), neighbors = 5) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_smote(all_outcomes())

# ---------- 7) Resampling ----------
set.seed(123)
folds <- vfold_cv(train, v = 5, strata = !!sym(response_var))

# Metrics: keep safe defaults for binary or multiclass
is_binary <- nlevels(train[[response_var]]) == 2
if (is_binary) {
  metrics <- metric_set(accuracy, bal_accuracy, kap, roc_auc, sens, spec, pr_auc)
} else {
  metrics <- metric_set(accuracy, bal_accuracy, kap)  # roc_auc for multiclass can be added if needed
}

# ---------- 8) Model spec ----------
# Random Forest handles multiclass + probabilities easily
rf_spec <- rand_forest(trees = 1000, mtry = tune(), min_n = tune()) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

wf <- workflow() %>% add_model(rf_spec) %>% add_recipe(rec)

# ---------- 9) Tune (optional but useful) ----------
set.seed(123)
grid <- grid_regular(
  finalize(mtry(), train %>% select(-all_of(response_var))),
  min_n(range = c(2L, 20L)),
  levels = 6
)

ctrl <- control_grid(save_pred = TRUE)
tuned <- tune_grid(wf, resamples = folds, grid = grid, metrics = metrics, control = ctrl)

autoplot(tuned) + ggtitle("Tuning Results") -> p_tune
ggsave("tuning_results.png", p_tune, width = 8, height = 5, dpi = 300)

best_params <- select_best(tuned, metric = if (is_binary) "roc_auc" else "accuracy")
best_params

final_wf <- finalize_workflow(wf, best_params)

# ---------- 10) Final fit on train; evaluate on test ----------
final_fit <- last_fit(final_wf, split = spl, metrics = metrics)

# Test metrics
test_metrics <- collect_metrics(final_fit)
print(test_metrics)
write.csv(test_metrics, "test_metrics.csv", row.names = FALSE)

# Confusion matrix
final_preds <- collect_predictions(final_fit)
cm <- conf_mat(final_preds, truth = !!sym(response_var), estimate = .pred_class)
cm
autoplot(cm, type = "heatmap") + ggtitle("Confusion Matrix (Test)") -> p_cm
ggsave("confusion_matrix.png", p_cm, width = 6, height = 5, dpi = 300)

# ROC curve (binary only)
if (is_binary) {
  roc_df <- roc_curve(final_preds, truth = !!sym(response_var), .pred_{{levels(train[[response_var]])[2]}})
  p_roc <- autoplot(roc_df) + ggtitle("ROC Curve (Test)")
  ggsave("roc_curve.png", p_roc, width = 6, height = 5, dpi = 300)
}

# ---------- 11) Variable importance (RF) ----------
final_model <- extract_fit_parsnip(final_fit$.workflow[[1]])
vip(final_model, num_features = 25) + ggtitle("Feature Importance (RF)") -> p_vip
ggsave("feature_importance.png", p_vip, width = 8, height = 6, dpi = 300)

# ---------- 12) Save preprocessed data (optional) ----------
# If you want the fully baked training/test matrices:
prep_rec <- prep(rec)
train_processed <- bake(prep_rec, new_data = train)
test_processed  <- bake(prep_rec, new_data = test)
write.csv(train_processed, "train_processed.csv", row.names = FALSE)
write.csv(test_processed,  "test_processed.csv",  row.names = FALSE)

#cat("\nDone. Key outputs saved:\n",
#    "- class_distribution.png\n",
#    "- missing_values.png (if any missing)\n",
#    "- correlation_heatmap.png (if >=2 numeric cols)\n",
#    "- numeric_boxplots.png, numeric_hist_density.png\n",
#    "- skewness_table.csv\n",
#    "- tuning_results.png\n",
#    "- test_metrics.csv, confusion_matrix.png\n",
#    "- roc_curve.png (binary only)\n",
#    "- feature_importance.png\n",
#    "- train_processed.csv, test_processed.csv\n")
