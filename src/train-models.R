# Program: train-models.R
# Purpose: Fit a random forest model to each botnet dataset

# Load packages -----------------------------------------------------------
library(ClusterR)
library(doMC)
library(IsolationForest)
library(tidyverse)
library(caret)

# Register parallel backend -----------------------------------------------
registerDoMC(5)

# Read data files ---------------------------------------------------------
capture_files <- list.files("data", "\\.rds$", full.names = TRUE)

capture_data <- capture_files %>% 
  map(read_rds) %>% 
  set_names(gsub("^data/(.+)\\.rds$", "\\1", capture_files))

# Partition data into training/test sets ----------------------------------
set.seed(6246)

labeled_data <- map(capture_data, ~ filter(.x, label != "background"))

partitions <- labeled_data %>% 
  map(~ createDataPartition(.x[["label"]], p = 0.75, list = FALSE))

train_data <- map2(labeled_data, partitions, ~ .x[.y, ])
test_data  <- map2(labeled_data, partitions, ~ .x[-.y, ])

# Fit models --------------------------------------------------------------
ctrl <- trainControl("repeatedcv", 10, 10)

rf_models <- train_data %>% 
  map(~ train(label ~ ., data = .x, trControl = ctrl, method = "rf"))

if_models <- capture_data %>% 
  map(~ select(.x, -label)) %>% 
  map(IsolationTrees, ntree = 100, rowSamp = TRUE, nRowSamp = 256)

km_models <- test_data %>% 
  map(~ select(.x, -label) %>% as.matrix() %>% center_scale()) %>% 
  map(KMeans_rcpp, clusters = 2, threads = 5)
  
# Evaluate model fits -----------------------------------------------------
rf_pred <- map2(rf_models, test_data, predict)
if_pred <- map(capture_data, ~ select(.x, -label)) %>% 
  map2(if_models, AnomalyScore) %>% 
  map(~ ifelse(.x$outF >= 0.5, 1, 0))

km_pred <- test_data %>% 
  map(~ select(.x, -label) %>% as.matrix() %>% center_scale()) %>%
  map2(km_models, ~ predict_KMeans(.x, .y[["centroids"]]))

eval_rf <- map2(rf_pred, test_data, ~ confusionMatrix(.x, .y[["label"]]))
eval_if <- map(capture_data, "label") %>% 
  map2(if_pred, table)

eval_km <- map(test_data, "label") %>% 
  map2(km_pred, table)

# Save models and evaluations ---------------------------------------------
walk2(rf_models, names(rf_models), ~ write_rds(.x, sprintf("data/models/rf_%s.rds", .y)))
walk2(if_models, names(if_models), ~ write_rds(.x, sprintf("data/models/if_%s.rds", .y)))
walk2(km_models, names(km_models), ~ write_rds(.x, sprintf("data/models/km_%s.rds", .y)))

walk2(eval_rf, names(eval_rf), ~ write_rds(.x, sprintf("data/evals/rf_%s.rds", .y)))
walk2(eval_if, names(eval_if), ~ write_rds(.x, sprintf("data/evals/if_%s.rds", .y)))
walk2(eval_km, names(eval_km), ~ write_rds(.x, sprintf("data/evals/km_%s.rds", .y)))
