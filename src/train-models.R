# Program: train-models.R
# Purpose: Fit a random forest model to each botnet dataset

# Load packages -----------------------------------------------------------
library(doMC)
library(IsolationForest)
library(tidyverse)
library(caret)

# Register parallel backend -----------------------------------------------
registerDoMC(2)

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

iso_models <- train_data %>% 
  map(~ select(.x, -label)) %>% 
  map(IsolationTrees, ntree = 100, rowSamp = TRUE, nRowSamp = 256)

# Evaluate model fits -----------------------------------------------------
rf_pred <- map2(rf_models, test_data, predict)
iso_pred <- map2(test_data, iso_models, AnomalyScore) %>% 
  map(~ ifelse(.x$outF >= 0.5, 1, 0))

eval_rf <- map2(rf_pred, test_data, ~ confusionMatrix(.x, .y[["label"]]))
eval_if <- map(test_data, "label") %>% 
  map2(iso_pred, table)
