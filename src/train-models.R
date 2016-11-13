# Program: train-models.R
# Purpose: Fit a random forest model to each botnet dataset

# Load packages -----------------------------------------------------------
library(doMC)
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

models <- train_data %>% 
  map(~ train(label ~ ., data = .x, trControl = ctrl, method = "rf"))

# Evaluate model fits -----------------------------------------------------
preds <- map2(models, test_data, predict)
confusion_matrices <- map2(preds, test_data, ~ confusionMatrix(.x, .y[["label"]]))


