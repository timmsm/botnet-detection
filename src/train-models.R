library(IsolationForest)
library(tidyverse)

set.seed(6246)

fit_iso_forest <- function(data) {
  
  model <- data %>% 
    select(-label) %>% 
    IsolationTrees(ntree = 100, rowSamp = TRUE, nRowSamp = 256)
  
  scores <- data %>% 
    select(-label) %>%
    AnomalyScore(model)
  
  data %>% 
    mutate(classification = ifelse(scores$outF <= 0.5, "normal", "botnet"))
  
}

capture_files <- list.files("data", "\\.rds$", full.names = TRUE)

capture_data <- capture_files %>% 
  map(read_rds) %>% 
  set_names(gsub("^data/(.+)\\.rds$", "\\1", capture_files)) %>% 
  parallel::mclapply(fit_iso_forest, mc.cores = 5)

capture_data %>% 
  map(~ table(.x[["label"]], .x[["classification"]]))
