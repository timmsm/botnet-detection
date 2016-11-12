# Program: create-features.R
# Purpose: Create features suitable for classification and clustering

# Load packages -----------------------------------------------------------
library(tidyverse)

# Specify column types ----------------------------------------------------
col_specs <- cols(
  StartTime = col_character(),
  Dur = col_double(),
  Proto = col_character(),
  SrcAddr = col_character(),
  Sport = col_character(),
  Dir = col_character(),
  DstAddr = col_character(),
  Dport = col_character(),
  State = col_character(),
  sTos = col_double(),
  dTos = col_double(),
  TotPkts = col_integer(),
  TotBytes = col_double(),
  SrcBytes = col_double(),
  Label = col_character()
)

# Define cleaning functions -----------------------------------------------
add_group_col <- function(data, group, expr) {
  data %>% 
    group_by_(.dots = group) %>% 
    mutate_(.dots = expr) %>% 
    ungroup()
}

process_binetflow <- . %>% 
  set_names(tolower(names(.))) %>% 
  mutate(
    pct_src = srcbytes / totbytes,
    label   = stringr::str_extract(tolower(label), "normal|botnet|background")
  ) %>% 
  add_group_col("srcaddr", c("pct_oneway" = "mean(dir == '->', na.rm = TRUE)")) %>% 
  add_group_col(c("srcaddr", "proto"), c("n_proto_pkts" = "n()")) %>% 
  add_group_col(c("srcaddr", "dstaddr", "dport"), c("n_connections" = "n()")) %>% 
  separate(state, c("s_state", "d_state"), fill = "right") %>% 
  add_group_col(c("s_state", "d_state"), c("n_states" = "n()")) %>% 
  select(-starttime, -proto, -srcaddr, -dstaddr, -totbytes, -srcbytes, -stos, 
         -dtos, -sport, -dport, -s_state, -d_state, -dir, -totpkts)

# Parse and clean data ----------------------------------------------------
binetflow_files <- list.files("data/raw", "\\.binetflow$", full.names = TRUE)

capture_data <- binetflow_files %>% 
  map(read_csv, col_types = col_specs) %>% 
  map(process_binetflow) %>% 
  set_names(gsub("^data/raw/([a-z0-9\\-]+)\\.binetflow", "data/\\1.rds", binetflow_files))

# Write processed files to disk -------------------------------------------
walk2(capture_data, names(capture_data), write_rds)
