---
# Title: Drinking Water Quality Explorer for Arsenic IC ICR Subgroup
# Description: Get SYR Data for Shiny App
# author: Natchaya Luangphairin
# date last revised: 12/27/24
# output: app.R
---

# Load packages and libraries ---------------------------------------------
if (!require("pacman")) install.packages("pacman")
p_load(ggplot2, plyr,dplyr,shiny,statsr,plotly,grid,gridExtra,
       readxl,readr,ggpubr,RColorBrewer,scales,naniar,tidyr,stringr,ggpubr,
       ggthemes,janitor,binr,mltools,gtools,formattable,foreign,utils,lubridate,
       data.table,hrbrthemes,tidyverse,zoo,purr,openxlsx, dplyr, fuzzyjoin, stringdist) 

# In R, open each .txt file in the folder "folder", naming the new dataframe as the file name. Then, filter each dataframe so that the column "SYSTEM_TYPE" equals "C". Then, save as .xlsx


# Convert downloaded SYR .txt to .csv files -------------------------------
# Function to standardize column names to uppercase with underscores
standardize_colnames <- function(df) {
  colnames(df) <- toupper(gsub(" ", "_", colnames(df)))  # Convert to uppercase and replace spaces with underscores
  return(df)
}

folder_path <- "data/raw/syr_arsenic_data"
# List all .txt files in the folder
files <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE)

# Read, standardize, and filter each file
dataframes <- map(files, ~ read_delim(.x, delim = "\t")) %>%
  set_names(tools::file_path_sans_ext(basename(files))) %>%
  map(standardize_colnames)  # Apply standardization to column names

# Save .txt files as .xlsx into the same folder
for (i in seq_along(dataframes)) {
  write.xlsx(dataframes[[i]], file.path(folder_path, paste0(names(dataframes)[i], ".xlsx")))
}


# Fix date format before merge --------------------------------------------
## Issues with SAMPLE_COLLECTION_DATE in 24-Oct-17 format, must convert before save for analysis
arsenic_syr4 <- read_xlsx("data/raw/syr_arsenic_data/arsenic_syr4.xlsx")
# Convert to Date object
arsenic_syr4 <- arsenic_syr4 %>%
  mutate(SAMPLE_COLLECTION_DATE = as.Date(SAMPLE_COLLECTION_DATE, format = "%d-%b-%y"))
write.xlsx(arsenic_syr4, "data/raw/syr_arsenic_data/arsenic_syr4.xlsx")


# Merge multiple SYR data (e.g. SYR2, SYR3, and SYR4) ----------------------------
# Function to standardize column names to uppercase with underscores
standardize_colnames <- function(df) {
  colnames(df) <- toupper(gsub(" ", "_", colnames(df)))  # Convert to uppercase and replace spaces with underscores
  return(df)
}

# Function to ensure SAMPLE_COLLECTION_DATE is in datetime format
standardize_date_column <- function(df) {
  if ("SAMPLE_COLLECTION_DATE" %in% colnames(df)) {
    df <- df %>%
      mutate(SAMPLE_COLLECTION_DATE = ymd_hms(SAMPLE_COLLECTION_DATE, quiet = TRUE) %>% 
               coalesce(ymd(SAMPLE_COLLECTION_DATE, quiet = TRUE)))  # Attempt to parse as datetime or date
  }
  return(df)
}

# List all .xlsx files in the folder
xlsx_files <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)
files <- xlsx_files[!grepl("^~\\$", basename(xlsx_files))]  # Exclude files starting with ~$

# Read, standardize, and prepare dataframes
dataframes <- map(files, ~ read_excel(.x)) %>%
  set_names(tools::file_path_sans_ext(basename(files))) %>%
  map(standardize_colnames) %>%
  map(standardize_date_column)


# Ensure all columns are character except 'VALUE' and 'DATE'
dataframes <- map(dataframes, function(df) {
  df <- df %>% mutate(across(-c(VALUE, SAMPLE_COLLECTION_DATE), as.character))
  df$VALUE <- as.numeric(df$VALUE)
  df
})


# List of contaminants to process
contaminants <- c("arsenic")

# Process and save merged dataframes for each contaminant
for (contaminant in contaminants) {
  contaminant_dataframes <- dataframes[grepl(contaminant, names(dataframes), ignore.case = TRUE)]
  
  if (length(contaminant_dataframes) > 0) {
    merged_contaminant_df <- bind_rows(contaminant_dataframes, .id = "DATA_SOURCE")
    write_csv(merged_contaminant_df, file.path(folder_path, paste0("syr_", contaminant, ".csv")))
  }
}

