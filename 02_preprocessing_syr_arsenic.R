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


# Clean data --------------------------------------------------------------
## Codes and values ref (SDWA_REF_CODE_VALUES.csv) -------------------------
sdwa_ref_code_values <- read_csv("data/SDWA_REF_CODE_VALUES.csv")
names(sdwa_ref_code_values) <- tolower(names(sdwa_ref_code_values))
sdwa_ref_code_values$value_type <- tolower(sdwa_ref_code_values$value_type)

# Arsenic Sites -----------------------------------------------------------
## from Matthew
arsenic_demo <- read_csv("data/SDWIS_arsenic_demo.csv")
arsenic_demo <- arsenic_demo %>%
  clean_names(case = "snake")
#write_csv(arsenic_demo,"data/SDWIS_arsenic_demo.csv")

# Six Year Review ---------------------------------------------------------
syr_arsenic_raw <- read_xlsx("data/raw/syr_arsenic_data/syr_arsenic.xlsx") # data-preprocessed (see SYR_data_R_script.R)
syr_arsenic <- syr_arsenic_raw %>%
  clean_names(case = "snake")
syr_arsenic <- syr_arsenic %>%
  mutate(
    analyte_code = "1005",
    analyte_name = "ARSENIC",
    value = ifelse(unit == "UG/L" | unit == "ug/l", value/1000, value),
    unit = "MG/L",
    pws_type_code = case_when(
      system_type == "C" ~ "CWS",
      system_type == "NTNC" ~ "NTNCWS",
      TRUE ~ system_type
    )
  )

syr_arsenic <- syr_arsenic %>%
  mutate(analyte_code = as.character(analyte_code),
         water_facility_id = as.character(water_facility_id),
         detect = as.character(detect),
         sample_type_code = as.character(sample_type_code),
         laboratory_assigned_id = as.character(laboratory_assigned_id),
         six_year_id = as.character(six_year_id),
         sample_id = as.character(sample_id),
         detection_limit_unit = as.character(detection_limit_unit),
         detection_limit_code = as.character(detection_limit_code),
         presence_indicator_code = as.character(presence_indicator_code)
         )

#write_csv(syr_arsenic, "data/raw/syr_arsenic_data/syr_arsenic.csv")

syr_arsenic <- read_csv("data/raw/syr_arsenic_data/syr_arsenic.csv") 
syr_arsenic <- syr_arsenic %>%
  mutate(analyte_code = as.character(analyte_code),
         water_facility_id = as.character(water_facility_id),
         detect = as.character(detect),
         sample_type_code = as.character(sample_type_code),
         laboratory_assigned_id = as.character(laboratory_assigned_id),
         six_year_id = as.character(six_year_id),
         sample_id = as.character(sample_id),
         detection_limit_unit = as.character(detection_limit_unit),
         detection_limit_code = as.character(detection_limit_code),
         presence_indicator_code = as.character(presence_indicator_code)
  )

# Treatment Info ----------------------------------------------------------
## from Matthew which he most likely downloaded from https://ofmpub.epa.gov/apex/sfdw/f?p=108:1:0::NO:1
## select Facilities and select column to include all to get comprehensive system info in one csv
## may need to filter and download in batches if file too large can throw error, then let join the two later
facility_report <- read_csv("data/cleaned/facility_report_20241230.csv")
facility_report <- facility_report %>%
  clean_names(case = "snake") %>%
  rename(pwsid = pws_id)

# Get unique combinations of pwsid, treatment_process, treatment_objective, and first_reported_date
## fix: Error in nchar(x) : invalid multibyte string, element 1 
facility_report <- facility_report %>%
  mutate(
    facility_name = iconv(facility_name, from = "", to = "UTF-8"),
    facility_id = iconv(facility_id, from = "", to = "UTF-8")
  )

## get treatment info
arsenic_demo_facility_treatment <- facility_report %>%
  filter(
    # Ensure facility_deactivation_date is either NA or >= 2000-01-01
    is.na(facility_deactivation_date) | dmy(facility_deactivation_date) >= as.Date("2000-01-01")
  ) %>%
  filter(
    facility_type_description == "Treatment Plant" & 
      facility_type_code == "TP" & 
      (str_detect(tolower(facility_name), "arsenic") |	
         str_detect(tolower(facility_id), "treated") |
         str_detect(tolower(treatment_process), "inonovative") |
         str_detect(tolower(treatment_objective), "inorganics removal") |
         str_detect(tolower(facility_id), "t") | 
         str_detect(tolower(facility_name), "treatment plant") | 
         str_detect(tolower(facility_name), "treatment facility") | 
         str_detect(tolower(facility_name), "plant") | 
         str_detect(tolower(facility_name), "tp"))   # Case-insensitive match
  ) %>%
  group_by(pwsid, pws_name, epa_region, epa_region_code, activity_status, activity_status_code, gw_or_sw_code, service_connections_count, owner_type_code, pws_type_code, phone_number, is_wholesaler, seller_pwsid, seller_pws_name, seller_treatment_description, email_address, zip_code) %>%
  summarise(
    treatment_objective_code = if (all(is.na(treatment_objective_code))) {
      NA_character_
    } else {
      paste(unique(treatment_objective_code[!is.na(treatment_objective_code)]), collapse = ", ")
    },
    treatment_process_code = if (all(is.na(treatment_process_code))) {
      NA_character_
    } else {
      paste(unique(treatment_process_code[!is.na(treatment_process_code)]), collapse = ", ")
    },
    treatment_objective = if (all(is.na(treatment_objective))) {
      NA_character_
    } else {
      paste(unique(treatment_objective[!is.na(treatment_objective)]), collapse = ", ")
    },
    treatment_process = if (all(is.na(treatment_process))) {
      NA_character_
    } else {
      paste(unique(treatment_process[!is.na(treatment_process)]), collapse = ", ")
    },
    facility_deactivation_date = {
      # Filter for "arsenic" facilities first
      arsenic_dates <- facility_deactivation_date[
        str_detect(tolower(facility_name), "arsenic")
      ]
      if (length(arsenic_dates) > 0) {
        # Use the date (or NA) associated with "arsenic" facility
        if (all(is.na(arsenic_dates))) {
          NA_character_  # Return NA if all arsenic-related dates are NA
        } else {
          paste(unique(arsenic_dates[!is.na(arsenic_dates)]), collapse = ", ")
        }
      } else {
        # Fallback to any other valid dates
        fallback_dates <- facility_deactivation_date[!is.na(facility_deactivation_date)]
        if (length(fallback_dates) > 0) {
          paste(unique(fallback_dates), collapse = ", ")
        } else {
          NA_character_  # Return NA if no valid date is available
        }
      }
    },
    .groups = "drop"
  )


# Joining aggregated treatment data with master data
syr_arsenic <- syr_arsenic %>%
  mutate(analyte_code = as.character(analyte_code),
         water_facility_id = as.character(water_facility_id),
         detect = as.character(detect),
         sample_type_code = as.character(sample_type_code),
         laboratory_assigned_id = as.character(laboratory_assigned_id),
         six_year_id = as.character(six_year_id),
         sample_id = as.character(sample_id),
         detection_limit_unit = as.character(detection_limit_unit),
         detection_limit_code = as.character(detection_limit_code),
         presence_indicator_code = as.character(presence_indicator_code)
  )

syr_arsenic_cleaned <- syr_arsenic %>%
  left_join(arsenic_demo_facility_treatment, by = "pwsid") 

#write_csv(syr_arsenic_cleaned,"data/cleaned/syr_arsenic.csv")


# PDF to excel table ------------------------------------------------------
# Load libraries
p_load(pdftools, tidyverse, writexl)

# Function to process multi-page PDF and combine into a single table
process_multi_page_pdf_to_excel <- function(pdf_path, excel_output_path) {
  # Extract text from all pages of the PDF
  pdf_text_data <- pdf_text(pdf_path)
  
  # Initialize an empty list to store data from each page
  all_pages_data <- list()
  
  # Loop through each page and process the data
  for (i in seq_along(pdf_text_data)) {
    # Split text into lines for the current page
    lines <- str_split(pdf_text_data[[i]], "\n")[[1]]
    
    # Convert lines to a dataframe
    page_data <- tibble(raw_text = lines) %>%
      filter(str_trim(raw_text) != "") %>% # Remove empty lines
      mutate(
        # Split the raw text into columns based on observed patterns
        columns = map(raw_text, ~ str_split(.x, "\\s{2,}")[[1]])
      ) %>%
      unnest_wider(columns, names_sep = "_") # Expand into multiple columns
    
    # Add to the list of page data
    all_pages_data[[i]] <- page_data
  }
  
  # Combine all pages into a single dataframe
  combined_data <- bind_rows(all_pages_data)
  
  # Rename columns dynamically
  colnames(combined_data) <- paste0("Column_", seq_along(colnames(combined_data)))
  
  # Fill missing values for merged cells
  combined_data <- combined_data %>%
    fill(everything(), .direction = "down")
  
  # Save as Excel
  write_xlsx(combined_data, excel_output_path)
  message("Excel file created at: ", excel_output_path)
}

# Define paths
pdf_path <- "data/raw/demo_sites/Arsenic-costs-600-R-11-090-table.pdf"
excel_output_path <- "data/raw/demo_sites/arsenic_demo_sites.xlsx"

# Execute the function
process_multi_page_pdf_to_excel(pdf_path, excel_output_path)


# Arsenic Demo Site PWSID -------------------------------------------------
syr_arsenic_cleaned <- read_csv("data/cleaned/syr_arsenic.csv")
syr_arsenic_cleaned <- syr_arsenic_cleaned %>%
  mutate(analyte_code = as.character(analyte_code),
         water_facility_id = as.character(water_facility_id),
         detect = as.character(detect),
         sample_type_code = as.character(sample_type_code),
         laboratory_assigned_id = as.character(laboratory_assigned_id),
         six_year_id = as.character(six_year_id),
         sample_id = as.character(sample_id),
         detection_limit_unit = as.character(detection_limit_unit),
         detection_limit_code = as.character(detection_limit_code),
         presence_indicator_code = as.character(presence_indicator_code)
  )
arsenic_demo_site <- read_xlsx("data/cleaned/arsenic_demo_sites.xlsx")
arsenic_demo_site$system_name <- arsenic_demo_site$site_name

# Helper function to clean system names
clean_system_name <- function(name) {
  name %>%
    tolower() %>%  # Convert to lowercase
    trimws() %>%  # Remove leading and trailing whitespace
    gsub("^village of |^town of |^city of ", "", ., ignore.case = TRUE) %>%  # Remove prefixes
    gsub("\\bsystems?\\b", "system", ., ignore.case = TRUE) %>%  # Normalize singular/plural
    gsub("\\bcompany\\b", "co", ., ignore.case = TRUE) %>%  # Normalize "company" to "co"
    gsub("\\bindependent\\b", "", ., ignore.case = TRUE) %>%  # Remove "independent"
    gsub("\\bdevelopment\\b", "", ., ignore.case = TRUE) %>%  # Remove "development"
    gsub("\\bdistrict\\b", "", ., ignore.case = TRUE) %>%  # Remove "district"
    gsub("\\bmutal domestic\\b", "", ., ignore.case = TRUE) %>%  # Remove "mutal domestic"
    gsub("\\s*-\\s*", " ", ., ignore.case = TRUE) %>% # Normalize hyphens
    gsub("[-]", "", ., ignore.case = TRUE) %>%  # Remove remaining hyphens
    gsub("\\s+", " ", .)  # Normalize multiple spaces to a single space
}

# Tokenize names into words for matching
tokenize_name <- function(name) {
  name %>%
    strsplit("\\s+") %>%  # Split by one or more spaces
    unlist() %>%          # Flatten the list into a vector
    trimws()              # Remove leading and trailing spaces from each token
}

# Preprocess system names for arsenic_demo_site
arsenic_demo_site <- arsenic_demo_site %>%
  mutate(
    system_name_clean = clean_system_name(system_name),  # Clean names
    two_letter_id_clean = tolower(trimws(gsub("\\s*\\(.*\\)", "", two_letter_id))), # Remove parentheses and content inside
    system_name_tokens = lapply(system_name_clean, tokenize_name),  # Tokenize names
    state_code = trimws(state_code)  # Ensure state_code has no extra spaces
  )

# Preprocess system names for syr_arsenic
syr_arsenic_cleaned_names <- syr_arsenic_cleaned %>%
  mutate(
    system_name_clean = clean_system_name(system_name),  # Clean names
    system_name_tokens = lapply(system_name_clean, tokenize_name),  # Tokenize names
    state_code = trimws(state_code)  # Ensure state_code has no extra spaces
  )

# Perform fuzzy matching with two_letter_id consideration
arsenic_demo_site_matched_pwsid <- arsenic_demo_site %>%
  fuzzy_left_join(
    syr_arsenic_cleaned_names,
    by = c(
      "system_name_clean" = "system_name_clean",  # Fuzzy match on cleaned system_name
      "state_code" = "state_code"                # Exact match on state_code
    ),
    match_fun = list(
      function(x, y) stringdist(x, y, method = "jw") <= 0.3,  # Fuzzy match with higher tolerance
      `==`                                                   # Exact match for state_code
    )
  ) %>%
  group_by(system_name_clean.x) %>%
  mutate(
    # Use the two_letter_id_clean column from the left dataset
    two_letter_id_match = grepl(two_letter_id_clean, system_name_clean.y, ignore.case = TRUE),
    # Check for token overlap between system_name_clean.x and system_name_clean.y
    token_match = sapply(system_name_tokens.x, function(tokens_x) {
      any(tokens_x %in% unlist(system_name_tokens.y))
    }),
    # Check if one name is a substring of the other
    substring_match = grepl(system_name_clean.x, system_name_clean.y, ignore.case = TRUE) |
      grepl(system_name_clean.y, system_name_clean.x, ignore.case = TRUE),
    # Assign priority to matches
    match_priority = case_when(
      substring_match & token_match ~ 0,        # Strongest match
      two_letter_id_match & token_match ~ 1,    # Two-letter ID w/ token match
      two_letter_id_match ~ 2,                  # Token match only
      substring_match ~ 3,                      # Substring match only
      token_match ~ 4,                          # Token match only
      TRUE ~ 5                                  # Fallback
    )
  ) %>%
  # Prioritize based on match priority
  slice_min(match_priority, with_ties = FALSE) %>%
  # Break ties using string distance
  slice_min(stringdist(system_name_clean.x, system_name_clean.y, method = "jw"), with_ties = FALSE) %>%
  ungroup()

# Apply corrections directly to the data (MANUAL OVERWRITE)
arsenic_demo_site_matched_pwsid <- arsenic_demo_site_matched_pwsid %>%
  mutate(
    # Correct PWSID and system_name for Arizona Water Company
    pwsid = case_when(
      site_name == "Arizona Water Company - Rimrock" & two_letter_id == "Rimrock (RR)" ~ "AZ0413046",
      site_name == "Arizona Water Company - Valley Vista" & two_letter_id == "Valley Vista (VV)" ~ "AZ0413114",
      site_name == "Town of Caneadea" & two_letter_id == "Houghton (HT)" ~ "NY0200320",
      site_name == "Upper Bodfish Well CH2-A" ~ "CA1510026",
      site_name == "Tohono O’odham Utility Authority" ~ "090411311",
      site_name == "Sunset Ranch Development" ~ "ID7100088",
      site_name == "Springbrook Mobile Home Park" ~ "ME0003639",
      site_name == "South Truckee Meadows General" ~ "NV0000215",
      site_name == "Richmond School District" ~ "CA1800573",
      site_name == "Queen Anne’s County" ~ "MD1170014",
      site_name == "Northeaster Elementary School" ~ "IN2890826",
      site_name == "Hot Springs Mobile Home Park" ~ "UTAH02055",
      site_name == "Golden Hills Community Service" ~ "CA1510045",
      site_name == "Desert Sands Mutual Domestic" ~ "NM3510007",
      site_name == "Chateau Estates Mobile Home Park" ~ "OH1200412",
      site_name == "Charette Mobile Home Park" ~ "VT0005621",
      site_name == "Buckeye Lake Head Start Building" ~ "OH4557212",
      TRUE ~ pwsid  # Keep original value otherwise	
    ),
    system_name.y = case_when(
      site_name == "Arizona Water Company - Rimrock" & two_letter_id == "Rimrock (RR)" ~ "AZ WATER CO-RIMROCK",
      site_name == "Arizona Water Company - Valley Vista" & two_letter_id == "Valley Vista (VV)" ~ "AZ WATER CO VALLEY VISTA",
      site_name == "Town of Caneadea" & two_letter_id == "Houghton (HT)" ~ "HOUGHTON WATER DISTRICT",
      site_name == "Upper Bodfish Well CH2-A" ~ "CWS - UPPER BODFISH WATER SYSTEM",
      site_name == "Tohono O’odham Utility Authority" ~ "Cyprus Tohono Corporation",
      site_name == "Sunset Ranch Development" ~ "SUNSET TRAILER RANCH",
      site_name == "Springbrook Mobile Home Park" ~ "SPRINGBROOK MOBILE HOME COURT",
      site_name == "South Truckee Meadows General" ~ "SOUTH TRUCKEE MEADOWS GID",
      site_name == "Richmond School District" ~ "RICHMOND ELEMENTARY SCHOOL",
      site_name == "Queen Anne’s County" ~ "QUEEN ANNE'S COUNTY DPW",
      site_name == "Northeaster Elementary School" ~ "NORTHEASTERN ELEMENTARY",
      site_name == "Hot Springs Mobile Home Park" ~ "HOT SPRINGS TRAILER COURT",
      site_name == "Golden Hills Community Service" ~ "GOLDEN HILLS CSD",
      site_name == "Desert Sands Mutual Domestic" ~ "DESERT SANDS MDWCA",
      site_name == "Chateau Estates Mobile Home Park" ~ "CHATEAU ESTATES LTD.",
      site_name == "Charette Mobile Home Park" ~ "CHARETTE WATER SYSTEM",
      site_name == "Buckeye Lake Head Start Building" ~ "BUCKEYE LAKE HEAD START",
      TRUE ~ system_name.y  # Keep original value otherwise	
    )
  )

# Rename and clean up columns for output
arsenic_demo_site_matched_pwsid <- arsenic_demo_site_matched_pwsid %>%
  select(
    pwsid,
    state_code_site = state_code.x, 
    state_code = state_code.x,
    two_letter_id,
    site_name, 
    system_name_demo_site = system_name.x, 
    system_name = system_name.y, 
    technology_media,
    vendor,
    flowrate_gpm,
    as_ug_l,
    fe_ug_l,
    ph
  )

#write_csv(arsenic_demo_site_matched_pwsid, "data/cleaned/arsenic_demo_site_pwsid.csv")

syr_arsenic_demo_cleaned <- syr_arsenic_cleaned_names %>%
  filter(pwsid %in% arsenic_demo_site_matched_pwsid$pwsid) %>%
  left_join(arsenic_demo_site_matched_pwsid, by = c("pwsid")) 

#write_csv(syr_arsenic_demo_cleaned, "data/cleaned/syr_arsenic_demo_site.csv")

# Subset the dataframe to include only the specified columns
syr_arsenic_demo_cleaned_subset <- syr_arsenic_demo_cleaned %>%
  mutate(value_ug_l = value * 1000) %>%
  select(
    pwsid,
    pws_name,
    epa_region,
    epa_region_code,
    data_source, 
    analyte_code, 
    analyte_name, 
    primacy_code, 
    state_code = state_code.x, 
    activity_status,
    activity_status_code,
    system_name = site_name, 
    system_type, 
    retail_population_served, 
    adjusted_total_population_served, 
    service_connections_count,
    source_water_type, 
    gw_or_sw_code,
    owner_type_code,
    pws_type_code = pws_type_code.x,
    water_facility_id,
    water_facility_type,  
    source_type_code, 
    sample_type_code, 
    sample_collection_date, 
    detect, 
    value = value, 
    unit, 
    value_mg_l = value,
    value_ug_l,
    pws_name, 
    treatment_objective_code, 
    treatment_process_code, 
    facility_deactivation_date, 
    treatment_objective_code, 
    treatment_process_code, 
    treatment_objective, 
    treatment_process, 
    facility_deactivation_date, 
    site_name, 
    system_name_demo_site, 
    technology_media, 
    vendor, 
    flowrate_gpm, 
    as_ug_l, 
    fe_ug_l, 
    ph,
    is_wholesaler, 
    seller_pwsid, 
    seller_pws_name, 
    seller_treatment_description, 
    phone_number, 
    email_address,
    zip_code
  )

#write_csv(syr_arsenic_demo_cleaned_subset, "data/cleaned/syr_arsenic_demo_site_cleaned.csv")

# Check
# Extract unique PWSID where activity_status is NA
unique_pwsid_na_activity_status <- syr_arsenic_demo_cleaned_subset %>%
  filter(is.na(activity_status)) %>%
  distinct(pwsid) %>%
  pull(pwsid)

# View the list (Sunset Ranch Development ID7100088, Queen Anne’s County MD1170014, and Terry Trojan Water District SD4600053 no treatment info from facility report)
unique_pwsid_na_activity_status


# Perform analysis with syr_arsenic_demo_site_cleaned ---------------------
data <- read_csv("data/cleaned/syr_arsenic_demo_site_cleaned.csv") 
# continue: go to ShinyApp script and build dashboard using this metadataset

# Violation ---------------------------------------------------------------
sdwis_violation_arsenic <- read_csv("data/raw/sdwis_violation_arsenic_data/sdwis_violation_arsenic_20240807.csv")
sdwis_violation_arsenic <- sdwis_violation_arsenic %>%
  clean_names(case = "snake")
#write_csv(sdwis_violation_arsenic,"data/raw/sdwis_violation_arsenic_data/sdwis_violation_arsenic_20240807.csv")

sdwis_violation_arsenic_demo <- sdwis_violation_arsenic %>%
  filter(pwsid %in% arsenic_demo$pwsid)

violation_data <- sdwis_violation_arsenic_demo



# Data Cleaning Complete - Proceed to Shiny App ---------------------------


