---
# Title: Drinking Water Quality Explorer for Arsenic IC ICR Subgroup
# Description: Shiny App
# author: Natchaya Luangphairin
# date last revised: 1/2/25
# output: app.R
---

# Load pre-processed data from script/data_preprocessing.R output in data folder
# Load packages and libraries ---------------------------------------------
if (!require("pacman")) install.packages("pacman"); library(pacman) #p_boot()
p_load(shiny, tidyverse, janitor, skimr, plotly, shinythemes, shinyWidgets, maps, viridis, DT, writexl, CausalImpact, data.table, tictoc, zoo, shinyBS)

# Example dataset (replace with your actual dataset)
# data-preprocessed (see 01_preprocessing_syr_data and 02_preprocessing_syr_arsenic)
syr_arsenic_cleaned <- read_csv("data/cleaned/syr_arsenic_demo_site_cleaned.csv") 
syr_arsenic <- syr_arsenic_cleaned %>%
  mutate(analyte_code = as.character(analyte_code),
         water_facility_id = as.character(water_facility_id),
         detect = as.character(detect),
         sample_collection_date = as.Date(sample_collection_date)
  )

arsenic_demo_sites <- read_csv("data/cleaned/arsenic_demo_site_pwsid.csv")

sdwa_ref_code_values <- read_csv("data/raw/syr_arsenic_data/SDWA_REF_CODE_VALUES.csv")
names(sdwa_ref_code_values) <- tolower(names(sdwa_ref_code_values))
sdwa_ref_code_values$value_type <- tolower(sdwa_ref_code_values$value_type)

violation_arsenic <- read_csv("data/cleaned/violation_arsenic_demo_site_cleaned.csv")
violation_arsenic <- violation_arsenic %>%
  mutate(
    compl_per_begin_date = as.Date(compl_per_begin_date),
    compl_per_end_date = as.Date(compl_per_end_date),
    rtc_date = as.Date(rtc_date),
    pws_name = site_name  # Create `pws_name` as a copy of `site_name`
  ) %>%
  select(pwsid, pws_name, everything(), -site_name)  # Reorder columns and remove `site_name`


# Extract unique violation_category_code and map descriptions
violation_category_code_unique <- violation_arsenic %>%
  select(violation_category_code) %>%
  distinct()

violation_category_code_mapping <- violation_category_code_unique %>%
  left_join(
    sdwa_ref_code_values %>%
      filter(value_type == "violation_category_code") %>%
      select(value_code, value_description),
    by = c("violation_category_code" = "value_code")
  ) %>%
  rename(
    display = value_description,
    value = violation_category_code
  ) %>%
  filter(!is.na(display)) # Remove rows with no matching description


# USER INTERFACE ----------------------------------------------------------
ui <- navbarPage(
  title = "Arsenic Demo Sites Data",
  theme = shinytheme("flatly"),
  

  ## SYR ---------------------------------------------------------------------
  tabPanel("Occurrence Dashboard",
           tags$div(
             style = "padding: 10px; background-color: #f8f9fa; border-bottom: 1px solid #ddd; font-size: 11px;",
             "On January 22, 2001, the EPA lowered the arsenic standard in drinking water from 50 µg/L to 10 µg/L. To support this change, the EPA initiated a program for research and development of more cost-effective treatment technologies and provided technical assistance to operators of small systems to help reduce compliance costs. This Dashboard is an independently developed tool to explore data from this study. It is not affiliated with the EPA and integrates publicly available water-quality data from multiple sources, including the United States Geological Survey (USGS), the Environmental Protection Agency (EPA), and other local agencies."
           ),
           br(),
           sidebarLayout(
             sidebarPanel(
               tags$h3("Demo Sites"),
               multiInput(
                 inputId = "system_name_demo_site",
                 label = "Site Name:",
                 choices = NULL,
                 choiceNames = unique(syr_arsenic$system_name_demo_site),
                 choiceValues = unique(syr_arsenic$system_name_demo_site)
               ),
               checkboxInput(inputId = "select_all_system_name_demo_site", label = "Select All Demo Sites", value = TRUE
               ),
               multiInput(
                 inputId = "technology_media",
                 label = "Technology Media:",
                 choices = NULL,
                 choiceNames = unique(syr_arsenic$technology_media),
                 choiceValues = unique(syr_arsenic$technology_media)
               ),
               checkboxInput(inputId = "select_all_technology_media", label = "Select All Treatment Technology", value = TRUE
               ),
               tags$div(
                 style = "font-size: 10px; margin-top: -10px;",
                 "AM = adsorptive media process; CF = coagulation/filtration; IR = iron removal; IR/IA = iron removal with iron addition; IX = ion exchange process; RO = reverse osmosis; ATS = Aquatic Treatment Systems; MEI = Magnesium Elektron, Inc.; STS = Severn Trent Services"
               ),
               br(),
               selectInput(
                 inputId = "activity_status",
                 label = "Activity Status:",
                 choices = c("All", "Active", "Inactive", "Changed from public to non-public", "Merged with another system"),
                 selected = "All"
               ),
               dateRangeInput(
                 inputId = "syr_date_range", 
                 label = "Select Date Range:",
                 start = "1998-01-01", 
                 end = "2020-01-01",
                 min = "1990-01-01", 
                 max = Sys.Date()
               ),
               dateInput(
                 inputId = "mark_date", 
                 label = "Intervention Date (e.g. Policy Change):",
                 value = "2001-01-22", 
                 min = "1990-01-01", 
                 max = Sys.Date()
               ),
               tags$div(
                 style = "font-size: 12px; color: gray; margin-top: -5px; font-style: italic;",
                 "Can leave blank to remove the marked line."
               ),
               br(),
               radioButtons(
                 "aggregate_by",
                 "Aggregate Data By:",
                 choices = list("Year" = "year", "Quarter" = "quarter", "Month" = "month", "Six Year Review" = "data_source"),
                 selected = "year"
               )
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel(
                   "Plots",
                   tags$div(
                     style = "padding: 10px; background-color: #f8f9fa; border-bottom: 1px solid #ddd; font-size: 14px;",
                     "This section displays arsenic data plots. Hover over points for detailed information. Click 'Download' tool on upper right corner of the plot to export plot as png."
                   ),
                   br(),
                   br(),
                   tags$div(
                     style = "font-size: 14px; margin-top: -10px;",
                     "The plot below shows arsenic concentrations across all selected demo sites and their current activity status."
                   ),
                   br(),
                   plotlyOutput("arsenic_plot"),
                   br(),
                   tags$div(
                     style = "font-size: 14px; margin-top: -10px;",
                     "The plot below displays yearly average arsenic data for each demo site. Double-click individual Demo Site(s) in the legend, then single-click to select multiple, to isolate Demo Sites of interest on the plot. Click once again on the selected Demo Site(s) to hide."
                   ),
                   br(),
                   plotlyOutput("arsenic_scatter_plot"),
                   br(),
                   tags$div(
                     style = "font-size: 14px; margin-top: -10px;",
                     "The plot below shows similar data, but aggregated by six-year review periods."
                   ),
                   br(),
                   plotlyOutput("arsenic_data_source_plot")
                 ),
                 tabPanel(
                   "Data Downloads",
                   tags$div(
                     style = "padding: 10px; background-color: #f8f9fa; border-bottom: 1px solid #ddd; font-size: 14px;",
                     "This section provides downloadable data summaries."
                   ),
                   br(),
                   fluidRow(
                     column(6, h4("Filtered Arsenic Data Table")),
                     column(6, downloadButton("download_filtered_data", "Download Filtered Data"))
                   ),
                   DTOutput("table_filtered_data"),
                   br(),
                   br(),
                   fluidRow(
                     column(6, h4("Aggregated Arsenic Data Table")),
                     column(6, downloadButton("download_aggregated_data", "Download Aggregated Data"))
                   ),
                   DTOutput("table_aggregated_data")
                 )
               )
             )
           )
  ),
  
  ## Violation ---------------------------------------------------------------
  tabPanel("Violation Dashboard",
           tags$div(
             style = "padding: 10px; background-color: #f8f9fa; border-bottom: 1px solid #ddd; font-size: 11px;",
             "On January 22, 2001, the EPA lowered the arsenic standard in drinking water from 50 µg/L to 10 µg/L. To support this change, the EPA initiated a program for research and development of more cost-effective treatment technologies and provided technical assistance to operators of small systems to help reduce compliance costs. This Dashboard is an independently developed tool to explore data from this study. It is not affiliated with the EPA and integrates publicly available water-quality data from multiple sources, including the United States Geological Survey (USGS), the Environmental Protection Agency (EPA), and other local agencies."
           ),
           br(),
           sidebarLayout(
             sidebarPanel(
               tags$h3("Demo Sites"),
               multiInput(
                 inputId = "system_name_demo_site_violation",
                 label = "Site Name:",
                 choices = NULL,
                 choiceNames = unique(violation_arsenic$system_name_demo_site),
                 choiceValues = unique(violation_arsenic$system_name_demo_site)
               ),
               checkboxInput(inputId = "select_all_system_name_demo_site_violation", label = "Select All Demo Sites", value = TRUE),
               multiInput(
                 inputId = "technology_media_violation",
                 label = "Technology Media:",
                 choices = NULL,
                 choiceNames = unique(violation_arsenic$technology_media),
                 choiceValues = unique(violation_arsenic$technology_media)
               ),
               checkboxInput(inputId = "select_all_technology_media_violation", label = "Select All Treatment Technology", value = TRUE
               ),
               multiInput(
                 inputId = "violation_category_code",
                 label = "Violation Category Code:",
                 choices = NULL,
                 choiceNames = violation_category_code_mapping$display,
                 choiceValues = violation_category_code_mapping$value
               ),
               checkboxInput(inputId = "select_all_violation_category", label = "Select All Violation Categories", value = TRUE),
               selectInput(
                 inputId = "activity_status_violation",
                 label = "Activity Status:",
                 choices = c("All", "Active", "Inactive", "Changed from public to non-public", "Merged with another system"),
                 selected = "All"
               ),
               dateRangeInput(
                 inputId = "violation_date_range",
                 label = "Select Date Range:",
                 start = "1998-01-01",
                 end = "2020-01-01",
                 min = "1990-01-01",
                 max = Sys.Date()
               ),
               radioButtons(
                 inputId = "aggregate_by_violation",
                 label = "Aggregate Data By:",
                 choices = list("Year" = "year", "Quarter" = "quarter", "Month" = "month"),
                 selected = "year"
               )
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel(
                   "Plots",
                   tags$div(
                     style = "padding: 10px; background-color: #f8f9fa; border-bottom: 1px solid #ddd; font-size: 14px;",
                     "This section displays violation count plots aggregated by the selected time interval (year, quarter, or month). Hover over points for detailed information."
                   ),
                   br(),
                   plotlyOutput("violation_plot"),
                   br(),
                   tabPanel("Time in Compliance", plotlyOutput("time_in_compliance_plot")),
                   br(),
                   tabPanel("Time to Return to Compliance", plotlyOutput("time_to_return_to_compliance_plot"))
                 ),
                 tabPanel(
                   "Data Downloads",
                   tags$div(
                     style = "padding: 10px; background-color: #f8f9fa; border-bottom: 1px solid #ddd; font-size: 14px;",
                     "This section provides downloadable data summaries."
                   ),
                   br(),
                   fluidRow(
                     column(6, h4("Filtered Violation Data Table")),
                     column(6, downloadButton("download_filtered_arsenic_violation", "Download Filtered Data"))
                   ),
                   DTOutput("table_filtered_arsenic_violation"),
                   br(),
                   br(),
                   fluidRow(
                     column(6, h4("Aggregated Violation Data Table")),
                     column(6, downloadButton("download_aggregated_arsenic_violation", "Download Aggregated Data"))
                   ),
                   DTOutput("table_aggregated_arsenic_violation")
                 )
               )
             )
           )
  ),
  

  ## Maps --------------------------------------------------------------------
  tabPanel("Map",
           tags$div(
             style = "padding: 10px; background-color: #f8f9fa; border-bottom: 1px solid #ddd; font-size: 11px;",
             "On January 22, 2001, the EPA lowered the arsenic standard in drinking water from 50 µg/L to 10 µg/L. To support this change, the EPA initiated a program for research and development of more cost-effective treatment technologies and provided technical assistance to operators of small systems to help reduce compliance costs. This Dashboard is an independently developed tool to explore data from this study. It is not affiliated with the EPA and integrates publicly available water-quality data from multiple sources, including the United States Geological Survey (USGS), the Environmental Protection Agency (EPA), and other local agencies."
           ),
           br(),
           br(),
           fluidRow(
             column(12, 
                    img(src = "demo_site_map.png", width = "100%"),
                    alt = "Map of 50 Arsenic Demo Sites",
                    p("Source: ", a("Arsenic Treatment Technology Demonstrations", href = "https://www.epa.gov/water-research/arsenic-treatment-technology-demonstrations", target = "_blank")),
                    p("MAP IN PROGRESS"),
             ),
             br(),
             br(),
             tags$div(
               style = "font-size: 16px; margin-top: -10px; text-align: center; font-weight: bold;",
               "Summary of 50 Arsenic Removal Demonstration Locations, Technologies, and Source Water Quality"
             ),
             br(),
             column(
               width = 12,
               DTOutput("demo_sites_table"),  # Table output
               br(),
               tags$div(
                 style = "font-size: 10px; margin-top: -10px;",
                 "AM = adsorptive media process; CF = coagulation/filtration; IR = iron removal; IR/IA = iron removal with iron addition; IX = ion exchange process; RO = reverse osmosis; ATS = Aquatic Treatment Systems; MEI = Magnesium Elektron, Inc.; STS = Severn Trent Services"
               ),
               br(),
               downloadButton("download_demo_sites", "Download Demo Sites Data"),  # Download button
             )
           )
  ),
  

  ## Data and Methods --------------------------------------------------------
  tabPanel("Data and Methods",
           tags$div(
             style = "padding: 10px; background-color: #f8f9fa; border-bottom: 1px solid #ddd; font-size: 11px;",
             "On January 22, 2001, the EPA lowered the arsenic standard in drinking water from 50 µg/L to 10 µg/L. To support this change, the EPA initiated a program for research and development of more cost-effective treatment technologies and provided technical assistance to operators of small systems to help reduce compliance costs. This Dashboard is an independently developed tool to explore data from this study. It is not affiliated with the EPA and integrates publicly available water-quality data from multiple sources, including the United States Geological Survey (USGS), the Environmental Protection Agency (EPA), and other local agencies."
           ),
           hr(),
           fluidRow(
             column(6,
                    h2("Data"),
                    p("The data used in this app comes from the ",
                      tags$a(href = "https://www.epa.gov/", target = "_blank", "EPA"),
                      ". It contains facility info and water quality violation and occurrence data from the Safe Drinking Water Information System (SDWIS) Federal Reporting Services",
                      tags$a(href = "https://sdwis.epa.gov/ords/sfdw_pub/r/sfdw/sdwis_fed_reports_public/6?p6_report=FAC", target = "_blank", "SDWIS Federal Reports"), ". Note: Potential lag in reporting as EPA updates data quarterly. There are some inaccuracies and underreporting of some data. EPA is working to improve data quality. More info on publicly available drinking water databases can be found at this",
                      tags$a(href = "https://github.com/natchayal/Drinking-Water-Database", target = "_blank", "Github Repository"), ". SDWA Reference codes can be found in",
                      tags$a(href = "https://echo.epa.gov/files/echodownloads/SDWA_latest_downloads.zip", target = "_blank", "SDWA Dataset (ZIP)"),". The major portion of details pertaining to the demo sites were taken from",
                      tags$a(href = "https://www.epa.gov/water-research/arsenic-treatment-technology-demonstrations", target = "_blank", "EPA Arsenic Demo Site Report"),"."
                    )
             ),
             column(6,
                    h2("Methods"),
                    p("The data was cleaned and analyzed using the ",
                      tags$a(href = "https://dplyr.tidyverse.org/", target = "_blank", "dplyr"),
                      " and ",
                      tags$a(href = "https://ggplot2.tidyverse.org/", target = "_blank", "ggplot2"),
                      " packages. The app was built using the ",
                      tags$a(href = "https://shiny.rstudio.com/", target = "_blank", "shiny"),
                      " package."),
                    p("Demo Sites were identified without Public Water System Identification (PWSID) and matched to PWSIDs using data from the Federal Facility Report. Fuzzy matching techniques included substring matching, token matching, and incorporation of two-letter state IDs. PWSID-matched facilities were supplemented with additional metadata through a left join with the Federal Facility Report, incorporating activity and status information."),
                    p("The supplemented demo site data was then left-joined with arsenic sampling data by PWSID to create a comprehensive metadata table. This table includes demo site information, facility details, and arsenic sampling data, forming the basis for subsequent analysis. Data referenced is available in the 'Data Downloads' tab.")
             )
           )
  )
)


# SERVER ------------------------------------------------------------------
server <- function(input, output, session) {
  

# SYR ---------------------------------------------------------------------
  # Handle "Select All" for system_name_demo_site
  observeEvent(input$select_all_system_name_demo_site, {
    if (input$select_all_system_name_demo_site) {
      updateMultiInput(session, "system_name_demo_site", selected = unique(syr_arsenic$system_name_demo_site))
    } else {
      updateMultiInput(session, "system_name_demo_site", selected = character(0))
    }
  })
  
  system_name_demo_site <- reactive({
    if (input$select_all_system_name_demo_site) {
      c(unique(syr_arsenic$system_name_demo_site), NA)
    } else {
      input$system_name_demo_site
    }
  })
  
  # Handle "Select All" for technology_media
  observeEvent(input$select_all_technology_media, {
    if (input$select_all_technology_media) {
      updateMultiInput(session, "technology_media", selected = unique(syr_arsenic$technology_media))
    } else {
      updateMultiInput(session, "technology_media", selected = character(0))
    }
  })
  
  
  technology_media <- reactive({
    if (input$select_all_technology_media) {
      c(unique(syr_arsenic$technology_media), NA)
    } else {
      input$technology_media
    }
  })
  
  # Reactive filtered data based on user inputs
  filtered_data <- reactive({
    syr_arsenic %>%
      filter(
        system_name_demo_site %in% input$system_name_demo_site,
        technology_media %in% input$technology_media,
        (input$activity_status == "All" | activity_status == input$activity_status),
        sample_collection_date >= input$syr_date_range[1],
        sample_collection_date <= input$syr_date_range[2]
      )
  })
  
  # Aggregated data based on user selection
  aggregated_data <- reactive({
    data <- filtered_data()
    if (input$aggregate_by == "year") {
      data <- data %>%
        mutate(aggregate_time = year(sample_collection_date),
               x_label = as.character(year(sample_collection_date)))
    } else if (input$aggregate_by == "quarter") {
      data <- data %>%
        mutate(aggregate_time = paste0(year(sample_collection_date), "-Q", quarter(sample_collection_date)),
               x_label = paste0(year(sample_collection_date), "-Q", quarter(sample_collection_date)))
    } else if (input$aggregate_by == "month") {
      data <- data %>%
        mutate(aggregate_time = format(sample_collection_date, "%Y-%m"),
               x_label = format(sample_collection_date, "%Y-%m"))
    } else if (input$aggregate_by == "data_source") {
      data <- data %>%
        mutate(aggregate_time = case_when(
          data_source == "arsenic_syr2" ~ "SYR2 (1998-2005)",
          data_source == "arsenic_syr3" ~ "SYR3 (2006-2011)",
          data_source == "arsenic_syr4" ~ "SYR4 (2012-2019)",
          TRUE ~ data_source
        ),
        x_label = aggregate_time)
    }
    
    data %>%
      group_by(system_name_demo_site, technology_media, aggregate_time, x_label) %>%
      summarise(
        pwsid_mean_value = round(mean(value_ug_l, na.rm = TRUE), 2),
        pwsid_min_value = round(min(value_ug_l, na.rm = TRUE), 2),
        pwsid_max_value = round(max(value_ug_l, na.rm = TRUE), 2),
        pwsid_percentile_95 = round(quantile(value_ug_l, 0.95, na.rm = TRUE), 2),
        .groups = 'drop'
      ) %>%
      group_by(aggregate_time, x_label) %>%
      summarise(
        system_name_demo_site = paste(unique(system_name_demo_site), collapse = ", "),
        technology_media = paste(unique(technology_media), collapse = ", "),
        mean_value = round(mean(pwsid_mean_value, na.rm = TRUE), 2),
        min_value = round(min(pwsid_min_value, na.rm = TRUE), 2),
        max_value = round(max(pwsid_max_value, na.rm = TRUE), 2),
        percentile_95 = round(quantile(pwsid_percentile_95, 0.95, na.rm = TRUE), 2),
        .groups = 'drop'
      )
  })
  
  # Render line plot
  output$arsenic_plot <- renderPlotly({
    plot_data <- aggregated_data()
    max_y <- ceiling(max(plot_data$mean_value, na.rm = TRUE) / 10) * 10
    
    p <- plot_ly(
      data = plot_data,
      x = ~x_label,
      y = ~mean_value,
      type = 'scatter',
      mode = 'lines+markers',
      text = ~paste(
        "Mean Value: ", mean_value, " \u00B5g/L",
        "\nMin Value: ", min_value, " \u00B5g/L",
        "\nMax Value: ", max_value, " \u00B5g/L",
        "\n95th Percentile: ", percentile_95, " \u00B5g/L"
      ),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Arsenic Levels Over Time",
        xaxis = list(
          title = ifelse(input$aggregate_by == "data_source", "Data Source", "Aggregate Time"),
          tickangle = ifelse(input$aggregate_by == "data_source", 0, -90)
        ),
        yaxis = list(title = "Mean Arsenic Conc. (\u00B5g/L)", range = c(0, max_y)),
        showlegend = FALSE
      )
    
    if (input$aggregate_by != "data_source") {
      intervention_x <- if (input$aggregate_by == "year") {
        as.character(year(as.Date(input$mark_date)))
      } else if (input$aggregate_by == "quarter") {
        paste0(year(as.Date(input$mark_date)), "-Q", quarter(as.Date(input$mark_date)))
      } else if (input$aggregate_by == "month") {
        format(as.Date(input$mark_date), "%Y-%m")
      }
      
      p <- p %>% add_segments(
        x = intervention_x, xend = intervention_x,
        y = 0, yend = max_y,
        line = list(color = "red", dash = "dash")
      )
    }
    
    p
  })
  
  
  # Render scatter plot
  output$arsenic_scatter_plot <- renderPlotly({
    scatter_data <- syr_arsenic %>%
      filter(
        system_name_demo_site %in% input$system_name_demo_site,
        technology_media %in% input$technology_media,
        (input$activity_status == "All" | activity_status == input$activity_status),
        sample_collection_date >= input$syr_date_range[1],
        sample_collection_date <= input$syr_date_range[2]
      ) %>%
      mutate(year = year(sample_collection_date)) %>%
      group_by(year, pwsid, primacy_code, system_name_demo_site, source_water_type, treatment_process, treatment_objective, technology_media, vendor, flowrate_gpm, deactivation_date) %>%
      summarise(
        mean_value = mean(value_ug_l, na.rm = TRUE),
        activity_status = first(activity_status), # Assuming consistent status per system per year
        .groups = 'drop'
      ) 
    
    
    plot_ly(
      data = scatter_data,
      x = ~year,
      y = ~mean_value,
      color = ~system_name_demo_site,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        symbol = "circle", # All markers are circles
        size = 10
      ),
      text = ~paste(
        "System Name: ", system_name_demo_site,
        "\nYear: ", year,
        "\nMean Value: ", round(mean_value, 2), " µg/L",
        "\nActivity Status: ", activity_status,
        "\nPWSID: ", pwsid,
        "\nPrimacy Code: ", primacy_code,
        "\nSource Water: ", source_water_type,
        "\nTreatment Process: ", treatment_process,
        "\nTreatment Objective: ", treatment_objective,
        "\nTechnology Media: ", technology_media,
        "\nVendor: ", vendor,
        "\nFlowrate (GPM): ", flowrate_gpm,
        "\nDeactivation Date: ", deactivation_date
      ),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = list(text = "Yearly Mean Arsenic Levels by Demo Site",
                     x = 0.36, # Align to the left
                     xanchor = "center"),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Mean Arsenic Conc. (µg/L)"),
        shapes = list(
          list(
            type = "line",
            x0 = year(as.Date(input$mark_date)),
            x1 = year(as.Date(input$mark_date)),
            y0 = 0,
            y1 = ceiling(max(scatter_data$mean_value, na.rm = TRUE) / 10) * 10,
            line = list(color = "red", dash = "dash")
          )
        ),
        legend = list(title = list(text = "Demo Sites"))
      )
    
  })
  
  
  
  # by SYR ------------------------------------------------------------------
  
  output$arsenic_data_source_plot <- renderPlotly({
    scatter_data <- syr_arsenic %>%
      filter(
        system_name_demo_site %in% input$system_name_demo_site,
        technology_media %in% input$technology_media,
        (input$activity_status == "All" | activity_status == input$activity_status),
        sample_collection_date >= input$syr_date_range[1],
        sample_collection_date <= input$syr_date_range[2]
      ) %>%
      mutate(data_source_group = case_when(
        data_source == "arsenic_syr2" ~ "SYR2 (1998-2005)",
        data_source == "arsenic_syr3" ~ "SYR3 (2006-2011)",
        data_source == "arsenic_syr4" ~ "SYR4 (2012-2019)",
        TRUE ~ "Unknown"
      )) %>%
      group_by(data_source_group, pwsid, primacy_code, system_name_demo_site, source_water_type, adjusted_total_population_served, treatment_process, treatment_objective, technology_media, vendor, flowrate_gpm, activity_status, deactivation_date) %>%
      summarise(
        mean_value = mean(value_ug_l, na.rm = TRUE),
        activity_status = first(activity_status), # Assuming consistent status per system per data source
        .groups = 'drop'
      ) 
    
    max_y <- ceiling(max(scatter_data$mean_value, na.rm = TRUE) / 10) * 10
    
    plot_ly(
      data = scatter_data,
      x = ~data_source_group,
      y = ~mean_value,
      color = ~system_name_demo_site,
      type = 'scatter',
      mode = 'markers+lines',
      marker = list(
        symbol = "circle", # All markers are circles
        size = 10
      ),
      text = ~paste(
        "System Name: ", system_name_demo_site,
        "\nData Source: ", data_source_group,
        "\nMean Value: ", round(mean_value, 2), " µg/L",
        "\nActivity Status: ", activity_status,
        "\nPWSID: ", pwsid,
        "\nPrimacy Code: ", primacy_code,
        "\nSource Water: ", source_water_type,
        "\nPopulation Served: ", adjusted_total_population_served,
        "\nTreatment Process: ", treatment_process,
        "\nTreatment Objective: ", treatment_objective,
        "\nTechnology Media: ", technology_media,
        "\nVendor: ", vendor,
        "\nFlowrate (GPM): ", flowrate_gpm,
        "\nDeactivation Date: ", deactivation_date
      ),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = list(text = "Arsenic Levels by Data Source",
                     x = 0.36, # Align to the left
                     xanchor = "center"),
        xaxis = list(title = "Data Source"),
        yaxis = list(
          title = "Mean Arsenic Conc. (µg/L)",
          range = c(0, max_y) # Setting y-axis limit
        ),
        legend = list(title = list(text = "Demo Sites"))
      )
  })
  
  
  
  # Tables ------------------------------------------------------------------
  
  # Render filtered data table
  output$table_filtered_data <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Render aggregated data table
  output$table_aggregated_data <- renderDT({
    datatable(
      aggregated_data() %>% 
        mutate(
          # Truncate long content for display
          system_name_demo_site = ifelse(nchar(system_name_demo_site) > 30, 
                              paste0(substr(system_name_demo_site, 1, 30), "..."), 
                              system_name_demo_site),
          technology_media = ifelse(nchar(technology_media) > 30, 
                                   paste0(substr(technology_media, 1, 30), "..."), 
                                   technology_media)
        ) %>%
        select(-c("x_label")),
      options = list(pageLength = 5, scrollX = TRUE),
      rownames = FALSE,
      colnames = c(
        "Time Period",
        "Selected Demo Sites",
        "Treatment Technology",
        "Mean Value (µg/L)",
        "Min Value (µg/L)",
        "Max Value (µg/L)",
        "95th Percentile (µg/L)"
      )
    )
  })
  
  

  
  
  # Download CSV for all filtered data
  output$download_filtered_data <- downloadHandler(
    filename = function() {
      paste("filtered_arsenic_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Download CSV for aggregated data
  output$download_aggregated_data <- downloadHandler(
    filename = function() {
      paste("aggregated_arsenic_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(aggregated_data(), file, row.names = FALSE)
    }
  )

  
  
  # Violation ---------------------------------------------------------------
  # Handle "Select All" for system_name_demo_site_violation
  observeEvent(input$select_all_system_name_demo_site_violation, {
    if (input$select_all_system_name_demo_site_violation) {
      updateMultiInput(session, "system_name_demo_site_violation", selected = unique(violation_arsenic$system_name_demo_site))
    } else {
      updateMultiInput(session, "system_name_demo_site_violation", selected = character(0))
    }
  })
  
  system_name_demo_site_violation <- reactive({
    if (input$select_all_system_name_demo_site_violation) {
      c(unique(violation_arsenic$system_name_demo_site), NA)
    } else {
      input$system_name_demo_site_violation
    }
  })

  
  # Handle "Select All" for technology_media
  observeEvent(input$select_all_technology_media_violation, {
    if (input$select_all_technology_media_violation) {
      updateMultiInput(session, "technology_media_violation", selected = unique(violation_arsenic$technology_media))
    } else {
      updateMultiInput(session, "technology_media_violation", selected = character(0))
    }
  })
  
  
  technology_media_violation <- reactive({
    if (input$select_all_technology_media_violation) {
      c(unique(violation_arsenic$technology_media), NA)
    } else {
      input$technology_media_violation
    }
  })
  
  
  # Handle "Select All" for violation_category_code
  observeEvent(input$select_all_violation_category, {
    if (input$select_all_violation_category) {
      updateMultiInput(session, "violation_category_code", selected = unique(violation_arsenic$violation_category_code))
    } else {
      updateMultiInput(session, "violation_category_code", selected = character(0))
    }
  })
  
  # Reactive filtered data
  filtered_violation_data <- reactive({
    violation_arsenic %>%
      # Convert viol_measure from mg/L to µg/L
      mutate(viol_measure_ug_l = viol_measure * 1000) %>%
      filter(
        system_name_demo_site %in% input$system_name_demo_site_violation,
        violation_category_code %in% input$violation_category_code,
        (input$activity_status_violation == "All" | activity_status == input$activity_status_violation),
        technology_media %in% input$technology_media_violation,
        compl_per_begin_date >= input$violation_date_range[1],
        compl_per_end_date <= input$violation_date_range[2]
      )
  })
  
  # Aggregated data for violation count and measures
  aggregated_violation_data <- reactive({
    data <- filtered_violation_data() %>%
      mutate(
        # Calculate durations
        time_in_compliance_days = as.numeric(difftime(compl_per_end_date, compl_per_begin_date, units = "days")),
        time_to_return_to_compliance_days = as.numeric(difftime(rtc_date, compl_per_end_date, units = "days")),
        # Aggregate period based on user selection
        aggregate_time = case_when(
          input$aggregate_by_violation == "year" ~ as.character(year(compl_per_begin_date)),
          input$aggregate_by_violation == "quarter" ~ paste0(year(compl_per_begin_date), "-Q", quarter(compl_per_begin_date)),
          input$aggregate_by_violation == "month" ~ format(compl_per_begin_date, "%Y-%m"),
          TRUE ~ NA_character_
        )
      ) %>%
      group_by(aggregate_time) %>%
      summarise(
        demo_sites = paste(unique(system_name_demo_site), collapse = ", "),
        technology_media = paste(unique(technology_media), collapse = ", "),
        total_violations = n(),
        avg_violation_measure = round(mean(viol_measure_ug_l, na.rm = TRUE), 2),
        avg_time_in_compliance = case_when(
          input$aggregate_by_violation == "year" ~ round(mean(time_in_compliance_days, na.rm = TRUE) / 365, 2),
          input$aggregate_by_violation == "quarter" ~ round(mean(time_in_compliance_days, na.rm = TRUE) / 90, 2),
          input$aggregate_by_violation == "month" ~ round(mean(time_in_compliance_days, na.rm = TRUE) / 30, 2)
        ),
        avg_time_to_return_to_compliance = case_when(
          input$aggregate_by_violation == "year" ~ round(mean(time_to_return_to_compliance_days, na.rm = TRUE) / 365, 2),
          input$aggregate_by_violation == "quarter" ~ round(mean(time_to_return_to_compliance_days, na.rm = TRUE) / 90, 2),
          input$aggregate_by_violation == "month" ~ round(mean(time_to_return_to_compliance_days, na.rm = TRUE) / 30, 2)
        ),
        .groups = "drop"
      )
    return(data)
  })
  
  
  
  # Plot: Total Violation Count
  output$violation_plot <- renderPlotly({
    data <- aggregated_violation_data()
    
    plot_ly(data, x = ~aggregate_time, y = ~total_violations, type = 'bar', name = 'Total Violations') %>%
      layout(
        title = "Total Violation Count Over Time",
        xaxis = list(title = "Time Period"),
        yaxis = list(title = "Violation Count"),
        barmode = "stack"
      )
  })
  
  compliance_durations_data <- reactive({
    data <- filtered_violation_data() %>%
      mutate(
        # Calculate time in compliance and time to return to compliance
        time_in_compliance_days = as.numeric(difftime(compl_per_end_date, compl_per_begin_date, units = "days")),
        time_to_return_to_compliance_days = as.numeric(difftime(rtc_date, compl_per_end_date, units = "days")),
        # Aggregate period based on user selection
        aggregate_time = case_when(
          input$aggregate_by_violation == "year" ~ as.character(year(compl_per_begin_date)),
          input$aggregate_by_violation == "quarter" ~ paste0(year(compl_per_begin_date), "-Q", quarter(compl_per_begin_date)),
          input$aggregate_by_violation == "month" ~ format(compl_per_begin_date, "%Y-%m"),
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(time_in_compliance_days) & !is.na(time_to_return_to_compliance_days)) %>% # Exclude rows with missing data
      group_by(aggregate_time) %>%
      summarise(
        avg_time_in_compliance = case_when(
          input$aggregate_by_violation == "year" ~ round(mean(time_in_compliance_days, na.rm = TRUE) / 365, 2),
          input$aggregate_by_violation == "quarter" ~ round(mean(time_in_compliance_days, na.rm = TRUE) / 90, 2),
          input$aggregate_by_violation == "month" ~ round(mean(time_in_compliance_days, na.rm = TRUE) / 30, 2)
        ),
        avg_time_to_return_to_compliance = case_when(
          input$aggregate_by_violation == "year" ~ round(mean(time_to_return_to_compliance_days, na.rm = TRUE) / 365, 2),
          input$aggregate_by_violation == "quarter" ~ round(mean(time_to_return_to_compliance_days, na.rm = TRUE) / 90, 2),
          input$aggregate_by_violation == "month" ~ round(mean(time_to_return_to_compliance_days, na.rm = TRUE) / 30, 2)
        ),
        .groups = "drop"
      )
    return(data)
  })
  
  
  output$time_in_compliance_plot <- renderPlotly({
    data <- compliance_durations_data()
    
    plot_ly(data, x = ~aggregate_time, y = ~avg_time_in_compliance, type = 'bar', name = 'Average Time in Compliance') %>%
      layout(
        title = "Time in Compliance",
        xaxis = list(title = "Time Period"),
        yaxis = list(
          title = case_when(
            input$aggregate_by_violation == "year" ~ "Duration (Years)",
            input$aggregate_by_violation == "quarter" ~ "Duration (Quarters)",
            input$aggregate_by_violation == "month" ~ "Duration (Months)"
          )
        ),
        barmode = "group"
      )
  })
  
  output$time_to_return_to_compliance_plot <- renderPlotly({
    data <- compliance_durations_data()
    
    plot_ly(data, x = ~aggregate_time, y = ~avg_time_to_return_to_compliance, type = 'bar', name = 'Average Time to Return to Compliance') %>%
      layout(
        title = "Time to Return to Compliance",
        xaxis = list(title = "Time Period"),
        yaxis = list(
          title = case_when(
            input$aggregate_by_violation == "year" ~ "Duration (Years)",
            input$aggregate_by_violation == "quarter" ~ "Duration (Quarters)",
            input$aggregate_by_violation == "month" ~ "Duration (Months)"
          )
        ),
        barmode = "group"
      )
  })
  

  
  # Render filtered data table
  output$table_filtered_arsenic_violation <- renderDT({
    datatable(
      filtered_violation_data(),
      options = list(pageLength = 5, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # Render aggregated data table
  output$table_aggregated_arsenic_violation <- renderDT({
    # Set dynamic column labels for time-related columns
    time_unit <- switch(
      input$aggregate_by_violation,
      "year" = " (Years)",
      "quarter" = " (Quarters)",
      "month" = " (Months)"
    )
    
    colnames <- c(
      "Time Period",
      "Selected Demo Sites",
      "Treatment Technology",
      "Total Violations",
      "Mean Violation Measure (µg/L)",
      paste0("Avg Time in Compliance", time_unit),
      paste0("Avg Time to Return to Compliance", time_unit)
    )
    
    datatable(
      aggregated_violation_data(),
      options = list(pageLength = 5, scrollX = TRUE),
      rownames = FALSE,
      colnames = colnames
    )
  })
  
  
  
  # Download CSV for filtered data
  output$download_filtered_arsenic_violation <- downloadHandler(
    filename = function() {
      paste("filtered_violation_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_violation_data(), file, row.names = FALSE)
    }
  )
  
  # Download CSV for aggregated data
  output$download_aggregated_arsenic_violation <- downloadHandler(
    filename = function() {
      paste("aggregated_violation_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(aggregated_violation_data(), file, row.names = FALSE)
    }
  )
  

# Maps --------------------------------------------------------------------
  # Render the table
  output$demo_sites_table <- renderDT({
    datatable(
      arsenic_demo_sites,
      options = list(pageLength = 5, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # Download handler
  output$download_demo_sites <- downloadHandler(
    filename = function() {
      paste("arsenic_demo_sites_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(arsenic_demo_sites, file, row.names = FALSE)
    }
  )
  
  
}


# Run App -----------------------------------------------------------------
shinyApp(ui = ui, server = server)

