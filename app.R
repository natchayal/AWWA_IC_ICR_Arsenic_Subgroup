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

# USER INTERFACE ----------------------------------------------------------
ui <- navbarPage(
  title = "Arsenic Demo Sites Data",
  theme = shinytheme("flatly"),
  
  tabPanel("Dashboard",
           tags$div(
             style = "padding: 10px; background-color: #f8f9fa; border-bottom: 1px solid #ddd; font-size: 14px;",
             "In 2001, EPA lowered the arsenic standard in drinking water from 50 µg/L to 10 µg/L. 
             EPA announced an initiative for research and development of more cost-effective treatment technologies and to provide technical assistance to operators of small systems to reduce compliance cost."
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
  
  tabPanel("Map",
           fluidRow(
             column(12, 
                    img(src = "demo_site_map.png", width = "100%"),
                    alt = "Map of 50 Arsenic Demo Sites",
                    p("Source: ", a("Arsenic Treatment Technology Demonstrations", href = "https://www.epa.gov/water-research/arsenic-treatment-technology-demonstrations", target = "_blank")),
                    p("IN PROGRESS"),
             ),
           )
  ),
  tabPanel("Data and Methods",
           p("This Dashboard is an independently developed tool that is not affiliated with the EPA. It integrates publicly available water-quality data from multiple sources, including the United States Geological Survey (USGS), the Environmental Protection Agency (EPA), and other local agencies."),
           hr(),
           fluidRow(
             column(6,
                    h2("Data"),
                    p("The data used in this app comes from the ",
                      tags$a(href = "https://www.epa.gov/", target = "_blank", "EPA"),
                      ". It contains facility info and water quality violation and occurrence data from the Safe Drinking Water Information System (SDWIS) Federal Reporting Services",
                      tags$a(href = "https://sdwis.epa.gov/ords/sfdw_pub/r/sfdw/sdwis_fed_reports_public/6?p6_report=FAC", target = "_blank", "SDWIS Federal Reports"), ". Note: Potential lag in reporting as EPA updates data quarterly. There are some inaccuracies and underreporting of some data. EPA is working to improve data quality. More info on publicly available drinking water databases can be found at this",
                      tags$a(href = "https://github.com/natchayal/Drinking-Water-Database", target = "_blank", "Github Repository"), ". SDWA Reference codes can be found in",
                      tags$a(href = "https://echo.epa.gov/files/echodownloads/SDWA_latest_downloads.zip", target = "_blank", "SDWA Dataset (ZIP)"),". In 2001, EPA adopted a new standard lowering the permissible amount of arsenic in drinking water from 50 micrograms per liter (µg/L) to 10 µg/L. To help states meet this new standard, EPA announced an initiative for research and development of more cost-effective treatment technologies and to provide technical assistance to operators of small systems to reduce compliance cost. The major portion of details pertaining to the demo sites were taken from",
                      tags$a(href = "https://www.epa.gov/water-research/arsenic-treatment-technology-demonstrations", target = "_blank", "EPA Arsenic Demo Site Report"),".",
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
                      " package.")
             )
           )
  )
)
  

# SERVER ------------------------------------------------------------------
server <- function(input, output, session) {
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
      group_by(system_name_demo_site, aggregate_time, x_label) %>%
      summarise(
        pwsid_mean_value = round(mean(value_ug_l, na.rm = TRUE), 2),
        pwsid_min_value = round(min(value_ug_l, na.rm = TRUE), 2),
        pwsid_max_value = round(max(value_ug_l, na.rm = TRUE), 2),
        pwsid_percentile_95 = round(quantile(value_ug_l, 0.95, na.rm = TRUE), 2),
        .groups = 'drop'
      ) %>%
      group_by(aggregate_time, x_label) %>%
      summarise(
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
          ),
          list(
            type = "line",
            x0 = 2001,
            x1 = 2001,
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
    datatable(aggregated_data() %>% select(-c("x_label")), options = list(pageLength = 5, scrollX = TRUE))
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
}

shinyApp(ui = ui, server = server)

