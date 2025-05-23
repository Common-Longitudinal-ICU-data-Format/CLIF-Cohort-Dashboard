library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
library(DT)
library(shinydashboard)
library(htmltools)

# Load data
sites <- read.csv("data/site_details.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Longitude = as.numeric(str_replace_all(Longitude, "[−–—]", "-")),
    Latitude = as.numeric(Latitude)
  )

metrics <- read.csv("data/metrics_data.csv", stringsAsFactors = FALSE)
site_data <- read.csv("data/final_website_data.csv", check.names = FALSE)

# Helper functions
get_metric_with_format <- function(metric_name, selected_site = "Consortium Aggregate") {
  metrics %>% 
    filter(Site == selected_site, Metric == metric_name) %>% 
    select(Value, Display_Format) %>% 
    head(1)
}

format_number <- function(value) {
  format(as.numeric(gsub("[^0-9]", "", value)), big.mark=",")
}

get_metric <- function(metric_name, selected_site = "Consortium Aggregate") {
  metrics %>% 
    filter(Site == selected_site, Metric == metric_name) %>% 
    pull(Value)
}

format_large_number <- function(value) {
  format(as.numeric(gsub("[^0-9]", "", value)), big.mark=",")
}

format_median_iqr <- function(metric_name, show_iqr = FALSE, selected_site = "Consortium Aggregate") {
  metric_data <- get_metric_with_format(metric_name, selected_site)
  if (metric_name == "Total hospital days, median [Q1,Q3]") {
    print("DEBUG: metric_data for LOS:")
    print(metric_data)
  }
  if (nrow(metric_data) == 0 || is.null(metric_data$Value) || length(metric_data$Value) == 0 || is.na(metric_data$Value) || metric_data$Value == "") return("N/A")
  value <- as.character(metric_data$Value)
  if (metric_name == "Total hospital days, median [Q1,Q3]") {
    print("DEBUG: value for LOS:")
    print(value)
  }
  if (length(value) == 0 || is.na(value) || value == "") return("N/A")
  format <- metric_data$Display_Format
  
  numbers <- str_extract_all(value, "\\d+\\.?\\d*")[[1]]
  if (metric_name == "Total hospital days, median [Q1,Q3]") {
    print("DEBUG: numbers for LOS:")
    print(numbers)
  }
  if(length(numbers) >= 3) {
    median <- as.numeric(numbers[1])
    q1 <- as.numeric(numbers[2])
    q3 <- as.numeric(numbers[3])
    if (metric_name == "Total hospital days, median [Q1,Q3]") {
      print("DEBUG: median/q1/q3 for LOS:")
      print(c(median, q1, q3))
    }
    if(show_iqr) {
      return(sprintf("%.1f [%.1f, %.1f]", median, q1, q3))
    } else {
      return(sprintf("%.1f", median))
    }
  }
  return(value)
}

format_percentage <- function(metric_name, show_n = FALSE, selected_site = "Consortium Aggregate") {
  metric_data <- get_metric_with_format(metric_name, selected_site)
  if (nrow(metric_data) == 0 || is.null(metric_data$Value) || length(metric_data$Value) == 0 || is.na(metric_data$Value) || metric_data$Value == "") return("N/A")
  value <- as.character(metric_data$Value)
  if (length(value) == 0 || is.na(value) || value == "") return("N/A")
  format <- metric_data$Display_Format
  
  n_match <- str_extract(value, "\\d+(?=\\s*\\()")
  pct_match <- str_extract(value, "\\d+\\.?\\d*(?=%\\))")
  
  if(!is.na(pct_match)) {
    if(show_n && !is.na(n_match)) {
      return(sprintf("%s (%s%%)", format_large_number(n_match), pct_match))
    } else {
      return(sprintf("%s%%", pct_match))
    }
  }
  return(value)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "CLIF Cohort Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data", tabName = "data", icon = icon("table"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$script(HTML('
        $(document).on("shiny:connected", function() {
          setTimeout(function() {
            $("#site_selector option").each(function() {
              if ($(this).text().includes("(Not Ready)")) {
                $(this).attr("disabled", "disabled");
              }
            });
          }, 500);
        });
      '))
    ),
    fluidRow(
      column(12, div(style = "margin-bottom: 1.5rem;", selectInput(
        "site_selector", "Select Site:",
        choices = c(
          "Consortium Aggregate",
          setNames(
            sites$Site,
            ifelse(sites$Data.Ready == "Yes", sites$Site, paste0(sites$Site, " (Not Ready)"))
          )
        ),
        selected = "Consortium Aggregate",
        selectize = TRUE
      )))
    ),
    tabItems(
      tabItem(tabName = "home",
        fluidRow(
          # Top stats cards
          box(
            width = 12,
            div(class = "top-stats-grid",
              div(class = "stat-card stat-card-horizontal",
                img(src = "icons/encounters.svg", class = "stat-icon-left", alt = "encounters"),
                div(
                  div(class = "stat-label-large", "Total ICU Encounters"),
                  div(class = "stat-value-large", textOutput("total_encounters"))
                )
              ),
              div(class = "stat-card stat-card-horizontal",
                img(src = "icons/patients.svg", class = "stat-icon-left", alt = "patients"),
                div(
                  div(class = "stat-label-large", "Patients"),
                  div(class = "stat-value-large", textOutput("total_patients"))
                )
              ),
              div(class = "stat-card stat-card-horizontal",
                img(src = "icons/years.svg", class = "stat-icon-left", alt = "years"),
                div(
                  div(class = "stat-label-large", "Years"),
                  div(class = "stat-value-large", textOutput("years"))
                )
              ),
              div(class = "stat-card stat-card-horizontal",
                img(src = "icons/sex.svg", class = "stat-icon-left", alt = "female"),
                div(
                  div(class = "stat-label-large", "Female"),
                  div(class = "stat-value-large", textOutput("female_percentage"))
                )
              ),
              div(class = "stat-card stat-card-horizontal",
                img(src = "icons/race.svg", class = "stat-icon-left", alt = "race"),
                div(
                  div(class = "stat-label-large", "White"),
                  div(class = "stat-value-large", textOutput("white_percentage"))
                )
              ),
              div(class = "stat-card stat-card-horizontal",
                img(src = "icons/hispanic.svg", class = "stat-icon-left", alt = "hispanic"),
                div(
                  div(class = "stat-label-large", "Hispanic"),
                  div(class = "stat-value-large", textOutput("hispanic_percentage"))
                )
              ),
              div(class = "stat-card stat-card-horizontal",
                img(src = "icons/vent2.svg", class = "stat-icon-left", alt = "vent"),
                div(
                  div(class = "stat-label-large", "IMV Encounters"),
                  div(class = "stat-value-large", textOutput("imv_percentage")),
                  div(class = "stat-label-small", "Encounters received mechanical ventilation")
                )
              ),
              div(class = "stat-card stat-card-horizontal",
                img(src = "icons/vasopressors.svg", class = "stat-icon-left", alt = "vasopressor"),
                div(
                  div(class = "stat-label-large", "Vasopressors"),
                  div(class = "stat-value-large", textOutput("vasopressor_percentage")),
                  div(class = "stat-label-small", "Encounters received Vasopressors")
                )
              ),
              div(class = "stat-card stat-card-horizontal",
                img(src = "icons/hospitals.svg", class = "stat-icon-left", alt = "hospitals"),
                div(
                  div(class = "stat-label-large", "Hospitals"),
                  div(class = "stat-value-large", textOutput("hospitals"))
                )
              ),
              div(class = "stat-card stat-card-horizontal",
                img(src = "icons/days.svg", class = "stat-icon-left", alt = "days"),
                div(
                  div(class = "stat-label-large", "Hospital Length of Stay (median days)"),
                  div(class = "stat-value-large", textOutput("hospital_los"))
                )
              ),
              div(class = "stat-card stat-card-horizontal",
                img(src = "icons/mortality.svg", class = "stat-icon-left", alt = "mortality"),
                div(
                  div(class = "stat-label-large", "Mortality"),
                  div(class = "stat-value-large", textOutput("mortality"))
                )
              ),
              div(class = "stat-card stat-card-horizontal",
                img(src = "icons/sofa.svg", class = "stat-icon-left", alt = "sofa"),
                div(
                  div(class = "stat-label-large", "SOFA Median"),
                  div(class = "stat-value-large", textOutput("sofa_median"))
                )
              )
            )
          ),
          # Map and details
          box(
            width = 8,
            leafletOutput("map", height = "500px")
          ),
          box(
            width = 4,
            tabsetPanel(
              tabPanel("IMV Encounters",
                tabsetPanel(
                  tabPanel("Initial Location",
                    div(
                      style = "text-align:center; color:#983232; font-size:16px; font-family:Inter; font-weight:600; margin-bottom:-1.5rem; margin-top:0.5rem;",
                      "First Location of Intubation"
                    ),
                    plotlyOutput("imv_locations")
                  ),
                  tabPanel("Initial Mode of Ventilation",
                    plotlyOutput("vent_modes")
                  ),
                  tabPanel("Initial Settings",
                    tableOutput("vent_settings_table"),
                    div(style = "color: #666; font-size: 0.8rem; margin-top: 1rem;",
                        "Units: FiO2 (fraction), PEEP (cmH2O), Respiratory rate (breaths/min), Tidal Volume (mL)")
                  )
                )
              ),
              tabPanel("Vasopressor Usage",
                tableOutput("vasopressor_table"),
                div(style = "color: #666; font-size: 0.8rem; margin-top: 1rem;",
                    "The unit of measurement for all medications presented here is mcg/kg/min, except vasopressin, for which we use units/min")
              )
            )
          )
        )
      ),
      tabItem(tabName = "data",
        fluidRow(
          box(
            width = 12,
            DTOutput("data_table")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive value for selected site
  selected_site <- reactive({
    sel <- input$site_selector
    # Remove (Not Ready) suffix if present
    gsub(" \\([Nn]ot Ready\\)$", "", sel)
  })
  
  # Flag to suppress popup when dropdown is updated by marker click
  suppress_popup <- reactiveVal(FALSE)
  
  # Helper to check if site is ready
  is_site_ready <- reactive({
    site <- selected_site()
    if (site == "Consortium Aggregate") return(TRUE)
    row <- sites[sites$Site == site, ]
    if (nrow(row) == 0) return(TRUE)
    row$Data.Ready == "Yes"
  })
  
  # Top stats outputs
  output$total_encounters <- renderText({
    req(is_site_ready())
    format_large_number(get_metric("N: ICU encounters", selected_site()))
  })
  
  output$total_patients <- renderText({
    req(is_site_ready())
    format_large_number(get_metric("N: Unique patients", selected_site()))
  })
  
  output$years <- renderText({
    req(is_site_ready())
    get_metric("Years", selected_site())
  })
  
  output$hospitals <- renderText({
    req(is_site_ready())
    format_large_number(get_metric("Hospitals ", selected_site()))
  })
  
  output$hospital_los <- renderText({
    req(is_site_ready())
    format_median_iqr("Total hospital days, median [Q1,Q3]", selected_site = selected_site())
  })
  
  output$female_percentage <- renderText({
    req(is_site_ready())
    format_percentage("Sex n (%): Female", selected_site = selected_site())
  })
  
  output$white_percentage <- renderText({
    req(is_site_ready())
    format_percentage("Race n (%): White", selected_site = selected_site())
  })
  
  output$hispanic_percentage <- renderText({
    req(is_site_ready())
    format_percentage("Ethnicity n (%): Hispanic", selected_site = selected_site())
  })
  
  output$mortality <- renderText({
    req(is_site_ready())
    format_percentage("Hospital mortality n (%)", selected_site = selected_site())
  })
  
  output$sofa_median <- renderText({
    req(is_site_ready())
    format_median_iqr("SOFA-97 TOTAL, median [Q1,Q3]", selected_site = selected_site())
  })
  
  # Map output
  output$map <- renderLeaflet({
    leaflet(sites) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addAwesomeMarkers(
        ~Longitude, ~Latitude,
        icon = ~awesomeIconList(
          ready = makeAwesomeIcon(
            icon = "check",
            markerColor = "green",
            iconColor = "white",
            library = "fa"
          ),
          pending = makeAwesomeIcon(
            icon = "clock",
            markerColor = "orange",
            iconColor = "white",
            library = "fa"
          )
        )[ifelse(Data.Ready == "Yes", "ready", "pending")],
        popup = ~paste0(
          "<b>", Site, "</b><br>",
          "Location: ", `City..State.Province`, "<br>",
          "Hospitals: ", `Hospitals`, "<br>",
          "Total ICU Encounters: ", `Encounters`, "<br>",
          "Total Patients: ", `Patients`, "<br>",
          "Status: ", if_else(Data.Ready == "Yes", 
                             "<span style='color: green;'>Ready</span>", 
                             "<span style='color: orange;'>Pending</span>")
        ),
        label = ~Site,
        clusterOptions = markerClusterOptions(),
        layerId = ~Site
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("#4DAF4A", "#FF7F00"),
        labels = c("Ready", "Pending"),
        title = "Site Status"
      )
  })
  
  # Sync dropdown to map: zoom and popup
  observeEvent(input$site_selector, {
    req(input$site_selector)
    if (input$site_selector != "Consortium Aggregate") {
      site_row <- sites[sites$Site == input$site_selector, ]
      if (nrow(site_row) == 1) {
        leafletProxy("map") %>%
          setView(lng = site_row$Longitude, lat = site_row$Latitude, zoom = 8) %>%
          clearPopups()
        if (!isTRUE(suppress_popup())) {
          leafletProxy("map") %>%
            addPopups(
              lng = site_row$Longitude,
              lat = site_row$Latitude,
              popup = paste0(
                "<b>", site_row$Site, "</b><br>",
                "Location: ", site_row$`City..State.Province`, "<br>",
                "Hospitals: ", site_row$Hospitals, "<br>",
                "Total ICU Encounters: ", site_row$Encounters, "<br>",
                "Total Patients: ", site_row$Patients, "<br>",
                "Status: ", ifelse(site_row$Data.Ready == "Yes", 
                                  "<span style='color: green;'>Ready</span>", 
                                  "<span style='color: orange;'>Pending</span>")
              )
            )
        }
        suppress_popup(FALSE) # Reset flag
      }
    } else {
      leafletProxy("map") %>%
        setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
        clearPopups()
    }
  })
  
  # IMV Encounters outputs
  output$imv_percentage <- renderText({
    req(is_site_ready())
    format_percentage("IMV encounters n (%)", selected_site = selected_site())
  })
  
  output$imv_locations <- renderPlotly({
    req(is_site_ready())
    site <- selected_site()
    # Find the row index for 'First location of intubation n(%)' for the selected site
    idx <- which(metrics$Site == site & metrics$Metric == "First location of intubation n(%)")
    if (length(idx) == 0) return(NULL)
    # The next 5 rows are ICU, ED, Procedural, Ward, Other
    loc_rows <- metrics[(idx+1):(idx+5), ]
    # Parse count and percentage from the Value column
    parse_count <- function(val) {
      as.numeric(gsub(" .*", "", val))
    }
    parse_pct <- function(val) {
      as.numeric(sub(".*\\((.*)%\\).*", "\\1", val))
    }
    locations <- data.frame(
      Location = loc_rows$Metric,
      Count = sapply(loc_rows$Value, parse_count),
      Percentage = sapply(loc_rows$Value, parse_pct)
    )
    # Remove rows with NA (in case of missing data)
    locations <- locations[!is.na(locations$Count) & !is.na(locations$Percentage), ]
    # Create custom color palette
    colors <- c("#983232", "#B25959", "#CC8080", "#E6A6A6", "#FFD9D9")
    # Create the donut chart
    plot_ly(locations, labels = ~Location, values = ~Percentage, type = 'pie',
            hole = 0.6,
            marker = list(colors = colors),
            textposition = 'outside',
            textinfo = 'label+percent',
            direction = 'clockwise',
            textfont = list(size = 9),
            insidetextorientation = 'radial',
            hovertemplate = paste0(
              "%{label}<br>",
              "Count: %{customdata:,.0f}<br>",
              "Percentage: %{percent}<br>",
              "<extra></extra>"
            ),
            customdata = ~Count) %>%
      layout(
        showlegend = FALSE,
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        margin = list(t = 80, l = 20, r = 20, b = 20),
        autosize = TRUE
      )
  })
  
  # Ventilator Settings outputs
  output$vent_modes <- renderPlotly({
    req(is_site_ready())
    site <- selected_site()
    # Find the row index for 'Initial Mode of IMV n(%)' for the selected site
    idx <- which(metrics$Site == site & metrics$Metric == "Initial Mode of IMV n(%)")
    if (length(idx) == 0) return(NULL)
    # The next 7 rows are the ventilation modes
    mode_rows <- metrics[(idx+1):(idx+7), ]
    # Parse count and percentage from the Value column
    parse_count <- function(val) {
      as.numeric(gsub(" .*", "", val))
    }
    parse_pct <- function(val) {
      as.numeric(sub(".*\\((.*)%\\).*", "\\1", val))
    }
    vent_modes <- data.frame(
      Mode = mode_rows$Metric,
      Count = sapply(mode_rows$Value, parse_count),
      Percentage = sapply(mode_rows$Value, parse_pct)
    )
    # Remove rows with NA (in case of missing data)
    vent_modes <- vent_modes[!is.na(vent_modes$Count) & !is.na(vent_modes$Percentage), ]
    # Sort by percentage in descending order
    vent_modes <- vent_modes[order(-vent_modes$Percentage),]
    # Create a color palette
    colors <- c('#983232', '#CC8080', '#E6A6A6', '#B25959', '#FFB3B3', '#FFCCCC', '#FFE6E6')
    # Create the vertical bar plot
    plot_ly() %>%
      add_trace(
        data = vent_modes,
        x = ~seq_along(Mode),
        y = ~Percentage,
        type = 'bar',
        name = ~Mode,
        marker = list(color = colors),
        text = ~paste0(sprintf("%.1f", Percentage), "%"),
        textposition = 'outside',
        hovertemplate = paste0(
          "<b>%{data.name}</b><br>",
          "Count: %{customdata:,.0f}<br>",
          "Percentage: %{y:.1f}%<br>",
          "<extra></extra>"
        ),
        customdata = ~Count
      ) %>%
      layout(
        title = list(
          text = "Initial Mode of Ventilation",
          font = list(
            color = '#983232',
            size = 16,
            family = "Inter"
          ),
          y = 0.95
        ),
        yaxis = list(
          title = "Percentage of Encounters (%)",
          showgrid = TRUE,
          gridcolor = '#E5E5E5',
          zeroline = FALSE,
          range = c(0, max(vent_modes$Percentage) * 1.2)
        ),
        xaxis = list(
          title = "",
          showticklabels = FALSE,
          showgrid = FALSE
        ),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        margin = list(l = 50, r = 20, t = 50, b = 100),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = -0.2,
          yanchor = "top",
          bgcolor = 'rgba(255, 255, 255, 0.9)',
          bordercolor = 'rgba(152, 50, 50, 0.2)',
          borderwidth = 1,
          font = list(size = 11),
          traceorder = "normal"
        ),
        showlegend = TRUE,
        font = list(family = "Inter"),
        bargap = 0.4,
        height = 400,
        autosize = TRUE
      )
  })
  
  output$vent_settings_table <- renderTable({
    req(is_site_ready())
    site <- selected_site()
    # Find the row index for 'Ventilator settings Median [Q1, Q2]' for the selected site
    idx <- which(metrics$Site == site & metrics$Metric == "Ventilator settings Median [Q1, Q2]")
    if (length(idx) == 0) return(NULL)
    # The next 4 rows are Peep, FiO2, Respiratory rate, Tidal Volume
    setting_rows <- metrics[(idx+1):(idx+4), ]
    data.frame(
      Setting = setting_rows$Metric,
      `Median [IQR]` = setting_rows$Value
    )
  })
  
  # Vasopressors outputs
  output$vasopressor_percentage <- renderText({
    req(is_site_ready())
    format_percentage("Vasopressor encounters n (%)", selected_site = selected_site())
  })
  
  output$vasopressor_table <- renderTable({
    req(is_site_ready())
    data.frame(
      Medication = c("Phenylephrine", "Norepinephrine", "Vasopressin", "Angiotensin", 
                    "Dopamine", "Epinephrine"),
      `Median [IQR]` = c(
        get_metric("Phenylephrine dose, median [Q1,Q3]", selected_site()),
        get_metric("Norepinephrine dose, median [Q1,Q3]", selected_site()),
        get_metric("Vasopressin dose, median [Q1,Q3]", selected_site()),
        get_metric("Angiotensin dose, median [Q1,Q3]", selected_site()),
        get_metric("Dopamine dose, median [Q1,Q3]", selected_site()),
        get_metric("Epinephrine dose, median [Q1,Q3]", selected_site())
      )
    )
  })
  
  # Data table output
  output$data_table <- renderDT({
    datatable(site_data,
              options = list(
                pageLength = 15,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#f8f9fa', 'color': '#983232'});",
                  "}"
                )
              ),
              class = 'cell-border stripe',
              rownames = FALSE,
              filter = 'top',
              extensions = c('Buttons', 'FixedHeader'),
              style = 'bootstrap') %>%
      formatStyle(names(site_data),
                 backgroundColor = '#ffffff',
                 borderBottom = '1px solid #ddd')
  })

  # Sync map marker click to dropdown (no popups here)
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (!is.null(click$id) && click$id %in% sites$Site) {
      suppress_popup(TRUE)
      updateSelectInput(session, "site_selector", selected = click$id)
    }
  })
}

# Run the app
shinyApp(ui, server) 