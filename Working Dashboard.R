# Install required packages
packages <- c("shiny", "shinydashboard", "readxl", "dplyr", "ggplot2", 
              "plotly", "leaflet", "DT", "sf", "tidyr")

new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load all installed packages
lapply(packages, library, character.only = TRUE)

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard Header
  dashboardHeader(title = "School Progression Dashboard", titleWidth = 300),
  
  # Dashboard Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("County Analysis", tabName = "county", icon = icon("map-marker")),
      menuItem("School Factors", tabName = "factors", icon = icon("building")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table"))
    ),
    
    # County selection panel
    conditionalPanel(
      condition = "input.tabs == 'county'",
      selectInput("county_select", "Select County:", choices = NULL)
    ),
    
    # Factor selection panel
    conditionalPanel(
      condition = "input.tabs == 'factors'",
      checkboxGroupInput("factor_select", "Select Factors:",
                         choices = c("DEIS", "Fee-Paying", "Irish Speaking", "Gender", "School Size", "Ethos"),
                         selected = c("DEIS", "Fee-Paying"))
    ),
    
    
    # Data filters panel
    conditionalPanel(
      condition = "input.tabs == 'data'",
      selectInput("filter_county", "County:", choices = NULL, multiple = TRUE),
      selectInput("filter_deis", "DEIS Status:", choices = c("All", "Yes", "No")),
      selectInput("filter_fee", "Fee-Paying:", choices = c("All", "Yes", "No")),
      sliderInput("filter_rate", "Progression Rate:", min = 0, max = 100, value = c(0, 100))
    )
  ),
  
  # Dashboard Body
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            title = "School Progression Analysis",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            p("This dashboard provides insights into school progression rates across Ireland."),
            p("Use the sidebar to navigate between different views.")
          )
        ),
        
        # Summary metrics
        fluidRow(
          valueBoxOutput("avg_progression_box", width = 3),
          valueBoxOutput("top_county_box", width = 3),
          valueBoxOutput("deis_gap_box", width = 3),
          valueBoxOutput("total_schools_box", width = 3)
        ),
        
        # Main overview charts
        fluidRow(
          box(
            title = "Progression Rate Distribution",
            status = "primary",
            width = 6,
            solidHeader = TRUE,
            plotlyOutput("progression_hist", height = 300)
          ),
          box(
            title = "DEIS vs Non-DEIS Comparison",
            status = "primary",
            width = 6,
            solidHeader = TRUE,
            plotlyOutput("deis_comparison", height = 300)
          )
        ),
        
        # Secondary overview charts
        fluidRow(
          box(
            title = "Top 5 Counties by Progression Rate",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            plotlyOutput("top_counties", height = 300)
          ),
          box(
            title = "Bottom 5 Counties by Progression Rate",
            status = "warning",
            width = 6,
            solidHeader = TRUE,
            plotlyOutput("bottom_counties", height = 300)
          )
        )
      ),
      
      # County Analysis Tab
      tabItem(
        tabName = "county",
        fluidRow(
          box(
            title = "County Progression Map",
            status = "primary",
            width = 8,
            solidHeader = TRUE,
            leafletOutput("county_map", height = 500)
          ),
          box(
            title = "County Profile",
            status = "info",
            width = 4,
            solidHeader = TRUE,
            height = 550,  # Increased height to match the county map
            htmlOutput("county_summary"),
            plotlyOutput("county_comparison", height = 400)  # Increased height for better display
          )
            ),
          
        
        # County Trend Graph
        fluidRow(
          box(
            title = "County Progression Trend (Last 5 Years)",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            plotlyOutput("county_trend", height = 400)
          )
        ),
        fluidRow(
          box(
            title = "Schools in Selected County",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            DT::dataTableOutput("county_schools")
          )
        )
      ),
      
      # School Factors Tab
      tabItem(
        tabName = "factors",
        fluidRow(
          box(
            title = "Progression Rates by Selected Factors",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            plotlyOutput("factors_plot", height = 400)
          )
        ),
        fluidRow(
          box(
            title = "School Size vs Progression Rate",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            plotlyOutput("size_plot", height = 300)
          ),
          box(
            title = "School Profile Analysis",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            DT::dataTableOutput("profiles_table")
          )
        )
      ),
      
      # Data Explorer Tab
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "School Data Explorer",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            downloadButton("download_data", "Download Filtered Data", class = "btn-primary"),
            br(), br(),
            DT::dataTableOutput("data_table")
          )
        )
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Load data
  df <- reactive({
    data <- read_excel("Consolidated_DataSet.xlsx", sheet=1)
    
    # Data cleaning and transformation
    data <- data %>%
      # Convert progression rate to percentage
      mutate(ProgressionRate_Mean = ProgressionRate_Mean * 100) %>%
      # Ensure minimum value is 10%
      mutate(ProgressionRate_Mean = pmax(ProgressionRate_Mean, 10)) %>%
      # Ensure LCSits_Mean is numeric and round to nearest whole number
      mutate(LCSits_Mean = round(as.numeric(LCSits_Mean))) %>%
      # Categorize schools by size
      mutate(School_Size = case_when(
        LCSits_Mean < 50 ~ "Small",
        LCSits_Mean >= 50 & LCSits_Mean < 110 ~ "Medium",
        LCSits_Mean >= 110 ~ "Large"
      )) %>%
      # Convert binary variables to factors
      mutate(
        DEIS = factor(DEIS, levels = c(0,1), labels = c("No", "Yes")),
        IrishSpeaking = factor(IrishSpeaking, levels = c(0,1), labels = c("No", "Yes")),
        FeePaying = factor(FeePaying, levels = c(0,1), labels = c("No", "Yes")),
        Gender = as.factor(Gender),
        School_Size = as.factor(School_Size),
        Ethos = as.factor(Ethos)
      )
    
    return(data)
  })
  
  # County-level aggregate data
  county_data <- reactive({
    req(df())
    
    df() %>%
      group_by(County) %>%
      summarise(
        CountyProgressionRate = weighted.mean(ProgressionRate_Mean, w = LCSits_Mean, na.rm = TRUE),
        MedianProgressionRate = median(ProgressionRate_Mean, na.rm = TRUE),  # optional
        TotalSchools = n(),
        DEIS_Schools = sum(DEIS == "Yes", na.rm = TRUE),
        FeePaying_Schools = sum(FeePaying == "Yes", na.rm = TRUE),
        IrishSpeaking_Schools = sum(IrishSpeaking == "Yes", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(CountyProgressionRate))
  })
  

  
  # County progression rate trend graph with improved scaling
  output$county_trend <- renderPlotly({
    req(input$county_select, df())
    
    # Load county trend data
    df_county_trend <- read_excel("Consolidated_DataSet.xlsx", sheet = 3)
    
    
    # Filter data for selected county
    county_trend_data <- df_county_trend %>%
      filter(County == input$county_select) %>%
      pivot_longer(cols = starts_with("ProgressionRate_"), 
                   names_to = "Year", 
                   values_to = "ProgressionRate") %>%
      mutate(Year = as.integer(gsub("ProgressionRate_", "", Year))) %>%
      filter(Year != 2020) %>%  # Exclude 2020
      mutate(ProgressionRate = ProgressionRate - 15)  # Reduce by 15%
    
    # Adjust Y-axis limits dynamically
    y_min <- min(county_trend_data$ProgressionRate) * 0.9  # 10% buffer below
    y_max <- max(county_trend_data$ProgressionRate) * 1.1  # 10% buffer above
    
    # Create improved trend plot
    p <- ggplot(county_trend_data, aes(x = Year, y = ProgressionRate)) +
      geom_line(color = "blue", size = 1.5) +
      geom_point(color = "red", size = 4) +
      scale_y_continuous(limits = c(y_min, y_max)) +  # Fix zoom issues
      labs(title = paste("Progression Rate Trend in", input$county_select),
           x = "Year", y = "Progression Rate (%) (Adjusted)") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)
      )
    
    ggplotly(p) %>% layout(margin = list(l = 80, r = 80, b = 80, t = 50))  # Increase margins
  })
  
  # Load Ireland map data
  ireland_map <- reactive({
    # Check if the file exists, if not create dummy data for testing
    if(file.exists("ie.json")) {
      sf_map <- st_read("ie.json")
    } else {
      # Create warning
      warning("Map file 'ie.json' not found. Using dummy data for testing.")
      sf_map <- data.frame(name = unique(df()$County)) %>%
        st_as_sf(coords = c(1, 1), crs = 4326)
    }
    
    sf_map$name <- as.character(sf_map$name)
    
    # Fix county name discrepancies
    sf_map <- sf_map %>%
      mutate(name = case_when(
        name == "Laoighis" ~ "Laois",
        TRUE ~ name
      ))
    
    # Merge with county progression rates
    sf_map <- sf_map %>%
      left_join(county_data(), by = c("name" = "County"))
    
    return(sf_map)
  })
  
  # Update UI select inputs
  observe({
    req(df())
    counties <- sort(unique(df()$County))
    updateSelectInput(session, "county_select", choices = counties)
    updateSelectInput(session, "filter_county", choices = c("All", counties))
  })
  
  # Overview tab outputs
  output$avg_progression_box <- renderValueBox({
    req(df())
    avg_rate <- mean(df()$ProgressionRate_Mean, na.rm = TRUE)
    valueBox(
      paste0(round(avg_rate, 1), "%"), 
      "Average Progression Rate",
      icon = icon("graduation-cap"),
      color = "blue"
    )
  })
  
  output$top_county_box <- renderValueBox({
    req(county_data())
    top_county <- county_data()[1,]
    valueBox(
      top_county$County, 
      paste0("Top County (", round(top_county$CountyProgressionRate, 1), "%)"),
      icon = icon("medal"),
      color = "green"
    )
  })
  
  output$deis_gap_box <- renderValueBox({
    req(df())
    deis_avg <- mean(df()[df()$DEIS == "Yes",]$ProgressionRate_Mean, na.rm = TRUE)
    non_deis_avg <- mean(df()[df()$DEIS == "No",]$ProgressionRate_Mean, na.rm = TRUE)
    gap <- non_deis_avg - deis_avg
    
    valueBox(
      paste0(round(gap, 1), "%"), 
      "DEIS vs Non-DEIS Gap",
      icon = icon("balance-scale"),
      color = "yellow"
    )
  })
  
  output$total_schools_box <- renderValueBox({
    req(df())
    valueBox(
      nrow(df()), 
      "Total Schools",
      icon = icon("school"),
      color = "purple"
    )
  })
  
  # Progression rate histogram
  output$progression_hist <- renderPlotly({
    req(df())
    p <- ggplot(df(), aes(x = ProgressionRate_Mean)) +
      geom_histogram(fill = "steelblue", bins = 25, alpha = 0.7) +
      labs(x = "Progression Rate (%)", y = "Number of Schools") +
      theme_minimal()
    
    ggplotly(p) %>% layout(margin = list(l = 50, r = 50, b = 50, t = 10))
  })
  
  # DEIS vs Non-DEIS comparison
  output$deis_comparison <- renderPlotly({
    req(df())
    deis_data <- df() %>%
      group_by(DEIS) %>%
      summarise(
        AvgProgression = weighted.mean(ProgressionRate_Mean, w = LCSits_Mean, na.rm = TRUE),
        Count = n(),
        .groups = 'drop'
      )
    
    p <- ggplot(deis_data, aes(x = DEIS, y = AvgProgression, fill = DEIS)) +
      geom_col() +
      geom_text(aes(label = round(AvgProgression, 1)), vjust = +5.0, fontface = "bold") +
      labs(x = "DEIS Status", y = "Average Progression Rate (%)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p) %>% layout(margin = list(l = 50, r = 50, b = 50, t = 10))
  })
  
  # Top 5 counties
  output$top_counties <- renderPlotly({
    req(county_data())
    top5 <- head(county_data(), 5)
    
    p <- ggplot(top5, aes(x = reorder(County, CountyProgressionRate), y = CountyProgressionRate, fill = CountyProgressionRate)) +
      geom_col() +
      geom_text(aes(label = paste0(round(CountyProgressionRate, 1), "%")), hjust = -0.3, fontface = "bold") +
      labs(x = "", y = "Progression Rate (%)") +
      theme_minimal() +
      theme(legend.position = "none") +
      coord_flip()
    
    ggplotly(p) %>% layout(margin = list(l = 80, r = 50, b = 50, t = 10))
  })
  
  # Bottom 5 counties
  output$bottom_counties <- renderPlotly({
    req(county_data())
    bottom5 <- tail(county_data(), 5)
    
    p <- ggplot(bottom5, aes(x = reorder(County, -CountyProgressionRate), y = CountyProgressionRate, fill = CountyProgressionRate)) +
      geom_col() +
      geom_text(aes(label = paste0(round(CountyProgressionRate, 1), "%")), hjust = -0.3, fontface = "bold") +
      labs(x = "", y = "Progression Rate (%)") +
      theme_minimal() +
      theme(legend.position = "none") +
      coord_flip()
    
    ggplotly(p) %>% layout(margin = list(l = 80, r = 50, b = 50, t = 10))
  })
  
  # County map
  output$county_map <- renderLeaflet({
    req(ireland_map())
    
    # Create color palette
    pal <- colorBin(
      palette = "YlOrRd",
      domain = ireland_map()$CountyProgressionRate,
      bins = 7,
      reverse = TRUE
    )
    
    # Create popup content
    popup_content <- paste(
      "<strong>County:</strong> ", ireland_map()$name, "<br>",
      "<strong>Progression Rate:</strong> ", round(ireland_map()$CountyProgressionRate, 1), "%<br>",
      "<strong>Total Schools:</strong> ", ireland_map()$TotalSchools, "<br>",
      "<strong>DEIS Schools:</strong> ", ireland_map()$DEIS_Schools, "<br>",
      "<strong>Fee-Paying Schools:</strong> ", ireland_map()$FeePaying_Schools,
      sep = ""
    )
    
    leaflet(data = ireland_map()) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(CountyProgressionRate),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        highlight = highlightOptions(weight = 3, color = "black", bringToFront = TRUE),
        popup = popup_content
      ) %>%
      addLegend(
        pal = pal, values = ireland_map()$CountyProgressionRate, opacity = 0.7,
        title = "Mean Progression Rate (%)", position = "bottomright"
      )
  })
  
  # County comparison plot with improved scaling and fix for missing bars
  output$county_comparison <- renderPlotly({
    req(input$county_select, df())
    
    # Load county trend data
    df_county_trend <- read_excel("Consolidated_DataSet.xlsx", sheet = 3)
    
    # Get selected county data
    selected_county_current <- df() %>%
      filter(County == input$county_select) %>%
      summarise(AvgRate = mean(ProgressionRate_Mean, na.rm = TRUE)) %>%
      pull(AvgRate)
    
    # Get national average
    national_avg <- df() %>%
      summarise(AvgRate = mean(ProgressionRate_Mean, na.rm = TRUE)) %>%
      pull(AvgRate)
    
    # Ensure no NA values
    selected_county_current <- ifelse(is.na(selected_county_current), 0, selected_county_current)
    national_avg <- ifelse(is.na(national_avg), 0, national_avg)
    
    # Create data frame for plotting
    compare_data <- data.frame(
      Category = c(input$county_select, "National Average"),
      Rate = c(selected_county_current, national_avg)
    )
    
    # Convert category to factor for proper grouping
    compare_data$Category <- factor(compare_data$Category, levels = compare_data$Category)
    
    # Ensure valid y-axis limits
    y_min <- 0
    y_max <- 100
    
    # Create the plot
    p <- ggplot(compare_data, aes(x = Category, y = Rate, fill = Category, group = Category)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = paste0(round(Rate, 1), "%")), vjust = -0.8, size = 4, fontface = "bold") +
      scale_y_continuous(limits = c(y_min, y_max), expand = c(0, 0)) +  # Fixes hidden bars
      labs(x = "", y = "Mean Progression Rate (%)") +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 12)
      )
    
    # Convert to plotly and ensure proper rendering
    ggplotly(p) %>%
      layout(
        autosize = TRUE,
        margin = list(l = 70, r = 40, b = 70, t = 30),
        height = 350
      )
  })
  
  
  
  
  # County summary
  output$county_summary <- renderUI({
    req(input$county_select, county_data())
    
    selected_data <- county_data() %>%
      filter(County == input$county_select)
    
    summary_text <- paste(
      "<strong>County:</strong> ", input$county_select, "<br>",
      "<strong>Mean Progression Rate:</strong> ", round(selected_data$CountyProgressionRate, 1), "%<br>",
      "<strong>Total Schools:</strong> ", selected_data$TotalSchools, "<br>",
      "<strong>DEIS Schools:</strong> ", selected_data$DEIS_Schools, "<br>",
      "<strong>Fee-Paying Schools:</strong> ", selected_data$FeePaying_Schools, "<br>",
      "<strong>Irish Speaking Schools:</strong> ", selected_data$IrishSpeaking_Schools,
      sep = ""
    )
    
    HTML(summary_text)
  })
  
  # County schools table
  output$county_schools <- DT::renderDataTable({
    req(input$county_select, df())
    
    county_schools <- df() %>%
      filter(County == input$county_select) %>%
      select(SchoolName, ProgressionRate_Mean, DEIS, FeePaying, Gender, School_Size, LCSits_Mean) %>%
      mutate(ProgressionRate_Mean = round(ProgressionRate_Mean, 1))
    
    DT::datatable(county_schools, options = list(pageLength = 10))
  })
  
  # Factors plot
  output$factors_plot <- renderPlotly({
    req(df(), input$factor_select)
    
    if(length(input$factor_select) == 0) {
      return(NULL)
    }
    
    # Choose the first factor for primary grouping
    primary_factor <- input$factor_select[1]
    
    # Handle potential column name issues
    if (primary_factor == "Fee-Paying") {
      primary_factor <- "FeePaying"
    } else if (primary_factor == "Irish Speaking") {
      primary_factor <- "IrishSpeaking"
    }
    
    # Handle multiple factor selections
    if (length(input$factor_select) > 1) {
      # Get secondary factor for filling
      secondary_factor <- input$factor_select[2]
      
      # Handle potential column name issues for secondary factor
      if (secondary_factor == "Fee-Paying") {
        secondary_factor <- "FeePaying"
      } else if (secondary_factor == "Irish Speaking") {
        secondary_factor <- "IrishSpeaking"
      }
      
      # Create formula for grouping
      group_vars <- c(primary_factor, secondary_factor)
      
      # Group by multiple factors
      factor_data <- df() %>%
        group_by_at(vars(all_of(group_vars))) %>%
        summarise(
          AvgProgression = mean(ProgressionRate_Mean, na.rm = TRUE),
          Count = n(),
          .groups = 'drop'
        )
      
      # Create a position dodge for side-by-side bars
      position <- position_dodge(width = 0.9)
      
      # Create the plot with secondary factor as fill
      p <- ggplot(factor_data, aes_string(x = primary_factor, y = "AvgProgression", 
                                          fill = secondary_factor)) +
        geom_col(position = position, width = 0.8) +
        geom_text(aes(label = paste0(round(AvgProgression, 1), "%")), 
                  position = position, vjust = -0.8, size = 3.5, fontface = "bold") +
        labs(x = gsub("([A-Z])", " \\1", primary_factor), 
             y = "Average Progression Rate (%)",
             fill = gsub("([A-Z])", " \\1", secondary_factor)) +
        theme_minimal() +
        theme(
          legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    } else {
      # Simple plot for single factor
      factor_data <- df() %>%
        group_by_at(vars(all_of(primary_factor))) %>%
        summarise(
          AvgProgression = mean(ProgressionRate_Mean, na.rm = TRUE),
          Count = n(),
          .groups = 'drop'
        )
      
      p <- ggplot(factor_data, aes_string(x = primary_factor, y = "AvgProgression", 
                                          fill = primary_factor)) +
        geom_col() +
        geom_text(aes(label = paste0(round(AvgProgression, 1), "%")), vjust = -0.8, fontface = "bold") +
        labs(x = gsub("([A-Z])", " \\1", primary_factor), 
             y = "Average Progression Rate (%)") +
        theme_minimal() +
        theme(legend.position = "none")
    }
    
    # Adjust the plot with margins that fit the container
    ggplotly(p) %>% layout(
      autosize = TRUE,
      margin = list(l = 60, r = 40, b = 80, t = 20),
      legend = list(orientation = "h", y = -0.2)
    )
  })
  
  # School size plot
  output$size_plot <- renderPlotly({
    req(df())
    
    p <- ggplot(df(), aes(x = LCSits_Mean, y = ProgressionRate_Mean, color = School_Size)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      labs(x = "Number of LC Students", y = "Progression Rate (%)") +
      theme_minimal()
    
    ggplotly(p) %>% layout(margin = list(l = 50, r = 50, b = 50, t = 10))
  })
  
  # School profiles table
  output$profiles_table <- renderDataTable({
    req(df())
    
    profile_summary <- df() %>%
      group_by(DEIS, FeePaying, School_Size) %>%
      summarise(
        AvgProgression = mean(ProgressionRate_Mean, na.rm = TRUE),
        Count = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(AvgProgression)) %>%
      mutate(AvgProgression = round(AvgProgression, 1))
    
    DT::datatable(profile_summary, options = list(pageLength = 10))
  })
  
  # Data explorer table with filtering
  output$data_table <- renderDataTable({
    req(df())
    
    filtered_data <- df()
    
    # Apply county filter
    if(!is.null(input$filter_county) && !("All" %in% input$filter_county)) {
      filtered_data <- filtered_data %>% filter(County %in% input$filter_county)
    }
    
    # Apply DEIS filter
    if(input$filter_deis != "All") {
      filtered_data <- filtered_data %>% filter(DEIS == input$filter_deis)
    }
    
    # Apply fee-paying filter
    if(input$filter_fee != "All") {
      filtered_data <- filtered_data %>% filter(FeePaying == input$filter_fee)
    }
    
    # Apply progression rate filter
    filtered_data <- filtered_data %>% 
      filter(ProgressionRate_Mean >= input$filter_rate[1], 
             ProgressionRate_Mean <= input$filter_rate[2]) %>%
      mutate(ProgressionRate_Mean = round(ProgressionRate_Mean, 1))
    
    DT::datatable(filtered_data, options = list(pageLength = 10))
  })
  
  # Download handler for filtered data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("school_progression_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Get the current filtered data
      filtered_data <- df()
      
      # Apply county filter
      if(!is.null(input$filter_county) && !("All" %in% input$filter_county)) {
        filtered_data <- filtered_data %>% filter(County %in% input$filter_county)
      }
      
      # Apply DEIS filter
      if(input$filter_deis != "All") {
        filtered_data <- filtered_data %>% filter(DEIS == input$filter_deis)
      }
      
      # Apply fee-paying filter
      if(input$filter_fee != "All") {
        filtered_data <- filtered_data %>% filter(FeePaying == input$filter_fee)
      }
      
      # Apply progression rate filter
      filtered_data <- filtered_data %>% 
        filter(ProgressionRate_Mean >= input$filter_rate[1], 
               ProgressionRate_Mean <= input$filter_rate[2]) %>%
        mutate(ProgressionRate_Mean = round(ProgressionRate_Mean, 1))
      
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

