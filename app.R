# Run the application 

library(shiny)
library(tidyverse)
library(DT)
library(plotly)
library(bslib)
library(leaflet)

# 1: Load Processed Datasets
cost_burden_2011_2012 <- read_csv("data/processed/cost_burden_2011_2012_clean.csv")
cost_burden_2013_2017 <- read_csv("data/processed/cost_burden_2013_2017_clean.csv")
cost_burden_2017_2024 <- read_csv("data/processed/cost_burden_2017_2024_clean.csv")
care_cost <- read_csv("data/processed/care_due_to_cost_2011_2024_clean.csv")
care_access <- read_csv("data/processed/care_access_2011_2024_clean.csv")
insurance <- read_csv("data/processed/insurance_clean.csv")

# 2: Combine Cost Burden Datasets
cost_burden_all <- bind_rows(
    cost_burden_2011_2012,
    cost_burden_2013_2017,
    cost_burden_2017_2024 |>
        filter(Timeframe != 2017)
)

# 3: Convert into Percentages
cost_analysis <- cost_burden_all |>
    mutate(
        Percent = Data * 100,
        MOE_Percent = MOE * 100
    )

care_analysis <- care_cost |>
    mutate(
        Percent = Data * 100,
        MOE_Percent = MOE * 100
    )

insurance_percent <- insurance |>
    filter(Data_Type == "Percent") |>
    mutate(
        Percent = Data * 100
    )

# 4: UI

ui <- page_navbar(
    title = "Health Care Affordability in Illinois",
    theme = bs_theme(
        version = 5,
        bootswatch = "flatly"
    ),
    
   # Overview
    nav_panel(
        "Overview",
        layout_columns(
            card(
                card_header("Research Question"),
                p(
                    "How have medical cost burdens and barriers to obtaining ",
                    "needed medical care changed over time in Illinois compared ",
                    "with the United States, and how do these patterns differ ",
                    "by race/ethnicity and insurance coverage?"
                )
            ),
            card(
                card_header("Study Period"),
                h3("2011–2024"),
                p("Illinois compared with the United States")
            )
        ),
        br(),
        layout_columns(
            value_box(
                title = "Illinois Cost Burden — 2011",
                value = textOutput("overview_cost_2011")
            ),
            value_box(
                title = "Illinois Cost Burden — 2024",
                value = textOutput("overview_cost_2024")
            ),
            value_box(
                title = "Illinois Uninsured — 2011",
                value = textOutput("overview_uninsured_2011")
            ),
            value_box(
                title = "Illinois Uninsured — 2024",
                value = textOutput("overview_uninsured_2024")
            )
        ),
        br(),
        card(
            card_header("Medical Cost Burden Over Time"),
            plotlyOutput("overview_cost_plot")
        ),
        br(),
        card(
            card_header("Key Findings"),
            uiOutput("overview_findings")
        )
    ),
   
    # Data Explorer
   nav_panel(
       "Data Explorer",
       layout_sidebar(
           sidebar = sidebar(
               selectInput(
                   "explorer_location",
                   "Location",
                   choices = c("Illinois", "United States"),
                   selected = "Illinois"
               ),
               sliderInput(
                   "explorer_year",
                   "Year Range",
                   min = 2011,
                   max = 2024,
                   value = c(2011, 2024),
                   sep = ""
               ),
               selectInput(
                   "explorer_race",
                   "Race/Ethnicity - select one or more",
                   choices = sort(unique(cost_analysis$Race_Ethnicity)),
                   selected = sort(unique(cost_analysis$Race_Ethnicity)),
                   multiple = TRUE
               ),
               textInput(
                   "search_text",
                   "Search Race/Ethnicity",
                   placeholder = "Type to search..."
               ),
               selectInput(
                   "explorer_measure",
                   "Measure",
                   choices = c(
                       "Medical Cost Burden",
                       "Care Barrier",
                       "Uninsured Rate"
                   ),
                   selected = "Medical Cost Burden"
               )
           ),
           card(
               card_header("Interactive Trend"),
               plotlyOutput("explorer_plot")
           ),
           card(
               card_header("Selected Data"),
               DTOutput("explorer_table")
           )
       )
   ),
   
    # Results
    nav_panel(
        "Results / Findings",
        card(
            card_header("Medical Cost Burden"),
            plotlyOutput("results_cost_race")
        ),
        br(),
        card(
            card_header("Cost-Related Barriers to Care"),
            plotlyOutput("results_care")
        ),
        br(),
        card(
            card_header("Uninsured Rates"),
            plotlyOutput("results_uninsured")
        ),
        br(),
        card(
            card_header("Medical Cost Burden vs. Care Barrier"),
            plotlyOutput("results_correlation")
        ),
        br(),
        card(
            card_header("Correlation Result"),
            verbatimTextOutput("correlation_result")
        )
    ),

    # About
    nav_panel(
        "About the Project",
        card(
            card_header("About This Project"),
            h3("Research Question"),
            p(
                "How have medical cost burdens and barriers to obtaining ",
                "needed medical care changed over time in Illinois compared ",
                "with the United States?"
            ),
            h3("Data"),
            p(
                "The analysis uses processed health-care datasets covering ",
                "2011 through 2024. The datasets contain information on ",
                "medical cost burden, cost-related barriers to care, and ",
                "health insurance coverage."
            ),
            h3("Methods"),
            tags$ul(
                tags$li("Data cleaning and transformation"),
                tags$li("Descriptive statistics"),
                tags$li("Longitudinal comparisons"),
                tags$li("Race and ethnicity comparisons"),
                tags$li("Pearson correlation"),
                tags$li("Interactive visualization")
            ),
            h3("Limitations"),
            p(
                "The analysis uses aggregated data and therefore cannot ",
                "establish individual-level relationships or causation. ",
                "The correlation analysis does not control for factors ",
                "such as income, employment, age, health status, or ",
                "insurance type."
            ),
            h3("Data Source"),
            p(
                "State Health Compare, State Health Access Data Assistance ",
                "Center (SHADAC), University of Minnesota."
            )
        )
    )
)

# Server

server <- function(input, output, session) {
    # Race/Ethnicity Choices
    observe({
        races <- sort(unique(cost_analysis$Race_Ethnicity))
        
        updateSelectInput(
            session,
            "explorer_race",
            choices = races,
            selected = races
        )
    })
    # Overview Values
    output$overview_cost_2011 <- renderText({
        value <- cost_analysis |>
            filter(
                Location == "Illinois",
                Timeframe == 2011
            ) |>
            summarise(
                value = mean(Percent, na.rm = TRUE)
            ) |>
            pull(value)
        paste0(round(value, 1), "%")
    })
    output$overview_cost_2024 <- renderText({
        value <- cost_analysis |>
            filter(
                Location == "Illinois",
                Timeframe == 2024
            ) |>
            summarise(
                value = mean(Percent, na.rm = TRUE)
            ) |>
            pull(value)
        paste0(round(value, 1), "%")
    })
    output$overview_uninsured_2011 <- renderText({
        value <- insurance_percent |>
            filter(
                Location == "Illinois",
                Timeframe == 2011,
                Coverage_Type == "Uninsured"
            ) |>
            summarise(
                value = mean(Percent, na.rm = TRUE)
            ) |>
            pull(value)
        paste0(round(value, 1), "%")
    })
    output$overview_uninsured_2024 <- renderText({
        value <- insurance_percent |>
            filter(
                Location == "Illinois",
                Timeframe == 2024,
                Coverage_Type == "Uninsured"
            ) |>
            summarise(
                value = mean(Percent, na.rm = TRUE)
            ) |>
            pull(value)
        paste0(round(value, 1), "%")
    })

    # Overview Plot 
    output$overview_cost_plot <- renderPlotly({
        data <- cost_analysis |>
            group_by(Location, Timeframe) |>
            summarise(
                Average_Burden = mean(
                    Percent,
                    na.rm = TRUE
                ),
                .groups = "drop"
            )
        p <- ggplot(
            data,
            aes(
                x = Timeframe,
                y = Average_Burden,
                color = Location
            )
        ) +
            geom_line(linewidth = 1) +
            geom_point(size = 2) +
            labs(
                title = "Average Medical Cost Burden Over Time",
                x = "Year",
                y = "Average Percent"
            ) +
            theme_minimal()
        ggplotly(p)
    })
    
    # Overview Findings
    output$overview_findings <- renderUI({
        tags$ul(
            tags$li(
                "Medical cost burden declined in both Illinois and the United States."
            ),
            tags$li(
                "Cost-related barriers to obtaining needed care also declined."
            ),
            tags$li(
                "Uninsured rates decreased substantially during the study period."
            ),
            tags$li(
                "Racial and ethnic disparities remained despite overall improvements."
            ),
            tags$li(
                "The Pearson correlation did not show a statistically significant linear relationship between cost burden and care barriers."
            )
        )
    })

    # Search and Filter Data
    filtered_cost <- reactive({
        req(input$explorer_race)
        data <- cost_analysis |>
            filter(
                Location == input$explorer_location,
                Timeframe >= input$explorer_year[1],
                Timeframe <= input$explorer_year[2],
                Race_Ethnicity %in% input$explorer_race
            )
        if (input$search_text != "") {
            data <- data |>
                filter(
                    str_detect(
                        str_to_lower(Race_Ethnicity),
                        str_to_lower(input$search_text)
                    )
                )
        }
        data
    })
    
    # Explorer 
    # Explorer Plot
    output$explorer_plot <- renderPlotly({
        req(input$explorer_race)
        # Medical Cost Burden
        if (input$explorer_measure == "Medical Cost Burden") {
            data <- cost_analysis |>
                filter(
                    Location == input$explorer_location,
                    Timeframe >= input$explorer_year[1],
                    Timeframe <= input$explorer_year[2],
                    Race_Ethnicity %in% input$explorer_race
                )
            
            y_label <- "Medical Cost Burden (%)"
            
            # Care Barrier
        } else if (input$explorer_measure == "Care Barrier") {
            
            data <- care_analysis |>
                filter(
                    Location == input$explorer_location,
                    Timeframe >= input$explorer_year[1],
                    Timeframe <= input$explorer_year[2],
                    Race_Ethnicity %in% input$explorer_race
                )
            
            y_label <- "Unable to Obtain Needed Care Due to Cost (%)"
            
            # Uninsured Rate
        } else {
            data <- insurance_percent |>
                filter(
                    Location == input$explorer_location,
                    Timeframe >= input$explorer_year[1],
                    Timeframe <= input$explorer_year[2],
                    Race_Ethnicity %in% input$explorer_race,
                    Coverage_Type == "Uninsured"
                )
            
            y_label <- "Uninsured Rate (%)"
        }
        # Search filter
        if (input$search_text != "") {
            data <- data |>
                filter(
                    str_detect(
                        str_to_lower(Race_Ethnicity),
                        str_to_lower(input$search_text)
                    )
                )
        }
        # Average values when multiple rows exist
        data <- data |>
            group_by(
                Location,
                Timeframe,
                Race_Ethnicity
            ) |>
            summarise(
                Average = mean(Percent, na.rm = TRUE),
                .groups = "drop"
            )
        # Create graph
        p <- ggplot(
            data,
            aes(
                x = Timeframe,
                y = Average,
                color = Race_Ethnicity,
                group = Race_Ethnicity,
                text = paste0(
                    "Race/Ethnicity: ", Race_Ethnicity,
                    "<br>Year: ", Timeframe,
                    "<br>Percent: ", round(Average, 1), "%"
                )
            )
        ) +
            geom_line(linewidth = 1) +
            geom_point(size = 2) +
            labs(
                title = input$explorer_measure,
                x = "Year",
                y = y_label,
                color = "Race/Ethnicity"
            ) +
            theme_minimal()
        ggplotly(
            p,
            tooltip = "text"
        )
    })
        
    # Interactive Data Table
    output$explorer_table <- renderDT({
        # Medical Cost Burden
        if (input$explorer_measure == "Medical Cost Burden") {
            data <- cost_analysis |>
                filter(
                    Location == input$explorer_location,
                    Timeframe >= input$explorer_year[1],
                    Timeframe <= input$explorer_year[2],
                    Race_Ethnicity %in% input$explorer_race
                )
            if (input$search_text != "") {
                data <- data |>
                    filter(
                        str_detect(
                            str_to_lower(Race_Ethnicity),
                            str_to_lower(input$search_text)
                        )
                    )
            }
            data <- data |>
                select(
                    Location,
                    Timeframe,
                    Race_Ethnicity,
                    Percent,
                    MOE_Percent
                )
        }
        
        # Care Barrier
        else if (input$explorer_measure == "Care Barrier") {
            data <- care_analysis |>
                filter(
                    Location == input$explorer_location,
                    Timeframe >= input$explorer_year[1],
                    Timeframe <= input$explorer_year[2],
                    Race_Ethnicity %in% input$explorer_race
                )
            if (input$search_text != "") {
                data <- data |>
                    filter(
                        str_detect(
                            str_to_lower(Race_Ethnicity),
                            str_to_lower(input$search_text)
                        )
                    )
            }
            data <- data |>
                select(
                    Location,
                    Timeframe,
                    Race_Ethnicity,
                    Percent,
                    MOE_Percent
                )
        }
        
        # Uninsured Rate
        else {
            data <- insurance_percent |>
                filter(
                    Location == input$explorer_location,
                    Timeframe >= input$explorer_year[1],
                    Timeframe <= input$explorer_year[2],
                    Race_Ethnicity %in% input$explorer_race,
                    Coverage_Type == "Uninsured"
                )
            if (input$search_text != "") {
                data <- data |>
                    filter(
                        str_detect(
                            str_to_lower(Race_Ethnicity),
                            str_to_lower(input$search_text)
                        )
                    )
            }
            data <- data |>
                select(
                    Location,
                    Timeframe,
                    Race_Ethnicity,
                    Coverage_Type,
                    Percent
                )
        }
        datatable(
            data,
            filter = "top",
            options = list(
                pageLength = 10,
                scrollX = TRUE
            )
        )
    })
    
    # Results: Cost Burden by Race
    output$results_cost_race <- renderPlotly({
        data <- cost_analysis |>
            group_by(
                Location,
                Race_Ethnicity
            ) |>
            summarise(
                Average_Burden = mean(
                    Percent,
                    na.rm = TRUE
                ),
                .groups = "drop"
            )
        p <- ggplot(
            data,
            aes(
                x = Race_Ethnicity,
                y = Average_Burden,
                fill = Location
            )
        ) +
            geom_col(
                position = "dodge"
            ) +
            labs(
                title = "Average Medical Cost Burden by Race/Ethnicity",
                x = "Race/Ethnicity",
                y = "Average Percent"
            ) +
            theme_minimal() +
            theme(
                axis.text.x = element_text(
                    angle = 30,
                    hjust = 1
                )
            )
        ggplotly(p)
    })
    
    # Results: Care Barriers
    output$results_care <- renderPlotly({
        data <- care_analysis |>
            group_by(
                Location,
                Timeframe
            ) |>
            summarise(
                Average = mean(
                    Percent,
                    na.rm = TRUE
                ),
                .groups = "drop"
            )
        p <- ggplot(
            data,
            aes(
                x = Timeframe,
                y = Average,
                color = Location
            )
        ) +
            geom_line(
                linewidth = 1
            ) +
            geom_point() +
            labs(
                title = "Difficulty Obtaining Needed Care Due to Cost",
                x = "Year",
                y = "Percent"
            ) +
            theme_minimal()
        ggplotly(p)
    })
    
    
    # Results: Uninsured
    output$results_uninsured <- renderPlotly({
        data <- insurance_percent |>
            filter(
                Coverage_Type == "Uninsured"
            ) |>
            group_by(
                Location,
                Timeframe
            ) |>
            summarise(
                Average = mean(
                    Percent,
                    na.rm = TRUE
                ),
                .groups = "drop"
            )
        p <- ggplot(
            data,
            aes(
                x = Timeframe,
                y = Average,
                color = Location
            )
        ) +
            geom_line(
                linewidth = 1
            ) +
            geom_point() +
            labs(
                title = "Uninsured Rate Over Time",
                x = "Year",
                y = "Uninsured (%)"
            ) +
            theme_minimal()
        ggplotly(p)
    })
    
    
    # Correlation Plot
    output$results_correlation <- renderPlotly({
        combined_analysis <- cost_analysis |>
            select(
                Location,
                Race_Ethnicity,
                Timeframe,
                Cost_Burden = Percent
            ) |>
            inner_join(
                care_analysis |>
                    select(
                        Location,
                        Race_Ethnicity,
                        Timeframe,
                        Care_Barrier = Percent
                    ),
                by = c(
                    "Location",
                    "Race_Ethnicity",
                    "Timeframe"
                )
            )
        p <- ggplot(
            combined_analysis,
            aes(
                x = Cost_Burden,
                y = Care_Barrier,
                color = Location
            )
        ) +
            geom_point(
                alpha = 0.7,
                size = 2.5
            ) +
            geom_smooth(
                method = "lm",
                se = FALSE
            ) +
            labs(
                title = "Medical Cost Burden and Difficulty Accessing Care",
                x = "Medical Cost Burden (%)",
                y = "Unable to Obtain Needed Care Due to Cost (%)"
            ) +
            theme_minimal()
        ggplotly(p)
    })
    
    # Correlation Result
    output$correlation_result <- renderPrint({
        combined_analysis <- cost_analysis |>
            select(
                Location,
                Race_Ethnicity,
                Timeframe,
                Cost_Burden = Percent
            ) |>
            inner_join(
                care_analysis |>
                    select(
                        Location,
                        Race_Ethnicity,
                        Timeframe,
                        Care_Barrier = Percent
                    ),
                by = c(
                    "Location",
                    "Race_Ethnicity",
                    "Timeframe"
                )
            )
        cor.test(
            combined_analysis$Cost_Burden,
            combined_analysis$Care_Barrier,
            use = "complete.obs"
        )
    })
}
shinyApp(ui = ui, server = server)
