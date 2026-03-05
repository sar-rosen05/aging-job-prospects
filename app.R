library(shiny)
library(tidyverse)
library(readxl)
library(plotly)

# Load & Clean 11b Data
load_11b <- function(file, year) {
  df <- read_excel(file, skip = 7, col_names = FALSE)
  colnames(df) <- c("Occupation", "Total", "Age_16_19", "Age_20_24",
                    "Age_25_34", "Age_35_44", "Age_45_54", "Age_55_64",
                    "Age_65plus", "Median_Age")
  df %>%
    filter(!is.na(Occupation), !Occupation %in% c("NA", "Total employed")) %>%
    mutate(Year = year) %>%
    mutate(across(c(Total, starts_with("Age_"), Median_Age),
                  ~ suppressWarnings(as.numeric(gsub(",", "", .)))))
}

years_files <- c("2011","2012","2013","2014","2015","2016","2017",
                 "2018","2019","2020","2021","2022","2023","2024")

all_11b <- map_dfr(years_files, function(yr) {
  file <- paste0("11b_", yr, ".xlsx")
  tryCatch(load_11b(file, as.integer(yr)), error = function(e) NULL)
})

# Load Table 8 Data
all_8 <- read_excel("Table_8_Data_Tidy.xlsx") %>%
  mutate(Year = as.integer(Year))

# UI
ui <- fluidPage(
  titlePanel("How Occupation Distribution Shifts Across Age Groups Over Time (Ages 25+)"),

  tabsetPanel(

    tabPanel("Occupation Distribution by Age",
      sidebarLayout(
        sidebarPanel(
          selectInput("occupation", "Select Occupation:",
                      choices = sort(unique(all_11b$Occupation)),
                      selected = "Management, professional, and related occupations"),
          sliderInput("year_range", "Year Range:",
                      min = 2011, max = 2024,
                      value = c(2011, 2024),
                      step = 1, sep = ""),
          selectInput("chart_type", "Chart Type:",
                      choices = c("Grouped Bar" = "bar", "Line Trend" = "line"))
        ),
        mainPanel(
          plotlyOutput("occ_chart", height = "400px"),
          br(),
          plotlyOutput("share_chart", height = "300px")
        )
      )
    ),

    tabPanel("Work Status by Age (25+)",
      sidebarLayout(
        sidebarPanel(
          selectInput("sex_filter", "Sex:",
                      choices = c("All", unique(all_8$Sex)),
                      selected = "All"),
          selectInput("race_filter", "Race:",
                      choices = c("All", unique(all_8$Race)),
                      selected = "All"),
          sliderInput("year_range_8", "Year Range:",
                      min = min(all_8$Year), max = max(all_8$Year),
                      value = c(min(all_8$Year), max(all_8$Year)),
                      step = 1, sep = "")
        ),
        mainPanel(
          plotlyOutput("status_chart", height = "400px")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  filtered_11b <- reactive({
    all_11b %>%
      filter(
        Occupation == input$occupation,
        Year >= input$year_range[1],
        Year <= input$year_range[2]
      ) %>%
      select(Year, Age_25_34, Age_35_44, Age_45_54, Age_55_64, Age_65plus) %>%
      pivot_longer(-Year, names_to = "Age_Group", values_to = "Workers") %>%
      mutate(
        Age_Label = factor(Age_Group,
                           levels = c("Age_25_34","Age_35_44","Age_45_54",
                                      "Age_55_64","Age_65plus"),
                           labels = c("25-34","35-44","45-54","55-64","65+")),
        Workers = replace_na(Workers, 0)
      )
  })

  output$occ_chart <- renderPlotly({
    df <- filtered_11b()
    if (input$chart_type == "bar") {
      plot_ly(df, x = ~factor(Year), y = ~Workers, color = ~Age_Label,
              type = "bar",
              text = ~paste0(Age_Label, ": ", scales::comma(Workers), "k"),
              hoverinfo = "text") %>%
        layout(barmode = "group",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Workers (thousands)"),
               title = paste("Occupation Distribution by Age:", input$occupation))
    } else {
      plot_ly(df, x = ~Year, y = ~Workers, color = ~Age_Label,
              type = "scatter", mode = "lines+markers",
              text = ~paste0(Age_Label, ": ", scales::comma(Workers), "k"),
              hoverinfo = "text") %>%
        layout(xaxis = list(title = "Year"),
               yaxis = list(title = "Workers (thousands)"),
               title = paste("Occupation Shift Over Time:", input$occupation))
    }
  })

  output$share_chart <- renderPlotly({
    df <- filtered_11b() %>%
      group_by(Year) %>%
      mutate(Share = round(Workers / sum(Workers, na.rm = TRUE) * 100, 1)) %>%
      ungroup()
    plot_ly(df, x = ~factor(Year), y = ~Share, color = ~Age_Label,
            type = "bar",
            text = ~paste0(Age_Label, ": ", Share, "%"),
            hoverinfo = "text") %>%
      layout(barmode = "stack",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Share (%)", range = c(0, 100)),
             title = "Age Group Share of Occupation Over Time")
  })

  filtered_8 <- reactive({
    df <- all_8 %>%
      filter(
        Year >= input$year_range_8[1],
        Year <= input$year_range_8[2],
        Age %in% c("25 to 54", "55+")
      )
    if (input$sex_filter != "All") df <- df %>% filter(Sex == input$sex_filter)
    if (input$race_filter != "All") df <- df %>% filter(Race == input$race_filter)
    df %>%
      group_by(Year, Age) %>%
      summarise(FT = sum(FT, na.rm = TRUE),
                PT = sum(PT, na.rm = TRUE),
                Unemp = sum(Unemp, na.rm = TRUE), .groups = "drop") %>%
      pivot_longer(c(FT, PT, Unemp), names_to = "Status", values_to = "Workers")
  })

  output$status_chart <- renderPlotly({
    df <- filtered_8()
    plot_ly(df, x = ~factor(Year), y = ~Workers, color = ~Status,
            type = "bar",
            text = ~paste0(Age, " - ", Status, ": ", scales::comma(Workers), "k"),
            hoverinfo = "text") %>%
      layout(barmode = "group",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Workers (thousands)"),
             title = "Full-Time vs Part-Time vs Unemployed (Ages 25+)")
  })
}

shinyApp(ui, server)
