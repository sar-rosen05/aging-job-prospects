# Occupational Employment Dashboard
library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyverse)
library(scales)
library(ggthemes)
library(readxl)
library(writexl)
library(janitor)


# LOAD CPS DATA + TABLE 8
data2011 <- read_excel("11b_2011.xlsx")
data2012 <- read_excel("11b_2012.xlsx")
data2013 <- read_excel("11b_2013.xlsx")
data2014 <- read_excel("11b_2014.xlsx")
data2015 <- read_excel("11b_2015.xlsx")
data2016 <- read_excel("11b_2016.xlsx")
data2017 <- read_excel("11b_2017.xlsx")
data2018 <- read_excel("11b_2018.xlsx")
data2019 <- read_excel("11b_2019.xlsx")
data2020 <- read_excel("11b_2020.xlsx")
data2021 <- read_excel("11b_2021.xlsx")
data2022 <- read_excel("11b_2022.xlsx")
data2023 <- read_excel("11b_2023.xlsx")
data2024 <- read_excel("11b_2024.xlsx")
table8_data <- read_excel("Table_8_Data.xlsx")


# CLEAN CPS DATA FUNCTION
clean_cps_year <- function(df, year) {
  
  df %>%
    slice(-c(1:3)) %>%   
    rename(
      occupation = 1,
      median_age = 2,
      `16_to_19_years` = 3,
      `20_to_24_years` = 4,
      `25_to_34_years` = 5,
      `35_to_44_years` = 6,
      `45_to_54_years` = 7,
      `55_to_64_years` = 8,
      `65_years_and_over` = 9
    ) %>%
    filter(occupation != "Total employed") %>%
    select(-median_age) %>%
    pivot_longer(
      cols = -occupation,
      names_to = "age_group",
      values_to = "employment_thousands"
    ) %>%
    mutate(
      year = year,
      employment_thousands = readr::parse_number(employment_thousands)
    )
}


# CLEAN EACH YEAR AND BIND
data2011_clean <- clean_cps_year(data2011, 2011)
data2012_clean <- clean_cps_year(data2012, 2012)
data2013_clean <- clean_cps_year(data2013, 2013)
data2014_clean <- clean_cps_year(data2014, 2014)
data2015_clean <- clean_cps_year(data2015, 2015)
data2016_clean <- clean_cps_year(data2016, 2016)
data2017_clean <- clean_cps_year(data2017, 2017)
data2018_clean <- clean_cps_year(data2018, 2018)
data2019_clean <- clean_cps_year(data2019, 2019)
data2020_clean <- clean_cps_year(data2020, 2020)
data2021_clean <- clean_cps_year(data2021, 2021)
data2022_clean <- clean_cps_year(data2022, 2022)
data2023_clean <- clean_cps_year(data2023, 2023)
data2024_clean <- clean_cps_year(data2024, 2024)

clean11b_data <- bind_rows(
  data2011_clean, data2012_clean, data2013_clean, data2014_clean,
  data2015_clean, data2016_clean, data2017_clean, data2018_clean,
  data2019_clean, data2020_clean, data2021_clean, data2022_clean,
  data2023_clean, data2024_clean
)


# CLEAN TABLE 8 DATA FOR DASHBOARD
clean_table8 <- function(df) {
  
  df %>%
    pivot_longer(
      cols = c(FT_Total, PT_Total, Unemp_FT, Unemp_PT),
      names_to = c("measure","time"),
      names_sep = "_",
      values_to = "count"
    ) %>%
    
    mutate(
      FT = if_else(measure == "FT" & time == "Total", count, NA_real_),
      PT = if_else(measure == "PT" & time == "Total", count, NA_real_),
      Unemp = if_else(measure == "Unemp", count, NA_real_)
    ) %>%
    
    group_by(Year, Sex, Race, Age) %>%
    summarise(
      FT = sum(FT, na.rm = TRUE),
      PT = sum(PT, na.rm = TRUE),
      Unemp = sum(Unemp, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    
    mutate(
      Total = FT + PT,
      Year = as.integer(Year),
      Age = factor(
        Age,
        levels = c("16 to 19","20 to 24","25 to 54","55+"),
        ordered = TRUE
      )
    ) %>%
    
    filter(!is.na(Age))
}

clean8_data <- clean_table8(table8_data)


# CREATE SUMMARY DATA
t8_unemp_age <- clean8_data %>%
  mutate(
    Labor_Force = Total + Unemp,
    Unemployment_Rate = Unemp / Labor_Force
  ) %>%
  group_by(Year, Age) %>%
  summarise(
    Unemployment_Rate = mean(Unemployment_Rate, na.rm = TRUE),
    .groups = "drop"
  )

retirement_data <- clean8_data %>%
  filter(Age == "55+") %>%
  mutate(
    Labor_Force = Total + Unemp,
    Employment_Rate = Total / Labor_Force
  ) %>%
  group_by(Year) %>%
  summarise(
    Employment_Rate = mean(Employment_Rate, na.rm = TRUE),
    .groups = "drop"
  )


# UI
ui <- navbarPage(
  
  title = "U.S. Occupational Employment Dashboard (2011–2024)",
  theme = shinytheme("flatly"),
  
  # Overview
  tabPanel("# Overview",
           h2("Put your plots here"),
           p("Add a summary of your code here and explain what the visualization is showing."),
           plotlyOutput("overviewPlot")
  ),
  
  # Age-Based Trends
  tabPanel(" Age-Based Trends",
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput(
                 "age_select",
                 "Select Age Groups:",
                 choices = unique(t8_unemp_age$Age),
                 selected = unique(t8_unemp_age$Age)
               ),
               sliderInput(
                 "year_range",
                 "Select Year Range:",
                 min = min(t8_unemp_age$Year),
                 max = max(t8_unemp_age$Year),
                 value = c(min(t8_unemp_age$Year), max(t8_unemp_age$Year)),
                 step = 1,
                 sep = ""
               ),
               selectInput(
                 "highlight_age",
                 "Highlight Age Group:",
                 choices = c("None", unique(t8_unemp_age$Age)),
                 selected = "None"
               ),
               checkboxInput(
                 "auto_highlight",
                 "Auto-highlight Age Group with Highest Unemployment",
                 value = FALSE
               )
             ),
             mainPanel(
               h3("The Age Gap in Employment: How Young Workers Bore the Brunt"),
               p("This line chart tracks unemployment across four age groups (16–19, 20–24, 25–54, 55+) 
                 from 2011–2024. Teens (16–19) faced the highest rates, peaking near 30% in 2011 and remaining 
                 above all other groups. Young adults (20–24) had the second-highest rates, generally declining 
                 but spiking in 2020, likely due to COVID-19. Prime-age (25–54) and older workers (55+) had lower, 
                 steadier unemployment between 4–10%, with only a notable 2020 increase. Overall, younger workers 
                 are more vulnerable to labor market fluctuations, while older groups show greater stability."),
               plotlyOutput("agePlot")
             )
           )
  ),
  
  # Industry Trends
  tabPanel(
    " Industry Trends",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "industry_year",
          label = "Select Year:",
          choices = sort(unique(clean11b_data$year)),
          selected = max(clean11b_data$year)
        ),
        selectInput(
          inputId = "top_n",
          label = "Number of Top Occupations:",
          choices = c(3, 5, 10, 15),
          selected = 5
        )
      ),
      mainPanel(
        h3("Occupational Employment Distribution"),
        p("This pie chart shows how employment is distributed across the top occupations in a selected year."),
        plotOutput("industryPlot", height = "450px")
      )
    )
  ),
  
  # Structural Shifts
  tabPanel("# Structural Shifts",
           h3("Put your plots here"),
           p("Add a summary of your code here."),
           plotOutput("shiftPlot")
  ),
  
  # Automation Impact
  tabPanel(" Automation Impact",
           h3("Put your plots here"),
           p("Add a summary of your code here."),
           plotOutput("automationPlot")
  ),
  
  # Unemployment Patterns
  tabPanel(" Unemployment Patterns",
           h3("Put your plots here"),
           p("Add a summary of your code here."),
           plotOutput("unempPlot")
  ),
  
  # Retirement Trends
  tabPanel(" Retirement Trends",
           h3("Put your plots here"),
           p("Add a summary of your code here."),
           plotOutput("retirementPlot")
  )
)


# SERVER
server <- function(input, output) {
  
  # Overview plot
  output$overviewPlot <- renderPlotly({
    p <- ggplot(t8_unemp_age,
                aes(x = Year,
                    y = Unemployment_Rate,
                    color = Age,
                    group = Age)) +
      geom_line(linewidth = 1.3) +
      scale_y_continuous(labels = percent_format()) +
      labs(
        title = "Overall Unemployment Trends by Age Group",
        x = "Year",
        y = "Unemployment Rate",
        color = "Age Group"
      ) +
      theme_fivethirtyeight(base_size = 14)
    
    ggplotly(p)
  })
  
  # Age-Based Trends plot
  output$agePlot <- renderPlotly({
    
    filtered_data <- t8_unemp_age %>%
      filter(Age %in% input$age_select,
             Year >= input$year_range[1],
             Year <= input$year_range[2])
    
    # Determine auto-highlight
    if(input$auto_highlight){
      max_age <- filtered_data %>%
        group_by(Age) %>%
        summarise(mean_unemp = mean(Unemployment_Rate, na.rm = TRUE)) %>%
        arrange(desc(mean_unemp)) %>%
        slice(1) %>%
        pull(Age)
      highlight <- max_age
    } else {
      highlight <- input$highlight_age
    }
    
    filtered_data <- filtered_data %>%
      mutate(
        highlight_flag = ifelse(Age == highlight, "yes", "no"),
        line_size = ifelse(Age == highlight, 2, 1.1),
        alpha_val = ifelse(highlight == "None", 0.8,
                           ifelse(Age == highlight, 1, 0.3))
      )
    
    p <- ggplot(
      filtered_data,
      aes(
        x = Year,
        y = Unemployment_Rate,
        color = Age,
        group = Age,
        text = paste(
          "Year:", Year,
          "<br>Age:", Age,
          "<br>Unemployment Rate:", percent(Unemployment_Rate, accuracy = 1)
        ),
        alpha = alpha_val,
        linewidth = line_size
      )
    ) +
      geom_line() +
      geom_point(size = 2) +
      geom_vline(xintercept = 2020, linetype = "dashed", color = "gray40") +
      annotate("text", x = 2020, y = max(filtered_data$Unemployment_Rate),
               label = "COVID-19 Shock", vjust = -0.5, size = 4) +
      scale_y_continuous(labels = percent_format()) +
      scale_alpha_identity() +
      scale_linewidth_identity() +
      labs(
        title = paste("Unemployment Rate by Age Group (", input$year_range[1], "-", input$year_range[2], ")", sep = ""),
        subtitle = ifelse(highlight == "None", "Comparing unemployment trends across age groups",
                          paste("Highlighting:", highlight)),
        x = NULL,
        y = NULL,
        color = "Age Group"
      ) +
      theme_fivethirtyeight() +
      theme(text = element_text(family = "Times New Roman"))
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE),
        hoverlabel = list(
          font = list(family = "Times New Roman", size = 12, color = "black"),
          bgcolor = "white",
          bordercolor = "transparent"
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Industry Trends
  output$industryPlot <- renderPlot({
    clean11b_data %>%
      dplyr::filter(year == input$industry_year) %>%
      dplyr::group_by(occupation) %>%
      dplyr::summarise(
        total_employment = sum(employment_thousands, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(total_employment)) %>%
      dplyr::slice_head(n = as.numeric(input$top_n)) %>%
      ggplot(aes(x = 1, y = total_employment, fill = occupation)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      labs(
        title = paste(
          "Top", input$top_n,
          "Occupations by Employment,", input$industry_year
        ),
        fill = "Occupation"
      ) +
      theme_void()
  })
  
  # Other placeholders
  output$shiftPlot <- renderPlot({ })
  output$automationPlot <- renderPlot({ })
  output$unempPlot <- renderPlot({ })
  output$retirementPlot <- renderPlot({ })
  
}

# RUN APP
shinyApp(ui, server)
