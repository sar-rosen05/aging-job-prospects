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
library(RColorBrewer)

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
      employment_thousands = employment_thousands %>%
        na_if("—") %>%
        na_if("-") %>%
        readr::parse_number()
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
#Retirement 
retirement_trend_data <- clean11b_data %>%
  group_by(year) %>%
  summarise(
    age55_64 = sum(employment_thousands[age_group == "55_to_64_years"], na.rm = TRUE),
    age65_plus = sum(employment_thousands[age_group == "65_years_and_over"], na.rm = TRUE),
    total_emp = sum(employment_thousands, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_55_64 = age55_64 / total_emp,
    pct_65_plus = age65_plus / total_emp
  ) %>%
  select(year, pct_55_64, pct_65_plus) %>%
  pivot_longer(
    cols = c(pct_55_64, pct_65_plus),
    names_to = "group",
    values_to = "share"
  ) %>%
  mutate(
    group = recode(
      group,
      "pct_55_64" = "Age 55–64",
      "pct_65_plus" = "Age 65+"
    )
  )
 
#Retirement Projections Setup
retire_metrics <- clean11b_data %>%
  group_by(occupation, year) %>%
  summarise(
    c55 = sum(employment_thousands[age_group == "55_to_64_years"], na.rm = TRUE),
    c65 = sum(employment_thousands[age_group == "65_years_and_over"], na.rm = TRUE),
    total_emp = sum(employment_thousands, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct55 = ifelse(total_emp > 0, c55 / total_emp, NA_real_),
    pct65 = ifelse(total_emp > 0, c65 / total_emp, NA_real_)
  )

occ_choices_ret <- sort(unique(retire_metrics$occupation))
year_min <- min(retire_metrics$year, na.rm = TRUE)
year_max <- max(retire_metrics$year, na.rm = TRUE)

# UI
ui <- navbarPage(
  title = "U.S. Occupational Employment Dashboard (2011–2024)",
  theme = shinytheme("flatly"),
  
  tabPanel("Overview",
           
           h2("How do factors like age, race, gender, and time affect employment?"),
           
           p("We were challenged to create a dashboard analyzing and visualizing trends
    within the job market between 2011–2024. Our goal was for each graph to analyze
    a different factor so that together they tell a story about how employment
    patterns have changed or remained consistent over time."),
           p("Link to our Github repository and full challenge :https://github.com/UWB-Adv-Data-Vis-2026-Wi-A/aging-job-prospects-group-3/tree/main"),
           p("Below is the objective and user guide for each of the seven graphs included in the dashboard:"),
           
           tags$ul(
             tags$li(strong("1. Age-Based Unemployment")),
             strong("Objective and User Guide:"),
             p("This visualization compares unemployment rates across four age groups (16–19, 20–24, 25–54, 
             and 55+) in the United States from 2011 to 2024. The goal is to highlight how unemployment varies across stages 
               of the workforce and how different age groups respond to economic changes. Age groups can be selected or 
               removed to compare specific populations. The year range slider allows users to focus on particular time 
               periods within the dataset. The highlight feature emphasizes one age group while dimming the others to 
               make comparisons easier. Users can also toggle data points and a smoothed trend line to reveal additional 
               details about the data. Lastly, users can hover over the lines to see the exact unemployment rate for each 
               year and age group.
               "),
             strong("Methods:"),
             p("The unemployment rate is calculated as the number of unemployed individuals divided by the total 
               labor force (employed plus unemployed) for each age group and year. An optional LOESS smoothing trend line 
               can be displayed to estimate the broader trajectory of unemployment over time by fitting localized regressions 
               across the time series.
               "),
             
             strong("Limitations:"),
             p("Several limitations should be considered when interpreting the results. The LOESS trend 
               line smooths short-term fluctuations, which may reduce the visibility of sudden economic shocks such 
               as the 2020 COVID-19 unemployment spike. Additionally, the analysis aggregates workers into broad age 
               categories and does not account for differences in education, occupation, region, or industry that may 
               also influence unemployment outcomes."),
             
             tags$li(
               strong("2. Industry and Occupation Trends"),
               strong("Objective and User Guide:"),
               p(" The objective for this graph was not only to see which occupation sector was most
         common for each age group but also how it varied as people grew up and how it 
         changed as the years went on. The graph allows for user interactivity such as changing the color pallet
         depending on the persons prefrence. Also for more data centered changes users can change factors such as the year,
         selected generation, the amount of top occupations shown (3, 5,10, 15), and a tool tip 
         in order to see the percentage and count for each occupation sector. For best understanding
         and data visualization it's best to start with one age group, and 5 sectors and then analyze, after,
         either change the year or age group in order to compare and see how the counts increased or occupations
         changed completly. ")
             ),
             
             strong("Methods:"),
             p("In order to find the employment total for the selected age range and year, employment counts 
             were summed up and grouped by age to calculate the total for that category. The code then ranks occupation
             by the employment size and displays the top # of occupations picked by the user."),
             
             strong("Limitations:"),
             p("This graph does contain several limitations, the first being retirement. 
               I note that employment decreases as age increases; however, since this graph only
               looks at occupation, users aren't able to see how many of those people retired. The chart
               also focuses on the top occupations, meaning that some patterns may be missing in employment trends/distribution.
               Lastly, the graph focuses on broad occupation not the wages, hours, or companies worked at meaning conditions
               and levels of the job are vague within the specific analysis
."),
             
             tags$li(strong("3. Entry Level Occupation Trends")),
             strong("Objective and User Guide"),
             p("This visualization explores the most common occupations for workers ages 20–24 and compares how these entry-level jobs 
             have changed across different years. The goal is to highlight the most common occupations for workers ages 20-24 or those 
             who have just graduated from college or are about to enter the workforce and comparing via two different graphs from two different years. 
             Users are able to interact with the graphs via selecting Year One and Year Two (2011 to 2024) to compare two years, 
             they’re also able to Select or Type an Occupation to highlight to see if the occupation made it to the list, and last feature is that 
             users are able to select how much to display ranging from (3, 5, 10, 20, 30). Users can also switch from two different tabs, a Graph and a Table, 
             hover over the bars to show what the occupation, total employment, and the year. They can also click and drag creating a square to zoom into the visualization."),
             
             strong("Methods:"), 
             p("To identify the most common occupations for workers ages 20–24, the cleaned dataset was first filtered to include only 
             individuals within the 20–24 age group and a user-selected year. The filtered data was then processed using the slice_max() function 
             to select the top occupations based on total employment (in thousands). The number of occupations displayed is determined by the user 
             through an input control that allows them to select how many occupations to display on the graph. The data is then visualized using interactive bar charts 
             created with ggplot2 and plotly, allowing users to compare the top occupations between two selected years. Additionally, users can highlight a specific occupation, 
             which is displayed in red while the remaining occupations appear in gray."), 
             
             strong("Limitations:"),
             p("One limitation of this visualization is that it only focuses on workers ages 20–24, which may not represent all individuals entering the workforce after college. 
               Additionally, the dashboard displays only the top occupations based on employment size, meaning smaller occupations may not appear depending on the number selected by the user. 
               The dashboard also compares only two years at a time, which helps with direct comparison but may limit the ability to see long-term trends. 
               Finally, the visualization shows total employment counts rather than percentages or growth rates, which does not account for changes in the overall workforce size over time."), 
             
             tags$li(strong("4. Workforce Breakdown")),
             strong("Objective and User Guide:"),
             p("This visualization explores how the distribution of workers aged 25 and older shifts across occupations over time from 2011 to 2024. The goal is to highlight how different age groups dominate or fade within specific occupations as the workforce ages. Users can select an occupation from the dropdown menu, adjust the year range slider, filter by sex and race, and toggle between grouped bar and line chart views. The dashboard displays three interactive charts: raw worker counts by age group, each age group's share of the occupation, and a breakdown of full-time, part-time, and unemployed workers for ages 25–54 and 55+. Hovering over any bar or point reveals exact values, and users can click legend items to show or hide specific age groups."),
             strong("Methods:"),
             p("The dataset was filtered based on user-selected inputs including occupation, year range, sex, and race to produce dynamic visualizations. Employment counts were pulled from cleaned CPS data and grouped by age category and year to show workforce composition over time. For the share chart, each age group's employment was divided by the total employment for that year and occupation to calculate a percentage. The full-time, part-time, and unemployed breakdown was summarized by aggregating worker counts across the 25–54 and 55+ age groups for each status category. All three charts were built using Plotly in R, enabling interactive zooming, hovering, and filtering directly in the dashboard."),
             strong("Limitations:"),
              p("This visualization relies on broad age categories, which may obscure finer differences in employment patterns within each group. The dataset aggregates workers across all industries within a selected occupation, meaning sector-specific trends may not be visible. Filtering by race and sex applies only to the third chart, not the first two, which may cause inconsistency when comparing across tabs. Additionally, employment figures are reported in thousands and rounded, so small fluctuations may not be fully captured. Finally, the data only extends to 2024, so emerging workforce trends beyond that point are not reflected in the visualization."),
             tags$li(strong("5. Unemployment by Race")),
             
             tags$li(strong("6. Retirement Trends")),
             strong("Objective and User Guide:"),
             p("This visualization explores retirement-related workforce trends by showing how the share of total employment made up by older workers has changed from 2011 to 2024. The chart focuses on workers ages 55–64 and 65+ to better show patterns connected to nearing retirement and working past traditional retirement age. Users can interact with the graph by adjusting the year range and selecting which age group they want to display. This makes it easier to compare whether workers closer to retirement age or workers past age 65 are making up a larger share of the labor force over time. For the best understanding, users can begin with both groups selected to compare the two trends together, then isolate one group at a time to focus on changes across specific years."),
             strong("Methods:"),
             p("To create this visualization, the cleaned occupational employment dataset was grouped by year and age group. Employment counts for workers ages 55–64 and 65+ were summed for each year, then divided by the total employment count for that same year to calculate each group’s share of total employment. The data was then reshaped into a long format so both age groups could be displayed as separate lines on the same interactive chart. Plotly was used to allow hovering and easier comparison of exact values across years."),
             strong("Limitations:"),
             p("One limitation of this visualization is that it does not directly measure retirement itself, since remaining in or leaving the labor force can happen for many reasons besides retirement. The graph shows the share of employment made up by older workers, but it cannot explain why those shares increase or decrease. It also does not capture differences in retirement patterns by region, income, education, health, or occupation type. In addition, because the data is aggregated at a broad level, it may hide more detailed patterns within smaller industries or demographic groups."),
             
             tags$li(strong("7. Retirement Projections")),
             strong("Objective and User Guide:"),
             p("This visualization explores retirement trends across U.S. occupations by examining the share and number of workers nearing retirement age. Users can select a specific occupation and metric to view how the percentage or count of workers aged 55+ or 65+ has changed over time. The year range slider allows users to focus on specific periods within the dataset. An optional projection feature extends the trend line several years into the future to provide a simple estimate of how retirement pressure may evolve if current patterns continue. Hovering over the line reveals the exact values for each year."),
            
             strong("Methods:"),
             p("The visualization uses occupational workforce data from 2011–2024 to calculate the percentage and count of workers aged 55+ and 65+. Users can filter by occupation and adjust the year range to focus on specific periods. When the projection option is enabled, a simple linear regression model is fit to the historical data to estimate future values for a selected number of years. These projected values are displayed as a dashed extension of the historical trend line, allowing users to visually compare past patterns with potential future trajectories."),
             
             strong("Limitations:"),
             p("The projections shown in this visualization are based on a simple linear model and should be interpreted as illustrative rather than predictive. The model assumes that past trends will continue at a similar rate and does not account for external factors such as economic shifts, policy changes, automation, workforce migration, or changes in labor participation. Additionally, the data aggregates workers within occupations and does not capture variation across regions, industries, or education levels that may influence retirement behavior.")
           ),
           
         
           
           plotlyOutput("overviewPlot")
  ),
      
      
      
   
  tabPanel(" Age-Based Unemployment Trends",
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
                 choices = c("None", levels(t8_unemp_age$Age)),
                 selected = "None"
               ),
               
               checkboxInput(
                 "show_points",
                 "Show Data Points",
                 value = TRUE
               ),
               
               checkboxInput(
                 "show_trend",
                 "Show Trend Line",
                 value = FALSE
               ),
               
               selectInput(
                 "color_palette",
                 "Color Theme",
                 choices = c(
                   "Soft Pastel" = "Set2",
                   "Bold Contrast" = "Set1",
                   "Professional Muted" = "Dark2",
                   "Extended Palette" = "Paired"
                 ),
                 selected = "Set2"
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
               plotlyOutput("agePlot"),
               uiOutput("trendNote")
             )
           )
  ),
#Occupation and Industry Trends set up (Obydah)
  tabPanel(
    "Industry Trends",
    sidebarLayout(
      sidebarPanel(
        
        selectInput(
          inputId = "industry_year",
          label = "Select Year:",
          choices = sort(unique(clean11b_data$year)),
          selected = max(clean11b_data$year)
        ),
        
        selectInput(
          inputId = "industry_age",
          label = "Select Generation(s):",
          choices = c(
            "Teenagers (16–19)" = "16_to_19_years",
            "Young Adults (20–24)" = "20_to_24_years",
            "Adults (25–34)" = "25_to_34_years",
            "Early Middle Age (35–44)" = "35_to_44_years",
            "Middle Age (45–54)" = "45_to_54_years",
            "Older (55–64)" = "55_to_64_years",
            "Retirees (65+)" = "65_years_and_over"
          ),
          selected = c("16_to_19_years", "25_to_34_years"),
          multiple = TRUE
        ),
        
        selectInput(
          inputId = "top_n",
          label = "Number of Top Occupations:",
          choices = c(3, 5, 10, 15),
          selected = 5
        ),
        
        selectInput(
          "color_palette",
          "Color Theme",
          choices = c(
            "Soft Pastel" = "Set2",
            "Bold Contrast" = "Set1",
            "Professional Muted" = "Dark2",
            "Extended Palette" = "Paired"
          ),
          selected = "Set2"
        )
        
      ),
      
      mainPanel(
        h3("Occupational Employment Distribution"),
        p("This pie chart shows how employment is distributed across the top occupations depending on selected age groups and year.
          One consist pattern over the years is that teenagers (16-19) a less percantage
          and aren't as commonly in higher level jobs. Most are in service or low level occupations, such as cashiers.
          This is pattern in consisten with older ages such as Adults to Early Middle age having a higher
          percentage in professional occupations such as management and sales. Number of employment also has 
          a gradual decrease as age increases. Overall this graph shows that over time with growth and 
          experince many people move into higher level jobs and as they start hitting 55-65+ they begin to retire
          and this has been a trend for the past 13 years."),
        plotlyOutput("industryPlot", height = "450px")
      )
    )
  ),
  
  # Entry Age Occupation (Dareen)
  tabPanel("Entry Level Occupations",
           sidebarLayout(
             sidebarPanel(
               selectInput("Select_FirstYear", 
                           "Select First Year", 
                           choices = unique(clean11b_data$year),
                           selected = "2011"
               ),
               selectInput("Select_SecondYear", 
                           "Select Second Year", 
                           choices = unique(clean11b_data$year),
                           selected = "2024"
               ), 
               selectInput("Highlight_Occupation",
                           "Select/Type Occupation to Highlight", 
                           choices = c("None", unique(clean11b_data$occupation)), 
                           selected = "None"
               ),
               radioButtons("Select_TopOccupations", 
                            "Select Top N Occupations",
                            choices = c(3, 5, 10, 20, 30), 
                            selected = 3
               )
               
             ),
             
             mainPanel(
               h3("Entry Level Occupations Over Time"),
               p("This tab explores the most common occupations for workers ages 20–24 and compares how these entry-level jobs have changed across different years."),
               tabsetPanel(
                 tabPanel("Graph", plotlyOutput("Entry_Age_Occupation_Map1")),
                 tabPanel("Table", tableOutput("Top_Occupations_Table1"))
               ),
               tabsetPanel(
                 tabPanel("Graph", plotlyOutput("Entry_Age_Occupation_Map2")),
                 tabPanel("Table", tableOutput("Top_Occupations_Table2"))
               ),
               helpText("Tip: Drag to zoom, double-click to reset. Hover for exact values.")
             )
           )
  ),
  
tabPanel(" Workforce Breakdown",
         sidebarLayout(
           sidebarPanel(
             selectInput("samuel_occupation", "Select Occupation:",
                         choices = sort(unique(clean11b_data$occupation)),
                         selected = "Management, professional, and related occupations"),
             sliderInput("samuel_year_range", "Year Range:",
                         min = 2011, max = 2024, value = c(2011, 2024), step = 1, sep = ""),
             selectInput("samuel_chart_type", "Chart Type:",
                         choices = c("Grouped Bar" = "bar", "Line Trend" = "line")),             
           selectInput("samuel_sex", "Select Sex:",
                         choices = c("All", "Men", "Women"),
                         selected = "All"),
             selectInput("samuel_race", "Select Race:",
                         choices = c("All", "Asian", "Black or African American",
                                     "Hispanic or Latino", "White"),
                         selected = "All"),
           ),
           mainPanel(
             h3("Workforce Breakdown: Occupation Shifts Across Age Groups (25+)"),
             p("This tab explores how the distribution of workers aged 25 and older shifts across 
          occupations over time (2011–2024). The first chart shows raw worker counts by age group 
          for a selected occupation, allowing users to compare how employment levels change year 
          to year. The second chart shows each age group's share of that occupation, revealing 
          whether older or younger workers are becoming more or less dominant over time. The third 
          chart breaks down full-time, part-time, and unemployed workers for the 25–54 and 55+ age 
          groups, showing how work status has shifted across the years. Use the sidebar to select 
          an occupation, adjust the year range, and toggle between bar and line chart views."),
             plotlyOutput("samuel_occ_chart", height = "350px"),
             br(),
             plotlyOutput("samuel_share_chart", height = "300px"),
             br(),
             plotlyOutput("samuel_status_chart", height = "350px")
           )
         )
),
tabPanel(" Unemployment by Race",
         
         sidebarLayout(
           sidebarPanel(
             checkboxGroupInput(
               "race_select",
               "Select Race Groups:",
               choices = unique(clean8_data$Race),
               selected = unique(clean8_data$Race)
             ),
             
             sliderInput(
               "year_range_race",
               "Select Year Range:",
               min = min(clean8_data$Year),
               max = max(clean8_data$Year),
               value = c(min(clean8_data$Year), max(clean8_data$Year)),
               step = 1,
               sep = ""
             )
           ),
           
           mainPanel(
             h3("Unemployment Rate Trends by Race"),
             p("This visualization compares unemployment rates across racial groups in the United States from 2011 to 2023. 
               The unemployment rate is calculated as unemployed individuals divided by the total labor force 
               (employed plus unemployed). Users can select specific racial groups and adjust the year range 
               to explore how unemployment patterns change across time."),
             
             plotlyOutput("racePlot")
           )
         )
),
  
  
  # Retirement Trends (Zuwidya)
  tabPanel(
    "Retirement Trends",
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "retire_year",
          "Select Year Range:",
          min = min(retirement_trend_data$year),
          max = max(retirement_trend_data$year),
          value = c(min(retirement_trend_data$year), max(retirement_trend_data$year)),
          step = 1,
          sep = ""
        ),
        checkboxGroupInput(
          "retire_groups",
          "Select Group:",
          choices = c("Age 55–64", "Age 65+"),
          selected = c("Age 55–64", "Age 65+")
        )
      ),
      mainPanel(
        h3("Retirement Trends Among Older Workers (2011–2024)"),
        p("This interactive chart tracks the share of total employment made up by older workers ages 55–64 and 65+ from 2011 to 2024. Unlike a general employment-rate chart, this focuses more directly on retirement-related patterns by showing how much of the workforce is made up of people nearing retirement or working past traditional retirement age. The trend helps show whether older workers are remaining in the labor force longer over time."),
        plotlyOutput("retirementPlot")
      )
    )
  ),
  
  #Retirement Projections
  tabPanel(
    "Retirement Projections",
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          "occ_ret",
          "Choose an occupation:",
          choices = occ_choices_ret,
          selected = occ_choices_ret[1],
          options = list(placeholder = "Type to search...", maxOptions = 5000)
        ),
        radioButtons(
          "metric_ret",
          "Metric:",
          choices = c(
            "Percent age 55+ (retirement pressure)" = "pct55",
            "Percent age 65+ (near/at retirement)"  = "pct65",
            "Count age 55+ (thousands)" = "c55",
            "Count age 65+ (thousands)" = "c65"
          ),
          selected = "pct55"
        ),
        sliderInput(
          "year_ret",
          "Year range:",
          min = year_min, max = year_max,
          value = c(year_min, year_max),
          step = 1, sep = ""
        ),
        checkboxInput("show_proj", "Add simple projection (linear trend)", value = TRUE),
        sliderInput("proj_years", "Projection years", min = 1, max = 10, value = 5)
      ),
      mainPanel(
        h3("Retirement Projections"),
        p("This chart tracks how near-retirement employment changes over time for a selected occupation. Users can switch between counts and percentages for ages 55+ and 65+ to compare retirement pressure across jobs. The dashed extension is a simple linear projection to help visualize where recent trends may be heading."),
        plotlyOutput("retire_line", height = "450px"),
        tags$hr(),
        textOutput("retire_interpretation"),
        helpText("Tip: Drag to zoom, double-click to reset. Hover for exact values.")
      )
    )
  )
)


# SERVER
server <- function(input, output) {
  
  output$agePlot <- renderPlotly({
    
    filtered_data <- t8_unemp_age %>%
      filter(Age %in% input$age_select,
             Year >= input$year_range[1],
             Year <= input$year_range[2])
    
    p <- ggplot(filtered_data,
                aes(x = Year,
                    y = Unemployment_Rate,
                    color = Age,
                    group = Age,
                    linetype = ifelse(Age == input$highlight_age | input$highlight_age == "None",
                                      "solid", "dashed"),
                    text = paste("Year:", Year,
                                 "<br>Age:", Age,
                                 "<br>Unemployment Rate:", percent(Unemployment_Rate, accuracy = 1))
                )) +
      # main lines
      geom_line(size = 1.2, alpha = ifelse(input$show_trend, 0.35, 1)) +
      # 0% baseline
      geom_hline(yintercept = 0, color = "gray70", linetype = "solid", linewidth = 0.1) +
      # color & linetype scales
      scale_color_brewer(palette = input$color_palette) +
      scale_linetype_identity() +
      scale_y_continuous(labels = percent_format()) +
      labs(
        title = paste("Unemployment Rate by Age Group (", input$year_range[1], "-", input$year_range[2], ")", sep = ""),
        subtitle = ifelse(input$highlight_age == "None",
                          "Comparing unemployment trends across age groups",
                          paste("Highlighting:", input$highlight_age)),
        x = NULL,
        y = NULL,
        color = "Age Group"
      ) +
      theme_fivethirtyeight() +
      theme(
        text = element_text(family = "Times New Roman")
      )
    
    # optional points
    if (input$show_points) {
      p <- p + geom_point(size = 2, alpha = ifelse(input$show_trend, 0.4, 1))
    }
    
    # optional LOESS trend
    if (input$show_trend) {
      p <- p + geom_smooth(
        method = "loess",
        se = FALSE,
        linetype = "dashed",
        linewidth = 1.1,
        alpha = 1
      )
    }
    
    # convert to plotly and force white background
    ggplotly(p, tooltip = "text") %>%
      layout(
        hovermode = "x unified",
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # trend note
  output$trendNote <- renderUI({
    if (input$show_trend) {
      div(
        style="margin-top:10px; font-size:13px; color:gray40;",
        HTML("<b>Note:</b> The dashed trend lines use LOESS smoothing to estimate the underlying unemployment trajectory. 
             Because the method smooths short-term shocks, the 2020 COVID-19 spike has less influence on the trend,
             approximating the broader direction the labor market may have followed without the pandemic disruption.")
      )
    }
  })
  
  
  # Zuwiyda plot
  
  output$retirementPlot <- renderPlotly({
    
    filtered_data <- retirement_trend_data %>%
      filter(
        year >= input$retire_year[1],
        year <= input$retire_year[2],
        group %in% input$retire_groups
      ) %>%
      mutate(
        tooltip = paste0(
          "<b>Year:</b> ", year,
          "<br><b>Group:</b> ", group,
          "<br><b>Share of Employment:</b> ", percent(share, accuracy = 0.1)
        )
      )
    
    # Brewer palette
    colors <- brewer.pal(
      min(length(unique(filtered_data$group)), 12),
      input$color_palette
    )
    
    p <- ggplot(
      filtered_data,
      aes(
        x = year,
        y = share,
        color = group,
        group = group,
        text = tooltip
      )
    ) +
      geom_line(linewidth = 1.4) +
      geom_point(size = 3) +
      scale_y_continuous(labels = percent_format()) +
      scale_color_manual(values = colors) +
      labs(
        title = "Share of Employment Among Older Workers",
        color = "Group"
      ) +
      theme_minimal(base_size = 14, base_family = "Times New Roman") +
      theme(
        plot.title = element_text(face = "bold", family = "Times New Roman"),
        legend.title = element_text(face = "bold", family = "Times New Roman"),
        axis.title = element_text(face = "bold", family = "Times New Roman"),
        axis.text = element_text(family = "Times New Roman")
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hovermode = "x unified",
        margin = list(t = 80),
        font = list(family = "Times New Roman")
      ) %>%
      config(displayModeBar = TRUE)
  })
  
#Obydahs Plot (Occuptation/Industry trends)
  output$industryPlot <- renderPlotly({
    
    age_map <- c(
      "16_to_19_years" = "Teenagers",
      "20_to_24_years" = "Young Adults",
      "25_to_34_years" = "Adults",
      "35_to_44_years" = "Early Middle Age",
      "45_to_54_years" = "Middle Age",
      "55_to_64_years" = "Older",
      "65_years_and_over" = "Retirees"
    )
    
    selected_generations <- age_map[input$industry_age]
    
  
    filtered_data <- clean11b_data %>%
      filter(
        year == input$industry_year,
        age_group %in% input$industry_age
      ) %>%
      group_by(occupation) %>%
      summarise(
        total_employment = sum(employment_thousands, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total_employment)) %>%
      slice_head(n = as.numeric(input$top_n))
    
    if (nrow(filtered_data) == 0) {
      return(plot_ly() %>% layout(title = "No data available for selection"))
    }
    
    
    filtered_data$tooltip <- paste0(
      "<b>Occupation:</b> ", filtered_data$occupation,
      "<br><b>Employment:</b> ", scales::comma(filtered_data$total_employment), " (thousands)",
      "<br><b>Generation(s):</b> ", paste(selected_generations, collapse = ", ")
    )
    
    
    colors <- brewer.pal(
      min(length(unique(filtered_data$occupation)), 12),
      input$color_palette
    )
    
    plot_ly(
      data = filtered_data,
      labels = ~occupation,
      values = ~total_employment,
      type = "pie",
      textinfo = "percent",
      textposition = "inside",
      hoverinfo = "text",
      hovertext = ~tooltip,
      marker = list(colors = colors),
      
      # Makes the pie chart larger
      domain = list(x = c(0,1), y = c(0.15,1))
      
    ) %>%
      layout(
        title = paste(
          "Top", input$top_n,
          "Occupations by Employment (", input$industry_year, ")"
        ), 
          font = list(family = "Times New Roman"),
       
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.1
        ),
        
        margin = list(t = 60, b = 80)
      )
  })

  
  
  ############################################################################### 
  # Dareen's Plot (Entry Level Occupation Graphs & Tables)
  
  # Function to prepare data 
  get_top_occupation <- function(selected_year){ 
    
    data <- clean11b_data %>% 
      filter(age_group == "20_to_24_years", 
             year == selected_year) %>%
      slice_max(employment_thousands,
                n = as.numeric(input$Select_TopOccupations))
    
    data %>%
      mutate(highlight = ifelse(occupation == input$Highlight_Occupation, 
                                "Selected", 
                                "Other"))
  }
  
  # Function to Create Plots 
  make_plot <- function(data, selected_year){
    p <- data %>%
      ggplot(aes(
        x = reorder(occupation, employment_thousands),
        y = employment_thousands,
        fill = if (input$Highlight_Occupation == "None") {
          occupation
        } else {
          highlight
        },
        text = paste(
          "Occupation:", occupation, 
          "<br>Total Employment:", employment_thousands, "(thousands)",
          "<br>Year:", year
        )
      )) +
      geom_col(width = 0.7) +
      labs(
        title = paste0("Top ", input$Select_TopOccupations, " Entry Level Occupations (", selected_year, ")"),
        x = NULL,
        y = "Total Employment"
      ) +
      coord_flip() +
      theme_minimal(base_family = "Times New Roman") +
      theme(
        legend.position = "none",
        plot.title = element_text(
          family = "Times New Roman",
          face = "bold",
          size = 16
        ),
        axis.title = element_text(
          family = "Times New Roman",
          face = "bold"
        ),
        axis.text = element_text(
          family = "Times New Roman"
        )
      )
    
    # If User Selected an Occupation Highlight it with red and others as gray 
    if(input$Highlight_Occupation != "None"){
      p <- p + scale_fill_manual(values = c(
        "Selected" = "red", 
        "Other" = "gray"))
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(font = list(family = "Times New Roman"))
  }
  
  # Graph 1
  output$Entry_Age_Occupation_Map1 <- renderPlotly({ 
    data <- get_top_occupation(input$Select_FirstYear) 
    make_plot(data, input$Select_FirstYear)
  })
  
  # Graph 2
  output$Entry_Age_Occupation_Map2 <- renderPlotly({
    data <- get_top_occupation(input$Select_SecondYear)
    make_plot(data, input$Select_SecondYear)
  })
  
  # Table 1
  output$Top_Occupations_Table1 <- renderTable({
    clean11b_data %>%
      filter(age_group == "20_to_24_years",
             year == input$Select_FirstYear) %>%
      slice_max(employment_thousands, n = as.numeric(input$Select_TopOccupations))
  })
  
  # Table 2
  output$Top_Occupations_Table2 <- renderTable({
    clean11b_data %>%
      filter(age_group == "20_to_24_years", 
             year == input$Select_SecondYear) %>%
      slice_max(employment_thousands, n = as.numeric(input$Select_TopOccupations))
  })
  
  ############################################################################
  
  output$shiftPlot <- renderPlot({ })
  output$automationPlot <- renderPlot({ })
  output$unempPlot <- renderPlot({ })
  


#Sarah's Code -- Retirement Projections
  metric_data <- reactive({
    req(input$occ_ret)
    
    retire_metrics %>%
      filter(
        occupation == input$occ_ret,
        year >= input$year_ret[1],
        year <= input$year_ret[2]
      ) %>%
      arrange(year) %>%
      mutate(
        value = dplyr::case_when(
          input$metric_ret == "pct55" ~ pct55,
          input$metric_ret == "pct65" ~ pct65,
          input$metric_ret == "c55"   ~ c55,
          input$metric_ret == "c65"   ~ c65,
          TRUE ~ pct55
        ),
        metric_label = dplyr::case_when(
          input$metric_ret == "pct55" ~ "% age 55+",
          input$metric_ret == "pct65" ~ "% age 65+",
          input$metric_ret == "c55"   ~ "Workers age 55+ (thousands)",
          input$metric_ret == "c65"   ~ "Workers age 65+ (thousands)",
          TRUE ~ "% age 55+"
        )
      )
  })
  
  output$retire_line <- plotly::renderPlotly({
    d <- metric_data()
    req(nrow(d) > 0)
    
    colors <- brewer.pal(
      min(length(unique(d$occupation)), 12),
      input$color_palette
    )
    
    p <- ggplot(d, aes(x = year, y = value)) +
      geom_line(color = colors[1], size = 1.2) +
      geom_point(color = colors[1], size = 2) +
      labs(
        title = paste("Retirement Trend:", input$occ_ret),
        x = NULL,
        y = unique(d$metric_label)
      ) +
      theme_minimal(base_size = 13, base_family = "Times New Roman") +
      theme(
        plot.title = element_text(face = "bold")
      )
    
    
    if (isTRUE(input$show_proj) && nrow(d) >= 3 && all(is.finite(d$value))) {
      fit <- lm(value ~ year, data = d)
      last_year <- max(d$year, na.rm = TRUE)
      future_years <- (last_year + 1):(last_year + input$proj_years)
      
      proj <- tibble(
        year = future_years,
        value = predict(fit, newdata = tibble(year = future_years))
      )
      
      p <- p +
        geom_line(data = proj, aes(x = year, y = value),
                  linetype = "dashed", color = colors[2]) +
        geom_point(data = proj, aes(x = year, y = value),
                   color = colors[2])
    }
    
    plotly::ggplotly(p, tooltip = c("x", "y")) %>%
      plotly::config(displayModeBar = TRUE) %>%
      plotly::layout(
        hovermode = "closest",
        legend = list(
          orientation = "v",
          bgcolor = "rgba(255,255,255,0.7)",
          x = 1.05,
          xanchor = "left",
          y = 1,
          yanchor = "top"
        ),
        margin = list(t = 80)
      )
  })
  
  # Rishita's Plot (Unemployement Rate by Race (2011-2023))
  
  race_data <- reactive({
    clean8_data %>%
      filter(
        Race %in% input$race_select,
        Year >= input$year_range_race[1],
        Year <= input$year_range_race[2]
      ) %>%
      mutate(
        Labor_Force = Total + Unemp,
        Unemployment_Rate = Unemp / Labor_Force
      ) %>%
      group_by(Year, Race) %>%
      summarise(
        Unemployment_Rate = mean(Unemployment_Rate, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  output$racePlot <- renderPlotly({
    
    p <- ggplot(
      race_data(),
      aes(
        x = Year,
        y = Unemployment_Rate,
        color = Race,
        group = Race,
        text = paste(
          "Year:", Year,
          "<br>Race:", Race,
          "<br>Unemployment Rate:",
          percent(Unemployment_Rate, accuracy = 0.1)
        )
      )
    ) +
      geom_line(size = 1.3) +
      geom_point(size = 2) +
      scale_y_continuous(labels = percent_format()) +
      labs(
        title = "Unemployment Rate Trends by Race",
        subtitle = "Comparing racial groups across time",
        x = "Year",
        y = "Unemployment Rate",
        color = "Race"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        text = element_text(family = "Times New Roman")
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest") %>%
      config(displayModeBar = FALSE)
  })
  
# Samuel - Workforce Breakdown
output$samuel_occ_chart <- renderPlotly({
  df <- clean11b_data %>%
    filter(occupation == input$samuel_occupation,
           year >= input$samuel_year_range[1], year <= input$samuel_year_range[2],
           age_group %in% c("25_to_34_years","35_to_44_years","45_to_54_years",
                            "55_to_64_years","65_years_and_over")) %>%
    mutate(Age_Label = factor(age_group,
                              levels = c("25_to_34_years","35_to_44_years","45_to_54_years",
                                         "55_to_64_years","65_years_and_over"),
                              labels = c("25-34","35-44","45-54","55-64","65+")),
           employment_thousands = replace_na(employment_thousands, 0))
  if (input$samuel_chart_type == "bar") {
    plot_ly(df, x = ~factor(year), y = ~employment_thousands, color = ~Age_Label,
            type = "bar", colors = brewer.pal(5, "Set2"),
            text = ~paste0(Age_Label, ": ", scales::comma(employment_thousands), "k"),
            hoverinfo = "text") %>%
      layout(barmode = "group", font = list(family = "Times New Roman"),
             xaxis = list(title = ""),
             yaxis = list(title = "Workers (thousands)"),
             title = list(text = "Workers by Age Group Over Time",
                          font = list(family = "Times New Roman")))
  } else {
    plot_ly(df, x = ~year, y = ~employment_thousands, color = ~Age_Label,
            type = "scatter", mode = "lines+markers", colors = brewer.pal(5, "Set2"),
            text = ~paste0(Age_Label, ": ", scales::comma(employment_thousands), "k"),
            hoverinfo = "text") %>%
      layout(font = list(family = "Times New Roman"),
             xaxis = list(title = ""),
             yaxis = list(title = "Share (%)"),
             title = list(text = "Workers by Age Group Over Time",
                          font = list(family = "Times New Roman")))
  }
})

output$samuel_share_chart <- renderPlotly({
  df <- clean11b_data %>%
    filter(occupation == input$samuel_occupation,
           year >= input$samuel_year_range[1], year <= input$samuel_year_range[2],
           age_group %in% c("25_to_34_years","35_to_44_years","45_to_54_years",
                            "55_to_64_years","65_years_and_over")) %>%
    mutate(Age_Label = factor(age_group,
                              levels = c("25_to_34_years","35_to_44_years","45_to_54_years",
                                         "55_to_64_years","65_years_and_over"),
                              labels = c("25-34","35-44","45-54","55-64","65+")),
           employment_thousands = replace_na(employment_thousands, 0)) %>%
    group_by(year) %>%
    mutate(Share = round(employment_thousands / sum(employment_thousands, na.rm = TRUE) * 100, 1)) %>%
    ungroup()
  plot_ly(df, x = ~factor(year), y = ~Share, color = ~Age_Label,
          type = "bar", colors = brewer.pal(5, "Set2"),
          text = ~paste0(Age_Label, ": ", Share, "%"), hoverinfo = "text") %>%
    layout(barmode = "stack", font = list(family = "Times New Roman"),
           xaxis = list(title = ""),
           yaxis = list(title = "Share (%)"),
           title = list(text = "Age Group Share of Occupation Over Time",
                        font = list(family = "Times New Roman")))
}) 

output$samuel_status_chart <- renderPlotly({
  df <- clean8_data %>%
    filter(Age %in% c("25 to 54", "55+"),
           Year >= input$samuel_year_range[1], Year <= input$samuel_year_range[2])
  if (input$samuel_sex != "All") df <- df %>% filter(Sex == input$samuel_sex)
  if (input$samuel_race != "All") df <- df %>% filter(Race == input$samuel_race)
  df <- df %>%
    group_by(Year, Age) %>%
    summarise(FT = sum(FT, na.rm = TRUE), PT = sum(PT, na.rm = TRUE),
              Unemp = sum(Unemp, na.rm = TRUE), .groups = "drop") %>%
    pivot_longer(cols = c(FT, PT, Unemp), names_to = "Status", values_to = "Workers")
  plot_ly(df, x = ~factor(Year), y = ~Workers, color = ~Status,
          type = "bar", colors = brewer.pal(3, "Set2"),
          text = ~paste0(Age, " - ", Status, ": ", scales::comma(Workers), "k"),
          hoverinfo = "text") %>%
    layout(barmode = "group", font = list(family = "Times New Roman"),
           xaxis = list(title = ""),
           yaxis = list(title = "Workers (thousands)"),
           title = list(text = "Full-Time vs Part-Time vs Unemployed (Ages 25+)",
                        font = list(family = "Times New Roman")))
})
}
shinyApp(ui, server)