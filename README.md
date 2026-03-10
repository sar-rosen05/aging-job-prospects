# Aging Job Prospects Dashboard (2011–2024)

## Project Description
This project analyzes and visualizes trends in the U.S. job market between 2011 and 2024 using an interactive dashboard built with **R Shiny**. The goal of the project is to explore how demographic and structural factors such as **age, race, gender, and time** influence employment patterns.

Each team member created a visualization examining a specific aspect of the labor market. Together, these visualizations highlight how employment conditions vary across different groups and how these patterns change over time.

The dashboard allows users to interact with the data by filtering variables, adjusting time ranges, and exploring trends in unemployment and employment across different populations.

Our central research question is:

**How do factors such as age, race, gender, and time affect employment trends in the United States?**

The project was developed collaboratively using **GitHub** to manage version control and coordinate updates through commits, pushes, and pulls.

---

## Team Members
- Dareen Jonathan Padilla  
- Rishita Bonepalli  
- Samuel Lemma  
- Sarah Rosen  
- Zuwiyda Hirse  
- Shahlan Finchan  
- Obydah Jazaeri  

---

## How to Access the Dashboard

The interactive dashboard is built using **R Shiny** and can be run locally.

### Steps to Run the Dashboard

1. Download or clone this GitHub repository.
2. Open the project in **RStudio**.
3. Ensure the required R packages are installed.
4. Open the file:

```
Dashboard.R
```

5. Click **Run App** in RStudio or run the following command in the console:

```r
shiny::runApp("Dashboard.R")
```

This will launch the interactive dashboard in your browser.

The dashboard contains:
- An **overview section**
- Multiple **interactive visualizations**
- Analysis of employment trends across demographic groups
- Interactive controls that allow users to filter and explore the data.

## Sources
Bureau of Labor Statistics. (2025, May). Labor Force Statistics from the Current Population Survey 
[Table Set](https://www.bls.gov/cps/tables.htm#otheryears). Current Population Survey, 2014 through 2024. 
U.S. Department of Labor. [https://www.bls.gov/cps/tables.htm#otheryears](https://www.bls.gov/cps/tables.htm#otheryears ) 

##Data Biography
The main data sets we collected were the employed persons by detailed occupation and age from the Bureau of Labor Statistics. 
A secondary data set was collected on unemployment from the same source. These data sets have the purpose of informing decisions 
regarding labor or giving evidence to support policies. The data was collected by the American government, by the agency of Bureau
of Labor Statistics under the United States Department of Labor. For the 2024 employed persons dataset, 161,346 observations are present,
with the observations representing numbers in thousands.

The collection methods for these data sets are explicitly mentioned in their Handbook of Methods section on their website. However, for a
brief summary of these methods, the Bureau of Labor Statistics surveys businesses and agencies, then receives lists of employees or job 
titles provided. As well as utilizes as much public information conducted from these surveys as well as census, government and private data. 
This has impacts on managers, analysts, and government officials. The public nature helps management occupations better understand the state
of occupations and age ranges.

This then lets management teams have a more informed idea of the teams they wish to create or keep using the public data in their decisions
in cultivating the overall make up of their workforce. Analysts on the other hand will be able to discover information on the trend of 
occupations and age, allowing them to create observations and study this data which will then be utilized in things such as reports to that
very management previously discussed. While government officials could use these analyses on occupation and age data to be better informed 
when proposing policies that affect the make up of the industries these occupations are situated in.

These data sets are all summarized data from another source, as they are all tables of counts. Known limitations here are that they may have under or over- representation. Occupation categories could also be too general and cover very different fields. There is missing data in the data sets in the median age column, represented by dashes. There are accessible notes at the bottom to explain what median age means and the dash (“-”) indicates that there is no data. There are quite a bit of papers specifically on the data taken from theUS Bureau of Labor and Statistics.

Research Papers: https://www.bls.gov/osmr/research-papers/
