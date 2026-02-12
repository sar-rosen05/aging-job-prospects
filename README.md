# Aging Job Prospects
Aging Job Prospects: The Employment Age Matrix, Forecasting the Uncertain Future

The data dashboard challenge for the Winter 2026 BDATA 412 Advanced Data Visualization course with Prof. Trujillo. The challenge uses data from the U.S. Bureau of Labor  Statistics for Labor Force (BLS) to develop interactive data visualizations to illumniate patterns in the labor force age across occupation.

### Team

-   📊 Challenge created by and made for the BDATA 412 [Advanced Data Visualization](https://github.com/UWB-Adv-Data-Vis) course at the [University of Washington Bothell](https://www.uwb.edu/).

-   ✍️ Authored by [Eric H. K.](https://github.com/EricKim117), [Richard J.](https://github.com/), [Minh T.](https://github.com/), and [Sean N.](https://github.com/sn2050-commits).
-   🔍 Challenge review committee: [Sophia W.](https://github.com/sophiawalters) (Chair), [Nikolai D.](https://github.com/NikolaiDecneut), [Aiden W.](https://github.com/aidenwal21), [Jeffery C.] , [Debbie]
-   🛠️ Edited and supervised by Prof. [Caleb Trujillo](https://github.com/calebtru).


### Learning Objectives
In this assignment you will demonstrate your ability to:

*    Prepare data by appropriately formatting it for analysis, summarizing, and filtering. For example, organize real-world data using a standard file format (CSV) and organizational approaches (Tidy).
*    Answer meaningful research questions using the tools in one or more software packages to work with authentic data.
*    Be capable of running, modifying, and sharing scripts to accomplish analyze data and visualize in one scripting language (R).
*    Manage project development to store, organize, and track code using digital collaboration tools for reproducibility (GitHub).
*    Create a data dashboard for the web to disseminate findings and visualization (R Shiny)
*    Describe the rationale, methods, results, and broader social context of your student-led project that used data to answer an interesting question.
*    Describe and use different types of critical and scientific thinking to develop inquiry into selected projects and critique visualizations.

## Overview
This data challenge directly addresses the concerns of the future labor market in relation to previous trends. This project examines data from the U.S. Labor Bureau spanning between 2011-2024 to portray the ages that workers joining and leaving the workforce across different occupations. 

## Intent of the Dashboard
The dashboard is intended to clearly present occupational employment trends by age and to provide insights into historical patterns, structural shifts, industry trends, and automation impacts, helping to inform policymakers and analysts through evidence-based decision-making. It functions as a supplemental resource to enable users to compare the different occupation performance across age groups.

## Data Challenge Statement

This challenge asks teams to turn complex labor data into a clear, easy-to-use dashboard using R Shiny and ggplot2. We intend the dashboards to be accessible and understandable to the general public, prioritizing creative layouts, clear labeling, and clear data storytelling so that it enables non-technical users to engage with the visualizations meaningfully. By making the dashboard easily accessible to the public, it will provide a good reference for students or any public stakeholders trying to understand the trends of labor markets. 

### Why This Challenge Matters:

These employment numbers helps users to identify emerging trends and risks in different age groups while helping to develop better policy interventions, economic planning, and potential workforce incentives strategies. 

### What We Hope to Learn:

The dashboard will help users explore the data and find information. Users should be able to quickly see which occupations have the entering age groups, retiring agegroups, and see how does occupation inequality shows up in generational changes.

## Purpose 

Inform stakeholders to navigate the job market and mangage expectations for career prospects in different occupations.
We aim to make complex labor force data clear and accessible through interactive visuals, helping users understand workforce entry, peak years, retirement patterns, and generational job prospects.

## Stakeholders:
 - Policymakers and city planners
 - Economists and researchers studying income inequality and cost of living
 - Non-profit groups and workforce advocates
 - Students and the general public interested in social issues and job markey
 - Recruiters and hiring professionals

## Targeted Questions for Visual Analytics

The dashboard _could_ answer these key questions through visuals:

 1.	What are the entry age groups for different occupations?
 2.	Peak years for occupations? Peak being defined as salary earned to hours worked
 3.	Retirement years for occupations?
 4.	Unemployment by occupation and unemployment?
 6.	Is gen-z screwed with their employment opportunities in the current job market?
 7.	Which generation(s) are projected to never retire?
 8.	How do these changes affect different social identities (e.g. race, ethnicity, gender)?

## Design Principles:
 - Simplify: Keep it simple but informative: focus on 1-2 strong visualizations per person.
 - Easy: Leverage the use of easy to use tabs and menus to support mobile devices.
 - Clean: Clear labels, details-demand, and notes for more information
 - Coherent: Same color scheme across all panels
 - Flexible: Works on different screen sizes including desktops, phones, tablets, and laptops.
 - Understandable: Follow ggplot2 best practices
 - **_Please replace this README with one for your produced dashboard and include a link to the original challenge._**


## The data
### Data Biography

The main data sets we collected were the employed persons by detailed occupation and age from the Bureau of Labor Statistics. A secondary data set was collected on unemployment from the same source. These data sets have the purpose of informing decisions regarding labor or giving evidence to support policies. The data was collected by the American government, by the agency of Bureau of Labor Statistics under the United States Department of Labor. For the 2024 employed persons dataset, 161,346 observations are present, with the observations representing numbers in thousands.  
 
The collection methods for these data sets are explicitly mentioned in their [Handbook of Methods](https://www.bls.gov/opub/hom/about.htm) section on their website. However, for a brief summary of these methods, the Bureau of Labor Statistics surveys businesses and agencies, then receives lists of employees or job titles provided. As well as utilizes as much public information conducted from these surveys as well as census, government and private data. This has impacts on managers, analysts, and government officials. The public nature helps management occupations better understand the state of occupations and age ranges.  
 
This then lets management teams have a more informed idea of the teams they wish to create or keep using the public data in their decisions in cultivating the overall make up of their workforce. Analysts on the other hand will be able to discover information on the trend of occupations and age, allowing them to create observations and study this data which will then be utilized in things such as reports to that very management previously discussed. While government officials could use these analyses on occupation and age data to be better informed when proposing policies that affect the make up of the industries these occupations are situated in.  
 
These data sets are all summarized data from another source, as they are all tables of counts. Known limitations here are that they may have under or over- representation. Occupation categories could also be too general and cover very different fields. There is missing data in the data sets  in the median age column, represented by dashes. There are accessible notes at the bottom to explain what median age means and the dash (“-”) indicates that there is no data. There are quite a bit of papers specifically on the data taken from theUS Bureau of Labor and Statistics. 
 
Research Papers: 
[https://www.bls.gov/osmr/research-papers/](https://www.bls.gov/osmr/research-papers/)  


### Dataset details:

The data source that we will be using is from the U.S. Bureau of Labor Statistics for Labor Force, we will be looking at the annual average years from 2011 to 2024 dataset of 11b. Employed persons by detailed occupation and age. This range attempts to capture the different trends of more than a decade of labor market dynamics from the Great Recession recovery, COVID-19, and now the current labor market restructuring. The dataset is split between management/professional occupations and service occupations, spanning over 300 occupations combined, each is a table. 
 
Data Source Links: 

 - U.S. Bureau of Labor Statistics for Labor Force Dataset [Link](https://www.bls.gov/cps/tables.htm#otheryears) 
 - U.S. Bureau of Labor Statistics for Labor Force Dataset [PDF](https://www.bls.gov/cps/cpsa2024.pdf)
 - Consider using a R package like blscrapeR, blsAPI, BLSloadR to avoid limitations related to multiple sheets and large data storage.

### Understanding Table Organization :

Example Table(s): 
 - Employed persons by detailed occupation and age [2023](https://www.bls.gov/cps/data/aa2023/cpsaat11b.htm) 
 - Employed persons by detailed occupation and age [2024](https://www.bls.gov/cps/aa2014/cpsaat11.htm) 

### Data Accessibility and Quality:
This is a bit more accessible in terms of data retrieval specifically where the data sources are:

- CPS (Current Population Survey, what is mostly going to be used)
[https://www.bls.gov/cps/data.htm](https://www.bls.gov/cps/data.htm) 
- Other sources (Aiding in the objectives of the dashboard, Demographic Data Sources, etc)
[https://www.bls.gov/data/apps.htm](https://www.bls.gov/data/apps.htm)


## Project Plan:
Week-phase planned tasks with goals are listed.

### Week 1 –
Design, Ideation, and Planning
 - Orient: Practice group commits, branching, merging, and writing issues.
 - Plan: Plan the workflow by setting assign tasks, creating *issues*, and exploring project development tools
 - Gather: Download and inspect BLS data, consider R packages best suited for the job.
 - Review: Review data structure and assign roles
 - Prepare: Search for R packages to preprocess/tidy data into a useful format  
 - Ideation: Brainstorm research questions
 - Design: Sketch dashboard layout onto paper
 - Assign: Set goals related for members contribute between class

Goal: Finalized dataset, clear dashboard objectives, achievable member-specific tasks assigned 

### Week 2 –
Prototype, Testing, Concepts
 - Orient: Revisit group commits and branches, merge and update issues as necessary.
 - Wrangle: Performing necessary cleaning and refining of the data.
 - Analysis: Conduct a first layer of analysis as needed.
 - Visualize: Create first draft visuals (ggplot charts, tables), each member is accountable for an original creation.
 - Evaluate: Judge the overall accessibility of the rough drafts
 - Discuss: Get feedback from peers
 - Compare: Consider alternative and creative visualization options
 - Modify: Interatively refine plots, labels, titles, and axes
 - Layout: Begin a template dash layout based on best sketches
 - Plan: Create new issues for project management
 
Goal: Prototype visuals through peer review, test member-specific contributions, template Shiny layout

### Week 3 –
Build, Coding, and Refinement
 - Orient: Revisit group commits and branches, merge and update issues as necessary.
 - Layout: Develop the dashboard components
 - Produce: Place visualizations in approporiate sections of the visual
 - Format: Check formating issues across platforms
 - Tidy: Review and further streamline the processing and summarize of data across members
 - Inputs: Connect the input components such as filters, checkboxes, and other interactive objects.
 - Fix: Check for errors and debug
 - Smooth: Assign issues of functionality to responsible members

Goal: Working interactive dashboard without bugs

### Week 4 -
Deploy, Testing, and Presentation
 - Test: Conduct user testing 
 - Polish: Refine aesthetics and layout
 - Review: Gather feedback from peers
 - Deploy: Host dash on Shiny or other server
 - Publish: Publish final dashboard and present the results

Goal: Polished, published dashboard ready for presentation

## Sources
Bureau of Labor Statistics. (2025, May). Labor Force Statistics from the Current Population Survey [Table Set](https://www.bls.gov/cps/tables.htm#otheryears). Current Population Survey, 2014 through 2024. U.S. Department of Labor. [https://www.bls.gov/cps/tables.htm#otheryears](https://www.bls.gov/cps/tables.htm#otheryears ) 

