library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(tidyr)

# Read the dataset
jobs_data <- read.csv("~/Desktop/usa_jobs.csv")

# Improved salary conversion function
convert_salary <- function(salary_range) {
  # Remove $ and K, split by hyphen
  vals <- as.numeric(gsub("[^0-9.]", "", unlist(strsplit(salary_range, "-"))))
  
  # If conversion fails, return NA
  if (length(vals) == 2) {
    return((vals[1] + vals[2]) / 2)
  } else {
    return(NA)
  }
}

# Apply salary conversion safely
jobs_data$Avg_Salary <- sapply(jobs_data$Salary.Range, convert_salary)

# Remove rows with NA salaries if needed
jobs_data <- jobs_data[!is.na(jobs_data$Avg_Salary), ]

ui <- dashboardPage(
  dashboardHeader(title = "Global Job Market Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Salary Insights", tabName = "salary", icon = icon("dollar-sign")),
      menuItem("Geographic Analysis", tabName = "geo", icon = icon("globe")),
      menuItem("Job Details", tabName = "details", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Body background */
        body {
          background-color: pink !important;
        }
        
        /* Box customization */
        .box.box-solid.box-primary {
          border-color: #ff69b4 !important; /* Pink border */
        }
        .box-header {
          background-color: #ff69b4 !important; /* Pink header */
          color: white !important; /* White text */
        }
        
        /* Value boxes */
        .small-box {
          background-color: #ffb6c1 !important; /* Light pink background */
          color: white !important; /* White text */
        }
        
        /* Sidebar */
        .skin-blue .main-sidebar, .skin-blue .left-side {
          background-color: #ffb6c1 !important; /* Light pink sidebar */
        }
        .skin-blue .sidebar-menu > li.active > a {
          background-color: #ff69b4 !important; /* Active menu item */
          color: white !important;
        }
        .skin-blue .sidebar-menu > li > a {
          color: white !important; /* Menu item text */
        }
        
        /* Header bar */
        .skin-blue .main-header .navbar {
          background-color: #ff69b4 !important; /* Pink header bar */
        }
        .skin-blue .main-header .logo {
          background-color: #ff69b4 !important; /* Pink logo background */
          color: white !important; /* Logo text */
        }
      "))
    ),
    
    tabItems(
      # Overview Dashboard
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_jobs"),
                valueBoxOutput("avg_salary"),
                valueBoxOutput("unique_countries")
              ),
              fluidRow(
                box(title = "Job Distribution by Work Type", status = "primary", plotlyOutput("work_type_plot"), width = 6),
                box(title = "Top Job Roles", status = "primary", plotlyOutput("top_roles_plot"), width = 6)
              )
      ),
      
      # Salary Insights
      tabItem(tabName = "salary",
              fluidRow(
                box(title = "Salary Distribution by Experience", status = "primary", plotlyOutput("salary_experience_plot"), width = 12),
                box(title = "Salary by Qualification", status = "primary", plotlyOutput("salary_qualification_plot"), width = 12)
              )
      ),
      
      # Geographic Analysis
      tabItem(tabName = "geo",
              fluidRow(
                box(title = "Job Opportunities by Country", status = "primary", plotlyOutput("country_job_distribution"), width = 12)
              )
      ),
      
      # Job Details
      tabItem(tabName = "details",
              fluidRow(
                box(title = "Detailed Job Information", status = "primary", 
                    width = 12, 
                    dataTableOutput("job_details_table")
                )
              )
      )
    )
  )
)


server <- function(input, output) {
  # Overview Boxes
  output$total_jobs <- renderValueBox({
    valueBox(
      nrow(jobs_data), 
      "Total Job Listings", 
      icon = icon("briefcase"),
      color = "blue"
    )
  })
  
  output$avg_salary <- renderValueBox({
    valueBox(
      paste0("$", round(mean(jobs_data$Avg_Salary, na.rm = TRUE), 2), "K"), 
      "Average Salary", 
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$unique_countries <- renderValueBox({
    valueBox(
      length(unique(jobs_data$Country)), 
      "Countries Represented", 
      icon = icon("globe"),
      color = "purple"
    )
  })
  
  # Work Type Distribution
  output$work_type_plot <- renderPlotly({
    work_type_counts <- jobs_data %>%
      group_by(Work.Type) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = Count / sum(Count) * 100)
    
    plot_ly(work_type_counts, labels = ~Work.Type, values = ~Count, type = "pie",
            textinfo = 'label+percent',
            marker = list(colors = c("#3c8dbc", "#00a65a", "#dd4b39", "#f39c12"))) %>%
      layout(title = "Job Distribution by Work Type")
  })
  
  # Top Job Roles
  output$top_roles_plot <- renderPlotly({
    top_roles <- jobs_data %>%
      group_by(Role) %>%
      summarise(Count = n()) %>%
      top_n(10, Count) %>%
      arrange(desc(Count))
    
    plot_ly(top_roles, x = ~Role, y = ~Count, type = "bar",
            marker = list(color = "#3c8dbc")) %>%
      layout(title = "Top 10 Job Roles", xaxis = list(tickangle = 45))
  })
  
  # Salary by Experience
  output$salary_experience_plot <- renderPlotly({
    salary_exp <- jobs_data %>%
      group_by(Experience) %>%
      summarise(
        Avg_Salary = mean(Avg_Salary, na.rm = TRUE),
        Min_Salary = min(Avg_Salary, na.rm = TRUE),
        Max_Salary = max(Avg_Salary, na.rm = TRUE)
      )
    
    plot_ly(salary_exp, x = ~Experience, y = ~Avg_Salary, type = "bar",
            error_y = list(
              type = "data",
              array = ~Max_Salary - Avg_Salary,
              arrayminus = ~Avg_Salary - Min_Salary,
              color = "#3c8dbc"
            )) %>%
      layout(title = "Average Salary by Experience Level", yaxis = list(title = "Salary (K$)"))
  })
  
  # Salary by Qualification
  output$salary_qualification_plot <- renderPlotly({
    salary_qual <- jobs_data %>%
      group_by(Qualifications) %>%
      summarise(
        Avg_Salary = mean(Avg_Salary, na.rm = TRUE),
        Job_Count = n()
      ) %>%
      arrange(desc(Job_Count)) %>%
      top_n(10, Job_Count)
    
    plot_ly(salary_qual, x = ~Qualifications, y = ~Avg_Salary, type = "bar",
            text = ~paste("Jobs:", Job_Count),
            hoverinfo = 'text+y',
            marker = list(color = "#00a65a")) %>%
      layout(title = "Average Salary by Qualification", 
             xaxis = list(tickangle = 45),
             yaxis = list(title = "Average Salary (K$)"))
  })
  
  # Country Job Distribution
  output$country_job_distribution <- renderPlotly({
    country_jobs <- jobs_data %>%
      group_by(Country) %>%
      summarise(Job_Count = n()) %>%
      top_n(20, Job_Count) %>%
      arrange(desc(Job_Count))
    
    plot_ly(country_jobs, x = ~Country, y = ~Job_Count, type = "bar",
            marker = list(color = "#dd4b39")) %>%
      layout(title = "Job Opportunities by Country (Top 20)", 
             xaxis = list(tickangle = 45),
             yaxis = list(title = "Number of Jobs"))
  })
  
  # Detailed Job Information
  output$job_details_table <- renderDataTable({
    datatable(jobs_data %>% 
                select(Job.Title, Role, Country, Experience, Qualifications, Salary.Range, Work.Type),
              options = list(pageLength = 10, scrollX = TRUE),
              filter = 'top')
  })
}

shinyApp(ui, server)
