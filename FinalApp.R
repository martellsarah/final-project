library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(tidyr)

# Read the dataset
jobs_data <- read.csv("~/Desktop/job_descriptions.csv")

# Improved salary conversion function
convert_salary <- function(salary_range) {
  # Remove $ and K, split by hyphen
  vals <- as.numeric(gsub("[^0-9.]", "", unlist(strsplit(salary_range, "-"))))
  if (length(vals) == 2) {
    return((vals[1] + vals[2]) / 2)
  } else {
    return(NA)
  }
}

# Apply salary conversion safely
jobs_data$Avg_Salary <- sapply(jobs_data$Salary.Range, convert_salary)
jobs_data <- jobs_data[!is.na(jobs_data$Avg_Salary), ]

ui <- dashboardPage(
  dashboardHeader(title = "Global Job Market Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Salary Insights", tabName = "salary", icon = icon("dollar-sign")),
      menuItem("Geographic Analysis", tabName = "geo", icon = icon("globe")),
      menuItem("Job Details", tabName = "details", icon = icon("table")),
      menuItem("Salary Prediction", tabName = "prediction", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    # Custom CSS for Pink Theme
    tags$head(
      tags$style(HTML("
        body { background-color: #ffe4e1 !important; } /* Light pink */
        .box.box-solid.box-primary { border-color: #ff69b4 !important; } /* Pink box border */
        .box-header { background-color: #ff69b4 !important; color: white !important; }
        .small-box { background-color: #ffb6c1 !important; color: white !important; }
        .skin-blue .main-header .logo { background-color: #ff69b4 !important; color: white !important; }
        .skin-blue .main-header .navbar { background-color: #ff69b4 !important; }
        .skin-blue .main-sidebar { background-color: #ffc0cb !important; } /* Light pink sidebar */
        .skin-blue .sidebar-menu > li.active > a { background-color: #ff69b4 !important; color: white !important; }
        .skin-blue .sidebar-menu > li > a { color: white !important; }
      "))
    ),
    tabItems(
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
      tabItem(tabName = "salary",
              fluidRow(
                box(title = "Salary Distribution by Experience", status = "primary", plotlyOutput("salary_experience_plot"), width = 12),
                box(title = "Salary by Qualification", status = "primary", plotlyOutput("salary_qualification_plot"), width = 12)
              )
      ),
      tabItem(tabName = "geo",
              fluidRow(
                box(title = "Job Opportunities by Country", status = "primary", plotlyOutput("country_job_distribution"), width = 12)
              )
      ),
      tabItem(tabName = "details",
              fluidRow(
                box(title = "Detailed Job Information", status = "primary", width = 12, dataTableOutput("job_details_table"))
              )
      ),
      tabItem(tabName = "prediction",
              fluidRow(
                box(title = "Select Job Role", status = "primary", width = 6,
                    selectInput("predict_role", "Job Role:", choices = unique(jobs_data$Role)),
                    actionButton("predict_btn", "Predict Salary", class = "btn-primary")
                ),
                box(title = "Predicted Salary", status = "primary", width = 6,
                    valueBoxOutput("predicted_salary", width = 12)
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  # Overview Metrics
  output$total_jobs <- renderValueBox({
    valueBox(nrow(jobs_data), "Total Job Listings", icon = icon("briefcase"), color = "blue")
  })
  
  output$avg_salary <- renderValueBox({
    valueBox(paste0("$", round(mean(jobs_data$Avg_Salary, na.rm = TRUE), 2), "K"), 
             "Average Salary", icon = icon("dollar-sign"), color = "green")
  })
  
  output$unique_countries <- renderValueBox({
    valueBox(length(unique(jobs_data$Country)), "Countries Represented", icon = icon("globe"), color = "purple")
  })
  
  # Overview Plots
  output$work_type_plot <- renderPlotly({
    work_type_counts <- jobs_data %>% group_by(Work.Type) %>% summarise(Count = n())
    plot_ly(work_type_counts, labels = ~Work.Type, values = ~Count, type = "pie") %>%
      layout(title = "Job Distribution by Work Type")
  })
  
  output$top_roles_plot <- renderPlotly({
    top_roles <- jobs_data %>% group_by(Role) %>% summarise(Count = n()) %>% arrange(desc(Count)) %>% head(10)
    plot_ly(top_roles, x = ~Role, y = ~Count, type = "bar") %>%
      layout(title = "Top 10 Job Roles", xaxis = list(tickangle = 45))
  })
  
  # Salary Insights
  output$salary_experience_plot <- renderPlotly({
    salary_exp <- jobs_data %>% group_by(Experience) %>% summarise(Avg_Salary = mean(Avg_Salary, na.rm = TRUE))
    plot_ly(salary_exp, x = ~Experience, y = ~Avg_Salary, type = "bar") %>%
      layout(title = "Average Salary by Experience Level")
  })
  
  output$salary_qualification_plot <- renderPlotly({
    salary_qual <- jobs_data %>% group_by(Qualifications) %>% summarise(Avg_Salary = mean(Avg_Salary, na.rm = TRUE))
    plot_ly(salary_qual, x = ~Qualifications, y = ~Avg_Salary, type = "bar") %>%
      layout(title = "Average Salary by Qualification")
  })
  
  # Geographic Analysis
  output$country_job_distribution <- renderPlotly({
    country_jobs <- jobs_data %>% group_by(Country) %>% summarise(Job_Count = n()) %>% arrange(desc(Job_Count)) %>% head(20)
    plot_ly(country_jobs, x = ~Country, y = ~Job_Count, type = "bar") %>%
      layout(title = "Job Opportunities by Country (Top 20)", xaxis = list(tickangle = 45))
  })
  
  # Job Details
  output$job_details_table <- renderDataTable({
    datatable(jobs_data %>% select(Job.Title, Role, Country, Experience, Qualifications, Salary.Range, Work.Type),
              options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Salary Prediction
  observeEvent(input$predict_btn, {
    predicted_salary <- jobs_data %>%
      filter(Role == input$predict_role) %>%
      summarise(Avg_Salary = mean(Avg_Salary, na.rm = TRUE)) %>%
      pull(Avg_Salary)
    
    output$predicted_salary <- renderValueBox({
      if (!is.na(predicted_salary)) {
        valueBox(paste0("$", round(predicted_salary, 2), "K"), "Predicted Salary", icon = icon("chart-line"), color = "orange")
      } else {
        valueBox("N/A", "Predicted Salary", icon = icon("exclamation-triangle"), color = "red")
      }
    })
  })
}

shinyApp(ui, server)
