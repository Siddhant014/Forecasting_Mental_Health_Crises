# app.R
# Fan and Aaron, January 2019

# This script read the shiny app script. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/library(rsconnect)


library(tidyverse)
library(shiny)
library(scales)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(shinyhelper)


# Load tidy survey data after gender and age values have been fixed
df <- read_csv("C:\\Users\\Aaron Stone\\Documents\\R\\Minor 2019\\small.csv")

# Factorize the survey responses in an order consistent with negative to positive attitudes towards mental health
df <- df %>%
  mutate_at(vars(Gender:obs_consequence), funs(factor(.))) %>%
  mutate_at(vars(work_interfere),
            funs(fct_relevel(., c("Never", "Rarely", "Sometimes", "Often")))) %>%
  mutate_at(vars(no_employees),
            funs(fct_relevel(.,c("1-5", "6-25", "26-100", "100-500", "500-1000")))) %>%
  mutate_at(vars(Physical_health),
            funs(fct_relevel(.,c("Don't know", "Very easy", "Somewhat easy", "Somewhat difficult", "Very difficult")))) %>%
  mutate_at(vars(mental_health_consequence, phys_health_consequence),
            funs(fct_relevel(., c("Yes","Maybe","No")))) %>%
  mutate_at(vars(coworkers, supervisor),
            funs(fct_relevel(., c("No","Some of them","Yes")))) %>%
  mutate_at(vars(mental_health_interview, phys_health_interview),
            funs(fct_relevel(., c("No","Maybe","Yes")))) %>%
  mutate_at(vars(mental_vs_physical),
            funs(fct_relevel(., c("No","Don't know","Yes")))) %>%
  mutate_at(vars(obs_consequence),
            funs(fct_relevel(., c("Yes","No"))))

# Organize column names by general grouping and label with human readable names
col_backround_info <- colnames(df)[1:12]

col_attitudes <- c("mental_health_consequence", "phys_health_consequence",
                   "coworkers", "supervisor","mental_health_interview",
                   "phys_health_interview")
                   

col_employer_policies <-c("Have you visited a doctor for routine checkup in last 6 months?" = "checkup",
                          
                                 "Do you have serious difficulty concentrating, remembering, 
                                 or making decision?" = "concentration",
                          
                                 "(Ever told) you have diabetes?" = "diabates",
                          
                                 "(Ever told) you have some form of arthritis, rheumatoid arthritis, 
                                  gout, lupus, or fibromyalgia?" = "arthritis",
                          
                                 "Are you blind or do you have serious difficulty seeing, 
                                  even when wearing glasses?" = "blindness",
                         
                                 "Do you have any kind of health care coverage, including health insurance, prepaid
                                  plans such as HMOs, government plans such as Medicare,
                                  or Indian Health Service?" = "Physical_health")

col_attitudes_labels <- c("Q1. Smoking",
                          "Q2. Sleeping",
                          "Q3. Special Requirements", 
                          "Q4.Support",
                          "Q5. Marital Status",
                          "Q6. Physical Health")

                          
name_change <- tibble(old_survey_q = col_attitudes, new_survey_q = col_attitudes_labels)

# Describe and format mental health survey questions to be used in the hovertext
question_text <-
  c("<br>Q1. Smoking:<br>  Do you smoke daily<br>?",
    
    "<br>Q2. Sleeping: <br> Over the last 2 weeks, how many days have you had trouble 
    falling asleep or staying asleep or sleeping too much? ",
    
    "<br>Q3. Health Insurance:<br> Did Healh Insurance pay for all of your expenses?",
    
    "<br>Q4. Support:<br>Do you get enough social and emotional <br>support that you need? ",
    
    "<br>Q5. Marital Status:<br> Are you married?<br>",
    
    "<br>Q6. Activity Limitation:<br>  Are you limited in any way in any activities because of physical, 
     mental, or emotional problems?")

   

## Build Shiny App

# UI
ui <- fluidPage(

  theme = shinytheme("journal"),
  titlePanel("Tendency Towards Depression?",
             windowTitle = " Mental Health Attitude <br>"),



  p("The dashboard visualizes survey results based on various questions that were asked. People were
     grouped based on the questions relating to the attributes whose information gain came out to be maximum 
     and the questions can be toggled below.
     Each group was aggregated to get an overall score on how positive their attitude was towards
     mental health based on the specific questions that were asked. The overall area of the plot gives insight 
     into the overall positive attitude. With this insight we can compare which attribute made the biggest difference."),

  sidebarLayout(
                sidebarPanel(

                  tags$head(tags$style(type="text/css",
                                       ".test_type {color: red;
                           font-size: 20px;
                           font-style: italic;}")
                  ),

                      selectInput(inputId = "employer_q",
                              label = "Survey Questions",
                              choices = col_employer_policies,
                              selected = "benefits"),

                       uiOutput("secondSelection")
                  ),

                mainPanel(
                    tabsetPanel(type = "tabs",
                                tabPanel("Plot", plotlyOutput("scatterpolar"))
                                
                    )

                )
   )
)


# Server
server <- function(input, output){

  output$secondSelection <- renderUI({
    checkboxGroupInput(inputId = "employer_q_options_var",
                       label = NULL,
                       choices = df[[input$employer_q]] %>% unique(),
                       selected = df[[input$employer_q]] %>% unique())
  })


  df_summary <- reactive({
    df %>%
      mutate_at(col_attitudes, funs(as.numeric(.))) %>%
      mutate_at(col_attitudes, funs(rescale(.))) %>%
      select(col_attitudes,foo = input$employer_q) %>%
      group_by(foo) %>%
      summarize_all(funs(mean(., na.rm = TRUE))) %>%
      gather(-foo, key = "Survey_Questions", value = "Mean_response") %>%
      filter(foo %in% input$employer_q_options_var) %>%
      left_join(name_change, by = c("Survey_Questions" = "old_survey_q"))
   })

  employer_q_options <- reactive({
    df_summary() %>%
      pull(foo) %>%
      unique()
   })

  hovertext <- reactive({
    paste(flatten_chr(map(question_text, rep, times = length(input$employer_q_options_var))),
          "<br><br>Average depressive Score:",
          df_summary() %>% pull(Mean_response) %>% round(2))
  })






  output$scatterpolar <- renderPlotly({

    req(input$employer_q)

    p <- plot_ly(
      type = 'scatterpolar',
      mode = "markers",
      fill = "toself",
      alpha = 0.4,
      colors = "Paired") %>%

      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,1)
          )
        )
      )

    p <- df_summary() %>%
            add_trace(p, data = ., theta = ~new_survey_q, r = ~Mean_response, color = ~foo,
                      hoverinfo = "text", hovertext = hovertext(), marker = list(size = 5))


    p
    })



}

shinyApp(ui, server)
