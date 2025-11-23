library(shiny)
library(tidyverse)
source("initialize.r")

# Define UI ----
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  # Custom CSS for improved styling
  tags$head(
    tags$style(HTML("
      /* Container for tabs */
      .nav-tabs {
        display: flex;
        flex-wrap: wrap;
        border-bottom: none;
        margin-bottom: 20px;
      }
      
      /* Individual Tab Styling */
      .nav-tabs .nav-item {
        width: 24%;
        margin: 0.5%;
        text-align: center;
      }
      
      .nav-tabs .nav-link {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 4px;
        color: #2c3e50 !important;
        font-weight: 500;
        height: 100%;
        display: flex;
        align-items: center;
        justify-content: center;
        transition: all 0.3s ease;
      }
      
      .nav-tabs .nav-link:hover {
        background-color: #e9ecef;
        border-color: #adb5bd;
        color: #1a252f !important;
      }
      
      .nav-tabs .nav-link.active {
        background-color: #2c3e50;
        color: white !important;
        border-color: #2c3e50;
      }
      
      /* Title Styling */
      .main-title {
        text-align: center;
        margin-top: 20px;
        margin-bottom: 10px;
      }
      
      .main-title h1 {
        font-size: 2.8em;
        font-weight: bold;
        color: #2c3e50;
        margin-bottom: 5px;
      }
      
      .main-title h4 {
        font-size: 1.2em;
        color: #7f8c8d;
        font-weight: normal;
        margin-top: 0;
      }
      
      /* Adjust well panel height */
      .well {
        min-height: 450px;
      }
      
      /* Sidebar transition */
      .sidebar-panel {
        transition: all 0.5s ease;
      }
      
      .sidebar-initial {
        width: 50% !important;
        margin: 0 auto;
        float: none !important;
      }
      
      .content-container {
        width: 100%;
        max-height: 85vh;
        overflow-y: auto;
      }
      
      /* Hide scrollbar for cleaner look */
      .content-container::-webkit-scrollbar {
        width: 8px;
      }
      
      .content-container::-webkit-scrollbar-track {
        background: #f1f1f1;
      }
      
      .content-container::-webkit-scrollbar-thumb {
        background: #888;
        border-radius: 4px;
      }
      
      .content-container::-webkit-scrollbar-thumb:hover {
        background: #555;
      }
    "))
  ),
  
  # Title
  div(class = "main-title",
    h1("BRFSS Health Survey Dashboard"),
    h4("by Spandan, Rituraj and Gaurav")
  ),
  
  # Conditional layout based on question selection
  conditionalPanel(
    condition = "input.question_select == ''",
    fluidRow(
      column(12,
        div(class = "sidebar-initial",
          wellPanel(
            h4("Select Survey Question"),
            hr(),
            
            selectInput("class_select", 
                        "1. Select Class:",
                        choices = c("Choose..." = "", sort(unique(layerQ$Class)))),
            
            conditionalPanel(
              condition = "input.class_select != ''",
              selectInput("topic_select",
                          "2. Select Topic:",
                          choices = NULL)
            ),
            
            conditionalPanel(
              condition = "input.topic_select != ''",
              selectInput("question_select",
                          "3. Select Question:",
                          choices = NULL)
            )
          )
        )
      )
    )
  ),
  
  conditionalPanel(
    condition = "input.question_select != ''",
    sidebarLayout(
      # Sidebar for question selection
      sidebarPanel(
        class = "sidebar-panel",
        width = 3,
        h4("Select Survey Question"),
        hr(),
        
        # Hierarchical selection
        selectInput("class_select2", 
                    "1. Select Class:",
                    choices = c("Choose..." = "", sort(unique(layerQ$Class)))),
        
        conditionalPanel(
          condition = "input.class_select2 != ''",
          selectInput("topic_select2",
                      "2. Select Topic:",
                      choices = NULL)
        ),
        
        conditionalPanel(
          condition = "input.topic_select2 != ''",
          selectInput("question_select2",
                      "3. Select Question:",
                      choices = NULL)
        ),
        
        hr(),
        
        tags$div(
          style = "background-color: #e8f4f8; padding: 10px; border-radius: 5px;",
          h5("Selected Question:"),
          textOutput("selected_question_display")
        )
      ),
      
      # Main panel with tabs
      mainPanel(
        width = 9,
        div(class = "content-container",
          tabsetPanel(
            id = "main_tabs",
            type = "tabs",
            
            # Row 1: Overall, Temporal, Age, Gender
            tabPanel("Overall",
              br(),
              wellPanel(
                style = "background-color: #f8f9fa;",
                h4("Overall Distribution"),
                radioButtons("gran_overall", "View:",
                             choices = c("Summary" = 1, "With Confidence Intervals" = 4),
                             selected = 1, inline = TRUE),
                hr(),
                plotOutput("overallPlot", height = "400px"),
                actionButton("expand_overall", "🔍 View Larger", 
                             class = "btn-sm btn-outline-primary")
              )
            ),
            
            tabPanel("Temporal",
              br(),
              wellPanel(
                style = "background-color: #f8f9fa;",
                h4("Temporal Trends"),
                p("Year-over-year distribution"),
                hr(),
                plotOutput("temporalPlot", height = "400px"),
                actionButton("expand_temporal", "🔍 View Larger", 
                             class = "btn-sm btn-outline-primary")
              )
            ),
            
            tabPanel("Age",
              br(),
              wellPanel(
                style = "background-color: #f8f9fa;",
                h4("By Age Group"),
                selectInput("gran_age", "Granularity:",
                            choices = c("Summary" = 1, "By Year" = 2,
                                        "By Top 12 States" = 3, "With CI" = 4),
                            selected = 1),
                hr(),
                plotOutput("agePlot", height = "400px"),
                actionButton("expand_age", "🔍 View Larger", 
                             class = "btn-sm btn-outline-primary")
              )
            ),
            
            tabPanel("Gender",
              br(),
              wellPanel(
                style = "background-color: #f8f9fa;",
                h4("By Gender"),
                selectInput("gran_gender", "Granularity:",
                            choices = c("Summary" = 1, "By Year" = 2,
                                        "By Top 12 States" = 3, "With CI" = 4),
                            selected = 1),
                hr(),
                plotOutput("genderPlot", height = "400px"),
                actionButton("expand_gender", "🔍 View Larger", 
                             class = "btn-sm btn-outline-primary")
              )
            ),
            
            # Row 2: Race, Education Level, Income Level, State/Territory
            tabPanel("Race",
              br(),
              wellPanel(
                style = "background-color: #f8f9fa;",
                h4("By Race/Ethnicity"),
                selectInput("gran_race", "Granularity:",
                            choices = c("Summary" = 1, "By Year" = 2,
                                        "By Top 12 States" = 3, "With CI" = 4),
                            selected = 1),
                hr(),
                plotOutput("racePlot", height = "400px"),
                actionButton("expand_race", "🔍 View Larger", 
                             class = "btn-sm btn-outline-primary")
              )
            ),
            
            tabPanel("Education Level",
              br(),
              wellPanel(
                style = "background-color: #f8f9fa;",
                h4("By Education Level"),
                selectInput("gran_edu", "Granularity:",
                            choices = c("Summary" = 1, "By Year" = 2,
                                        "By Top 12 States" = 3, "With CI" = 4),
                            selected = 1),
                hr(),
                plotOutput("eduPlot", height = "400px"),
                actionButton("expand_edu", "🔍 View Larger", 
                             class = "btn-sm btn-outline-primary")
              )
            ),
            
            tabPanel("Income Level",
              br(),
              wellPanel(
                style = "background-color: #f8f9fa;",
                h4("By Income Level"),
                selectInput("gran_income", "Granularity:",
                            choices = c("Summary" = 1, "By Year" = 2,
                                        "By Top 12 States" = 3, "With CI" = 4),
                            selected = 1),
                hr(),
                plotOutput("incomePlot", height = "400px"),
                actionButton("expand_income", "🔍 View Larger", 
                             class = "btn-sm btn-outline-primary")
              )
            ),
            
            tabPanel("State/Territory",
              br(),
              wellPanel(
                style = "background-color: #f8f9fa;",
                h4("By State/Territory"),
                p("Distribution across all U.S. states and territories"),
                hr(),
                plotOutput("statePlot", height = "500px"),
                actionButton("expand_state", "🔍 View Larger", 
                             class = "btn-sm btn-outline-primary")
              )
            )
          )
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  # Professional color palette
  color_palette <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", 
                     "#9b59b6", "#1abc9c", "#34495e", "#e67e22")
  
  # Sync the two sets of inputs
  observeEvent(input$class_select, {
    updateSelectInput(session, "class_select2", selected = input$class_select)
  })
  
  observeEvent(input$class_select2, {
    updateSelectInput(session, "class_select", selected = input$class_select2)
  })
  
  observeEvent(input$topic_select, {
    updateSelectInput(session, "topic_select2", selected = input$topic_select)
  })
  
  observeEvent(input$topic_select2, {
    updateSelectInput(session, "topic_select", selected = input$topic_select2)
  })
  
  observeEvent(input$question_select, {
    updateSelectInput(session, "question_select2", selected = input$question_select)
  })
  
  observeEvent(input$question_select2, {
    updateSelectInput(session, "question_select", selected = input$question_select2)
  })
  
  # Update topic choices based on class selection
  observeEvent(input$class_select, {
    req(input$class_select)
    
    topics <- layerQ |>
      filter(Class == input$class_select) |>
      pull(Topic) |>
      unique() |>
      sort()
    
    updateSelectInput(session, "topic_select",
                      choices = c("Choose..." = "", topics),
                      selected = "")
    updateSelectInput(session, "topic_select2",
                      choices = c("Choose..." = "", topics),
                      selected = "")
  })
  
  observeEvent(input$class_select2, {
    req(input$class_select2)
    
    topics <- layerQ |>
      filter(Class == input$class_select2) |>
      pull(Topic) |>
      unique() |>
      sort()
    
    updateSelectInput(session, "topic_select",
                      choices = c("Choose..." = "", topics),
                      selected = "")
    updateSelectInput(session, "topic_select2",
                      choices = c("Choose..." = "", topics),
                      selected = "")
  })
  
  # Update question choices based on topic selection
  observeEvent(input$topic_select, {
    req(input$topic_select)
    
    questions <- layerQ |>
      filter(Class == input$class_select,
             Topic == input$topic_select) |>
      pull(Question) |>
      unique() |>
      sort()
    
    updateSelectInput(session, "question_select",
                      choices = c("Choose..." = "", questions),
                      selected = "")
    updateSelectInput(session, "question_select2",
                      choices = c("Choose..." = "", questions),
                      selected = "")
  })
  
  observeEvent(input$topic_select2, {
    req(input$topic_select2)
    
    questions <- layerQ |>
      filter(Class == input$class_select2,
             Topic == input$topic_select2) |>
      pull(Question) |>
      unique() |>
      sort()
    
    updateSelectInput(session, "question_select",
                      choices = c("Choose..." = "", questions),
                      selected = "")
    updateSelectInput(session, "question_select2",
                      choices = c("Choose..." = "", questions),
                      selected = "")
  })
  
  # Display selected question
  output$selected_question_display <- renderText({
    req(input$question_select)
    input$question_select
  })
  
  # Reactive expression for processed data
  qData <- reactive({
    req(input$question_select)
    
    qDf <- df |>
      filter(Question == input$question_select) |>
      filter(!(Locationabbr == "US")) |>
      filter(!(Locationabbr == "UW"))
    
    # Apply merges
    qDf$ResponseID <- merge_ResponseID(qDf$ResponseID)
    qDf$Response <- merge_Response(unlist(qDf$ResponseID),
                                   unlist(qDf$Response))
    qDf$BreakoutID <- merge_BreakoutID(qDf$BreakoutID)
    qDf$Break_Out <- merge_Break_Out(unlist(qDf$BreakoutID),
                                     unlist(qDf$Break_Out))
    
    return(qDf)
  })
  
  # Overall Plot
  output$overallPlot <- renderPlot({
    qDf <- qData()
    gran <- as.numeric(input$gran_overall)
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID == "CAT1") |>
      select(Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Response) |>
      reframe(agg_persons = sum(persons))
    
    plotDf <- plotDf |>
      mutate(agg_ss = sum(agg_persons)) |>
      mutate(agg_percent = agg_persons * 100 / agg_ss) |>
      mutate(agg_percent_sdev = sqrt(agg_percent * (100 - agg_percent) / agg_ss)) |>
      mutate(agg_low_ci_limit = agg_percent - 2 * agg_percent_sdev) |>
      mutate(agg_high_ci_limit = agg_percent + 2 * agg_percent_sdev) |>
      select(-c(agg_persons, agg_ss, agg_percent_sdev))
    
    if (gran == 1) {
      plotDf |>
        ggplot(aes(x = "Overall", y = agg_percent, fill = Response)) +
        geom_col(position = "fill") + 
        scale_fill_manual(values = color_palette) +
        labs(x = NULL, y = "Percentage", fill = "Response") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "right")
    } else {
      plotDf |>
        ggplot(aes(x = Response, y = agg_percent, fill = Response)) +
        geom_col() +
        geom_errorbar(aes(ymin = agg_low_ci_limit, ymax = agg_high_ci_limit),
                      width = 0.3, linewidth = 1) +
        scale_fill_manual(values = color_palette) +
        labs(y = "Percentage (%)", x = NULL) +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              legend.position = "none")
    }
  })
  
  # Temporal Plot
  output$temporalPlot <- renderPlot({
    qDf <- qData()
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID == "CAT1") |>
      select(Year, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Year) |>
      reframe(Response = Response,
              persons = persons,
              agg_ss = sum(persons)) |>
      group_by(Year, Response) |>
      reframe(agg_ss = agg_ss,
              agg_persons = sum(persons),
              agg_percent = agg_persons * 100 / agg_ss) |>
      distinct()
    
    plotDf |>
      ggplot(aes(x = Year, y = agg_percent, fill = Response)) +
      geom_col(position = "fill") +
      scale_fill_manual(values = color_palette) +
      labs(x = "Year", y = "Percentage", fill = "Response") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            legend.position = "right")
  })
  
  # State Plot
  output$statePlot <- renderPlot({
    qDf <- qData()
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID == "CAT1") |>
      select(Locationabbr, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Locationabbr) |>
      reframe(Response = Response,
              persons = persons,
              agg_ss = sum(persons)) |>
      group_by(Locationabbr, Response) |>
      reframe(agg_ss = agg_ss,
              agg_persons = sum(persons),
              agg_percent = agg_persons * 100 / agg_ss) |>
      distinct()
    
    plotDf |>
      ggplot(aes(x = reorder(Locationabbr, -agg_percent), y = agg_percent,
                 fill = Response)) +
      geom_col(position = "fill") +
      scale_fill_manual(values = color_palette) +
      labs(x = "State/Territory", y = "Percentage", fill = "Response") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
            legend.position = "bottom")
  })
  
  # Age Plot with Granularity
  output$agePlot <- renderPlot({
    qDf <- qData()
    gran <- as.numeric(input$gran_age)
    
    plotDf <- aggregate_by_category(qDf, "CAT3", granularity = gran)
    create_granular_plot(plotDf, "Break_Out", granularity = gran, colors = color_palette)
  })
  
  # Gender Plot with Granularity
  output$genderPlot <- renderPlot({
    qDf <- qData()
    gran <- as.numeric(input$gran_gender)
    
    plotDf <- aggregate_by_category(qDf, "CAT2", granularity = gran)
    create_granular_plot(plotDf, "Break_Out", granularity = gran, colors = color_palette)
  })
  
  # Race Plot with Granularity
  output$racePlot <- renderPlot({
    qDf <- qData()
    gran <- as.numeric(input$gran_race)
    
    plotDf <- aggregate_by_category(qDf, "CAT4", granularity = gran)
    create_granular_plot(plotDf, "Break_Out", granularity = gran, colors = color_palette)
  })
  
  # Education Plot with Granularity
  output$eduPlot <- renderPlot({
    qDf <- qData()
    gran <- as.numeric(input$gran_edu)
    
    plotDf <- aggregate_by_category(qDf, "CAT5", granularity = gran)
    create_granular_plot(plotDf, "Break_Out", granularity = gran, colors = color_palette)
  })
  
  # Income Plot with Granularity
  output$incomePlot <- renderPlot({
    qDf <- qData()
    gran <- as.numeric(input$gran_income)
    
    plotDf <- aggregate_by_category(qDf, "CAT6", granularity = gran)
    create_granular_plot(plotDf, "Break_Out", granularity = gran, colors = color_palette)
  })
  
  # Expand buttons - Modal dialogs for larger views
  observeEvent(input$expand_overall, {
    showModal(modalDialog(
      title = "Overall Distribution - Expanded View",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      plotOutput("overall_expanded", height = "600px")
    ))
  })
  
  output$overall_expanded <- renderPlot({
    qDf <- qData()
    gran <- as.numeric(input$gran_overall)
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID == "CAT1") |>
      select(Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Response) |>
      reframe(agg_persons = sum(persons)) |>
      mutate(agg_ss = sum(agg_persons)) |>
      mutate(agg_percent = agg_persons * 100 / agg_ss) |>
      mutate(agg_percent_sdev = sqrt(agg_percent * (100 - agg_percent) / agg_ss)) |>
      mutate(agg_low_ci_limit = agg_percent - 2 * agg_percent_sdev) |>
      mutate(agg_high_ci_limit = agg_percent + 2 * agg_percent_sdev)
    
    if (gran == 1) {
      plotDf |>
        ggplot(aes(x = "Overall", y = agg_percent, fill = Response)) +
        geom_col(position = "fill") + 
        scale_fill_manual(values = color_palette) +
        labs(x = NULL, y = "Percentage", fill = "Response") +
        theme_minimal(base_size = 16) +
        theme(legend.position = "right")
    } else {
      plotDf |>
        ggplot(aes(x = Response, y = agg_percent, fill = Response)) +
        geom_col() +
        geom_errorbar(aes(ymin = agg_low_ci_limit, ymax = agg_high_ci_limit),
                      width = 0.3) +
        scale_fill_manual(values = color_palette) +
        labs(y = "Percentage (%)", x = NULL) +
        theme_minimal(base_size = 16) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    }
  })
  
  # Similar expand handlers for other plots
  observeEvent(input$expand_temporal, {
    showModal(modalDialog(
      title = "Temporal Trends - Expanded View",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      plotOutput("temporal_expanded", height = "600px")
    ))
  })
  
  output$temporal_expanded <- renderPlot({
    qDf <- qData()
    plotDf <- qDf |>
      filter(BreakOutCategoryID == "CAT1") |>
      select(Year, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Year) |>
      reframe(Response = Response, persons = persons, agg_ss = sum(persons)) |>
      group_by(Year, Response) |>
      reframe(agg_persons = sum(persons), agg_ss = agg_ss,
              agg_percent = agg_persons * 100 / agg_ss) |>
      distinct()
    
    plotDf |>
      ggplot(aes(x = Year, y = agg_percent, fill = Response)) +
      geom_col(position = "fill") +
      scale_fill_manual(values = color_palette) +
      labs(x = "Year", y = "Percentage", fill = "Response") +
      theme_minimal(base_size = 16) +
      theme(legend.position = "right")
  })
  
  # Expand handlers for demographic plots
  observeEvent(input$expand_age, {
    showModal(modalDialog(
      title = "Age Distribution - Expanded View",
      size = "xl",
      easyClose = TRUE,
      footer = modalButton("Close"),
      plotOutput("age_expanded", height = "700px")
    ))
  })
  
  output$age_expanded <- renderPlot({
    qDf <- qData()
    plotDf <- aggregate_by_category(qDf, "CAT3", granularity = 3)
    create_granular_plot(plotDf, "Break_Out", granularity = 3, colors = color_palette)
  })
  
  observeEvent(input$expand_gender, {
    showModal(modalDialog(
      title = "Gender Distribution - Expanded View",
      size = "xl",
      easyClose = TRUE,
      footer = modalButton("Close"),
      plotOutput("gender_expanded", height = "700px")
    ))
  })
  
  output$gender_expanded <- renderPlot({
    qDf <- qData()
    plotDf <- aggregate_by_category(qDf, "CAT2", granularity = 3)
    create_granular_plot(plotDf, "Break_Out", granularity = 3, colors = color_palette)
  })
  
  observeEvent(input$expand_race, {
    showModal(modalDialog(
      title = "Race/Ethnicity Distribution - Expanded View",
      size = "xl",
      easyClose = TRUE,
      footer = modalButton("Close"),
      plotOutput("race_expanded", height = "700px")
    ))
  })
  
  output$race_expanded <- renderPlot({
    qDf <- qData()
    plotDf <- aggregate_by_category(qDf, "CAT4", granularity = 3)
    create_granular_plot(plotDf, "Break_Out", granularity = 3, colors = color_palette)
  })
  
  observeEvent(input$expand_edu, {
    showModal(modalDialog(
      title = "Education Level Distribution - Expanded View",
      size = "xl",
      easyClose = TRUE,
      footer = modalButton("Close"),
      plotOutput("edu_expanded", height = "700px")
    ))
  })
  
  output$edu_expanded <- renderPlot({
    qDf <- qData()
    plotDf <- aggregate_by_category(qDf, "CAT5", granularity = 3)
    create_granular_plot(plotDf, "Break_Out", granularity = 3, colors = color_palette)
  })
  
  observeEvent(input$expand_income, {
    showModal(modalDialog(
      title = "Income Level Distribution - Expanded View",
      size = "xl",
      easyClose = TRUE,
      footer = modalButton("Close"),
      plotOutput("income_expanded", height = "700px")
    ))
  })
  
  output$income_expanded <- renderPlot({
    qDf <- qData()
    plotDf <- aggregate_by_category(qDf, "CAT6", granularity = 3)
    create_granular_plot(plotDf, "Break_Out", granularity = 3, colors = color_palette)
  })
  
  observeEvent(input$expand_state, {
    showModal(modalDialog(
      title = "State/Territory Distribution - Expanded View",
      size = "xl",
      easyClose = TRUE,
      footer = modalButton("Close"),
      plotOutput("state_expanded", height = "800px")
    ))
  })
  
  output$state_expanded <- renderPlot({
    qDf <- qData()
    plotDf <- qDf |>
      filter(BreakOutCategoryID == "CAT1") |>
      select(Locationabbr, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Locationabbr) |>
      reframe(Response = Response, persons = persons, agg_ss = sum(persons)) |>
      group_by(Locationabbr, Response) |>
      reframe(agg_persons = sum(persons), agg_ss = agg_ss,
              agg_percent = agg_persons * 100 / agg_ss) |>
      distinct()
    
    plotDf |>
      ggplot(aes(x = reorder(Locationabbr, -agg_percent), 
                 y = agg_percent, fill = Response)) +
      geom_col(position = "fill") +
      scale_fill_manual(values = color_palette) +
      labs(x = "State/Territory", y = "Percentage", fill = "Response") +
      theme_minimal(base_size = 16) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "bottom")
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
