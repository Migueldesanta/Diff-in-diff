# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(DT)

# Load additional dependencies and setup functions
# source("global.R")

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "blue",
    ### Create the app header ----
    dashboardHeader(
      title = "Two-Period DID",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Diff_In_Diff")
      ),
      tags$li(
        class = "dropdown",
        tags$a(
          id = "home",
          href = 'https://shinyapps.science.psu.edu/',
          icon("house")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore 1", tabName = "explore1", icon = icon("wpexplorer")),
        menuItem("Explore 2", tabName = "explore2", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Two-Period Difference in Difference Regression(Two-Period DID)"),
          p("This app is designed to help students explore and understand the core concepts, 
            assumptions, and interpretations of Two-Period DID regression 
            by experimenting with real data and simulation."),
          h2("Instructions"),
          p("Explore the app based on the following instructions:"),
          tags$ol(
            tags$li("Review any prerequiste ideas using the Prerequistes tab."),
            tags$li("Explore the assumptions by the Exploration 1 Tab."),
            tags$li("Explore the interpretations by the Exploration 2 Tab.")
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Xin(Michael) Yun(2024).",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 09/08/2022 by XY.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p('What is Two-Period DID Regression? Two-Period DID (',
            a(href = 'https://www.publichealth.columbia.edu/research/population-health-methods/difference-difference-estimation', 
              'Two-Period Difference-in-Difference', class = 'bodylinks'), ') 
             a statistical method used to estimate causal effects by comparing changes 
            in outcomes between a treatment group and a control group over two time periods: 
            before and after the intervention. It accounts for time-invariant differences 
            between the groups and isolates the impact of the intervention by assuming that, 
            in the absence of treatment, both groups would follow parallel trends over time. 
            This method is particularly useful when randomization is not feasible..'),
          
          h3('Regression Model'),
          p('DID is usually implemented as an interaction term between time and 
            treatment group dummy variables in a regression model:'),
          withMathJax(
            p('$$Y_{it} = \\beta_0 + \\beta_1 \\cdot Post_t + \\beta_2 \\cdot Treat_i + 
              \\beta_3 \\cdot (Post_t \\times Treat_i) + \\epsilon_{it}$$')
          ),
          withMathJax(
            p('Where: 
    $$\\begin{aligned}
    &\\bullet \\ Y_{it} \\text{ is the outcome for individual } i \\text{ at time } t, \\\\
    &\\bullet \\ Post_t \\text{ is a time dummy (1 if post-treatment, 0 if pre-treatment)}, \\\\
    &\\bullet \\ Treat_i \\text{ is a group dummy (1 if in the treatment group, 0 otherwise)}, \\\\
    &\\bullet \\ Post_t \\times Treat_i \\text{ is the interaction term measuring the treatment effect}, \\\\
    &\\bullet \\ \\epsilon_{it} \\text{ is the error term}.
    \\end{aligned}$$')
          ),
          br(),
          
          box(
            title = strong(
               
              a(href = 'https://www.jstor.org/stable/43551404', 
                'Correlation vs. Causation', class = 'bodylinks')
            ),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            
            tags$style(HTML("
    table {
      border-collapse: collapse;
      width: 100%;
    }
    th, td {
      border: 1px solid black;
      padding: 8px;
      text-align: left;
    }
  ")),
            
            tags$table(
              tags$thead(
                tags$tr(
                  tags$th(scope = "col", "Aspect"),
                  tags$th(scope = "col", "Correlation"),
                  tags$th(scope = "col", "Causation")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td("Definition"),
                  tags$td("Statistical association between variables."),
                  tags$td("One variable causes changes in another.")
                ),
                tags$tr(
                  tags$td("Relationship"),
                  tags$td("Variables covary but without a direct link."),
                  tags$td("Direct cause-and-effect relationship between variables.")
                ),
                tags$tr(
                  tags$td("Third Variable Problem"),
                  tags$td("Can be influenced by confounding variables."),
                  tags$td("Accounts for third variables in controlled experiments.")
                ),
                tags$tr(
                  tags$td("Example"),
                  tags$td("Ice cream sales and crime rates are correlated, but unrelated."),
                  tags$td("A new policy reduces unemployment rates.")
                )
              )
            )
          ),
          
          box(
            title = strong("Assumptions"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            
            tags$style(HTML("
    table {
      border-collapse: collapse;
      width: 100%;
    }
    th, td {
      border: 1px solid black;
      padding: 8px;
      text-align: left;
    }
  ")),
            
            tags$table(
              tags$thead(
                tags$tr(
                  tags$th(scope = "col", "Assumption"),
                  tags$th(scope = "col", "Description")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td("Exchangeability"),
                  tags$td("No systematic differences between treatment and control groups that would affect outcomes.")
                ),
                tags$tr(
                  tags$td("Positivity"),
                  tags$td("All units have a non-zero probability of receiving the treatment.")
                ),
                tags$tr(
                  tags$td("SUTVA"),
                  tags$td("No spillover effects; treatment of one unit doesn’t affect another.")
                ),
                tags$tr(
                  tags$td("Parallel Trends"),
                  tags$td("In the absence of treatment, outcome trends for both groups are the same over time.")
                ),
                tags$tr(
                  tags$td("Stable Composition"),
                  tags$td("Group composition must remain stable in repeated cross-sectional designs.")
                )
              )
            )
          )
        ),
        #### Note: you must have at least one of the following pages. You might
        #### have more than one type and/or more than one of the same type. This
        #### will be up to you and the goals for your app.
        #### Set up an Explore 1 Page ----
        tabItem(
          tabName = "explore1",
          withMathJax(),
          
          h2("Explore Assumptions"),
          p("This page allows you to explore key assumptions of a Two-Period Difference-in-Difference (DID) model, 
     specifically focusing on the Parallel Trends assumption. You can use the sliders to adjust the control 
     and treatment group trend slopes before treatment, as well as the post-treatment effect. The graphs 
     will automatically update to show how these changes influence the model's results, helping you understand 
     whether the Parallel Trends assumption holds or is violated in different scenarios."),
          br(),
          
          # Main content for exploring assumptions
          fluidPage(
            tabsetPanel(
              id = "whichAssumption",
              type = "tabs",
              
              ##### Parallel Trends Assumption ----
              tabPanel(
                title = "Parallel Trends",
                br(),
                
                # Input column (left side) ----
                column(
                  width = 4,
                  wellPanel(
                    tags$strong("Parallel Trends Assumption"),
                    
                    # Sliders for trend adjustment
                    sliderInput(
                      inputId = "trend_control",
                      label = "Control Group Trend Slope (Pre-Treatment):",
                      min = 0.5,
                      max = 3,
                      value = 1.5,
                      step = 0.1
                    ),
                    sliderInput(
                      inputId = "trend_treatment",
                      label = "Treatment Group Trend Slope (Pre-Treatment):",
                      min = 0.5,
                      max = 3,
                      value = 1.5,
                      step = 0.1
                    ),
                    sliderInput(
                      inputId = "treatment_effect",
                      label = "Treatment Effect (Post-Treatment):",
                      min = 0,
                      max = 5,
                      value = 2,
                      step = 0.5
                    )
                  )
                ),
                
                # Output column (right side) ----
                column(
                  width = 8,
                  plotOutput("didPlot", height = "400px"),
                  tags$b(dataTableOutput('analysis1')),
                  br(),
                  uiOutput('assumptionCheck')
                )
              ),
              ##### Exchangeability Assumption ----
              tabPanel(
                title = "Exchangeability",
                br(),
                
                # Input column (left side) ----
                column(
                  width = 4,
                  wellPanel(
                    tags$strong("Exchangeability Assumption"),
                    p("This assumption requires that treatment and control groups follow the same trend before treatment. 
         You can adjust the initial outcome differences between the groups and add a confounder to see 
         how this affects the model."),
                    
                    # Slider for initial outcome differences (baseline difference)
                    sliderInput(
                      inputId = "initial_diff",
                      label = "Initial Outcome Difference Between Groups (Pre-Treatment):",
                      min = -5,
                      max = 5,
                      value = 0,
                      step = 0.5
                    ),
                    
                    # Slider for confounder effect (simulating trend difference)
                    sliderInput(
                      inputId = "confounder",
                      label = "Impact of Confounding Variable on Treatment Group:",
                      min = 0,
                      max = 5,
                      value = 0,
                      step = 0.5
                    )
                  )
                ),
                
                # Output column (right side) ----
                column(
                  width = 8,
                  plotOutput("plotExchangeability", height = "400px"),
                  br(),
                  tags$b(dataTableOutput('analysis2')),
                  br(),
                  uiOutput("exchangeabilityCheck")
                )
              )
            )
          )
        ),

        #### Set up an Explore 2 Page ----
        tabItem(
          tabName = "explore2",
          withMathJax(),
          h2("Challenge Yourself"),
          p("The general intent of a Challenge page is to have the user take
            what they learned in an Exploration and apply that knowledge in new
            contexts/situations. In essence, to have them challenge their
            understanding by testing themselves."),
          p("What this page looks like will be up to you. Something you might
            consider is to re-create the tools of the Exploration page and then
            a list of questions for the user to then answer.")
        ),
        #### Set up a Game Page ----
        tabItem(
          tabName = "game",
          withMathJax(),
          h2("Practice/Test Yourself with [Type of Game]"),
          p("On this type of page, you'll set up a game for the user to play.
            Game types include Tic-Tac-Toe, Matching, and a version Hangman to
            name a few. If you have ideas for new game type, please let us know.")
        ),
        #### Set up a Wizard Page ----
        tabItem(
          tabName = "wizard",
          withMathJax(),
          h2("Wizard"),
          p("This page will have a series of inputs and questions for the user to
            answer/work through in order to have the app create something. These
            types of Activity pages are currently rare as we try to avoid
            creating 'calculators' in the BOAST project.")
        ),
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Barrowman, N. (2014). Correlation, Causation, and Confusion. 
            The New Atlantis, 43, 23–44. 
            http://www.jstor.org/stable/43551404"
          ),
          p(
            class = "hangingindent",
            "Columbia University Mailman School of Public Health. (n.d.). 
            Difference-in-difference estimation. Columbia University. 
            https://www.publichealth.columbia.edu/research/population-health-methods/
            difference-difference-estimation"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Info button logic
  observeEvent(input$info, {
    sendSweetAlert(
      session = session,  
      type = "info",
      title = "Information",
      text = "This App helps you explore different assumptions using the DID model."
    )
  })
  
  # Generate data for Parallel Trends Assumption
  generate_data <- reactive({
    years <- 1959:1969  # Simulated years
    intervention_year <- 1964  # The year of intervention
    
    control_pre <- 8  # Baseline for control group
    treatment_pre <- 7  # Baseline for treatment group
    
    control_slope <- input$trend_control  # Control group trend slope
    treatment_slope <- input$trend_treatment  # Treatment group trend slope
    treatment_effect <- input$treatment_effect  # Treatment effect applied after intervention year
    
    # Post-intervention treatment slope
    if (treatment_effect == 0) {
      treatment_slope_post <- control_slope  # If treatment effect is 0, post-intervention slope is the same as control group
    } else {
      treatment_slope_post <- treatment_slope + treatment_effect  # If treatment effect is non-zero, slope changes
    }
    
    # Create data for control group (before and after intervention)
    control_values <- control_pre + control_slope * (years - min(years))
    
    # Create data for treatment group (before and after intervention)
    treatment_values_pre <- treatment_pre + treatment_slope * (years[years <= intervention_year] - min(years))
    treatment_values_post <- treatment_pre + treatment_slope * (intervention_year - min(years)) + 
      treatment_slope_post * (years[years > intervention_year] - intervention_year)
    
    # Combine pre and post treatment values
    treatment_values <- c(treatment_values_pre, treatment_values_post)
    
    # No Treatment Effect trend (i.e., treatment group with no effect applied)
    no_treatment_effect_values <- treatment_pre + control_slope * (years - min(years))  # No treatment effect trend (parallel to control group)
    
    # Combine all data into a data frame
    data.frame(
      year = rep(years, 3),
      outcome = c(control_values, treatment_values, no_treatment_effect_values),
      group = factor(rep(c("Control Group", "Treatment Group", "No Treatment Effect"), each = length(years))),
      type = factor(rep(c("Control", "Treatment", "No Treatment Effect"), each = length(years)))
    )
  })
  
  # Render the DID plot ----
  output$didPlot <- renderPlot({
    data <- generate_data()
    intervention_year <- 1964  # The year of intervention
    
    # Plot using ggplot2
    ggplot(data, aes(x = year, y = outcome, color = group, linetype = type)) +
      geom_line(size = 1.2) +
      geom_vline(xintercept = intervention_year, color = "red", size = 1) +
      labs(title = "Explore Parallel Trends Assumption with Treatment Effect", x = "Year", y = "Outcome") +
      theme_minimal() +
      
      # Customize x-axis labels to show "Pre intervention" and "Post intervention"
      scale_x_continuous(breaks = c(1959, intervention_year, 1969), labels = c("Pre intervention", "Intervention", "Post intervention")) +
      
      # Customize line types for control, treatment, and ideal trends
      scale_linetype_manual(values = c("Control" = "solid", "Treatment" = "solid", "No Treatment Effect" = "dashed")) +
      
      # Only include the three legend items (Control, Treatment, No Treatment Effect)
      scale_color_manual(values = c("Control Group" = "blue", 
                                    "Treatment Group" = "brown", 
                                    "No Treatment Effect" = "grey")) +
      
      # Customize the legend to appear at the bottom without a title
      theme(legend.position = "bottom", legend.title = element_blank())
  })
  
  # Assumption Check ----
  output$assumptionCheck <- renderPrint({
    if (input$trend_control == input$trend_treatment) {
      cat("Assumption Satisfied: The control and treatment 
          groups have parallel trends before the intervention, 
          meaning the assumption holds.")
    } else {
      cat("Assumption Violated: The control and treatment groups 
          do not follow parallel trends before the intervention, 
          meaning the assumption is violated.")
    }
  })
  # --- Exchangeability Assumption Logic (New) ---
  
  # Generate data for Exchangeability Assumption
  generate_exchangeability_data <- reactive({
    years <- 1959:1969
    intervention_year <- 1964
    control_pre <- 8  # Baseline for control group
    
    # Apply user's input for initial difference and confounding effect
    treatment_pre <- control_pre + input$initial_diff
    confounder_effect <- input$confounder
    
    # Control group has constant slope
    control_slope <- 1
    
    # Treatment group has an adjusted slope due to the confounder
    treatment_slope <- 1 + confounder_effect
    
    # Create data for control group
    control_values <- control_pre + control_slope * (years - min(years))
    
    # Create data for treatment group with bias from confounder
    treatment_values <- treatment_pre + treatment_slope * (years - min(years))
    
    # Combine all data into a data frame
    data.frame(
      year = rep(years, 2),
      outcome = c(control_values, treatment_values),
      group = factor(rep(c("Control", "Treatment"), each = length(years)))
    )
  })
  
  # Plot for Exchangeability Assumption (Line Plot)
  output$plotExchangeability <- renderPlot({
    data <- generate_exchangeability_data()
    
    ggplot(data, aes(x = year, y = outcome, color = group)) +
      geom_line(size = 1.2) +  # Using line plot to show trends
      labs(title = "Exchangeability Assumption (Line Plot)", x = "Year", y = "Outcome") +
      theme_minimal() +
      scale_color_manual(values = c("Control" = "blue", "Treatment" = "red")) +
      theme(legend.position = "bottom", legend.title = element_blank())
  })
  
  # Analysis for Exchangeability Assumption
  output$analysis2 <- renderDataTable({
    data <- generate_exchangeability_data()
    
    # Display the generated data for analysis
    datatable(data, options = list(pageLength = 5))
  })
  
  # Assumption Check for Exchangeability
  output$exchangeabilityCheck <- renderPrint({
    if (input$confounder == 0) {
      cat("Exchangeability Assumption holds: No systematic differences between treatment and control groups.")
    } else {
      cat("Exchangeability Assumption violated: Systematic differences between treatment and control groups exist.")
    }
  })
  
  
}

# Run the application using boastApp ----
boastUtils::boastApp(ui = ui, server = server)
