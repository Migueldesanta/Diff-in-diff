# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)

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
          tags$figure(
            align = "center",
            tags$img(
              src = "model.jpg",
              width = 500,
              alt = "Diff-in-Diff Model"
            ),
            tags$figcaption("Diff-in-Diff Model (Columbia University)")
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
          withMathJax(),  # Enable LaTeX rendering for mathematical equations
          h2("Explore the Parallel Trends Assumption"),
          p("This page allows you to explore the Parallel Trends Assumption by adjusting the pre-intervention trends 
      for both the control and treatment groups, and also specifying the treatment effect."),
          
          sidebarLayout(
            sidebarPanel(
              sliderInput("trend_control", 
                          "Control Group Trend Slope (Pre-Intervention):", 
                          min = 0.5, max = 3, value = 1.5, step = 0.1),  # Control group trend
              sliderInput("trend_treatment", 
                          "Treatment Group Trend Slope (Pre-Intervention):", 
                          min = 0.5, max = 3, value = 1.5, step = 0.1),  # Treatment group trend
              sliderInput("treatment_effect", 
                          "Treatment Effect (Post-Intervention):", 
                          min = 0, max = 5, value = 2, step = 0.5)  # Treatment effect post-intervention
            ),
            
            mainPanel(
              plotOutput("didPlot"),  # Display the DID plot
              
              h4("Parameters Summary:"),
              uiOutput("numericalTable"),  # Display the numerical summary for β0, β1, β2, β3
              
              h4("DID Model"),
              withMathJax(uiOutput("finalModel")),  # Render the DID model in LaTeX format
              
              h4("Assumption Check:"),
              uiOutput("assumptionCheck")  # Display the assumption check results
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

# Define server logic ----
server <- function(input, output, session) {
  
  # Set up Info button ----
  observeEvent(input$info, {
    sendSweetAlert(
      session = session,  # Make sure session is passed here
      type = "info",
      title = "Information",
      text = "This App Template will help you get started building your own app"
    )
  })
  
  # Generate data based on user inputs ----
  generate_data <- reactive({
    years <- 1959:1969  # Simulated years
    intervention_year <- 1964  # The year of intervention
    
    # Pre-intervention values
    control_pre <- 8
    treatment_pre <- 7
    
    # Slopes
    control_slope <- input$trend_control
    treatment_slope <- input$trend_treatment
    treatment_effect <- input$treatment_effect
    
    # Create data for control and treatment groups (before and after intervention)
    control_values <- control_pre + control_slope * (years - min(years))
    treatment_values_pre <- treatment_pre + treatment_slope * (years[years <= intervention_year] - min(years))
    treatment_values_post <- treatment_pre + treatment_slope * (intervention_year - min(years)) + 
      treatment_effect * (years[years > intervention_year] - intervention_year)
    treatment_values <- c(treatment_values_pre, treatment_values_post)
    
    # Ideal parallel trend for treatment group (no treatment effect)
    ideal_treatment <- treatment_pre + treatment_slope * (years - min(years))
    
    data.frame(
      year = rep(years, 3),
      outcome = c(control_values, treatment_values, ideal_treatment),
      group = rep(c("Control Group", "Treatment Group", "Ideal Treatment (No Effect)"), each = length(years)),
      type = rep(c("Control", "Treatment", "Ideal"), each = length(years))
    )
  })
  
  # Render the DID plot ----
  output$didPlot <- renderPlot({
    data <- generate_data()
    intervention_year <- 1964  # The year of intervention
    
    # Plot using ggplot2
    ggplot(data, aes(x = year, y = outcome, color = group)) +
      geom_line(size = 1.2) +
      geom_vline(xintercept = intervention_year, color = "red", size = 1) +
      geom_point(size = 2) +
      labs(title = "Difference-in-Difference (DID) Visualization", 
           x = "Year", 
           y = "Outcome") +
      theme_minimal() +
      
      # Customize x-axis labels to show "Pre intervention" and "Post intervention"
      scale_x_continuous(breaks = c(1959, intervention_year, 1969), 
                         labels = c("Pre intervention", "Intervention", "Post intervention")) +
      
      # Customize line types for control, treatment, and ideal trends
      scale_linetype_manual(values = c("Control" = "solid", "Treatment" = "solid", "Ideal" = "dashed")) +
      
      # Only include the three legend items (Control, Treatment, Ideal Effect)
      scale_color_manual(values = c("Control Group" = "blue", 
                                    "Treatment Group" = "brown", 
                                    "Ideal Treatment (No Effect)" = "grey")) +
      
      # Customize the legend to appear at the bottom without a title
      theme(legend.position = "bottom", legend.title = element_blank())
  })
  
  # Numerical summary of parameters ----
  output$numericalTable <- renderUI({
    beta0 <- 8  # Control group pre-intervention mean
    beta1 <- input$trend_control  # Control group slope (Post effect)
    beta2 <- input$treatment_effect  # Treatment effect
    beta3 <- input$treatment_effect - input$trend_control  # Interaction term
    
    tags$table(
      tags$thead(
        tags$tr(
          tags$th("Parameter"),
          tags$th("Value"),
          tags$th("Explanation")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("β0 (Control Group Pre)"),
          tags$td(beta0),
          tags$td("Initial mean of the control group (pre-intervention)")
        ),
        tags$tr(
          tags$td("β1 (Control Group Slope)"),
          tags$td(beta1),
          tags$td("Change in the control group after intervention")
        ),
        tags$tr(
          tags$td("β2 (Treatment Effect)"),
          tags$td(beta2),
          tags$td("Effect of the treatment on the treatment group")
        ),
        tags$tr(
          tags$td("β3 (Interaction Term)"),
          tags$td(beta3),
          tags$td("Difference in the change between control and treatment groups")
        )
      )
    )
  })
  
  # Generate the DID model  ----
  output$finalModel <- renderUI({
    beta0 <- 8  # Control group pre-intervention mean
    beta1 <- input$trend_control  # Control group slope (Post effect)
    beta2 <- input$treatment_effect  # Treatment effect
    beta3 <- input$treatment_effect - input$trend_control  # Interaction term
    
    withMathJax(
      sprintf(
        "$$ Y_{it} = %s + %s \\cdot Post_t + %s \\cdot Treat_i + %s \\cdot (Post_t \\times Treat_i) + \\epsilon_{it} $$",
        beta0, beta1, beta2, beta3
      )
    )
  })
  
  # Check Parallel Trends Assumption ----
  output$assumptionCheck <- renderPrint({
    control_slope <- input$trend_control
    treatment_slope <- input$trend_treatment
    
    # Check Parallel Trends Assumption
    if (control_slope == treatment_slope) {
      cat("Parallel Trends Assumption: Satisfied - The control and treatment groups had parallel trends before the intervention.\n")
    } else {
      cat("Parallel Trends Assumption: Violated - The control and treatment groups did not have parallel trends before the intervention.\n")
    }
  })
}

  

# Run the app using boastUtils or shiny
boastUtils::boastApp(ui = ui, server = server)