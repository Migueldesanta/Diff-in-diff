# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)

# Load additional dependencies and setup functions
# source("global.R")

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "blue",
    ### Create the app header ----
    dashboardHeader(
      title = "Diff In Diff", # You may use a shortened form of the title here
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
          h1("Difference in Difference Regression(Diff-in-Diff)"),
          p("This app is designed to help students explore and understand the core concepts, 
            assumptions, and interpretations of DID regression 
            by experimenting with real data and simulation."),
          h2("Instructions"),
          p("This information will change depending on what you want to do."),
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
            "This version of the app was developed and coded by Xin(Michael) Yun(2024)",
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
          p('What is Diff-in-Diff Regression? Diff-in-Diff (',
            a(href = 'https://www.publichealth.columbia.edu/research/
              population-health-methods/difference-difference-estimation', 
              'Difference-in-Difference', class = 'bodylinks'), ') 
            Regression is  a statistical method used to estimate causal effects 
            by comparing changes in outcomes over time between a treatment group 
            and a control group, accounting for biases from time-invariant differences between the groups.'),
          
          tags$ul(
            tags$li("Pre-req 1--Technical/Conceptual Prerequisites are ideas that
                    users need to have in order to engage with your app fully."),
            tags$li("Pre-req 2--Contextual Prerequisites refer to any information
                    about a context in your app that will enrich a user's
                    understandings."),
            tags$li("Pre-req 3"),
            tags$li("Pre-req 4")
          ),
          p("Notice the use of an unordered list; users can move through the
            list any way they wish."),
          tags$tbody(
  tags$tr(
    tags$td(tags$strong("Aspect")),
    tags$td(tags$strong("Definition")),
    tags$td("Statistical association between variables."),
    tags$td("One variable causes changes in another.")
  ),
  tags$tr(
    tags$td(tags$strong("Relationship")),
    tags$td("Variables covary but without a direct link."),
    tags$td("Direct cause-and-effect relationship between variables.")
  ),
  tags$tr(
    tags$td(tags$strong("Third Variable Problem")),
    tags$td("Can be influenced by confounding variables."),
    tags$td("Accounts for third variables in controlled experiments.")
  ),
  tags$tr(
    tags$td(tags$strong("Example")),
    tags$td("Ice cream sales and crime rates are correlated, but unrelated."),
    tags$td("A new policy reduces unemployment rates.")
  )
)
            )
          ),
          
          box(
            title = strong(tags$em("p"), "-values"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            "The probability that our selected estimator takes on a value at
            least as extreme as what we observed given our null hypothesis. If
            we were to carry out our study infinitely many times and the null
            hypothesis accurately modeled what we're studying, then we would
            expect for our estimator to produce a value at least as extreme as
            what we have seen 100*(p-value)% of the time. The larger the
            p-value, the more often we would expect our estimator to take on a
            value at least as extreme as what we've seen; the smaller, the less
            often."
          )
        ),
        #### Note: you must have at least one of the following pages. You might
        #### have more than one type and/or more than one of the same type. This
        #### will be up to you and the goals for your app.
        #### Set up an Explore 1 Page ----
        tabItem(
          tabName = "explore1",
          withMathJax(),
          h2("Explore the Concept"),
          p("This page should include something for the user to do, the more
            active and engaging, the better. The purpose of this page is to help
            the user build a productive understanding of the concept your app
            is dedicated to."),
          p("Common elements include graphs, sliders, buttons, etc."),
          p("The following comes from the NHST Caveats App:"),
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
          p("You'll need to fill in this page with all of the appropriate
            references for your app."),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
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

  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This App Template will help you get started building your own app"
      )
    }
  )


}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
