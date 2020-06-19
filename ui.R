library(boastUtils)
library(shiny)
library(shinydashboard)
library(shinyBS)

# App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Population Growth Models"
APP_DESCP  <<- paste(
  "This app explores how growth rate, carrying capacity, density-independent",
  "limiting factors, and density-dependent limiting factors, affects the growth",
  "and variability of a hypothetical population of rabbits."
)
# End App Meta Data------------------------------------------------------------

# Begin UI Definition ----------------------------------------------------------
dashboardPage(
  skin = "green",
  # Header ---------------------------------------------------------------------
  dashboardHeader(
    title = "Pop. Growth Model",
    titleWidth = "250",
    tags$li(class = "dropdown",
            actionLink("info", icon("info"))),
    tags$li(class = "dropdown",
            tags$a(href = "https://shinyapps.science.psu.edu/",
                   icon("home")))
  ),
  # Sidebar --------------------------------------------------------------------
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id="tabs",
      menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
      menuItem("Prerequisites", tabName="preq", icon=icon("book")),
      menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
      menuItem("References", tabName = "References", icon = icon("leanpub"))
    ),
    tags$div(
      class = "sidebar-logo",
      boastUtils::psu_eberly_logo("reversed")
    )
  ),
  # Body -----------------------------------------------------------------------
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css",
                href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
    ),
    tabItems(
      tabItem(
        # Overview -------------------------------------------------------------
        tabName = "overview",
        withMathJax(),
        h1("Population Growth Models"),
        p("Explore how growth rate, carrying capacity, variation of the
          population, density-independent limiting factors, and
          density-dependent limiting factors, affects a simulated population of
          rabbits."),
        h2("Instructions"),
        tags$ol(
          tags$li("Review any information in the Prerequisistes as needed."),
          tags$li("When ready go to the Exploration Tab."),
          tags$li("Set the initial values for the rabbit population."),
          tags$li("Pick a model type to explore."),
          tags$li("Add/remove factors and move the sliders around to explore how
                  the different factors impact the population growth over time."),
          tags$li("See how each slider (population parameters and factors)
                  influence the population model."),
          tags$li("Notice what happens when the factors go to the extremes.")
        ),
        div(
          style = "text-align: center",
          bsButton(
            inputId = "start",
            label = "GO!",
            size = "large",
            icon = icon("bolt"),
            style = "default"
          )
        ),
        br(),
        br(),
        h2("Acknowledgements"),
        p("This app was originally developed and coded by Yutong Wu. We would
          to thank Dr. Stephen Schaeffer from the Huck Institutes of Life
          Sciences for his advice and assistance for the biological content. The
          currentversion of the app was modified by Neil J. Hatfield.",
          br(),
          br(),
          br(),
          div(class = "updated", "Last Update: 6/19/2020 by NJH.")
        )
      ),
      tabItem(
        # Prerequisites --------------------------------------------------------
        tabName = 'preq',
        h2("Prerequisites"),
        tags$ul(
          tags$li("Population growth depends on the rate of births and how that
                  compares to the death rate (the difference is called the
                  growth rate)."),
          tags$li("The exponential growth of a population results when the
                  growth rate is positive; the population faces exponential decay
                  when the growth rate is negative."),
          tags$li("When there is a finite carrying capacity (i.e., an upper bound
                  on how many members the environment can support), the
                  exponential growth changes to logistic growth. This appears as
                  an S-shaped curve."),
          tags$li('In this app, we see how random fluctuations or "stochasticity"
                  affects population growth in both the finite and "infinite"
                  capacity situations.'),
          tags$li("Deterministic models do not account for random fluctuations,
                  so whatever parameters you set for population growth and
                  carrying capacity will result in a pre-determined result."),
          tags$li("Reality is not deterministic in nature. Individuals in a
                  population will vary in how many offspring they have, how long
                  they will live under natural conditions, and whether they will
                  be affected by a disease. Further, the carrying capacity will
                  vary from time to time due to random fluctuations in the
                  weather, in the development of food sources, and the like."),
          tags$li("Population growth in the face of stochasticity can be quite
                  different from the predictions that would be made by a
                  deterministic model. This is especially true with small
                  populations since a few extra deaths or a few less births
                  happening randomly might wipe out a population altogether.")
        ),
        br(),
        br(),
        div(
          style = "text-align: center",
          bsButton(
            inputId = "go",
            label = "GO!",
            size = "large",
            icon = icon("bolt"),
            style = "default"
          )
        )
      ),
      tabItem(
        # Exploration Tab ------------------------------------------------------
        tabName = "explore",
        h2("Modeling Rabbit Population Growth"),
        br(),
        h3("Step 1: Set Initial Values for the Population"),
        fluidRow(
          tags$form(
            class = "form-inline",
            column(
              width = 4,
              offset = 1,
              sliderInput(
                inputId = "initPop",
                label = "Starting population size",
                min = 0,
                max = 100,
                value = 50,
                step = 5
              )
            ),
            column(
              width = 4,
              offset = 1,
              sliderInput(
                inputId = "birthRate",
                label = "Birth rate",
                min = 0,
                max = 1,
                value = 0.5,
                step = 0.002
              )
            )
          )
        ),
        fluidRow(
          tags$form(
            class = "form-inline",
            column(
              width = 4,
              offset = 1,
              sliderInput(
                inputId = "obsPeriod",
                label = "Observation period",
                min = 24,
                max = 60,
                step = 1,
                value = 24,
                post = " months"
              )
            ),
            column(
              width = 4,
              offset = 1,
              sliderInput(
                inputId = "deathRate",
                label = "Death rate",
                min = 0,
                max = 1,
                value = 0.3,
                step = 0.002
            )
          )
        )
      ),
      br(),
      htmlOutput("initSummary"),
      br(),
      h3("Step 2: Pick a Model to Explore"),
      ## Inset Tabs ------------------------------------------------------------
      tabsetPanel(
        id = "models",
        type = "tabs",
        # Deterministic Models Tab ----------------------------------------------
        tabPanel(
          title = "Deterministic Models",
          br(),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h3("Step 3: Add Factors"),
                checkboxInput(
                  inputId = "addK1",
                  label = "Carrying capacity",
                  value = FALSE
                ),
                uiOutput("kControl")
              )
            ),
            column(
              width = 8,
              plotOutput("deterPlot"),
              tags$script(HTML(
                "#(document).ready(function() {
                document.getElementById('deterPlot').setAttribute('aria-label',
                'The plot shows the exponential growth of the population when
                the birth rate is larger than the death rate. When there is a
                carrying capacity, the plot will show a S-curve that increases
                towards the capacity.')
                })"
              )),
              bsPopover(
                id = "deterPlot",
                title =  "Rabbits Population Plot",
                content = "This plot shows the estimated rabbit population",
                placement = "top"
              )
            )
          )
        ),
        # Infinite Capacity Stochastic Model Tab -------------------------------
        tabPanel(
          title = "Infinite Capacity Model",
          br(),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h3("Step 3: Add Factors"),
                p("Density-dependent limiting factors"),
                checkboxInput(
                  inputId = "addDisease1",
                  label = "Add disease",
                  value = FALSE
                ),
                uiOutput("diseaseSlider1"),
                hr(),
                checkboxInput(
                  inputId = "expMod",
                  label = "Show estimated deterministic model",
                  value = FALSE)
              )
            ),
            column(
              width = 8,
              plotOutput("infCapPlot"),
              tags$script(HTML(
                "#(document).ready(function() {
                document.getElementById('infCapPlot').setAttribute('aria-label',
                'This plot will show the exponential growth of the rabbit
                population when the birth rate is larger than the death rate.
                When a disease is added, the death rate increases. There is
                randomness (stochasticity) in the graph. A deterministic
                exponetial plot may be added to highlight that reality is
                typically stochastic.')
                })"
              )),
              bsPopover(
                id = "infCapPlot",
                title =  "Rabbits Population Plot",
                content = "This plot shows the estimated rabbit population",
                placement = "top"
              )
            )
          )
        ),
        # Finite Capacity Stochastic Model Tab ---------------------------------
        tabPanel(
          title = "Finite Capacity Model",
          br(),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h3("Step 3: Add Factors"),
                sliderInput(
                  inputId = "kSlider2",
                  label = "Carrying capacity",
                  min = 500,
                  max = 10000,
                  step = 10,
                  value = 5000
                ),
                sliderInput(
                  inputId = "cv",
                  label = "Carrying capacity variation",
                  min = 0,
                  max = 30,
                  step = 5,
                  value = 0,
                  post = "%"
                ),
                tags$hr(),
                p("Density-independent limiting factors"),
                checkboxInput(
                  inputId = "addResource",
                  label = "Add environmental factor",
                  value = FALSE
                ),
                uiOutput("resourceSlider"),
                tags$hr(),
                p("Density-dependent limiting factors"),
                checkboxInput(
                  inputId = "addCompetition",
                  label = "Add competition",
                  value = FALSE
                ),
                uiOutput("competitionSlider"),
                br(),
                checkboxInput(
                  inputId = "addDisease2",
                  label = "Add disease",
                  value = FALSE
                ),
                uiOutput("diseaseSlider2")
              )
            ),
            column(
              width = 8,
              plotOutput("finCapPlot"),
              tags$script(HTML(
                "#(document).ready(function() {
                document.getElementById('finCapPlot').setAttribute('aria-label',
                'In this plot there is a dashed line for the theoretical
                carrying capacity but also a line showing an actual carrying
                capacity that has a yearly seasonal trend and randomness. The
                plots for the rabbits and hares (the competition) are both
                logistic (s-curves), increasing towards the capacity, when the
                birth rate is greater than the death rate (i.e., growth rate is
                positive). A deterministic model may be added to highlight the
                presence of randomness in real life.')
                })"
              )),
              bsPopover(id = "finCapPlot",
                        title =  "Rabbits Population Plot",
                        content = "This plot shows the estimated rabbit population",
                        placement = "top"
              )
            )
          )
        )
      )
    ),
    tabItem(
      tabName = "References",
      withMathJax(),
      h2("References"),
      p(
        class = "hangingindent",
        "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny,
            R package. Available from https://CRAN.R-project.org/package=shinyBS"
      ),
      p(
        class = "hangingindent",
        "Carey, R. (2019), boastUtils: BOAST Utilities, R Package.
            Available from https://github.com/EducationShinyAppTeam/boastUtils"
      ),
      p(
        class = "hangingindent",
        "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
            dashboards with 'Shiny', R Package. Available from
            https://CRAN.R-project.org/package=shinydashboard"
      ),
      p(
        class = "hangingindent",
        "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019), shiny: Web application framework for R, R Package,
            Available from https://CRAN.R-project.org/package=shiny"
      ),
      p(
        class = "hangingindent",
        "Perrier, V., Meyer, F., and Granjon, D. (2020), shinyWidgets: Custom
        inputs widgets for shiny, R Package, Available from
        https://CRAN.R-project.org/package=shinyWidgets"
      ),
      p(
        class = "hangingindent",
        "Wickham, H. (2016), ggplot2: Elegant graphics for data analysis,
            R Package, Springer-Verlag New York. Available from
            https://ggplot2.tidyverse.org"
      ),
      p(
        class = "hangingindent",
        "Wickham, H., François, R., Henry, L., and Müller, K. (2020),
        dplyr: A grammar of data manipulation, R Package, Available from
        https://CRAN.R-project.org/package=dplyr"
      )
    )
  )
)
)