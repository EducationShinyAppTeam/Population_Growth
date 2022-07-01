# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(dplyr)

# Helper Functions ----
calcPop <- function(cap, init, bRate, dRate1, dRate2, time, date) {
  if (is.null(date)) {
    rate = bRate - dRate1
  } else {
    rate = ifelse(time <= date, bRate - dRate1, bRate - dRate2)
  }
  floor((cap * init) / (init + (cap - init) * exp(-1 * rate * time)))
}

detPop <- function(init, bRate, dRate1, dRate2, date, time) {
  if(is.null(date)) {
    rate = bRate - dRate1
  } else {
    rate = ifelse(time <= date, bRate - dRate1, bRate - dRate2)
  }
  floor(init * exp(rate * time))
}

wrapText <- function(txt, ...) {paste(strwrap(txt, ...), collapse = "\n")}

# Define UI ----
ui <- list(
  dashboardPage(
    skin = "green",
    ## Header ----
    dashboardHeader(
      title = "Pop. Growth", 
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Population_Growth")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview Page ----
        tabItem(
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
            style = "text-align: center;",
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
          like to thank Dr. Stephen Schaeffer from the Huck Institutes of Life
          Sciences for his advice and assistance for the biological content. The
          current version of the app was modified by Neil J. Hatfield.",
          br(),
          br(),
          "Cite this app as:",
          br(),
          citeApp(),
          br(),
          br(),
          div(class = "updated", "Last Update: 7/1/2022 by NJH.")
          )
        ),
        ### Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
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
            style = "text-align: center;",
            bsButton(
              inputId = "go",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          )
        ),
        ### Explore Page ----
        tabItem(
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
          tabsetPanel(
            id = "models",
            type = "tabs",
            #### Deterministic Models Tab ----
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
                bsPopover(
                  id = "deterPlot",
                  title =  "Rabbits Population Plot",
                  content = "This plot shows the estimated rabbit population",
                  placement = "top"
                )
                )
              )
            ),
            #### Infinite Capacity Stochastic Model Tab ----
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
                bsPopover(
                  id = "infCapPlot",
                  title =  "Rabbits Population Plot",
                  content = "This plot shows the estimated rabbit population",
                  placement = "top"
                )
                )
              )
            ),
            #### Finite Capacity Stochastic Model Tab ----
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
                bsPopover(id = "finCapPlot",
                          title =  "Rabbits Population Plot",
                          content = "This plot shows the estimated rabbit population",
                          placement = "top"
                )
                ),
                p("The dashed horizontal line denotes the theoretical, expected
              capacity.")
              )
            )
          )
        ),
        ###  References Page ----
        tabItem(
          tabName = "references",
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
        title = "Instructions",
        text = "Move the sliders to explore how different factors affect the
      rabbit population growth.",
      type = "info"
      )
    }
  )
  
  ## Go Buttons ----
  observeEvent(
    eventExpr = input$go, 
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "explore"
      )
    }
  )
  
  observeEvent(
    eventExpr = input$start, 
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages", 
        selected = "explore"
      )
    }
  )
  
  ## Summary sentence for initial population settings ----
  output$initSummary <- renderText({
    paste0("We are starting with a grassland environment with ", input$initPop,
           " rabbits. The growth rate is ", 
           round((input$birthRate - input$deathRate), 3)*100,
           "% per month, and we will observe the population for ", 
           input$obsPeriod, " months.")
  }) 
  
  ## Deterministic Model ----
  ### Carrying Capacity Slider ----
  output$kControl <- renderUI({
    if (input$addK1) {
      sliderInput(
        inputId = "kSlider1",
        label = "Carrying capacity",
        min = 500,
        max = 9000,
        value = 1000
      )
    }
  })
  
  ### Deterministic plot ----
  #### uses Geometric and Logistic 
  output$deterPlot <- renderPlot(
    expr = {
      #### Generate Data
      detData <- data.frame(
        month = 1:(input$obsPeriod + 1),
        pop = rep(input$initPop, (input$obsPeriod + 1))
      )
      graphTitle1 <- "determinisitic model" # placeholder title
      
      growthRate <- input$birthRate - input$deathRate
      
      #### Carrying capacity check/make data-logistic vs. geometric 
      ifelse(
        test = input$addK1, 
        yes = { # carrying capacity present-logistic
          # P(t+1) = P(t) + r*(1-P(t)/K)*P(t)
          capacity1 <- ifelse(is.null(input$kSlider1), 1e10, input$kSlider1)
          for (i in 2:(input$obsPeriod + 1)) {
            changePop <- growthRate * detData[i - 1, "pop"] *
              (1 - (detData[i - 1, "pop"] / capacity1))
            detData[i, "pop"] <- floor(detData[i - 1, "pop"] + changePop)
            if (detData[i, "pop"] < 0) { detData[i, "pop"] <- 0 }
          }
          graphTitle1 <- "Logistic Population Growth"
        },
        no = { # carrying capacity absent-geometric
          # P(t+1) = P(t) + r*P(t) OR P(t)=(1+r)^t*P
          for (i in 2:(input$obsPeriod + 1)) {
            changePop <- growthRate * detData[i - 1, "pop"]
            detData[i, "pop"] <- floor(detData[i - 1, "pop"] + changePop)
            if (detData[i, "pop"] < 0) {detData[i, "pop"] <- 0}
          }
          graphTitle1 <- "Geometric Population Growth"
        }
      )
      
      detData$month <- detData$month - 1
      
      #### Create plot ----
      g1 <- ggplot(data = detData, mapping = aes(x = month, y = pop)) +
        geom_point(size = 3, color = boastPalette[1]) +
        geom_line(size = 1, color = boastPalette[1]) +
        labs(
          x = "Months since start",
          y = "Poplulation",
          title = graphTitle1
        ) +
        scale_x_continuous(expand = expansion(mult = 0, add = c(0, 1.1))) +
        scale_y_continuous(
          expand = expansion(mult = c(0, 0.1), add = 0),
          limits = c(0, NA)
        ) +
        theme_bw() +
        ggplot2::theme(
          text = element_text(size = 18)
        )
      
      #### Add carrying capacity line 
      if (input$addK1) {
        g1 <- g1 + geom_hline(
          yintercept = input$kSlider1,
          linetype = "dashed",
          color = boastPalette[5],
          size = 1
        ) +
          annotate(
            geom = "text",
            x = 10,
            y = 1.05 * input$kSlider1,
            label = "Carrying Capacity",
            color = boastPalette[5],
            size = 5
          )
      }
      
      g1
    },
    alt = "The plot shows the exponential growth of the population when the
    birth rate is larger than the death rate. When there is a carrying capacity,
    the plot will show a S-curve that increases towards the capacity."
  )
  
  ## Infinite Capacity Model ----
  ### Disease Sliders ----
  output$diseaseSlider1 <- renderUI({
    if (input$addDisease1) {
      list(
        sliderInput(
          inputId = "date1",
          label = "Start month",
          min = 1,
          max = (input$obsPeriod - 6),
          value = 1,
          step = 1
        ),
        sliderInput(
          inputId = "disease1",
          label = "Disease severity",
          min = 0,
          max = 50,
          value = 0,
          step = 1,
          post = "%")
      )
    }
  })
  
  ### Infinite Capacity plot ----
  output$infCapPlot <- renderPlot(
    expr = {
      #### Create base data
      infCapData <- data.frame(
        month = 1:(input$obsPeriod + 1),
        pop = rep(input$initPop, (input$obsPeriod + 1))
      )
      graphTitle2 <- "infinite capacity model" # Placeholder title
      
      #### Disease Check 
      if (input$addDisease1) {
        modDeathRate1 <- input$deathRate + 0.005 * input$disease1
        graphTitle2 <- "Stochastic Population Growth w/ Disease"
      } else {
        modDeathRate1 <- input$deathRate
        graphTitle2 <- "Stochastic Population Growth"
      }
      
      #### Create stochastic data ----
      # N(t+1) = N(t) + F(t) - D(t), where
      # F(t) ~ Poi(birthRate*N(t)),
      # D(t) ~ Bin(N(t), modDeathRate1)
      for (i in 2:(input$obsPeriod + 1)) {
        infCapData[i, "pop"] <- floor(
          infCapData[i - 1, "pop"] + rpois(
            n = 1, lambda = input$birthRate * infCapData[i - 1, "pop"]) -
            rpois(n = 1, lambda = infCapData[i - 1, "pop"] * ifelse(
              test = i <= input$date1 || is.null(input$date1),
              yes = input$deathRate,
              no = modDeathRate1
            )
            )
        )
        if (infCapData[i, "pop"] < 0) {infCapData[i, "pop"] <- 0}
      }
      
      infCapData$month <- infCapData$month - 1
      
      #### Create plot ----
      g2 <- ggplot(
        data = infCapData,
        mapping = aes(x = month, y = pop, color = "Stochastic")
      ) +
        geom_point(size = 3) +
        geom_line(size = 1) +
        labs(
          x = "Months since start",
          y = "Poplulation",
          title = graphTitle2
        ) +
        scale_x_continuous(expand = expansion(mult = 0, add = c(0, 1.1))) +
        scale_y_continuous(
          expand = expansion(mult = c(0, 0.1), add = 0),
          limits = c(0, NA)
        ) +
        theme_bw() +
        theme(
          text = element_text(size = 18),
          legend.position = "bottom"
        ) +
        scale_color_manual(
          name = "Models",
          values = c(
            "Stochastic" = boastPalette[1],
            "Deterministic" = boastPalette[2]
          )
        )
      
      #### Add estimated deterministic model 
      if (input$expMod) {
        g2 <- g2 + geom_line(
          stat = "function",
          fun = detPop,
          args = list(init = input$initPop,
                      bRate = input$birthRate,
                      dRate1 = input$deathRate,
                      dRate2 = modDeathRate1,
                      date = input$date1),
          mapping = aes(color = "Deterministic"),
          linetype = "dashed",
          inherit.aes = F,
          size = 1
        )
      }
      
      g2
    },
    alt = "This plot will show the exponential growth of the rabbit population
    when the birth rate is larger than the death rate. When a disease is added,
    the death rate increases. There is randomness (stochasticity) in the graph.
    A deterministic exponetial plot may be added to highlight that reality is
    typically stochastic."
  )
  
  ## Finite Capacity Model ----
  ### Extra Sliders ----
  output$resourceSlider <- renderUI({
    if (input$addResource) {
      sliderInput(
        inputId = "abundance",
        label = "Resource abundance",
        # Min needs to be limited to ensure non-negative capacity
        min = max(-1 * floor(0.9 * (input$kSlider2 / 50)), -50),
        max = 50,
        step = 1,
        value = 0
      )
    }
  })
  
  output$competitionSlider <- renderUI({
    if (input$addCompetition) {
      list(
        sliderInput(
          inputId = "competition",
          label = "Competitor's growth rate",
          min = -0.5,
          max = 0.5,
          step = 0.05,
          value = 0
        ),
        p("The hares (i.e., the competitors) start with the same initial population
          and birth rate as the rabbits.")
      )
    }
  })
  
  output$diseaseSlider2 <- renderUI({
    if (input$addDisease2) {
      list(
        sliderInput(
          inputId = "date2",
          label = "Start month",
          min = 1,
          max = (input$obsPeriod - 6),
          step = 2,
          value = 1
        ),
        sliderInput(
          inputId = "disease2",
          label = "Disease severity",
          min = 0,
          max = 50,
          step = 1,
          value = 0,
          post = "%"
        ),
        p("The disease only affects the rabbits, not the hares.")
      )
    }
  })
  
  ### Finite Capacity plot ----
  output$finCapPlot <- renderPlot({
    #### Make base data 
    finCapData <- data.frame(
      month = 1:(input$obsPeriod + 1),
      rabbit = rep(input$initPop, (input$obsPeriod + 1)),
      hare = rep(input$initPop, (input$obsPeriod + 1)),
      # Create noise for capacity
      error = rnorm(
        n = (input$obsPeriod + 1),
        mean = 0,
        sd = input$cv * input$kSlider2 / 500
      ),
      # Create scale for seasonal trend for capacity
      scale = rep(
        x = ifelse(
          test = input$cv == 0,
          yes = runif(n = 1,min = 0.05 * input$kSlider2, max = 0.1 * input$kSlider2),
          no = input$cv / 100 * input$kSlider2
        ),
        (input$obsPeriod + 1)
      )
    )
    graphTitle3 <- "Stochastic Finite Capacity Model" # Placeholder title
    
    #### Create seasonal and stochastic capacity
    finCapData <- finCapData %>%
      dplyr::mutate(
        capacity = floor(input$kSlider2 + 50 * ifelse(
          test = is.null(input$abundance),
          yes = 0,
          no = input$abundance
        ) + scale * sin(month * pi / 6) + error
        )
      )
    
    #### Check for disease
    if (input$addDisease2) {
      modDeathRate2 <- input$deathRate + 0.005 * ifelse(
        test = is.null(input$disease2),
        yes = 0,
        no = input$disease2
      )
    } else {
      modDeathRate2 <- input$deathRate
    }
    
    #### Hare Death Rate
    if (input$addCompetition) {
      hareDeath <- ifelse(
        test = is.null(input$competition),
        yes = input$deathRate,
        no = input$birthRate - input$competition
      )
    } else {
      hareDeath <- input$deathRate
    }
    
    #### Create and adjust populations ----
    for (i in 2:(input$obsPeriod + 1)) {
      ##### Rabbits 
      finCapData[i, "rabbit"] <- (calcPop(
        cap = finCapData[i, "capacity"],
        init = input$initPop,
        bRate = input$birthRate,
        dRate1 = input$deathRate,
        dRate2 = modDeathRate2,
        date = input$date2,
        time = finCapData[i, "month"]
      ) +
        rpois(n = 1, lambda = finCapData[i - 1, "rabbit"] * input$birthRate) -
        rpois(n = 1, lambda = finCapData[i - 1, "rabbit"] *
                ifelse(
                  test = i <= input$date2 || is.null(input$date2),
                  yes = input$deathRate,
                  no = modDeathRate2
                )
        )
      )
      
      if (finCapData[i, "rabbit"] < 0) {finCapData[i, "rabbit"] <- 0}
      
      ##### Hares 
      finCapData[i, "hare"] <- (calcPop(
        cap = finCapData[i, "capacity"],
        init = ifelse(
          test = !(input$addCompetition),
          yes = 0,
          no = input$initPop
        ),
        bRate = ifelse(
          test = is.null(input$competition),
          yes = 0,
          no = input$birthRate
        ),
        dRate1 = ifelse(
          test = is.null(input$competition),
          yes = 0,
          no = (input$birthRate - input$competition)
        ),
        dRate2 = ifelse(
          test = is.null(input$competition),
          yes = 0,
          no = (input$birthRate - input$competition)
        ),
        date = 0,
        time = finCapData[i, "month"]) + ifelse(
          test = input$addCompetition,
          yes = rpois(n = 1, lambda = finCapData[i - 1, "hare"] * input$birthRate),
          no = 0
        ) - ifelse(
          test = input$addCompetition,
          yes = rpois(n = 1, lambda = finCapData[i - 1, "hare"] * hareDeath),
          no = 0
        )
      )
      
      if (finCapData[i, "hare"] < 0 || is.na(finCapData[i, "hare"])) {
        finCapData[i, "hare"] <- 0
      }
      
      #### First Capacity Check 
      if (finCapData[i, "rabbit"] + finCapData[i, "hare"] >
          finCapData[i, "capacity"]) {
        finCapData[i, "rabbit"] <- finCapData[i, "rabbit"] -
          ifelse(
            test = finCapData[i, "hare"] <= 10,
            yes = (finCapData[i, "rabbit"] - finCapData[i, "capacity"]),
            no = ceiling((finCapData[i, "rabbit"] +
                            finCapData[i, "hare"] - finCapData[i, "capacity"]) / 2)
          )
        finCapData[i, "hare"] <- finCapData[i, "hare"] - ifelse(
          test = finCapData[i, "hare"] == 0,
          yes = 0,
          no = floor((finCapData[i, "rabbit"] + finCapData[i, "hare"] -
                        finCapData[i, "capacity"]) / 2)
        )
      }
      
      #### Secondary capacity check 
      #### Populations tend to "run away", ignoring capacity
      if (finCapData[i, "hare"] > finCapData[i, "capacity"]) {
        finCapData[i, "hare"] <- finCapData[i, "hare"] - 
          runif(n = 1, min = 0.75, max = 1.2) *
          (finCapData[i, "hare"] - finCapData[i, "capacity"])
      }
      if (finCapData[i, "rabbit"] > finCapData[i, "capacity"]) {
        finCapData[i, "rabbit"] <- finCapData[i, "rabbit"] - rpois(
          n = 1,
          lambda = (finCapData[i, "rabbit"] - finCapData[i, "capacity"]) *
            input$deathRate * runif(n = 1, min = 2, max = 4)
        )
      }
      
      
      #### Negative value check 
      if (finCapData[i, "rabbit"] < 0) {finCapData[i, "rabbit"] <- 0}
      if (finCapData[i, "hare"] < 0) {finCapData[i, "hare"] <- 0}
    }
    if (!(input$addCompetition)) {finCapData[1, "hare"] <- 0}
    
    finCapData$month <- finCapData$month - 1
    
    #### Create plot ----
    g3 <- ggplot(
      data = finCapData,
      mapping = aes(x = month, y = rabbit, color = "Rabbits")
    ) +
      geom_point(size = 3) +
      geom_path(size = 1) +
      labs(
        x = "Months since start",
        y = "Population",
        title = graphTitle3
      ) +
      geom_hline(
        yintercept = input$kSlider2,
        linetype = "dashed",
        color = boastPalette[5],
        size = 1
      ) +
      geom_point(mapping = aes(y = capacity, color = "Actual Capacity")) +
      geom_line(mapping = aes(y = capacity, color = "Actual Capacity")) +
      scale_x_continuous(expand = expansion(mult = 0, add = c(0, 1.1))) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.1), add = 0),
        limits = c(0, NA)
      ) +
      theme_bw() +
      theme(
        text = element_text(size = 18),
        legend.position = "bottom"
      ) +
      scale_color_manual(
        name = "Models",
        values = c(
          "Rabbits" = boastPalette[1],
          "Actual Capacity" = boastPalette[5],
          "Deterministic" = boastPalette[2],
          "Hares" = boastPalette[3]
        )
      )
    
    ##### Add deterministic model 
    g3 <- g3 + geom_line(
      stat = "function",
      fun = calcPop,
      args = list(
        cap = input$kSlider2,
        init = input$initPop,
        bRate = input$birthRate,
        dRate1 = input$deathRate,
        dRate2 = modDeathRate2,
        date = input$date2
      ),
      mapping = aes(color = "Deterministic"),
      linetype = "dashed",
      inherit.aes = F,
      size = 1
    )
    ##### Add Competition 
    if (input$addCompetition) {
      g3 <- g3 + geom_point(
        mapping = aes(y = hare, color = "Hares"),
        size = 3
      ) +
        geom_line(
          mapping = aes(y = hare, color = "Hares"),
          size = 1
        )
    }
    
    g3
  },
  alt = "In this plot there is a dashed line for the theoretical carrying capacity
  but also a line showing an actual carrying capacity that has a yearly seasonal
  trend and randomness. The plots for the rabbits and hares (the competition) are
  both logistic (s-curves), increasing towards the capacity, when the birth rate
  is greater than the death rate (i.e., growth rate is positive). A deterministic
  model may be added to highlight the presence of randomness in real life."
  )
  
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
