library(shiny)
library(ggplot2)
library(shinyWidgets)
library(dplyr)

# Helper Functions -------------------------------------------------------------
calcPop <- function(cap, init, rate, time) {
  floor((cap * init) / (init + (cap - init) * exp(-1 * rate * time)))
}

wrapText <- function(txt, ...) {paste(strwrap(txt, ...), collapse = "\n")}

boastPalette <- c("#0072B2","#D55E00","#009E73","#CE77A8",
                  "#000000","#E69F00","#999999","#56B4E9","#CC79A7")

shinyServer(function(input, output,session) {
  # Set up the header info button ----------------------------------------------
  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "Instructions",
      text = "Move the sliders to explore how different factors affect the
      rabbit population growth.",
      type = "info"
    )
  })

  # Define the actions of the buttons ------------------------------------------
  observeEvent(input$go, {
    updateTabItems(session,"tabs","explore")
  })
  observeEvent(input$start, {
    updateTabItems(session,"tabs","explore")
  })

  # Create the summary sentence for initial population settings ----------------
  output$initSummary <- renderText({
    paste0("We are starting with a grassland environment with ",
          input$initPop,
          " rabbits. The growth rate is ",
          round((input$birthRate - input$deathRate), 3)*100,
          "% per month, and we will observe the population for ",
          input$obsPeriod,
          " months.")
  })

  # Create the deterministic model ---------------------------------------------

  ## Carrying Capacity Slider for Deterministic Model --------------------------
  output$kControl <- renderUI({
    if(input$addK1){
        sliderInput(
          inputId = "kSlider1",
          label = "Carrying capacity",
          min = 500,
          max = 9000,
          value = 1000
        )
    }
  })

  ## Create the deterministic plots-Geometric and Logistic ---------------------
  output$deterPlot <- renderPlot({
    ### Create base elements--data, growth rate, graph title -------------------
    detData <- data.frame(
      month = 1:(input$obsPeriod + 1),
      pop = rep(input$initPop, (input$obsPeriod + 1))
    )
    graphTitle1 <- "determinisitic model" # placeholder title

    growthRate <- input$birthRate - input$deathRate

    ### Carrying capacity check/make data-logistic vs. geometric ---------------
    ifelse(input$addK1,
           { # carrying capacity present-logistic
             # P(t+1) = P(t) + r*(1-P(t)/K)*P(t)
             capacity1 <- ifelse(is.null(input$kSlider1), 1e10, input$kSlider1)
             for(i in 2:(input$obsPeriod + 1)) {
               changePop <- growthRate * detData[i-1, "pop"] *
                 (1 - (detData[i-1, "pop"] / capacity1))
               detData[i, "pop"] <- floor(detData[i-1, "pop"] + changePop)
               if(detData[i, "pop"] < 0){ detData[i, "pop"] <- 0 }
             }
              graphTitle1 <- "Logistic Population Growth"
           },
           { # carrying capacity absent-geometric
             # P(t+1) = P(t) + r*P(t) OR P(t)=(1+r)^t*P
             for(i in 2:(input$obsPeriod + 1)) {
               changePop <- growthRate * detData[i-1, "pop"]
               detData[i, "pop"] <- floor(detData[i-1, "pop"] + changePop)
               if(detData[i, "pop"] < 0) {detData[i, "pop"] <- 0}
             }
             graphTitle1 <- "Geometric Population Growth"
            }
          )

    detData$month <- detData$month - 1

    ### Create deterministic plot ----------------------------------------------
    g1 <- ggplot2::ggplot(data = detData,
                        mapping = aes(x = month,
                                      y = pop)) +
      ggplot2::geom_point(size = 3, color = boastPalette[1]) +
      ggplot2::geom_line(size = 1, color = boastPalette[1]) +
      ggplot2::labs(
        x = "Months since start",
        y = "Poplulation",
        title = graphTitle1
      ) +
      ggplot2::scale_x_continuous(
        expand = expansion(mult = 0, add = c(0, 1.1))) +
      ggplot2::scale_y_continuous(
        expand = expansion(mult = c(0, 0.1), add = 0),
        limits = c(0, NA)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)
      )
    ### Add carrying capacity line ---------------------------------------------
    if(input$addK1) {
      g1 <- g1 + ggplot2::geom_hline(
        yintercept = input$kSlider1,
        linetype = "dashed",
        color = boastPalette[5],
        size = 1) +
        ggplot2::annotate(geom = "text", x = 10, y = 1.05 * input$kSlider1,
                          label = "Carrying Capacity",
                          color = boastPalette[5],
                          size = 5)
    }
    return(g1)
  })

  # Create the Infinite Capacity Model -----------------------------------------

  ## Disease Sliders for Infinite Capacity Model -------------------------------
  output$diseaseSlider1 <- renderUI({
    if(input$addDisease1){
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

  ## Create the stochastic plots for infinite capacity -------------------------
  output$infCapPlot <- renderPlot({
    ### Create base elements--data, growth rate, graph title -------------------
    infCapData <- data.frame(
      month = 1:(input$obsPeriod + 1),
      pop = rep(input$initPop, (input$obsPeriod + 1))
    )
    graphTitle2 <- "infinite capacity model" # Placeholder title

    ### Disease Check ----------------------------------------------------------
    if(input$addDisease1) {
      modDeathRate1 <- input$deathRate + 0.005 * input$disease1
      graphTitle2 <- "Stochastic Population Growth w/ Disease"
    } else {
      modDeathRate1 <- input$deathRate
      graphTitle2 <- "Stochastic Population Growth"
    }

    ### Create stochastic data -------------------------------------------------
    # N(t+1) = N(t) + F(t) - D(t), where
    # F(t) ~ Poi(birthRate*N(t)),
    # D(t) ~ Bin(N(t), modDeathRate1)

    for(i in 2:(input$obsPeriod + 1)){
      infCapData[i, "pop"] <- floor(infCapData[i-1, "pop"] +
            rpois(n = 1, lambda = input$birthRate * infCapData[i-1, "pop"]) -
            rpois(n = 1, lambda = infCapData[i-1, "pop"] *
                    ifelse(i <= input$date1 || is.null(input$date1),
                                                                  input$deathRate,
                                                                  modDeathRate1)))
      if(infCapData[i, "pop"] < 0) {infCapData[i, "pop"] <- 0}
    }

    infCapData$month <- infCapData$month - 1

    ### Create infinite capacity plot ------------------------------------------
    g2 <- ggplot2::ggplot(data = infCapData,
                          mapping = aes(x = month,
                                        y = pop,
                                        color="Stochastic")) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_line(size = 1) +
      ggplot2::labs(
        x = "Months since start",
        y = "Poplulation",
        title = graphTitle2
      ) +
      ggplot2::scale_x_continuous(
        expand = expansion(mult = 0, add = c(0, 1.1))) +
      ggplot2::scale_y_continuous(
        expand = expansion(mult = c(0, 0.1), add = 0),
        limits = c(0, NA)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)
      )+
      ggplot2::scale_color_manual(name = "Models",
                                  values = c("Stochastic" = boastPalette[1],
                                             "Deterministic" = boastPalette[2]))

    ### Add estimated deterministic model --------------------------------------
    if (input$expMod) {
      g2 <- g2 + ggplot2::geom_line(stat = "function",
                  fun = function(x) {floor(input$initPop *
                                      exp((input$birthRate - modDeathRate1) * x))},
                  mapping = aes(color="Deterministic"),
                  linetype = "dashed",
                  inherit.aes = F,
                  size = 1
      )
    }
    return(g2)
  })


  # Create the Finite Capacity Model -------------------------------------------
  ## Make the additional sliders -----------------------------------------------
  output$resourceSlider <- renderUI({
    if(input$addResource){
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
    if(input$addCompetition){
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
    if(input$addDisease2){
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

  ## Create plots for Finite Capacity
  output$finCapPlot <- renderPlot({
    ### Create base Data for Finite -------------------------------------------------
    finCapData <- data.frame(
      month = 1:(input$obsPeriod + 1),
      rabbit = rep(input$initPop, (input$obsPeriod + 1)),
      hare = rep(input$initPop, (input$obsPeriod + 1)),
      # Create noise for capacity
      error = rnorm(n = (input$obsPeriod + 1), mean = 0,
                    sd = input$cv * input$kSlider2 / 500),
      # Create scale for seasonal trend for capacity
      scale = rep(ifelse(input$cv == 0,
                         runif(1,
                               min = 0.05 * input$kSlider2,
                               max = 0.1 * input$kSlider2),
                         input$cv / 100 * input$kSlider2),
                  (input$obsPeriod + 1))
    )
    graphTitle3 <- "Stochatic Finite Capacity Model" # Placeholder title

    #### Create seasonal and stochastic capacity -------------------------------
    finCapData <- finCapData %>%
      dplyr::mutate(capacity = floor(input$kSlider2 +
                                       50 * ifelse(is.null(input$abundance),
                                                   0, input$abundance) +
                                       scale * sin(month * pi / 6) + error))

    #### Check for disease
    if(input$addDisease2) {
      modDeathRate2 <- input$deathRate + 0.005 * ifelse(is.null(input$disease2),
                                                        0,
                                                        input$disease2)
    } else {
      modDeathRate2 <- input$deathRate
    }

    #### Hare Death Rate
    if(input$addCompetition) {
      hareDeath <- ifelse(is.null(input$competition),
                          input$deathRate,
                          input$birthRate - input$competition)
    } else {
      hareDeath <- input$deathRate
    }

    #### Create and adjust populations -----------------------------------------
    for (i in 2:(input$obsPeriod + 1)){
      ##### Rabbits ----
      finCapData[i, "rabbit"] <- (calcPop(cap = finCapData[i, "capacity"],
                                          init = input$initPop,
                                          rate = input$birthRate - ifelse(
                                            i <= input$date2 || is.null(input$date2),
                                            input$deathRate,
                                            modDeathRate2),
                                          time = finCapData[i, "month"]) +
        rpois(n = 1, lambda = finCapData[i-1, "rabbit"] * input$birthRate) -
        rpois(n = 1, lambda = finCapData[i-1, "rabbit"] *
                ifelse( i <= input$date2 || is.null(input$date2),
                        input$deathRate,
                        modDeathRate2))
      )
      if(finCapData[i, "rabbit"] < 0) {finCapData[i, "rabbit"] <- 0}
      ##### Hares ----
      finCapData[i, "hare"] <- (calcPop(cap = finCapData[i, "capacity"],
                                         init = ifelse(!(input$addCompetition),
                                                       0,
                                                       input$initPop),
                                         rate = ifelse(is.null(input$competition),
                                                       0,
                                                       input$competition),
                                           time = finCapData[i, "month"]) +
          ifelse(input$addCompetition,
                 rpois(n = 1, lambda = finCapData[i-1, "hare"] * input$birthRate),
                 0) -
          ifelse(input$addCompetition,
                 rpois(n = 1, lambda = finCapData[i-1, "hare"] * hareDeath),
                 0)
      )
      if(finCapData[i, "hare"] < 0 || is.na(finCapData[i, "hare"]))
        {finCapData[i, "hare"] <- 0}

      #### Capacity Check ----
      if(finCapData[i, "rabbit"] + finCapData[i, "hare"] >
         finCapData[i, "capacity"]) {
        finCapData[i, "rabbit"] <- finCapData[i, "rabbit"] -
          ifelse(finCapData[i, "hare"] <= 10,
                 (finCapData[i, "rabbit"] - finCapData[i, "capacity"]),
                 ceiling((finCapData[i, "rabbit"] +
                            finCapData[i, "hare"] - finCapData[i, "capacity"]) / 2))
        finCapData[i, "hare"] <- finCapData[i, "hare"] -
          ifelse(finCapData[i, "hare"] == 0,
                 0,
                 floor((finCapData[i, "rabbit"] +
                            finCapData[i, "hare"] - finCapData[i, "capacity"]) / 2))
      }

      #### Secondary capacity check ----
      #### Rabbit population tends to "run away", almost ignoring capacity
      if(finCapData[i, "rabbit"] > finCapData[i, "capacity"]) {
        finCapData[i, "rabbit"] <- finCapData[i, "rabbit"] -
          rpois(n = 1,
                lambda = (finCapData[i, "rabbit"] - finCapData[i, "capacity"]) *
                  input$deathRate * runif(1, min = 2, max = 4))
      }

      #### Negative value check ----
      if(finCapData[i, "rabbit"] < 0) {finCapData[i, "rabbit"] <- 0}
      if(finCapData[i, "hare"] < 0) {finCapData[i, "hare"] <- 0}
    }
    if(!(input$addCompetition)) {finCapData[1, "hare"] <- 0}

    finCapData$month <- finCapData$month - 1

    ### Create plot for Finite -------------------------------------------------
    g3 <- ggplot2::ggplot(data = finCapData,
                    mapping = aes(x = month, y = rabbit, color = "Rabbits")) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_path(size = 1) +
      ggplot2::labs(
        x = "Months since start",
        y = "Population",
        title = graphTitle3
      ) +
      ggplot2::geom_hline(
        yintercept = input$kSlider2,
        linetype = "dashed",
        color = boastPalette[5],
        size = 1
      ) +
      ggplot2::annotate(geom = "text", x = 7.5, y = 1.15 * input$kSlider2,
                        label = wrapText("Theoretical Expected Capacity", width = 5),
                        color = boastPalette[5],
                        size = 5) +
      ggplot2::geom_point(mapping = aes(y = capacity, color = "Actual Capacity")) +
      ggplot2::geom_line(mapping = aes(y = capacity, color = "Actual Capacity")) +
      ggplot2::scale_x_continuous(expand = expansion(mult = 0, add = c(0, 1.1))) +
      ggplot2::scale_y_continuous(expand = expansion(mult = c(0, 0.1), add = 0),
                                  limits = c(0, NA)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)
      ) +
      ggplot2::scale_color_manual(name = "Models",
                                  values = c("Rabbits" = boastPalette[1],
                                             "Actual Capacity" = boastPalette[5],
                                             "Deterministic" = boastPalette[2],
                                             "Hares" = boastPalette[3]))

    #### Add deterministic model ----
    g3 <- g3 + ggplot2::geom_line(stat = "function",
                         fun = calcPop,
                         args = list(cap = input$kSlider2,
                                     init = input$initPop,
                                     rate = input$birthRate - input$deathRate),
                         mapping = aes(color = "Deterministic"),
                         linetype = "dashed",
                         inherit.aes = F,
                         size = 1
    )
    #### Add Competition ----
    if (input$addCompetition) {
      g3 <- g3 + ggplot2::geom_point(mapping = aes(y = hare, color = "Hares"),
                                     size = 3) +
        ggplot2::geom_line(mapping = aes(y = hare, color = "Hares"),
                           size = 1)
    }
    return(g3)
  })
})