library(shiny)
library(RColorBrewer)
library(ggplot2)
library(shinyBS)
library(shinydashboard)
library(shinycssloaders)
library(dplyr)


library(shinyWidgets)
library(shinythemes)

sliderInput3 <- function(inputId, label, min, max, value, step=NULL, from_min, from_max){
  x <- sliderInput(inputId, label, min, max, value, step, animate=FALSE)
  x$children[[2]]$attribs <- c(x$children[[2]]$attribs, 
                               "data-from-min" = from_min, 
                               "data-from-max" = from_max 
  )
  x
}
ui <- dashboardPage(
  
  dashboardHeader(title = "Population Growth Model",
                  tags$li(class = "dropdown",
                          tags$a(href = "https://shinyapps.science.psu.edu/",
                                 icon("home",lib ="font-awesome"))),
                  tags$li(class = "dropdown",
                          actionLink("info",icon("info",class = "myClass"))),
                  titleWidth = 300),
  dashboardSidebar(width = 180,
                   sidebarMenu(id='tabs',
                               menuItem('Prerequisites', tabName='preq', icon=icon('book')),
                               menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
                               menuItem("Explore", tabName = "explore", icon = icon("wpexplorer"))
                               #menuItem("Exercise", tabName = "exercise", icon = icon("file-alt"))
                   )
  ),
  dashboardBody(
    tags$head( 
      #change all sliders color
      tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css"),
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-8 .irs-single, .js-irs-8 .irs-bar-edge, .js-irs-8 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-9 .irs-single, .js-irs-9 .irs-bar-edge, .js-irs-9 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-10 .irs-single, .js-irs-10 .irs-bar-edge, .js-irs-10 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-11 .irs-single, .js-irs-11 .irs-bar-edge, .js-irs-11 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-12 .irs-single, .js-irs-12 .irs-bar-edge, .js-irs-12 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-13 .irs-single, .js-irs-13 .irs-bar-edge, .js-irs-13 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-14 .irs-single, .js-irs-14 .irs-bar-edge, .js-irs-14 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-15 .irs-single, .js-irs-15 .irs-bar-edge, .js-irs-15 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-16 .irs-single, .js-irs-16 .irs-bar-edge, .js-irs-16 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-17 .irs-single, .js-irs-17 .irs-bar-edge, .js-irs-17 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-18 .irs-single, .js-irs-18 .irs-bar-edge, .js-irs-18 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-19 .irs-single, .js-irs-19 .irs-bar-edge, .js-irs-19 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-20 .irs-single, .js-irs-20 .irs-bar-edge, .js-irs-20 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-21 .irs-single, .js-irs-21 .irs-bar-edge, .js-irs-21 .irs-bar {background: #3CBAAD}")),
      tags$style(HTML(".js-irs-22 .irs-single, .js-irs-22 .irs-bar-edge, .js-irs-22 .irs-bar {background: #3CBAAD}")),
      
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-8 .irs-single, .js-irs-8 .irs-bar-edge, .js-irs-8 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-9 .irs-single, .js-irs-9 .irs-bar-edge, .js-irs-9 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-10 .irs-single, .js-irs-10 .irs-bar-edge, .js-irs-10 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-11 .irs-single, .js-irs-11 .irs-bar-edge, .js-irs-11 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-12 .irs-single, .js-irs-12 .irs-bar-edge, .js-irs-12 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-13 .irs-single, .js-irs-13 .irs-bar-edge, .js-irs-13 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-14 .irs-single, .js-irs-14 .irs-bar-edge, .js-irs-14 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-15 .irs-single, .js-irs-15 .irs-bar-edge, .js-irs-15 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-16 .irs-single, .js-irs-16 .irs-bar-edge, .js-irs-16 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-17 .irs-single, .js-irs-17 .irs-bar-edge, .js-irs-17 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-18 .irs-single, .js-irs-18 .irs-bar-edge, .js-irs-18 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-19 .irs-single, .js-irs-19 .irs-bar-edge, .js-irs-19 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-20 .irs-single, .js-irs-20 .irs-bar-edge, .js-irs-20 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-21 .irs-single, .js-irs-21 .irs-bar-edge, .js-irs-21 .irs-bar {border-color: #3CBAAD}")),
      tags$style(HTML(".js-irs-22 .irs-single, .js-irs-22 .irs-bar-edge, .js-irs-22 .irs-bar {border-color: #3CBAAD}"))
      
    ),
    tabItems(
      tabItem(tabName = 'preq',
              
              h3(strong('Population Growth Background:')),br(),
              
              h4(tags$li("Population growth depends on the rate of births and how that compares to the death rate (the difference is called the growth rate).  The exponential growth of a population that results is then tempered by a finite carrying capacity, which is the maximum sustainable population given the resources available in the environment.")),
              
              h4(tags$li("In this app we see how random fluctuations or “stochasticity” affects population growth in both the finite and “infinite” capacity situations.")),
              
              h4(tags$li("Deterministic models do not account for random fluctuations, so whatever parameters you set for population growth and carrying capacity will result in a pre-determined result.")),
              
              h4(tags$li("But reality is not deterministic in nature.  Individuals in a population will vary in how many offspring they have, how long they will live under natural conditions, and whether they will be affected by a disease. Further, the carrying capacity will vary from time to time due to random fluctuations in the weather, in the development of food sources, and the like. ")),
              
              h4(tags$li("Population growth in the face of stochasticity can be quite different from the predictions that would be made by a deterministic model.  This is especially true with small populations since a few extra deaths or a few less births happening randomly might wipe out a population altogether.")),
              
              
              br(),
              div(style = "text-align: center",actionButton("go", "G O !", icon("bolt"), size = "medium",style = 'color: #fff; background-color: #3CBAAD',class="circle grow"))
              
              
              ),
      tabItem(tabName = "overview",
              
              fluidPage(
                h3(strong("About:")),
                h4("Explore how growth rate, carrying capacity, variation of the population, density-independent limiting factors, and density-dependent limiting factors, affects a simulated population of rabbits."),br(),
                h3(strong("Instructions:")),
                h4(tags$li("Move the sliders around to explore how the different factors change the population growth over time.")),
                h4(tags$li("Use the checkboxes to add each factor one by one to see the influence of each of those assumptions.")),
                h4(tags$li("Notice the changes that occur as the factors go from the extremes.")),
                div(style = "text-align: center",
                    actionButton("start", "G O !", icon("bolt"), size = "medium",style = 'color: #fff; background-color: #3CBAAD',class="circle grow")),
                #bsButton("start", "GO", icon("bolt"),size = "large", style = "warning")),
                br(),
                h3(strong("Acknowledgements:")),
                h4("This app was developed and coded by Yutong Wu.")
                   #tags$a(href = "https://www.nytimes.com/interactive/2016/11/08/us/politics/election-exit-polls.html","Election 2016: Exit Polls.", style = "text-decoration: underline; color: #3CBAAD"),"on July 20, 2017.")
              )
      ),
      tabItem(tabName = "explore",
              
              navbarPage(
                tags$style(HTML(" 
                                .navbar { background-color: white;}
                                .navbar-default .navbar-nav > li > a {color:black;}
                                .navbar-default .navbar-nav > .active > a,
                                .navbar-default .navbar-nav > .active > a:focus,
                                .navbar-default .navbar-nav > li > a:hover {color: black;background-color:#8ad5cd;text-decoration:underline;}
                                ")), 
                tabPanel(strong("Deterministic"),
                         # Application title
                         titlePanel("Population Growth in Rabbits"),
                         
                         # Sidebar with a slider input for number of bins 
                         sidebarLayout(
                           sidebarPanel(
                             h4(strong("Summary:")),
                             htmlOutput("inform"),
                             hr(),
                             setSliderColor(c("#3CBAAD","#3CBAAD","#3CBAAD","#3CBAAD"),c(1,2,3,4)),
                             tags$div(title="Click here to set the starting population of rabbits",
                                      sliderInput3("start_size",
                                                   "Starting Population Size:",
                                                   min = 0,
                                                   max = 100,
                                                   value = 50,
                                                   from_min = 1,
                                                   from_max = 100,
                                                   step = 5)
                             ),
                             sliderInput3("brith_rate",
                                          "Birth Rate:",
                                          min = 0,
                                          max = 1,
                                          value = 0.5,
                                          from_min = 0.01,
                                          from_max = 1,
                                          step = 0.002),
                             sliderInput3("death_rate",
                                          "Death Rate:",
                                          min = 0,
                                          max = 1,
                                          value = 0.3,
                                          from_min = 0.01,
                                          from_max = 1,
                                          step = 0.002),
                             tags$head(tags$style(HTML('
                                                       #checkbox :after, #checkbox :before{
                                                       background-color:#bff442;
                                }'))),
                                        
                             checkboxInput("add_k", "Carrying Capacity", value = FALSE),
                             uiOutput("kControl")
                             ),
                           
                           # Show a plot of the generated distribution
                           mainPanel(
                             plotOutput("deterPlot") %>% withSpinner(color="#3CBAAD"),
                             bsPopover(id = "deterPlot",
                                       title =  "Rabbits Population Plot", 
                                       content = "This plot shows the estimated Rabbits population", 
                                       trigger = "hover")
                           ))
                             ),
                tabPanel(strong("Infinite Capacity"),
                         # Application title
                         titlePanel("Population Growth in Rabbits with Infinite Carrying Capacity"),
                         sidebarLayout(
                           sidebarPanel(
                             h4(strong("Summary:")),
                             htmlOutput("inform2"),
                             hr(),
                             setSliderColor(c("#3CBAAD","#3CBAAD","#3CBAAD","#3CBAAD"),c(1,2,3)),
                             sliderInput3("start_size2",
                                          "Starting Population Size:",
                                          min = 0,
                                          max = 100,
                                          value = 50,
                                          from_min = 1,
                                          from_max = 100,
                                          step = 2),
                             sliderInput3("brith_rate2",
                                          "Birth Rate:",
                                          min = 0,
                                          max = 1,
                                          value = 0.5,
                                          from_min = 0.01,
                                          from_max = 1,
                                          step = 0.002),
                             sliderInput3("death_rate2",
                                          "Death Rate:",
                                          min = 0.00,
                                          max = 1,
                                          value = 0.3,
                                          from_min = 0.01,
                                          from_max = 1,
                                          step = 0.002),
                             
                             h4("Density-dependent limiting factors:"),
                             
                             checkboxInput("add_disease", "Add Disease", value = FALSE),
                             uiOutput("disease_exp"),
                             
                             hr(),
                             checkboxInput("expMod", "Estimated Model", value = FALSE)
                             
                           ),
                           mainPanel(
                             plotOutput("expGrowthPlot") %>% withSpinner(color="#3CBAAD"),
                             bsPopover(id = "expGrowthPlot",
                                       title =  "Rabbits Population Plot", 
                                       content = "This plot shows the simulated Rabbits population with infinite carrying capacity", 
                                       trigger = "hover")
                           )
                           
                           
                           
                         )
                         
                         
                         
                         
                ),
                tabPanel(strong("Finite Capacity"),
                         # Application title
                         titlePanel("Population Growth in Rabbits with Finite Carrying Capacity"),
                         
                         # Sidebar with a slider input for number of bins 
                         sidebarLayout(
                           sidebarPanel(
                             h4(strong("Summary:")),
                             htmlOutput("inform3"),
                             hr(),
                             fluidRow(
                               column(6,
                                      ailgn="center",
                                      
                                      sliderInput3("start_size3",
                                                   "Starting Population Size:",
                                                   min = 0,
                                                   max = 100,
                                                   value = 50,
                                                   from_min = 1,
                                                   from_max = 100,
                                                   step = 5),  
                                      sliderInput3("brith_rate3",
                                                   "Birth Rate:",
                                                   min = 0.00,
                                                   max = 1,
                                                   value = 0.5,
                                                   from_min = 0.01,
                                                   from_max = 1,
                                                   step = 0.002),
                                      
                                      sliderInput3("death_rate3",
                                                   "Death Rate:",
                                                   min = 0.00,
                                                   max = 1,
                                                   value = 0.3,
                                                   from_min = 0.01,
                                                   from_max = 1,
                                                   step = 0.002),
                                      h4("Density-independent limiting factors:"),
                                      checkboxInput("add_resource", "Add Environmental Factor", value = FALSE),
                                      uiOutput("resource_added")
                                      
                                      
                                      
                               ),
                               column(6,
                                      ailgn="center",
                                      sliderInput("k_pop",
                                                  "Carrying Capacity:",
                                                  min = 100,
                                                  max = 20000,
                                                  value = 10000,
                                                  step = 10),
                                      
                                      sliderInput("var_k_pop",
                                                  "Variation of Carrying Capacity: (percent)",
                                                  min = 0,
                                                  max = 30,
                                                  value = 0,
                                                  step = 5),
                                      
                                      h4("Density-dependent limiting factors:"),
                                      
                                      checkboxInput("add_competition", "Add Competition", value = FALSE),
                                      tags$div(title="Competitors have same birthrate as rabbits.",
                                               uiOutput("competition_added")),
                                      
                                      
                                      checkboxInput("add_disease_log", "Add Disease", value = FALSE),
                                      uiOutput("disease_added_log")
                                      
                                      #checkboxInput("logMod", "Estimated Model", value = FALSE)
                                      
                               )
                             )
                             
                             ,width = 5),
                           
                           # Show a plot of the generated distribution
                           mainPanel(
                             plotOutput("logGrowthPlot") %>% withSpinner(color="#3CBAAD"),
                             bsPopover(id = "logGrowthPlot",
                                       title =  "Rabbits Population Plot", 
                                       content = "This plot shows the simulated Rabbits population with finite carrying capacity", 
                                       trigger = "hover"),
                             tags$style(type="text/css",
                                        ".shiny-output-error { visibility: hidden; }",
                                        ".shiny-output-error:before { visibility: hidden; }")
                             ,width = 7))
                )
                
                
                
                
                
                
                )
              
              
                )
      # tabItem(tabName = "exercise",
      #         fluidPage( # Application title
      #           titlePanel("Lobster Fishery Management"),
      #           
      #           fluidRow(
      #             column(2, 
      #                    # Sidebar with a slider input for number of bins
      #                    #style = "display:flex; align-items:flex-start",
      #                    # wellPanel(#sidebar panel
      #                    #style = "overflow-y: auto; position:fixed; width:300px; top:0; bottom:0",
      #                    #br(),
      #                    
      #                    #sliders for control
      #                    h4(strong("Legal Restrictions:")),
      #                    tags$div(title="The minimum landing size (MLS) is the smallest fish measurement at which it is legal to keep or sell a fish. What the MLS is depends on the species of fish. Sizes also vary around the world, as they are legal definitions which are defined by the local regulatory authority. Commercial trawl and seine fisheries can control the size of their catch by adjusting the mesh size of their nets.",
      #                             sliderInput("limit_1",
      #                                         "Provision of Minimum Landing Size (CL,mm):",
      #                                         min = 80,
      #                                         max = 140,
      #                                         value = 82,
      #                                         step = 5)),
      #                    tags$div(title="Commercial fishing regulators in the United States, such as the Atlantic States Marine Fisheries Commission and the National Marine Fisheries Service, enforce restrictions through the use of lobster fishing licenses and lobster pot tags that correspond to the fisher's permit number. Tag manufacturers also maintain databases for each state's licensed fisheries, tracking how many tags each fisher purchases every year.",
      #                             sliderInput3("limit_2",
      #                                          "Commercial Lobster Licenses :",
      #                                          min = 0,
      #                                          max = 10000,
      #                                          value = 4000,
      #                                          from_min = 100,
      #                                          from_max = 10000,
      #                                          step = 100)),
      #                    h4(strong("Fishing:")),
      #                    tags$div(title="The legal minimum length is 3 1/4 inches. Lobsters under this length are call “shorts” or “snappers” and must be thrown back into the ocean. The maximum legal length of a lobster is 5 inches carapace-length; which are called “jumbos”. The maximum size limit is regulated to protect the breeding stock.",         
      #                             sliderInput("fishing",
      #                                         "Number of Traps(thousands):",
      #                                         min = 40,
      #                                         max = 80,
      #                                         value = 40,
      #                                         step = 5)),
      #                    h4(strong("Natural Factor:")),
      #                    sliderInput("Temp",
      #                                "Food/Temperature:",
      #                                min = 0,
      #                                max = 100,
      #                                value = 0,
      #                                step = 5)
      #                    
      #             ),
      #             column(6, # Show a plot of the generated distribution
      #                    plotOutput("lobster_pop") %>% withSpinner(color="#3CBAAD"),
      #                    bsPopover(id = "lobster_pop",
      #                              title =  "Lobster Population", 
      #                              content = "This plot shows the simulated Lobster population from 1981 to 2013", 
      #                              trigger = "hover")
      #             ),
      #             column(4,
      #                    h3(strong("Challenge")),
      #                    box(background = "olive",  width = 400, height = 210,
      #                        shiny::tags$style(shiny::HTML(
      #                          "#text { font-size: 17px; height: 200px; overflow: auto; }"
      #                        )),
      #                        div(id = "text", paste("The American lobster fishery is one of the most valuable fisheries along the Atlantic coast.  In 2017, 136.7 million pounds of lobster were landed coastwide, representing a $566.4 million ex-vessel value. The vast majority of these landings came from the Gulf of Maine/Georges Bank (GOM/GBK), where the stock is at record high abundance. In contrast, there has been an overall decrease in the percentage of landings from the Southern New England stock, which is depleted and experiencing recruitment failure.
      #                                      Total U.S. landings in the fishery have steadily increased in the past 35 years. Between 1950 and 1975, landings were fairly stable around 30 million pounds; however, from 1976 – 2008 the average coastwide landings tripled, reaching 92 million pounds in 2006. Since then, landings have continued to increase, reaching 117 million pounds in 2010 and peaking at 158 million pounds in 2016."
      #                        ))
      #                    )
      #             ))
      #           
      #         )
      # )
      
              )
    
    
      )
              )
