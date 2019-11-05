library(shiny)
library(RColorBrewer)
library(ggplot2)
library(shinyBS)
library(shinyWidgets)
library(MASS)


sliderInput3 <- function(inputId, label, min, max, value, step=NULL, from_min, from_max){
  x <- sliderInput(inputId, label, min, max, value, step, animate=FALSE)
  x$children[[2]]$attribs <- c(x$children[[2]]$attribs, 
                               "data-from-min" = from_min, 
                               "data-from-max" = from_max 
  )
  x
}
theme_update(plot.title = element_text(hjust = 0.5))
theme_set(theme_bw(base_size = 15))


shinyServer(function(input, output,session) {
  
  
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Move the sliders to explore how different factors affect the population growth.",
      type = "info"
    )
  })
  observeEvent(input$info1,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Move the sliders to explore how different factors affect the population growth.",
      type = "info"
    )
  })
  
  observeEvent(input$go,{
    updateTabItems(session,"tabs","overview")
  })
  observeEvent(input$start,{
    updateTabItems(session,"tabs","explore")
  })
  
  
  ###########################################################################################
  #poisson distribution of birth and death population
  #birthPoisson = rpois(100,lambda = N[i-1]*input$birth_rate)
  logData_stoch <- vector("integer",24)
  expData_stoch <- vector("integer",24)
  geoData_stoch <- vector("integer",24)
  
  lambdaB <- vector("numeric",199)
  lambdaD <-  vector("numeric",199)
  birthPoisson<- vector("numeric",24)
  deathPoisson<- vector("numeric",24)
  birthPoisson_D<- vector("numeric",24)
  deathPoisson_D<- vector("numeric",24)
  birthPoisson_E<- vector("numeric",24)
  deathPoisson_E<- vector("numeric",24)
  birthPoisson_C<- vector("numeric",24)
  deathPoisson_C<- vector("numeric",24)
  birthPoisson_SD<- vector("numeric",24)
  deathPoisson_SD<- vector("numeric",24)
  birthPoisson_SDC<- vector("numeric",24)
  deathPoisson_SDC<- vector("numeric",24)
  birthPoisson_SDE<- vector("numeric",24)
  deathPoisson_SDE<- vector("numeric",24)
  
  
  month = 1:24
  
  
  #informaion  display
  output$inform <- renderText({
    paste("<font color=\"#246f67\"><b>","We starting with", input$start_size, "rabbits on grassland with a growth rate at", (input$brith_rate-input$death_rate)*100,"% per month", "</b></font>" )
  })
  
  output$inform2 <- renderText({
    
    paste("<font color=\"#246f67\"><b>","We starting with", input$start_size2, "rabbits on grassland with a growth rate at", (input$brith_rate2-input$death_rate2)*100,"% per month", "</b></font>" )
  })
  
  output$inform3 <- renderText({
    paste("<font color=\"#246f67\"><b>","We starting with", input$start_size3, "rabbits on grassland with a growth rate at", (input$brith_rate3-input$death_rate3)*100,"% per month", "</b></font>" )
  })
  
  #######################################################
  ############## plot exponential model #################
  #######################################################
  output$expGrowthPlot <- renderPlot({
    expData <- matrix(1:24)
    growth_rate2 = input$brith_rate2 -input$death_rate2
    expData[1] <- input$start_size2
    for(i in 2:24){
      expData[i] <- input$start_size2*exp(growth_rate2*i)
    }
    
    #plot(expData, ylim = c(0,25000), xlim  = c(0,24), type = "l", col = "red")
    
    #create stochasticity on birth and death on logistic growth 
    expData_stoch[1]<- expData[1]
    birthPoisson[1]<- 1
    deathPoisson[1]<- 1
    
    for(i in 2:24){
      lambdaB_ans = expData[i-1]*input$brith_rate2
      lambdaD_ans = expData[i-1]*input$death_rate2
      
      lambdaB[i-1]<-lambdaB_ans
      lambdaD[i-1]<-lambdaD_ans
      
      birthPoisson[i] <- rpois(1, lambda = lambdaB[i-1] )
      deathPoisson[i] <- rpois(1, lambda = lambdaD[i-1])
      
      
    }
    expData_stoch<- expData + birthPoisson - deathPoisson
    
    #when population access 0, it stays at 0
    for(i in 1:24){
      if(expData_stoch[i]<=0){
        expData_stoch[i:24] = 0
      }
    }
    
    if(input$add_disease == FALSE){
      #plot(expData, type = "l",col = "blue", xlab = "Month", ylab = "Population", main = "Exponential Population Growth ", ylim = c(0,25000), xlim  = c(0,24))
      #lines(expData_stoch,ylim = c(0,30000), col = "skyblue2", type = "p")
      
      expData.df <- as.data.frame(expData)
      expData_stoch.df <- as.data.frame(expData_stoch)
      
      
      #checkbox of adding or not adding estimated model
      if(input$expMod == TRUE){
        g <- ggplot(data = expData_stoch.df, aes(x=1:24,y = V1, colour = "Rabbits Population"))+
          geom_point(size =  3)+
          geom_line(size = 2)+
          labs(title = "Exponential Population Growth ",
               x = "Month",
               y = "Population")+
          coord_cartesian(ylim = c(0,125000))+
          geom_line(aes(x=1:24,y = V1, colour = "Estimated Rabbits Population"),expData.df)+
          scale_color_discrete(labels = c("Estimated Rabbits Population", "Rabbit Population    "))+
          theme(legend.position = "bottom")+
          theme(legend.title=element_blank(),legend.text=element_text(size=14))+
          theme(
            axis.title.x = element_text(size=16, face="bold"),
            axis.title.y = element_text(size=16, face="bold")
          )
          
        
        
        print(g)
      }
      else{
        
        g <- ggplot(data = expData_stoch.df, aes(x=1:24,y = V1, colour = "Rabbits Population"))+
          geom_point(size = 3)+
          geom_line(size = 2)+
          labs(title = "Exponential Population Growth ",
               x = "Month",
               y = "Population")+
          scale_color_discrete(labels = "Rabbit Population    ")+
          theme(legend.position = "bottom")+
          theme(legend.title=element_blank(),legend.text=element_text(size=14))+
          theme(
            axis.title.x = element_text(size=16, face="bold"),
            axis.title.y = element_text(size=16, face="bold")
          )
        
        
        print(g)
      }
      
      
    }
    #add_disease == TRUE
    else{        
      if(input$date ==  0  | input$disease == 0){
        expData.df <- as.data.frame(expData)
        expData_stoch.df <- as.data.frame(expData_stoch)
        
        
        #checkbox of adding or not adding estimated model
        if(input$expMod == TRUE){
          g <- ggplot(data = expData_stoch.df, aes(x=1:24,y = V1, colour ="Rabbit Population" ))+
            geom_point(size = 3)+
            geom_line(size = 2)+
            labs(title = "Exponential Population Growth ",
                 x = "Month",
                 y = "Population")+
            geom_line(aes(x=1:24,y = V1,"Estimated Rabbits Population" ),expData.df)+
            scale_color_discrete(labels = c("Estimated Rabbits Population", "Rabbit Population    "))+
            theme(legend.position = "bottom")+
            theme(legend.title=element_blank(),legend.text=element_text(size=14))+
            theme(
              axis.title.x = element_text(size=16, face="bold"),
              axis.title.y = element_text(size=16, face="bold")
            )
          
          print(g)
        }
        else{
          
          g <- ggplot(data = expData_stoch.df, aes(x=1:24,y = V1, colour = "Rabbits population"))+
            geom_point(size = 3)+
            geom_line(size = 2)+
            labs(title = "Exponential Population Growth ",
                 x = "Month",
                 y = "Population")+
            scale_color_discrete(labels = "Rabbit Population    ")+
            theme(legend.position = "bottom")+
            theme(legend.title=element_blank(),legend.text=element_text(size=14))+
            theme(
              axis.title.x = element_text(size=16, face="bold"),
              axis.title.y = element_text(size=16, face="bold")
            )
            
          print(g)
        }
      }else{
        #################################
        ######### With Disease ##########
        #################################
        expDiseaseData <- vector("integer",24)
        expDiseaseData[1] <- input$start_size2
        num <- input$date
        
        for(i in 2:num){
          disease_death_rate = input$death_rate2
          
          lambdaB_ans = expData_stoch[i-1]*input$brith_rate2
          lambdaD_ans = expData_stoch[i-1]*disease_death_rate
          
          lambdaB[i-1]<-lambdaB_ans
          lambdaD[i-1]<-lambdaD_ans
          
          birthPoisson[i] <- rpois(1, lambda = lambdaB[i-1] )
          deathPoisson[i] <- rpois(1, lambda = lambdaD[i-1])
          
          expDiseaseData[i]<- expData_stoch[i-1] + birthPoisson[i] - deathPoisson[i]
        }
        
        for(i in num:24){
          disease_death_rate = input$disease*0.005+input$death_rate2
          
          lambdaB_ans = expDiseaseData[i-1]*input$brith_rate2
          lambdaD_ans = expDiseaseData[i-1]*disease_death_rate
          
          lambdaB[i-1]<-lambdaB_ans
          lambdaD[i-1]<-lambdaD_ans
          
          birthPoisson[i] <- rpois(1, lambda = lambdaB[i-1] )
          deathPoisson[i] <- rpois(1, lambda = lambdaD[i-1])
          
          expDiseaseData[i]<- expDiseaseData[i-1] + birthPoisson[i] - deathPoisson[i]
        }
        
        #when population access 0, it stays at 0
        for(i in 1:24){
          if(expDiseaseData[i]<=0){
            expDiseaseData[i:24] = 0
          }
        }
        
        
        expData.df <- as.data.frame(expData)
        expDiseaseData.df <- as.data.frame(expDiseaseData)
        colnames(expDiseaseData.df) = "population"
        
        #checkbox of adding or not adding estimated model
        if(input$expMod == TRUE){
          g <-  ggplot(data = expDiseaseData.df, aes(x=1:24,y = population, colour = "Rabbit Population"))+
            geom_point(size = 3)+
            geom_line(size = 2)+
            labs(title = "Exponential Population Growth ",
                 x = "Month",
                 y = "Population")+
            geom_line(aes(x=1:24,y = V1,colour =  "Estimated Rabbits Population"), expData.df)+
            scale_color_discrete(labels = c("Estimated Rabbits Population", "Rabbit Population    "))+
            theme(legend.position = "bottom")+
            theme(legend.title=element_blank(),legend.text=element_text(size=14))+
            theme(
              axis.title.x = element_text(size=16, face="bold"),
              axis.title.y = element_text(size=16, face="bold")
            )
          print(g)
        }
        else{
          g <- ggplot(data = expDiseaseData.df, aes(x=1:24,y = population, colour = "Rabbit Population"))+
            geom_point(size = 3)+
            geom_line(size = 2)+
            labs(title = "Exponential Population Growth ",
                 x = "Month",
                 y = "Population")+
            scale_color_discrete(labels = "Rabbit Population    ")+
            theme(legend.position = "bottom")+
            theme(legend.title=element_blank(),legend.text=element_text(size=14))+
            theme(
              axis.title.x = element_text(size=16, face="bold"),
              axis.title.y = element_text(size=16, face="bold")
            )
          print(g)
          
          
          
          
          
        }
        
        
      }
    }
  })
  

  
  ##############################################################
  ##################### plot logistic model ####################
  ##############################################################
  
  output$logGrowthPlot <- renderPlot({
    #variation of carrying capacity
    
    var_capacity <- matrix(1:24)
    var_capacity[1] <- input$k_pop
    
    for(i in 2:24){
      var_capacity[i]  <-rnorm(1, mean = var_capacity[i-1], sd = input$k_pop*input$var_k_pop*0.01*0.1)
    }
    
    #rabbit logistic population growth plot 
    growth_rate3 = input$brith_rate3 -input$death_rate3
    logData <- matrix(1:24)
    logData[1] <- input$start_size3
    for(i in 2:24){
      
      changeInPop <- (input$brith_rate3 - input$death_rate3)*logData[i-1]*(1-(logData[i-1]/var_capacity[i-1]))
      logData[i] <- logData[i-1]+changeInPop
      
      if(logData[i]<=0){
        logData[i:24] = 0
      }
      #logData[i] <- (input$k_pop*input$start_size3)/(input$start_size3+(input$k_pop-input$start_size3)*exp(-(growth_rate3*i)))
    }
    
    #create stochasticity on birth and death on logistic growth 
    logData_stoch[1]<- logData[1]
    birthPoisson[1]<- 1
    deathPoisson[1]<- 1
    
    for(i in 2:24){
      lambdaB_ans = logData[i-1]*input$brith_rate3
      lambdaD_ans = logData[i-1]*input$death_rate3
      
      lambdaB[i-1]<-lambdaB_ans
      lambdaD[i-1]<-lambdaD_ans
      
      birthPoisson[i] <- rpois(1, lambda = lambdaB[i-1] )
      deathPoisson[i] <- rpois(1, lambda = lambdaD[i-1])
      
    }
    
    logData_stoch<- logData + birthPoisson - deathPoisson
    
    #when population access 0, it stays at 0
    for(i in 1:24){
      if(logData_stoch[i]<=0){
        logData_stoch[i:24] = 0
      }
    }
    
    #Case1: no factor added
    if(input$add_disease_log == FALSE  & input$add_competition == FALSE & input$add_resource == FALSE ){
      
      
      # if(input$expMod == TRUE)
      
      logData.df <- as.data.frame(logData)
      logData_stoch.df <- as.data.frame(logData_stoch)
      var_capacity.df <- as.data.frame(var_capacity)
      
      g <- ggplot(data = logData_stoch.df,aes(x=1:24,y = V1,colour = "Rabbit Population"))+
        geom_point(size = 3)+
        geom_line(size = 2)+
        labs(title = "Logistic Population Growth",
             x = "Month",
             y = "Population")+
        geom_line(aes(x=1:24,y = V1,colour = "Estimated Rabbits Population"),logData.df)+
        geom_line(aes(x=1:24,y = V1,colour = "Carrying Capacity"),var_capacity.df)+
        geom_hline(yintercept = input$k_pop, linetype = "dashed", color = "red")+
        scale_color_discrete(labels = c("Carrying Capacity    ","Estimated Rabbits Population  ", "Rabbit Population    "))+
        theme(legend.position = "bottom")+
        theme(legend.title=element_blank(),legend.text=element_text(size=14))+
        theme(
          axis.title.x = element_text(size=16, face="bold"),
          axis.title.y = element_text(size=16, face="bold")
        )
      
      print(g)
    }
    
    
    #Case2: only disease  added
    if(input$add_disease_log == TRUE  & input$add_competition == FALSE & input$add_resource == FALSE){
      ###################################################################
      ################## population growth with disease #################
      ###################################################################
      logDiseaseData <- vector("integer",24)
      logDiseaseData[1] <- logData_stoch[1]
      birthPoisson_D[1] = 1
      deathPoisson_D[1] = 1
      num <- input$date2 
      disease_death_rate = input$disease2*0.01+input$death_rate3
      
      logPopulation <- vector("integer",24)
      logPopulation[1] <-input$start_size3
      
      for(i in 2:num){
        changeInPop <- (input$brith_rate3 - input$death_rate3)*logPopulation[i-1]*(1-(logPopulation[i-1]/var_capacity[i-1]))
        logPopulation[i] <- logPopulation[i-1]+changeInPop
        
        if(logPopulation[i]<=0){
          logPopulation[i:24] = 0
        }
        
      }
      
      #create stochasticity for population before disease
      logData_stoch_disease <- vector("integer",24)
      logData_stoch_disease[1]<- logPopulation[1]
      birthPoisson_SD[1]<- 1
      deathPoisson_SD[1]<- 1
      
      for(i in 2:num){
        lambdaB_ans = logPopulation[i-1]*input$brith_rate3
        lambdaD_ans = logPopulation[i-1]*input$death_rate3
        
        lambdaB[i-1]<-lambdaB_ans
        lambdaD[i-1]<-lambdaD_ans
        
        birthPoisson_SD[i] <- rpois(1, lambda = lambdaB[i-1] )
        deathPoisson_SD[i] <- rpois(1, lambda = lambdaD[i-1])
       
        logData_stoch_disease[i]<- logPopulation[i] + birthPoisson_SD[i] - deathPoisson_SD[i] 
      }
      
      
      
      
      
      
      
      for (i in num:24) {
        changeInPop <- (input$brith_rate3 - disease_death_rate)*logPopulation[i-1]*(1-(logPopulation[i-1]/var_capacity[i-1]))
        logPopulation[i] <- logPopulation[i-1]+changeInPop
        
        if(logPopulation[i]<=0){
          logPopulation[i:24] = 0
        }
      }
      
      #create stochasticity for population after disease
      
      for(i in num:24){
        lambdaB_ans = logPopulation[i-1]*input$brith_rate3
        lambdaD_ans = logPopulation[i-1]*disease_death_rate
        
        lambdaB[i-1]<-lambdaB_ans
        lambdaD[i-1]<-lambdaD_ans
        
        birthPoisson_SD[i] <- rpois(1, lambda = lambdaB[i-1] )
        deathPoisson_SD[i] <- rpois(1, lambda = lambdaD[i-1])
        
        logData_stoch_disease[i]<- logPopulation[i] + birthPoisson_SD[i] - deathPoisson_SD[i]
      }
      
     
      
      
      
      
      logData.df <- as.data.frame(logData)
      logData_stoch_disease.df <- as.data.frame(logData_stoch_disease)
      colnames(logData_stoch_disease.df) = "population"
      var_capacity.df <- as.data.frame(var_capacity)
      
      g <- ggplot(data = logData_stoch_disease.df , aes(x=1:24,y = population, colour = "Rabbit Population with disease"))+
        geom_line(size = 2)+
        geom_point(size = 3)+
        labs(title = "Logistic Population Growth ",
             x = "Month",
             y = "Population")+
        geom_line(aes(x=1:24,y = V1,colour = "Estimated Rabbits Population"),logData.df)+
        geom_line(aes(x=1:24,y = V1,colour = "Carrying Capacity"),var_capacity.df)+
        scale_color_discrete(labels = c("Carrying Capacity    ","Estimated Rabbits Population  ", "Rabbit Population with disease    "))+
        theme(legend.position = "bottom")+
        theme(legend.title=element_blank(),legend.text=element_text(size=14))+
        theme(
          axis.title.x = element_text(size=16, face="bold"),
          axis.title.y = element_text(size=16, face="bold")
        )
      
      print(g)
      
      
      
      
      
    }
    
    
    
    
    
    #Case3: only  competition added
    if(input$add_disease_log == FALSE  & input$add_competition == TRUE & input$add_resource == FALSE){
      ########################################################################
      #################### Competitors Population Growth #####################
      ########################################################################
      compData <- matrix(1:24)
      compData_stoch <- vector("integer",24)
      #same starting population size 
      compData[1] <- input$start_size3
      for(i in 2:24){
        compData[i] <- (input$k_pop*input$start_size3)/(input$start_size3+(input$k_pop-input$start_size3)*exp(-(input$competition*i)))
      }
      
      #create stochasticity on birth and death on competitors growth 
      compData_stoch[1]<- compData[1]
      birthPoisson_C[1]<- 1
      deathPoisson_C[1]<- 1
      
      compDeath_rate <- input$brith_rate3 - input$competition
      
      for(i in 2:24){
        lambdaB_ans = logData[i-1]*input$brith_rate3
        lambdaD_ans = logData[i-1]*compDeath_rate
        
        lambdaB[i-1]<-lambdaB_ans
        lambdaD[i-1]<-lambdaD_ans
        
        birthPoisson_C[i] <- rpois(1, lambda = lambdaB[i-1] )
        deathPoisson_C[i] <- rpois(1, lambda = lambdaD[i-1])
        
      }
      compData_stoch<- compData + birthPoisson_C - deathPoisson_C
      
      
      
      for(i in 1:24){
        if(compData_stoch[i]<=0){
          compData_stoch[i:24] = 0
        }
      }
      
      if(input$competition != 0){
        
        for(i in 1:24){
          if(compData_stoch[i] + logData_stoch[i] >= var_capacity[i]){
            differenceVal <- ((compData_stoch[i] + logData_stoch[i]) - var_capacity[i])/2
            compData_stoch[i] <- compData_stoch[i] - differenceVal
            logData_stoch[i] <- logData_stoch[i] - differenceVal
            
            if(compData_stoch[i]<=0){
              compData_stoch[i:24] = 0 
            }
            
            if(logData_stoch[i]<=0){
              logData_stoch[i:24] = 0 
            }
          }
        }
        
        compData_stoch.df <- as.data.frame(compData_stoch)
        logData_stoch.df <- as.data.frame(logData_stoch)
        colnames(compData_stoch.df) = "population"
        var_capacity.df <- as.data.frame(var_capacity)
        
        g <- ggplot(data = compData_stoch.df , aes(x=1:24,y = population,colour = "Competitors Population"))+
          geom_point(size = 3)+
          geom_line(size =  2)+
          labs(title = "Logistic Population Growth ",
               x = "Month",
               y = "Population")+
          geom_line(aes(x=1:24,y = V1,colour = "Rabbits Population"),logData_stoch.df, size = 2)+
          geom_line(aes(x=1:24,y = V1,colour = "Carrying Capacity"),var_capacity.df)+
          scale_color_discrete(labels = c("Carrying Capacity    ","Competitors Population    ", "Rabbits Population    "))+
          theme(legend.position = "bottom")+
          theme(legend.title=element_blank(), legend.text=element_text(size=14))+
          theme(
            axis.title.x = element_text(size=16, face="bold"),
            axis.title.y = element_text(size=16, face="bold")
          )
        
        print(g)
        
      }
      
    }
    
    #Case4: only resource abundance added
    if(input$add_disease_log == FALSE  & input$add_competition == FALSE & input$add_resource == TRUE){
      
      
      
      ###########################################################################
      ############### plot population growth with resource abundance #############
      ###########################################################################
      updated_k_pop <- matrix(1:24)
      updated_k_pop[1] <-input$k_pop + input$environment*50
      for (i in 2:24) {
        updated_k_pop[i] <- rnorm(1, mean = updated_k_pop[i-1], sd = updated_k_pop[1]*input$var_k_pop*0.01*0.1)
      }
      
      growth_rate3 = input$brith_rate3 -input$death_rate3
      logData_env <- matrix(1:24)
      logData_stoch_env <- matrix(1:24)
      #populationLog = (input$k_pop*start_pop)/(start_pop+(input$k_pop-start_pop)*exp(-(growth_rate*0.01*month)))
      #P0 <- input$start_size
      logData_env[1] <- input$start_size3
      
      for(i in 2:24){
        logData_env[i] <- (updated_k_pop[i]*input$start_size3)/(input$start_size3+(updated_k_pop[i]-input$start_size3)*exp(-(growth_rate3*i)))
        if(logData_env[i]<=0){
          logData_env[i:24] = 0
        }
      }
      
      #create stochasticity on birth and death on logistic growth 
      logData_stoch_env[1]<- logData_env[1]
      birthPoisson_E[1]<- 1
      deathPoisson_E[1]<- 1
      
      for(i in 2:24){
        lambdaB_ans = logData[i-1]*input$brith_rate3
        lambdaD_ans = logData[i-1]*input$death_rate3
        
        lambdaB[i-1]<-lambdaB_ans
        lambdaD[i-1]<-lambdaD_ans
        
        birthPoisson_E[i] <- rpois(1, lambda = lambdaB[i-1] )
        deathPoisson_E[i] <- rpois(1, lambda = lambdaD[i-1])
        
      }
      logData_stoch_env<- logData_env+ birthPoisson_E - deathPoisson_E
      
      for(i in 1:24){
        if(logData_stoch_env[i]<=0){
          logData_stoch_env[i:24] = 0
        }
      }
      
      
      if(input$environment !=0){
        logData_env.df <- as.data.frame(logData_env)
        logData_stoch_env.df <- as.data.frame(logData_stoch_env)
        colnames(logData_stoch_env.df) = "population"
        updated_k_pop.df <- as.data.frame(updated_k_pop)
        
        g <- ggplot(data = logData_stoch_env.df , aes(x=1:24,y = population, colour = "Rabbits Population"))+
          geom_point(size = 3)+
          geom_line(size = 2)+
          labs(title = "Logistic Population Growth ",
               x = "Month",
               y = "Population")+
          geom_line(aes(x=1:24,y = V1,colour = "Estimated Rabbits Population"),logData_env.df)+
          geom_line(aes(x=1:24,y = V1,colour = "Carrying Capacity"),updated_k_pop.df)+
          scale_color_discrete(labels = c("Carrying Capacity    ","Rabbits Population    ", "Estimated Rabbits Population    "))+
          theme(legend.position = "bottom")+
          theme(legend.title=element_blank(),legend.text=element_text(size=14))+
          theme(
            axis.title.x = element_text(size=16, face="bold"),
            axis.title.y = element_text(size=16, face="bold")
          )
        
        print(g)
      }
    }
    
    
    #Case5: Both disease and competition added
    if(input$add_disease_log == TRUE  & input$add_competition == TRUE & input$add_resource == FALSE){
      ########################################################################
      #################### Competitors Population Growth #####################
      ########################################################################
      compData <- matrix(1:24)
      compData_stoch <- vector("integer",24)
      #same starting population size 
      compData[1] <- input$start_size3
      for(i in 2:24){
        compData[i] <- (input$k_pop*input$start_size3)/(input$start_size3+(input$k_pop-input$start_size3)*exp(-(input$competition*i)))
      }
      
      #create stochasticity on birth and death on competitors growth 
      compData_stoch[1]<- compData[1]
      birthPoisson_C[1]<- 1
      deathPoisson_C[1]<- 1
      
      compDeath_rate <- input$brith_rate3 - input$competition
      
      for(i in 2:24){
        lambdaB_ans = logData[i-1]*input$brith_rate3
        lambdaD_ans = logData[i-1]*compDeath_rate
        
        lambdaB[i-1]<-lambdaB_ans
        lambdaD[i-1]<-lambdaD_ans
        
        birthPoisson_C[i] <- rpois(1, lambda = lambdaB[i-1] )
        deathPoisson_C[i] <- rpois(1, lambda = lambdaD[i-1])
        
      }
      compData_stoch<- compData + birthPoisson_C - deathPoisson_C
      

      for(i in 1:24){
        if(compData_stoch[i]<=0){
          compData_stoch[i:24] = 0
        }
      }
      
      if(input$competition != 0){
        
        for(i in 1:24){
          if(compData_stoch[i] + logData_stoch[i] >= var_capacity[i]){
            differenceVal <- ((compData_stoch[i] + logData_stoch[i]) - var_capacity[i])/2
            compData_stoch[i] <- compData_stoch[i] - differenceVal
            logData_stoch[i] <- logData_stoch[i] - differenceVal
            
            if(compData_stoch[i]<=0){
              compData_stoch[i:24] = 0 
            }
            
            if(logData_stoch[i]<=0){
              logData_stoch[i:24] = 0 
            }
          }
        }
        
        
        ###################################################################
        ################## population growth with disease #################
        ###################################################################
        #compData_stoch
        #logData_stoch
        #logDiseaseData_comp -> after adding competitors, rabbits population with disease
        logDiseaseData_comp <- vector("integer",24)
        logDiseaseData_comp[1] <- input$start_size3
        num <- input$date2 
        disease_death_rate = input$disease2*0.01+input$death_rate3
        
        for(i in 2:num){
          changeInPop <- (input$brith_rate3 - input$death_rate3)*logData_stoch[i-1]*(1-(logData_stoch[i-1]/var_capacity[i-1]))
          logDiseaseData_comp[i] <- logData_stoch[i-1]+changeInPop
          
          if(logDiseaseData_comp[i]<=0){
            logDiseaseData_comp[i:24] = 0
          }
          
          #==================#
          #create stochasticity for population before disease
          logData_stoch_disease_comp <- vector("integer",24)
          logData_stoch_disease_comp[1]<- logDiseaseData_comp[1]
          birthPoisson_SDC[1]<- 1
          deathPoisson_SDC[1]<- 1
          
          for(i in 2:num){
            lambdaB_ans = logDiseaseData_comp[i-1]*input$brith_rate3
            lambdaD_ans = logDiseaseData_comp[i-1]*input$death_rate3
            
            lambdaB[i-1]<-lambdaB_ans
            lambdaD[i-1]<-lambdaD_ans
            
            birthPoisson_SDC[i] <- rpois(1, lambda = lambdaB[i-1] )
            deathPoisson_SDC[i] <- rpois(1, lambda = lambdaD[i-1])
            
            logData_stoch_disease_comp[i]<- logDiseaseData_comp[i] + birthPoisson_SDC[i] - deathPoisson_SDC[i] 
          }
        }
        for (i in num:24) {
          changeInPop <- (input$brith_rate3 - disease_death_rate)*logDiseaseData_comp[i-1]*(1-(logDiseaseData_comp[i-1]/var_capacity[i-1]))
          logDiseaseData_comp[i] <- logDiseaseData_comp[i-1]+changeInPop
          
          if(logDiseaseData_comp[i]<=0){
            logDiseaseData_comp[i:24] = 0
          }
          
        }
        
        #create stochasticity for population after disease
        
        for(i in num:24){
          lambdaB_ans = logDiseaseData_comp[i-1]*input$brith_rate3
          lambdaD_ans = logDiseaseData_comp[i-1]*disease_death_rate
          
          lambdaB[i-1]<-lambdaB_ans
          lambdaD[i-1]<-lambdaD_ans
          
          birthPoisson_SDC[i] <- rpois(1, lambda = lambdaB[i-1] )
          deathPoisson_SDC[i] <- rpois(1, lambda = lambdaD[i-1])
          
          logData_stoch_disease_comp[i]<- logDiseaseData_comp[i] + birthPoisson_SDC[i] - deathPoisson_SDC[i]
        }
        
        
        #####################################
        
        if(input$competition != 0){
          
          for(i in 1:24){
            if(compData_stoch[i] + logData_stoch_disease_comp[i] >= var_capacity[i]){
              differenceVal <- ((compData_stoch[i] + logData_stoch_disease_comp[i]) - var_capacity[i])/2
              compData_stoch[i] <- compData_stoch[i] - differenceVal
              logData_stoch_disease_comp[i] <- logData_stoch_disease_comp[i] - differenceVal
              
              if(compData_stoch[i]<=0){
                compData_stoch[i:24] = 0 
              }
              
              if(logData_stoch_disease_comp[i]<=0){
                logData_stoch_disease_comp[i:24] = 0 
              }
            }
          }
          
          
          logData.df <- as.data.frame(logData)
          logData_stoch_disease_comp.df <- as.data.frame(logData_stoch_disease_comp)
          
          
          compData_stoch.df <- as.data.frame(compData_stoch)
          
          colnames(compData_stoch.df) = "population"
          colnames(logData_stoch_disease_comp.df) = "population"
          var_capacity.df <- as.data.frame(var_capacity)
          
          g <- ggplot(data = compData_stoch.df , aes(x=1:24,y = population,colour = "Competitors Population"))+
            geom_point(size = 3)+
            geom_line(size =  2)+
            labs(title = "Logistic Population Growth ",
                 x = "Month",
                 y = "Population")+
            geom_line(aes(x=1:24,y = population,colour = "Rabbits Population"),logData_stoch_disease_comp.df, size = 2)+
            geom_line(aes(x=1:24,y = V1,colour = "Carrying Capacity"),var_capacity.df)+
            scale_color_discrete(labels = c("Carrying Capacity    ","Competitors Population    ", "Rabbits Population    "))+
            theme(legend.position = "bottom")+
            theme(legend.title=element_blank(),legend.text=element_text(size=14))+
            theme(
              axis.title.x = element_text(size=16, face="bold"),
              axis.title.y = element_text(size=16, face="bold")
            )
            
          
          print(g)
          
        }
        
      }
      
    }
    
    
    #Case6: Competition Factor  & environmental Factor
    if(input$add_disease_log == FALSE  & input$add_competition == TRUE & input$add_resource == TRUE){
      
      ###########################################################################
      ############### plot population growth with resource abundance ############
      ###########################################################################
      updated_k_pop <- matrix(1:24)
      updated_k_pop[1] <-input$k_pop + input$environment*50
      for (i in 2:24) {
        updated_k_pop[i] <- rnorm(1, mean = updated_k_pop[i-1], sd = updated_k_pop[1]*input$var_k_pop*0.01*0.1)
      }
      
      growth_rate3 = input$brith_rate3 -input$death_rate3
      logData_env_comp <- matrix(1:24)
      logData_stoch_env_comp <- matrix(1:24)
      compData_env <- matrix(1:24)
      compData_stoch_env <- vector("integer",24)
      #same starting population size
      compData_env[1] <- input$start_size3
      logData_env_comp[1] <- input$start_size3
      
      for(i in 2:24){
        logData_env_comp[i] <- (updated_k_pop[i]*input$start_size3)/(input$start_size3+(updated_k_pop[i]-input$start_size3)*exp(-(growth_rate3*i)))
        if(logData_env_comp[i]<=0){
          logData_env_comp[i:24] = 0
        }
      }
      
      #print(logData_env_comp)
      
      #create stochasticity on birth and death on logistic growth
      logData_stoch_env_comp[1]<- logData_env_comp[1]
      birthPoisson_E[1]<- 1
      deathPoisson_E[1]<- 1
      
      for(i in 2:24){
        lambdaB_ans = logData[i-1]*input$brith_rate3
        lambdaD_ans = logData[i-1]*input$death_rate3
        
        lambdaB[i-1]<-lambdaB_ans
        lambdaD[i-1]<-lambdaD_ans
        
        birthPoisson_E[i] <- rpois(1, lambda = lambdaB[i-1] )
        deathPoisson_E[i] <- rpois(1, lambda = lambdaD[i-1])
        
      }
      logData_stoch_env_comp<- logData_env_comp+ birthPoisson_E - deathPoisson_E
      
      for(i in 1:24){
        if(logData_stoch_env_comp[i]<=0){
          logData_stoch_env_comp[i:24] = 0
        }
      }
      
      #Competitors population
      for(i in 2:24){
        compData_env[i] <- (updated_k_pop[i]*input$start_size3)/(input$start_size3+(updated_k_pop[i]-input$start_size3)*exp(-(input$competition*i)))
      }
      
      #create stochasticity on birth and death on competitors growth
      compData_stoch_env[1]<- compData_env[1]
      birthPoisson_C[1]<- 1
      deathPoisson_C[1]<- 1
      
      compDeath_rate <- input$brith_rate3 - input$competition
      
      for(i in 2:24){
        lambdaB_ans = logData[i-1]*input$brith_rate3
        lambdaD_ans = logData[i-1]*compDeath_rate
        
        lambdaB[i-1]<-lambdaB_ans
        lambdaD[i-1]<-lambdaD_ans
        
        birthPoisson_C[i] <- rpois(1, lambda = lambdaB[i-1] )
        deathPoisson_C[i] <- rpois(1, lambda = lambdaD[i-1])
        
      }
      compData_stoch_env<- compData_env + birthPoisson_C - deathPoisson_C
      
      
      
      for(i in 1:24){
        if(compData_stoch_env[i]<=0){
          compData_stoch_env[i:24] = 0
        }
      }
      
      
      
      for(i in 1:24){
        if(compData_stoch_env[i] + logData_stoch_env_comp[i] >= updated_k_pop[i]){
          differenceVal <- ((compData_stoch_env[i] + logData_stoch_env_comp[i]) - updated_k_pop[i])/2
          compData_stoch_env[i] <- compData_stoch_env[i] - differenceVal
          logData_stoch_env_comp[i] <- logData_stoch_env_comp[i] - differenceVal
          
          if(compData_stoch_env[i]<=0){
            compData_stoch_env[i:24] = 0
          }
          
          if(logData_stoch_env_comp[i]<=0){
            logData_stoch_env_comp[i:24] = 0
          }
        }
      }
      
      
      compData_stoch_env.df <- as.data.frame(compData_stoch_env)
      colnames(compData_stoch_env.df) = "population"
      logData_stoch_env_comp.df <- as.data.frame(logData_stoch_env_comp)
      colnames(logData_stoch_env_comp.df) = "population"
      updated_k_pop.df <- as.data.frame(updated_k_pop)
      
      
      
      g <- ggplot(data = compData_stoch_env.df , aes(x=1:24,y = population,colour = "Competitors Population"))+
        geom_point(size = 3)+
        geom_line(size =  2)+
        labs(title = "Logistic Population Growth ",
             x = "Month",
             y = "Population")+
        geom_line(aes(x=1:24,y = population,colour = "Rabbits Population"),logData_stoch_env_comp.df, size = 2)+
        geom_line(aes(x=1:24,y = V1,colour = "Carrying Capacity"),updated_k_pop.df)+
        scale_color_discrete(labels = c("Carrying Capacity    ","Competitors Population    ", "Rabbits Population    "))+
        theme(legend.position = "bottom")+
        theme(legend.title=element_blank())+
        theme(
          axis.title.x = element_text(size=16, face="bold"),
          axis.title.y = element_text(size=16, face="bold")
        )
      print(g)
      
      
      
      
      
      
    }
    
    #Case7: Both disease and environmental factors added
    if(input$add_disease_log == TRUE  & input$add_competition == FALSE & input$add_resource == TRUE){
      ###########################################################################
      ############### plot population growth with resource abundance #############
      ###########################################################################
      updated_k_pop <- matrix(1:24)
      updated_k_pop[1] <-input$k_pop + input$environment*50
      for (i in 2:24) {
        updated_k_pop[i] <- rnorm(1, mean = updated_k_pop[i-1], sd = updated_k_pop[1]*input$var_k_pop*0.01*0.1)
      }
      
      growth_rate3 = input$brith_rate3 -input$death_rate3
      logData_env <- matrix(1:24)
      logData_stoch_env <- matrix(1:24)
      logData_env[1] <- input$start_size3
      
      for(i in 2:24){
        logData_env[i] <- (updated_k_pop[i]*input$start_size3)/(input$start_size3+(updated_k_pop[i]-input$start_size3)*exp(-(growth_rate3*i)))
        if(logData_env[i]<=0){
          logData_env[i:24] = 0
        }
      }
      
      #create stochasticity on birth and death on logistic growth 
      logData_stoch_env[1]<- logData_env[1]
      birthPoisson_E[1]<- 1
      deathPoisson_E[1]<- 1
      
      for(i in 2:24){
        lambdaB_ans = logData[i-1]*input$brith_rate3
        lambdaD_ans = logData[i-1]*input$death_rate3
        
        lambdaB[i-1]<-lambdaB_ans
        lambdaD[i-1]<-lambdaD_ans
        
        birthPoisson_E[i] <- rpois(1, lambda = lambdaB[i-1] )
        deathPoisson_E[i] <- rpois(1, lambda = lambdaD[i-1])
        
      }
      logData_stoch_env<- logData_env+ birthPoisson_E - deathPoisson_E
      
      for(i in 1:24){
        if(logData_stoch_env[i]<=0){
          logData_stoch_env[i:24] = 0
        }
      }
      
      
      ###################################################################
      ################## population growth with disease #################
      ###################################################################
    
      num <- input$date2 
      disease_death_rate = input$disease2*0.01+input$death_rate3 #porportion of the animal death
      
      logPopulation <- vector("integer",24)
      logPopulation[1] <-input$start_size3
      
      for(i in 2:num){
        changeInPop <- (input$brith_rate3 - input$death_rate3)*logPopulation[i-1]*(1-(logPopulation[i-1]/updated_k_pop[i-1]))
        logPopulation[i] <- logPopulation[i-1]+changeInPop
        
        if(logPopulation[i]<=0){
          logPopulation[i:24] = 0
        }
        
      }
      
      #==================#
      #create stochasticity for population before disease
      logPopulation_disease <- vector("integer",24)
      logPopulation_disease[1]<- logPopulation[1]
      birthPoisson_SDE[1]<- 1
      deathPoisson_SDE[1]<- 1
      
      for(i in 2:num){
        lambdaB_ans = logPopulation_disease[i-1]*input$brith_rate3
        lambdaD_ans = logPopulation_disease[i-1]*input$death_rate3
        
        lambdaB[i-1]<-lambdaB_ans
        lambdaD[i-1]<-lambdaD_ans
        
        birthPoisson_SDE[i] <- rpois(1, lambda = lambdaB[i-1] )
        deathPoisson_SDE[i] <- rpois(1, lambda = lambdaD[i-1])
        
        logPopulation_disease[i]<- logPopulation[i] + birthPoisson_SDE[i] - deathPoisson_SDE[i] 
      }
      #=============#
      
      for (i in num:24) {
        changeInPop <- (input$brith_rate3 - disease_death_rate)*logPopulation[i-1]*(1-(logPopulation[i-1]/updated_k_pop[i-1]))
        logPopulation[i] <- logPopulation[i-1]+changeInPop
        
        if(logPopulation[i]<=0){
          logPopulation[i:24] = 0
        }
      }
      
      #create stochasticity for population after disease
      
      for(i in num:24){
        lambdaB_ans = logPopulation_disease[i-1]*input$brith_rate3
        lambdaD_ans = logPopulation_disease[i-1]*disease_death_rate
        
        lambdaB[i-1]<-lambdaB_ans
        lambdaD[i-1]<-lambdaD_ans
        
        birthPoisson_SDE[i] <- rpois(1, lambda = lambdaB[i-1] )
        deathPoisson_SDE[i] <- rpois(1, lambda = lambdaD[i-1])
        
        logPopulation_disease[i]<- logPopulation[i] + birthPoisson_SDE[i] - deathPoisson_SDE[i]
      }
      
      
      logData.df <- as.data.frame(logData)
      logPopulation_disease.df <- as.data.frame(logPopulation_disease)
      colnames(logPopulation_disease.df) = "population"
      updated_k_pop.df <- as.data.frame(updated_k_pop)
      
      g <- ggplot(data = logPopulation_disease.df , aes(x=1:24,y = population, colour = "Rabbits Population"))+
        geom_point(size = 3)+
        geom_line(size = 2)+
        labs(title = "Logistic Population Growth ",
             x = "Month",
             y = "Population")+
        geom_line(aes(x=1:24,y = V1, colour = "Estimated Rabbits Population"),logData.df)+
        geom_line(aes(x=1:24,y = V1, colour = "Carrying Capacity"),updated_k_pop.df)+
        scale_color_discrete(labels = c("Carrying Capacity    ","Estimated Rabbits Population    ", "Rabbits Population    "))+
        theme(legend.position = "bottom")+
        theme(legend.title=element_blank(),legend.text=element_text(size=14))+
        theme(
          axis.title.x = element_text(size=16, face="bold"),
          axis.title.y = element_text(size=16, face="bold")
        )
      
      print(g)
    }
    
    #Case8:all factors added
    if(input$add_disease_log == TRUE  & input$add_competition == TRUE & input$add_resource == TRUE){
      ###########################################################################
      ############### plot population growth with resource abundance #############
      ###########################################################################
      updated_k_pop <- matrix(1:24)
      updated_k_pop[1] <-input$k_pop + input$environment*50
      for (i in 2:24) {
        updated_k_pop[i] <- rnorm(1, mean = updated_k_pop[i-1], sd = updated_k_pop[1]*(input$var_k_pop*0.01)*0.1)
      }
      
      growth_rate3 = input$brith_rate3 -input$death_rate3
      logData_env <- matrix(1:24)
      logData_stoch_env <- matrix(1:24)
      logData_env[1] <- input$start_size3
      
      for(i in 2:24){
        logData_env[i] <- (updated_k_pop[i]*input$start_size3)/(input$start_size3+(updated_k_pop[i]-input$start_size3)*exp(-(growth_rate3*i)))
        if(logData_env[i]<=0){
          logData_env[i:24] = 0
        }
      }
      
      #create stochasticity on birth and death on logistic growth 
      logData_stoch_env[1]<- logData_env[1]
      birthPoisson_E[1]<- 1
      deathPoisson_E[1]<- 1
      
      for(i in 2:24){
        lambdaB_ans = logData[i-1]*input$brith_rate3
        lambdaD_ans = logData[i-1]*input$death_rate3
        
        lambdaB[i-1]<-lambdaB_ans
        lambdaD[i-1]<-lambdaD_ans
        
        birthPoisson_E[i] <- rpois(1, lambda = lambdaB[i-1] )
        deathPoisson_E[i] <- rpois(1, lambda = lambdaD[i-1])
        
      }
      logData_stoch_env<- logData_env+ birthPoisson_E - deathPoisson_E
      
      for(i in 1:24){
        if(logData_stoch_env[i]<=0){
          logData_stoch_env[i:24] = 0
        }
      }
      
      #### add disease to rabbit population###
      ###################################################################
      ################## population growth with disease #################
      ###################################################################
      num <- input$date2 
      disease_death_rate = input$disease2*0.01+input$death_rate3
      
      logPopulation <- vector("integer",24)
      logPopulation[1] <-input$start_size3
      
      for(i in 2:num){
        changeInPop <- (input$brith_rate3 - input$death_rate3)*logPopulation[i-1]*(1-(logPopulation[i-1]/updated_k_pop[i-1]))
        logPopulation[i] <- logPopulation[i-1]+changeInPop
        
        if(logPopulation[i]<=0){
          logPopulation[i:24] = 0
        }
        
      }
      for (i in num:24) {
        changeInPop <- (input$brith_rate3 - disease_death_rate)*logPopulation[i-1]*(1-(logPopulation[i-1]/updated_k_pop[i-1]))
        logPopulation[i] <- logPopulation[i-1]+changeInPop
        
        if(logPopulation[i]<=0){
          logPopulation[i:24] = 0
        }
      }
      
      
      compData <- matrix(1:24)
      compData_stoch <- vector("integer",24)
      #same starting population size 
      compData[1] <- input$start_size3
      for(i in 2:24){
        compData[i] <- (input$k_pop*input$start_size3)/(input$start_size3+(input$k_pop-input$start_size3)*exp(-(input$competition*i)))
      }
      
      #create stochasticity on birth and death on competitors growth 
      compData_stoch[1]<- compData[1]
      birthPoisson_C[1]<- 1
      deathPoisson_C[1]<- 1
      
      compDeath_rate <- input$brith_rate3 - input$competition
      
      for(i in 2:24){
        lambdaB_ans = logData[i-1]*input$brith_rate3
        lambdaD_ans = logData[i-1]*compDeath_rate
        
        lambdaB[i-1]<-lambdaB_ans
        lambdaD[i-1]<-lambdaD_ans
        
        birthPoisson_C[i] <- rpois(1, lambda = lambdaB[i-1] )
        deathPoisson_C[i] <- rpois(1, lambda = lambdaD[i-1])
        
      }
      compData_stoch<- compData + birthPoisson_C - deathPoisson_C
      
      
      
      for(i in 1:24){
        if(compData_stoch[i]<=0){
          compData_stoch[i:24] = 0
        }
      }
      
      if(input$competition != 0){
        
        for(i in 1:24){
          if(compData_stoch[i] + logPopulation[i] >= var_capacity[i]){
            differenceVal <- ((compData_stoch[i] + logPopulation[i]) - var_capacity[i])/2
            compData_stoch[i] <- compData_stoch[i] - differenceVal
            logPopulation[i] <- logPopulation[i] - differenceVal
            
            if(compData_stoch[i]<=0){
              compData_stoch[i:24] = 0 
            }
            
            if(logPopulation[i]<=0){
              logPopulation[i:24] = 0 
            }
          }
        }
      }
      
      ####################################################################
      ################### population growth with disease #################
      ####################################################################
    
      logData.df <- as.data.frame(logData)
      #logDiseaseData_comp.df <- as.data.frame(logDiseaseData_comp)
      #logData.df <- as.data.frame(logData)
      logPopulation.df <- as.data.frame(logPopulation)#disease rabbit population
      colnames(logPopulation.df) = "population"
      updated_k_pop.df <- as.data.frame(updated_k_pop)
      
      compData_stoch.df <- as.data.frame(compData_stoch)
      
      colnames(compData_stoch.df) = "population"
      #colnames(logDiseaseData_comp.df) = "population"
      var_capacity.df <- as.data.frame(var_capacity)
      
      g <- ggplot(data = compData_stoch.df , aes(x=1:24,y = population,colour = "Competitors Population"))+
        geom_point(size = 3)+
        geom_line(size =  2)+
        labs(title = "Logistic Population Growth ",
             x = "Month",
             y = "Population")+
        geom_line(aes(x=1:24,y = population,colour = "Rabbits Population"),logPopulation.df, size = 2)+
        geom_line(aes(x=1:24,y = V1,colour = "Carrying Capacity"),var_capacity.df)+
        scale_color_discrete(labels = c("Carrying Capacity    ","Competitors Population    ", "Rabbits Population    "))+
        theme(legend.position = "bottom")+
        theme(legend.title=element_blank(),legend.text=element_text(size=14))+
        theme(
          axis.title.x = element_text(size=16, face="bold"),
          axis.title.y = element_text(size=16, face="bold")
        )
      
      print(g)
      
    }
    
    
  })
  
  
  
  ############################################################
  ##################### plot geometric growth ################
  ############################################################
  output$geoGrowthPlot <- renderPlot({
    N1 <- input$start_size
    #N2 <- N1 + (input$brith_rate - input$death_rate)*N1
    
    #Create empty matix
    geoData <- matrix(1:24)
    geoData[1] <- N1
    #create 24 population data
    for(i in 2:24){
      geoData[i] <- geoData[i-1] + (input$brith_rate - input$death_rate)*geoData[i-1]
    }
    
    #create stochasticity on birth and death on logistic growth 
    geoData_stoch[1]<- geoData[1]
    birthPoisson[1]<- 1
    deathPoisson[1]<- 1
    
    for(i in 2:24){
      lambdaB_ans = geoData[i-1]*input$brith_rate
      lambdaD_ans = geoData[i-1]*input$death_rate
      
      lambdaB[i-1]<-lambdaB_ans
      lambdaD[i-1]<-lambdaD_ans
      
      birthPoisson[i] <- rpois(1, lambda = lambdaB[i-1] )
      deathPoisson[i] <- rpois(1, lambda = lambdaD[i-1])
      
    }
    geoData_stoch<- geoData + birthPoisson - deathPoisson
    
    for(i in 1:24){
      if(geoData_stoch[i]<=0){
        geoData_stoch[i:24] = 0
      }
    }
    
    geoData.df <- as.data.frame(geoData)
    geoData_stoch.df <- as.data.frame(geoData_stoch)
    g <- ggplot(data = geoData_stoch.df, aes(x=1:24,y = V1))+
      geom_point(color = "skyblue")+
      labs(title = "Geometic Population Growth ",
           x = "Month",
           y = "Population")+
      geom_line(aes(x=1:24,y = V1),geoData.df, color = "blue")
    print(g)
    
  })
  ######################################################
  ################ Deterministic #######################
  ######################################################
  output$deterPlot <- renderPlot({
    if(input$add_k == FALSE){
      N1 <- input$start_size
      #N2 <- N1 + (input$brith_rate - input$death_rate)*N1
      
      #Create empty matix
      geoData <- matrix(1:24)
      geoData[1] <- N1
      #create 24 population data
      for(i in 2:24){
        geoData[i] <- geoData[i-1] + (input$brith_rate - input$death_rate)*geoData[i-1]
      }
      
      geoData.df <- as.data.frame(geoData)
      
      g <- ggplot(data = geoData.df, aes(x=1:24,y = V1, colour = "Rabbits Population"))+
        geom_point(size = 3)+
        geom_line(size = 2)+
        labs(title = "Geometic Population Growth ",
             x = "Month",
             y = "Population")+
        scale_color_discrete(labels = c("Rabbit Population"))+
        theme(legend.position = "bottom")+
        theme(legend.title=element_blank(),legend.text=element_text(size=14))+
        theme(
          axis.title.x = element_text(size=16, face="bold"),
          axis.title.y = element_text(size=16, face="bold")
        )
        
      
      print(g)
    }
    
    if(input$add_k  == TRUE){
      growth_rate = input$brith_rate -input$death_rate
      logData <- matrix(1:24)
      
      
      logData[1] <- input$start_size
      for(i in 2:24){
        
        changeInPop <- (input$brith_rate - input$death_rate)*logData[i-1]*(1-(logData[i-1]/input$k_slider))
        logData[i] <- logData[i-1]+changeInPop
        
        if(logData[i]<=0){
          logData[i:24] = 0
        }
        #logData[i] <- (input$k_pop*input$start_size3)/(input$start_size3+(input$k_pop-input$start_size3)*exp(-(growth_rate3*i)))
      }
      logData.df <- as.data.frame(logData)
      
      ggplot(data = logData.df, aes(x=1:24,y = V1, colour = "Rabbits Population"))+
        geom_point(size = 3)+
        geom_line(size = 2)+
        labs(title = "Logistic Population Growth ",
             x = "Month",
             y = "Population")+
        geom_hline(yintercept = input$k_slider, linetype = "dashed", color = "red")+
        scale_color_discrete(labels = c("Rabbit Population", "Carrying Capacity"))+
        theme(legend.position = "bottom")+
        theme(legend.title=element_blank(),legend.text=element_text(size=14))+
        theme(
          axis.title.x = element_text(size=16, face="bold"),
          axis.title.y = element_text(size=16, face="bold")
        )
      
      
    }
    
  })
  
  #checkbox control carrying capacity slider
  output$kControl <- renderUI({
    
    if(input$add_k == TRUE){
      list(
        setSliderColor("#3CBAAD",1),
        sliderInput(
          "k_slider",
          "Carrying Capacity",
          min = 500,
          max = 9000,
          value = 50
        )
      )
    }else{
      updateSliderInput(session = session, "k_slider", value =  0)
    }
    
    
  })
  
  #checkbox control disease factor in exponential growth
  output$disease_exp <- renderUI({
    if(input$add_disease == TRUE){
      list(
        sliderInput("date",
                    "Disease Starting Month:",
                    min = 0,
                    max = 24,
                    value = 0,
                    step = 1),
        sliderInput("disease",
                    "Disease Severity:(Proportion of population exposed to the disease)",
                    min = 0,
                    max = 50,
                    value = 0,
                    step = 1)
      )
    }else{
      updateSliderInput(session = session, "date", value =  0)
      updateSliderInput(session = session, "disease", value =  0)
    }
    
  })
  
  
  
  #checkbox control competition factor 
  
  output$competition_added <- renderUI({
    if(input$add_competition == TRUE){
      sliderInput("competition",
                  "Competitor's Growth Rate:",
                  min = -0.5,
                  max = 0.5,
                  value = 0.01,
                  step = 0.005)
      
    }else{
      updateSliderInput(session = session, "competition", value =  0)
    }
    
  })
  
  #checkbox control disease factor in logistic population growth
  output$disease_added_log <- renderUI({
    if(input$add_disease_log == TRUE){
      list(
        sliderInput3("date2",
                     "Disease Starting Month:",
                     min = 0,
                     max = 24,
                     value = 2,
                     from_min = 2,
                     from_max = 24,
                     step = 1),
        sliderInput3("disease2",
                     "Disease Severity:(Proportion of population exposed to the disease)",
                     min = 0,
                     max = 50,
                     value = 1,
                     from_min = 1,
                     from_max = 50,
                     step = 1)
      )
    }else{
      updateSliderInput(session = session, "date2", value =  0)
      updateSliderInput(session = session, "disease2", value =  0)
    }
  })
  
  #checkbox control resource abundance factor 
  output$resource_added <- renderUI({
    if(input$add_resource == TRUE){
      sliderInput("environment",
                  "Resource abundance:",
                  min = -50,
                  max = 50,
                  value = 0.5,
                  step = 1)
      
    }else{
      updateSliderInput(session = session, "environment", value =  0)
    }
  })
  
  
  output$lobster_pop <- renderPlot({
    #Set lobster production rate to 0.4
    #Set carrying capacity to 8 million = 8000 thousand
    generation = 2013-1981 #33
    lobsterData <- matrix(1:generation)
    lobsterData[1] <- 1000  #starting population
    growthrate <- 0.4
    generation <- 33
    carryingCap <-  8000
    #create 33 population data
    
    #Lobser logistic population growth plot    
    for(i in 2:generation){
      
      lobsterData[i] <- (carryingCap*1000)/(1000+(8000-1000)*exp(-(0.4*i)))
      # changeInPop <- growthrate*lobsterData[i-1]*(1-(lobsterData[i-1]/carryingCap))
      # lobsterData[i] <- lobsterData[i-1]+changeInPop
      
      if(lobsterData[i]<=0){
        lobsterData[i:24] = 0
      }
      
    }
    
    
    lobsterData.df <- as.data.frame(lobsterData)
    colnames(lobsterData.df) =  "population"
    
    
    g <- ggplot(data = lobsterData.df, aes(x=1:generation,y = population))+
      geom_line(color = "skyblue")+
      labs(title = "Lobster Population Growth ",
           x = "generation",
           y = "Population(thousands)")
    
    print(g)
    
    
    
    
  })
  

  
})
