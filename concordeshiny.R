#*****************************************************************************#                                                                             # Program name: MUK11.R                                                       #
#                                                                             #
#	Purpose:  R shiny app to display CONCORDE simulations	results               #
#                                                                             #
# Trial statistician: Rachel Phillip                                          #
#                                                                             #
#                                                                             #
# Date created: 26/3/2021                                                     #
#                                                                             #
# Date last modified: 08/07/21                                                #
#                                                                             #
# Datasets created: None                                                      #
#                                                                             #
# Output: None                                                                #
#                                                                             #
# Reviewed by: Rachel Phillip                                                 #
#                                                                             #
# Date reviewed: 08/07/2021                                                   #
#                                                                             #
#*****************************************************************************#

rm(list=ls())

#install.packages("shiny")
library(shiny)
#install.packages("flextable")
library(flextable)
library(magrittr)
#install.packages('dplyr')
library(dplyr)
#install.packages('tibble')
library(tibble)


###########
### UI ####
###########

ui <- fluidPage(
  
  # App title ----
  headerPanel("CONCORDE Simulation Results"),
  
  
  # Sidebar panel with inputs ----
  sidebarPanel(
    
    radioButtons("data", "Please pick a dataset:", c("CONCORDE-A: Uniform time to toxicity", "CONCORDE-A: Truncated normal time to toxicity", "CONCORDE-B: Uniform time to toxicity", "CONCORDE-B: Truncated normal time to toxicity")),
    
    checkboxGroupInput("rate", "Rate of recruitment:", c("Fast: 4 patients per month", "Average: 2 patients per month", "Slow: Less than 1 patient per month")),

    uiOutput("scen"),
    

    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    
    h1("Trial introduction"),
    p("CONCORDE is the first phase I drug-radiotherapy combination platform in NSCLC, designed to assess 
      multiple different DNA Damage Response inhibitors (DDRi) in combination with radical thoracic RT. TiTE-CRM methodology inform the dose 
      escalation individually for each DDRi-RT combination and a randomised calibration arm aids with the attribution 
      of toxicities.The primary objective is to determine the RP2D for each DDRi in combination with radical thoracic RT of 60Gy in 30 fractions for patients with locally advanced NSCLC."),
    
    HTML("<p>For more information on the CONCORDE trial please see the <a href='https://www.ctro.science/article/S2405-6308(20)30078-1/fulltext'> CONCORDE protocol paper</a>.</p>"),
    
    h1("Simulations summary"),
    
    p("Simulations were performed to assess the operating characteristics of the CONCORDE design under a number of scenarios (see Toxicity scenario section below). 
      Rates of recruitment were important to consider given the time to event nature of toxicity events, 
      3 differing scenarios representing fast, average and slow recruitment were simulated. 
      All simulations used 5000 replicates and were performed using edited code from the R package, dfcrm."),
    
    
    p("For more information on the CONCORDE simulations please see the CONCORDE TiTE-CRM methodology paper."),
    # Hyperlink in future?
    
    h2("Toxicity scenarios"),
    
    p("The following toxicity scenarios were simulated:"),
    
    HTML("<ul><li> <b>Scenario 1:</b> All doses considered to be safe </li><li> <b>Scenario 2:</b> All doses are too toxic </li>
    <li> <b>Scenario 3:</b> Dose levels 3 or 4 considered to be the RP2D </li><li> <b>Scenario 4:</b> Dose level 2 is the RP2D </li>
    <li> <b>Scenario 5:</b> Dose level 1 is the RP2D </li><li> <b>Scenario 6 (CONCORDE-A):</b> Dose level 0.5 is the RP2D </li>
         <li><b>Scenario 6 (CONCORDE-B):</B> Intermittent dose level 2a is the RP2D</li></ul>"),
    
    h1("Simulation results"),
    
    tableOutput("t11"),
    tableOutput("t12"),
    tableOutput("t13"),
    tableOutput("t14"),
    tableOutput("t15"),
    tableOutput("t16"),
    
    tableOutput("t21"),
    tableOutput("t22"),
    tableOutput("t23"),
    tableOutput("t24"),
    tableOutput("t25"),
    tableOutput("t26"),

    tableOutput("t31"),
    tableOutput("t32"),
    tableOutput("t33"),
    tableOutput("t34"),
    tableOutput("t35"),
    tableOutput("t36"),
  )
)


#############
### Server ##
#############


server <- function(input, output){
    
  
  output$scen <- renderUI({
    if ((input$data == "CONCORDE-A: Uniform time to toxicity") | (input$data == "CONCORDE-A: Truncated normal time to toxicity")){
      checkboxGroupInput("scenario", "Toxicity scenario:", c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5", "Scenario 6"))
    } else if ((input$data == "CONCORDE-B: Uniform time to toxicity") | (input$data == "CONCORDE-B: Truncated normal time to toxicity")){
      checkboxGroupInput("scenario", "Toxicity scenario:", c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5", "Scenario 6"))
    }
  })
  

  
  
  load_data <- function(data){
    
    if (data=="CONCORDE-A: Uniform time to toxicity"){
      #load("P:/CTRU/Projects/Cancer/Early phase/CONCORDE/Stats/01_Trial Design/01_Design_documentation/Dose simulations/Programs/R Workspace/Simulations1_olap_6doses_FINAL_20200731.RData", envir = .GlobalEnv)
      load(url("https://github.com/rachelphillipctru/concordesims/raw/main/Simulations1_olap_6doses_FINAL_20210702.RData"), envir = .GlobalEnv)
    } else if (data=="CONCORDE-A: Truncated normal time to toxicity"){
      #load("P:/CTRU/Projects/Cancer/Early phase/CONCORDE/Stats/01_Trial Design/01_Design_documentation/Dose simulations/Programs/R Workspace/Simulations1_olap_6doses_truncnorm_FINAL_20200730.RData", envir = .GlobalEnv)
      load(url("https://github.com/rachelphillipctru/concordesims/raw/main/Simulations1_olap_6doses_truncnorm_FINAL_20210702.RData"), envir = .GlobalEnv)
    } else if (data=="CONCORDE-B: Uniform time to toxicity"){
      #load("P:/CTRU/Projects/Cancer/Early phase/CONCORDE/Stats/01_Trial Design/01_Design_documentation/Dose simulations/Programs/R Workspace/Simulations3_10doses_FINAL_20200805.RData", envir = .GlobalEnv)
      load(url("https://github.com/rachelphillipctru/concordesims/raw/main/Simulations3_10doses_FINAL_20210702.RData"), envir = .GlobalEnv)
    } else {
      #load("P:/CTRU/Projects/Cancer/Early phase/CONCORDE/Stats/01_Trial Design/01_Design_documentation/Dose simulations/Programs/R Workspace/Simulations3_10doses_truncnorm_FINAL_20200806.RData", envir = .GlobalEnv)
      load(url("https://github.com/rachelphillipctru/concordesims/raw/main/Simulations3_10doses_truncnorm_FINAL_20210702.RData"), envir = .GlobalEnv)
    }
  }
  
  observeEvent(input$data, {
    load_data(input$data)
  })
  



  simfunction<-function(dataset){
    
    if ((input$data == "CONCORDE-A: Uniform time to toxicity") | (input$data == "CONCORDE-A: Truncated normal time to toxicity")){
      doses <- c("-1","0.5","1","2","3","4")
    } else{
      doses <- c("-1","1","1a","2","2a","3","3a","4","4a","5")
    }
    
    simout<-rbind(doses,round(dataset$PI,3),
                 format(round(dataset$MTD,3),nsmall=2),
                 format(round(dataset$level,3),nsmall=2),
                 format(round(dataset$tox,3),nsmall=2),
                 format(round(dataset$tox/dataset$level,3),nsmall=2))
    rownames(simout)<-c("Dose level","True probability of toxicity","P(selected as MTD)",
                      "Number of patients treated","Number of DLTs observed","DLTs/patients")
    a<-rownames(simout)
    simout2<-as.data.frame(simout)

  }
  
  tab11 <- reactive({
    t11 <- simfunction(sims1_1)
  })
  tab12 <- eventReactive(input$data, {
    t12 <- simfunction(sims1_2)
  })
  tab13 <- eventReactive(input$data, {
    t13 <- simfunction(sims1_3)
  })
  tab14 <- eventReactive(input$data, {
    t14 <- simfunction(sims1_4)
  })
  tab15 <- eventReactive(input$data, {
    t15 <- simfunction(sims1_5)
  })
  tab16 <- eventReactive(input$data, {
    t16 <- simfunction(sims1_6)
  })

    
    tab21 <- reactive({
      t21 <- simfunction(sims2_1)
    })
    tab22 <- eventReactive(input$data, {
      t22 <- simfunction(sims2_2)
    })
    tab23 <- eventReactive(input$data, {
      t23 <- simfunction(sims2_3)
    })
    tab24 <- eventReactive(input$data, {
      t24 <- simfunction(sims2_4)
    })
    tab25 <- eventReactive(input$data, {
      t25 <- simfunction(sims2_5)
    })
    tab26 <- eventReactive(input$data, {
      t26 <- simfunction(sims2_6)
    })

    
    tab31 <- reactive({
      t31 <- simfunction(sims3_1)
    })
    tab32 <- eventReactive(input$data, {
      t32 <- simfunction(sims3_2)
    })
    tab33 <- eventReactive(input$data, {
      t33 <- simfunction(sims3_3)
    })
    tab34 <- eventReactive(input$data, {
      t34 <- simfunction(sims3_4)
    })
    tab35 <- eventReactive(input$data, {
      t35 <- simfunction(sims3_5)
    })
    tab36 <- eventReactive(input$data, {
      t36 <- simfunction(sims3_6)
    })

  
  output$t11 <- renderTable({

    if (("Scenario 1" %in% input$scenario) & ("Slow: Less than 1 patient per month" %in% input$rate)){
      tab11()
    }

  }, rownames = T, colnames = F, bordered = T, caption = "Simulation results: Scenario 1, slow recruitment")

  output$t12 <- renderTable({

    if (("Scenario 2" %in% input$scenario) & ("Slow: Less than 1 patient per month" %in% input$rate)){
    tab12()
    }

  }, rownames = T, colnames = F, bordered = T, caption = "Simulation results: Scenario 2, slow recruitment")

  output$t13 <- renderTable({

    if (("Scenario 3" %in% input$scenario) & ("Slow: Less than 1 patient per month" %in% input$rate)){
      tab13()
    }

  }, rownames = T, colnames = F, bordered = T, caption = "Simulation results: Scenario 3, slow recruitment")

  output$t14 <- renderTable({

    if (("Scenario 4" %in% input$scenario) & ("Slow: Less than 1 patient per month" %in% input$rate)){
      tab14()
    }

  }, rownames = T, colnames = F, bordered = T, caption = "Simulation results: Scenario 4, slow recruitment")

  output$t15 <- renderTable({

    if (("Scenario 5" %in% input$scenario) & ("Slow: Less than 1 patient per month" %in% input$rate)){
      tab15()
    }

  }, rownames = T, colnames = F, bordered = T, caption = "Simulation results: Scenario 5, slow recruitment")

  output$t16 <- renderTable({


    if (("Scenario 6" %in% input$scenario) & ("Slow: Less than 1 patient per month" %in% input$rate)){
      tab16()
    } 

  }, rownames = T, colnames = F, bordered = T, caption = "Simulation results: Scenario 6, slow recruitment")
  
  output$t21 <- renderTable({
    
    
    if (("Scenario 1" %in% input$scenario) & ("Average: 2 patients per month" %in% input$rate)){
      tab21()
    }
    
  }, rownames = T, colnames = F, bordered = T, caption = "Simulation results: Scenario 1, average recruitment")
  
  output$t22 <- renderTable({
    
    
    if (("Scenario 2" %in% input$scenario) & ("Average: 2 patients per month" %in% input$rate)){
      tab22()
    }
    
  }, rownames = T, colnames = F, bordered = T, caption = "Simulation results: Scenario 2, average recruitment")
  
  output$t23 <- renderTable({
    
    
    if (("Scenario 3" %in% input$scenario) & ("Average: 2 patients per month" %in% input$rate)){
      tab23()
    }
    
  }, rownames = T, colnames = F, bordered = T, caption = "Simulation results: Scenario 3, average recruitment")
  
  output$t24 <- renderTable({
    
    
    if (("Scenario 4" %in% input$scenario) & ("Average: 2 patients per month" %in% input$rate)){
      tab24()
    }
    
  }, rownames = T, colnames = F, bordered = T, caption = "Simulation results: Scenario 4, average recruitment")
  
  output$t25 <- renderTable({
    
    
    if (("Scenario 5" %in% input$scenario) & ("Average: 2 patients per month" %in% input$rate)){
      tab25()
    }
    
  }, rownames = T, colnames = F, bordered = T, caption = "Simulation results: Scenario 5, average recruitment")
  
  output$t26 <- renderTable({
    
    
    if (("Scenario 6" %in% input$scenario) & ("Average: 2 patients per month" %in% input$rate)){
      tab26()
    }
    
  }, rownames = T, colnames = F, bordered = T, caption = "Simulation results: Scenario 6, average recruitment")
  
  output$t31 <- renderTable({
    
    
    if (("Scenario 1" %in% input$scenario) & ("Fast: 4 patients per month" %in% input$rate)){
      tab31()
    }
    
  }, rownames = T, colnames = F, bordered = T, caption = "Simulation results: Scenario 1, fast recruitment")
  
  output$t32 <- renderTable({
    
    
    if (("Scenario 2" %in% input$scenario) & ("Fast: 4 patients per month" %in% input$rate)){
      tab32()
    }
    
  }, rownames = T, colnames = F, bordered = T, caption = "Simulation results: Scenario 2, fast recruitment")
  
  output$t33 <- renderTable({
    
    
    if (("Scenario 3" %in% input$scenario) & ("Fast: 4 patients per month" %in% input$rate)){
      tab33()
    }
    
  }, rownames = T, colnames = F, bordered = T, caption = "Simulation results: Scenario 3, fast recruitment")
  
  output$t34 <- renderTable({
    
    
    if (("Scenario 4" %in% input$scenario) & ("Fast: 4 patients per month" %in% input$rate)){
      tab34()
    }
    
  }, rownames = T, colnames = F, bordered = T, caption = "Simulation results: Scenario 4, fast recruitment")
  
  output$t35 <- renderTable({
    
    
    if (("Scenario 5" %in% input$scenario) & ("Fast: 4 patients per month" %in% input$rate)){
      tab35()
    }
    
  }, rownames = T, colnames = F, bordered = T, caption = "Simulation results: Scenario 5, fast recruitment")
  
  output$t36 <- renderTable({
    
    
    if (("Scenario 6" %in% input$scenario) & ("Fast: 4 patients per month" %in% input$rate)){
      tab36()
    }
    
  }, rownames = T, colnames = F, bordered = T, caption = "Simulation results: Scenario 6, fast recruitment")
  
  
}


############
### App ####
############

shinyApp(ui, server)

