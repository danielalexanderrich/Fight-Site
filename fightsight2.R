#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(PlayerRatings)


#Dataset containing all fighters from simulation
simdata <- read.csv('simulation database.csv', stringsAsFactors = FALSE, fileEncoding = "latin1")
simdata2 <- read.csv('recordTable2.csv', stringsAsFactors = FALSE, fileEncoding = "latin1")
nameset2 <- as.character(simdata2$Fighter)
opponentset <- sort(as.character(simdata2$Opponent))
nameset <- as.character(sort(simdata$Fighter))

###########

#ranking data stuff

#imports CSV file containing all UFC fights from 1996 to 2018
records = read.csv("C:/Users/Bailey W/Desktop/Ranking data.csv", fill=TRUE, header=TRUE)
#reverses order of dataframe so that 1996 is at the top
records <-records[dim(records)[1]:1,]


#changes certain columns to character and numeric values
records[,2] <- as.character(records[,2])
records[,3] <- as.character(records[,3])
records[,4] <- as.character(records[,4])
records[,4]<- as.numeric(records[,4])

records <- unique(records)

#Generates Elo rating with default K-factors
elo_rated_fighters = elo(records, history =TRUE)
elo_frame = elo_rated_fighters$ratings$Rating
numgames = elo_rated_fighters$ratings$Games

#Generates and tests custom K-factors
kfactors = kfide(elo_frame, numgames, elite = NULL, kv = c(10,35,60))

#Generates Elo rating with custiom K-factors
elo_rated_fighters2 = elo(records, history =TRUE, kfac=kfactors)
elo_frame2 = elo_rated_fighters2$ratings$Rating
numgames = elo_rated_fighters$ratings$Games

#List of fighters used for ranking and ranking prediction
fighterslist <- elo_rated_fighters2$ratings$Player

############

wrrinput <- intersect(fighterslist, intersect(nameset,nameset2))


ui <- fluidPage(

  #Builds the navigating panel
  navlistPanel(
    "Welcome to FightSight",
    tabPanel("Home",
             h3("Home Page"),
             mainPanel(
               fluidRow(
                 selectInput(inputId = "fh1",label = "Fighter 1", choices = wrrinput),
                 selectInput(inputId = "fh2",label = "Fighter 2", choices = wrrinput),
                 textOutput("wrrtext"),
                 plotOutput("wrr")
               )
             )),
    #about tab
    tabPanel("About", 
            h3("What is FightSight?"),
               textOutput("introduction")),
    #ranking tab
    tabPanel("Ranking",
             tabsetPanel(
               tabPanel("",
                        titlePanel("Fighter Rankings"),
                        title = "Fighter Rankings",
                        sidebarLayout(
                          sidebarPanel(
                            conditionalPanel(
                              'input.dataset === "Pound-for-Pound Rankings"',
                              checkboxGroupInput("show_vars", "What would you like to see?",
                                                 names(elo_rated_fighters2$ratings), selected = names(elo_rated_fighters2$ratings))
                            )
                          ),
                          mainPanel(
                            tabsetPanel(
                              id = 'dataset',
                              tabPanel("Pound-for-Pound Rankings", DT::dataTableOutput("mytable1"))
                            )
                          )
                        )    
               ),
               
               tabPanel("", 
                        title = "Ranking Predictions",
                        pageWithSidebar(
                          headerPanel('Prediction Based on Elo Ranking'),
                          sidebarPanel(
                            selectInput('f1', 'Fighter 1', fighterslist),
                            selectInput('f2', 'Fighter 2', fighterslist)
                          ),
                          mainPanel(
                            textOutput('prediction')
                          )
                        )  
               ),
               
               tabPanel("",
                        title = "Rank Distribution",
                        mainPanel(
                          plotOutput("distribution")
                        )     
               )
             )
    ),
    
    #clustering tab
    tabPanel("Clustering",
             tabsetPanel( tabPanel("Clustering Prediction",  
                                   
                                   titlePanel("CageRank Clustering"),
                                   
                                   # Sidebar layout with a input and output definitions ----
                                   sidebarLayout(
                                     
                                     # Sidebar panel for inputs ----
                                     sidebarPanel(
                                       
                                       # Input: Selector for choosing dataset ----
                                       selectInput(inputId = "fighter",
                                                   label = "Choose A Fighter:",
                                                   choices = nameset2),
                                       
                                       # Input: Numeric entry for number of obs to view ----
                                       selectInput(inputId = "opponent",
                                                   label = "Choose An Opponent",
                                                   choices = opponentset)
                                       
                                     ),
                                     
                                     # Main panel for displaying outputs ----
                                     mainPanel(
                                       
                                       #Output: Verbatim text for data summary ----
                                       # textOutput("Prediction: "),
                                       # Output: HTML table with requested number of observations ----
                                       textOutput("summary"),
                                       textOutput("summary3"),
                                       textOutput("summary2"),
                                       splitLayout(plotOutput("barPlot"),
                                                   plotOutput("barPlot2"))
                                       
                                     )
                                   )
             ),
             tabPanel("Record Table",
                      
                      titlePanel("Fight History"),
                      
                      # Sidebar layout with a input and output definitions ----
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          
                          
                          # Input: Selector for choosing dataset ----
                          selectInput(inputId = "fighter3",
                                      label = "Choose A Fighter:",
                                      choices = nameset2)
                        ),
                        
                        mainPanel(
                          tableOutput("individualTable"),
                          
                          tableOutput("individualTable2")
                        ))
    )
    )
    ),
    
    
    
    
    
    #simulation tab
    tabPanel("Simulation",
             # App title ----
             titlePanel("Simulation"),
             
             # Sidebar layout with a input and output definitions ----
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 # Input: Selector for choosing dataset ----
                 selectInput(inputId = "fighter1",
                             label = "Choose fighter 1:",
                             choices = nameset),
                 
                 selectInput(inputId = "fighter2",
                             label = "Choose fighter 2:",
                             choices = nameset)
                 
               ),
               
               # Main panel for displaying outputs ----
               mainPanel(
                 #textOutput("simprediction"),
                   plotOutput("piechartsim"),
                   plotOutput("barplotsim")
               )
             )
    )
             
############             
    )
  )



server <- function(input, output) {
  # Return the requested dataset ----
  datasetInput1 <- reactive({
    input$fighter1
  })
  
  datasetInput2 <- reactive({
    input$fighter2
  })
  
  creInput1 <- reactive({
    input$cfighter1
  })
  crestrikedef <- reactive({
    input$cfightstrikedef
  })
  crestrikes <- reactive({
    input$cfightstrikes
  })
  
  cretddef <- reactive({
    input$cfighttddef
  })
  
  cretds <- reactive({
    input$cfighttakedowns
  })
  
  crekoratio <- reactive({
    input$cfightkowin
  })
  
  cresubratio <- reactive({
    input$cfightsubwin
  })
  
  # Return the requested dataset ----
  clustfighter <- reactive({
    input$fighter
  })
  
  clustopponent <- reactive({
    input$opponent
  })
  
  recfighter <- reactive({
    input$fighter3
  })
  
  wrrfighter <- reactive({
    input$fh1
  })
  
  wrropponent <- reactive({
    input$fh2
  })
  
  
  
  
  
  #########
  
  #ranked data set stuff
  fighterrankings = elo_rated_fighters2$ratings[sample(nrow(elo_rated_fighters2$ratings), length(elo_rated_fighters2$ratings$Rating)), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(fighterrankings[, input$show_vars, drop = FALSE])
  })
  #predicts fight outcome based on rating
  selectedData <- reactive({
    predict(elo_rated_fighters2, data.frame(2018, input$f1, input$f2))
  })
  #outputs prediction
  output$prediction <- renderText({
    
    print(paste("Probability of fighter 1 winning:", selectedData()*100))
    
  })
  #outputs elo distribution
  output$distribution <- renderPlot({hist(elo_rated_fighters2$ratings$Rating, xlim = c(1800, 2900), xlab = "Rating", ylab = "Fighters", main = "Elo Ratings Distribution", col = "Red")
  })
  #about page intro
  output$introduction <- renderText({print("FightSight is an MMA prediction platform which utilizes fighter's
                                           attributes and performance history to predict MMA bout outcomes and rank
                                           MMA fighters. Using this data, predictive analytics are performed via clustering,
                                           Monte Carlo Simulation, and a customized ranking algorithm.")})
  
  data <- reactive({
    fighter <- datasetInput1()
    opponent <- datasetInput2()
    
    ftotalfights <- simdata[which(simdata$Fighter == fighter), 10]
    ototalfights <- simdata[which(simdata$Fighter == opponent), 10]
    
    ftotalstrikes <- simdata[which(simdata$Fighter == fighter),3]
    ototalstrikes <- simdata[which(simdata$Fighter == opponent),3]
    
    ftotaltds <- simdata[which(simdata$Fighter == fighter),6]
    ototaltds <- simdata[which(simdata$Fighter == opponent),6]
    
    frounds <- simdata[which(simdata$Fighter == fighter),4]
    orounds <- simdata[which(simdata$Fighter == opponent),4]
    
    fstrikedef <- simdata[which(simdata$Fighter == fighter), 2]
    ostrikedef <- simdata[which(simdata$Fighter == opponent), 2]
    fstrikesavg <- (1 - ostrikedef) * ceiling(ftotalstrikes / frounds)
    ostrikesavg <- (1- fstrikedef) * ceiling(ototalstrikes / orounds)
    ftddf <- simdata[which(simdata$Fighter == fighter), 5]
    otddf <- simdata[which(simdata$Fighter == opponent), 5]
    ftdsavg <- (1 - otddf) * ceiling(ftotaltds/frounds)
    otdsavg <- (1 - ftddf) * ceiling(ototaltds/orounds)
    fkochance <- simdata[which(simdata$Fighter == fighter),8] / ftotalfights
    okochance <- simdata[which(simdata$Fighter == opponent),8] / ototalfights
    fsubchance <- simdata[which(simdata$Fighter == fighter),9] / ftotalfights
    osubchance <- simdata[which(simdata$Fighter == opponent),9] / ototalfights
    fgroundavg <- simdata[which(simdata$Fighter == fighter),7] / ftotalfights
    ogroundavg <- simdata[which(simdata$Fighter == opponent),7] / ototalfights
    
    tdmpl <- 1
    grdmpl <- 1
    strmpl <- 2
    
    
    fights <- 0
    ftally <- 0
    otally <- 0
    fwintally <- 0
    owintally <- 0
    draws <- 0
    fdecwintally <- 0
    odecwintally <- 0
    fkowintally <- 0
    okowintally <- 0
    fsubwintally <- 0
    osubwintally <- 0
    
    for (i in seq(1,1000)){
      finishindic <- 0
      fscore <- 0
      oscore <- 0
      fstrikes <- rpois(3,fstrikesavg)
      ostrikes <- rpois(3,ostrikesavg)
      ftds <- rpois(3,ftdsavg)
      otds <- rpois(3,otdsavg)
      fground <- rpois(3,fgroundavg)
      oground <- rpois(3,ogroundavg)
      
      
      ffinishroll = runif(1)
      ofinishroll = runif(1)
      
      if(ffinishroll != ofinishroll){
        
        if(ffinishroll < ofinishroll){
          if(ffinishroll <= fkochance + .05){
            fwintally <- fwintally + 1
            fkowintally <- fkowintally + 1
            finishindic <- 1
          }
          else if(ffinishroll < fsubchance + fkochance + .05 && ffinishroll > fkochance + .05){
            fwintally <- fwintally + 1
            fsubwintally <- fsubwintally + 1
            finishindic <- 1
          }
        }
        
        if(ofinishroll < ffinishroll){  
          if(ofinishroll <= okochance + .05){
            owintally <- owintally + 1
            okowintally <- okowintally + 1
            finishindic <- 1
          }
          else if(ofinishroll < osubchance + okochance + .05 && ofinishroll > okochance + .05){
            owintally <- owintally + 1
            osubwintally <- osubwintally + 1
            finishindic <- 1
          }
        }
      }
      
      if (finishindic == 0){
        if (fstrikes[1] + ftds[1]*15 > ostrikes[1] + otds[1]*15){
          fscore <- fscore + 10
          oscore <- oscore + 9
        }
        else if (fstrikes[1] + ftds[1]*15 < ostrikes[1] + otds[1]*15){
          oscore <- oscore + 10
          fscore <- fscore + 9
        }
        else if( fstrikes[1] + ftds[1]*15 == ostrikes[1] + otds[1]*15){
          fscore <- fscore + 10
          oscore <- oscore + 10
        }
        if (fstrikes[2] + ftds[2]*15 > ostrikes[2] + otds[2]*15){
          fscore <- fscore + 10
          oscore <- oscore + 9
        }
        else if (fstrikes[2] + ftds[2]*15 < ostrikes[2] + otds[2]*15){
          oscore <- oscore + 10
          fscore <- fscore + 9
        }
        else if (fstrikes[2] + ftds[2]*15 == ostrikes[2] + otds[2]*15){
          fscore <- fscore + 10
          oscore <- oscore + 10
        }
        if (fstrikes[3] + ftds[3]*15 > ostrikes[3] + otds[3]*15 ){
          fscore <- fscore + 10
          oscore <- oscore + 9
        }
        else if (fstrikes[3] + ftds[3]*15 < ostrikes[3] + otds[3]*15){
          oscore <- oscore + 10
          fscore <- fscore + 9
        }
        else if (fstrikes[3] + ftds[3]*15 == ostrikes[3] + otds[3]*15){
          fscore <- fscore + 10
          oscore <- oscore + 10
        }
        
        
        
        
        if (fscore > oscore){
          fwintally <- fwintally + 1
          fdecwintally <- fdecwintally + 1
        }
        else if(fscore < oscore){
          owintally <- owintally + 1
          odecwintally <- odecwintally + 1
        }
        else if (fscore == oscore) {
          draws <- draws + 1
        }
      }
      fights <- fights + 1
      #print(fights)
      if(fwintally + owintally + draws >= 1000){
        break
      }
      
    }
    
    fwinchance <- fwintally/1000
    #print(fwinchance)
    owinchance <- owintally / 1000
    #print(owinchance)
    drawchance <- (1000- fwintally - owintally)/1000
    #print(drawchance)
    
    fkowinchance <- fkowintally/1000
    okowinchance <- okowintally/1000
    fsubwinchance <- fsubwintally/1000
    osubwinchance <- osubwintally/1000
    fdecwinchance <- fdecwintally/1000
    odecwinchance <- odecwintally/1000
    
    
    
    if(fwinchance > owinchance){
      fighter1 <- paste(fighter,"wins",fwinchance*100,"percent of the time")
      result <- fighter1
    }
    else{
      fighter2 <- paste(opponent,"wins",owinchance*100,"percent of the time")
      result <- fighter2
    }
    
    outvec <- c(fwinchance, owinchance, drawchance, fighter, opponent,result, fkowinchance, okowinchance,
                fsubwinchance,osubwinchance,fdecwinchance, odecwinchance)
    return(outvec)
    
  })
  
  
  data2 <- reactive({
    fighter <- wrrfighter()
    opponent <- wrropponent()
    
    ftotalfights <- simdata[which(simdata$Fighter == fighter), 10]
    ototalfights <- simdata[which(simdata$Fighter == opponent), 10]
    
    ftotalstrikes <- simdata[which(simdata$Fighter == fighter),3]
    ototalstrikes <- simdata[which(simdata$Fighter == opponent),3]
    
    ftotaltds <- simdata[which(simdata$Fighter == fighter),6]
    ototaltds <- simdata[which(simdata$Fighter == opponent),6]
    
    frounds <- simdata[which(simdata$Fighter == fighter),4]
    orounds <- simdata[which(simdata$Fighter == opponent),4]
    
    fstrikedef <- simdata[which(simdata$Fighter == fighter), 2]
    ostrikedef <- simdata[which(simdata$Fighter == opponent), 2]
    fstrikesavg <- (1 - ostrikedef) * ceiling(ftotalstrikes / frounds)
    ostrikesavg <- (1- fstrikedef) * ceiling(ototalstrikes / orounds)
    ftddf <- simdata[which(simdata$Fighter == fighter), 5]
    otddf <- simdata[which(simdata$Fighter == opponent), 5]
    ftdsavg <- (1 - otddf) * ceiling(ftotaltds/frounds)
    otdsavg <- (1 - ftddf) * ceiling(ototaltds/orounds)
    fkochance <- simdata[which(simdata$Fighter == fighter),8] / ftotalfights
    okochance <- simdata[which(simdata$Fighter == opponent),8] / ototalfights
    fsubchance <- simdata[which(simdata$Fighter == fighter),9] / ftotalfights
    osubchance <- simdata[which(simdata$Fighter == opponent),9] / ototalfights
    fgroundavg <- simdata[which(simdata$Fighter == fighter),7] / ftotalfights
    ogroundavg <- simdata[which(simdata$Fighter == opponent),7] / ototalfights
    
    tdmpl <- 1
    grdmpl <- 1
    strmpl <- 2
    
    
    fights <- 0
    ftally <- 0
    otally <- 0
    fwintally <- 0
    owintally <- 0
    draws <- 0
    fdecwintally <- 0
    odecwintally <- 0
    fkowintally <- 0
    okowintally <- 0
    fsubwintally <- 0
    osubwintally <- 0
    
    for (i in seq(1,1000)){
      finishindic <- 0
      fscore <- 0
      oscore <- 0
      fstrikes <- rpois(3,fstrikesavg)
      ostrikes <- rpois(3,ostrikesavg)
      ftds <- rpois(3,ftdsavg)
      otds <- rpois(3,otdsavg)
      fground <- rpois(3,fgroundavg)
      oground <- rpois(3,ogroundavg)
      
      
      ffinishroll = runif(1)
      ofinishroll = runif(1)
      
      if(ffinishroll != ofinishroll){
        
        if(ffinishroll < ofinishroll){
          if(ffinishroll <= fkochance + .05){
            fwintally <- fwintally + 1
            fkowintally <- fkowintally + 1
            finishindic <- 1
          }
          else if(ffinishroll < fsubchance + fkochance + .05 && ffinishroll > fkochance + .05){
            fwintally <- fwintally + 1
            fsubwintally <- fsubwintally + 1
            finishindic <- 1
          }
        }
        
        if(ofinishroll < ffinishroll){  
          if(ofinishroll <= okochance + .05){
            owintally <- owintally + 1
            okowintally <- okowintally + 1
            finishindic <- 1
          }
          else if(ofinishroll < osubchance + okochance + .05 && ofinishroll > okochance + .05){
            owintally <- owintally + 1
            osubwintally <- osubwintally + 1
            finishindic <- 1
          }
        }
      }
      
      if (finishindic == 0){
        if (fstrikes[1] + ftds[1]*15 > ostrikes[1] + otds[1]*15){
          fscore <- fscore + 10
          oscore <- oscore + 9
        }
        else if (fstrikes[1] + ftds[1]*15 < ostrikes[1] + otds[1]*15){
          oscore <- oscore + 10
          fscore <- fscore + 9
        }
        else if( fstrikes[1] + ftds[1]*15 == ostrikes[1] + otds[1]*15){
          fscore <- fscore + 10
          oscore <- oscore + 10
        }
        if (fstrikes[2] + ftds[2]*15 > ostrikes[2] + otds[2]*15){
          fscore <- fscore + 10
          oscore <- oscore + 9
        }
        else if (fstrikes[2] + ftds[2]*15 < ostrikes[2] + otds[2]*15){
          oscore <- oscore + 10
          fscore <- fscore + 9
        }
        else if (fstrikes[2] + ftds[2]*15 == ostrikes[2] + otds[2]*15){
          fscore <- fscore + 10
          oscore <- oscore + 10
        }
        if (fstrikes[3] + ftds[3]*15 > ostrikes[3] + otds[3]*15 ){
          fscore <- fscore + 10
          oscore <- oscore + 9
        }
        else if (fstrikes[3] + ftds[3]*15 < ostrikes[3] + otds[3]*15){
          oscore <- oscore + 10
          fscore <- fscore + 9
        }
        else if (fstrikes[3] + ftds[3]*15 == ostrikes[3] + otds[3]*15){
          fscore <- fscore + 10
          oscore <- oscore + 10
        }
        
        
        
        
        if (fscore > oscore){
          fwintally <- fwintally + 1
          fdecwintally <- fdecwintally + 1
        }
        else if(fscore < oscore){
          owintally <- owintally + 1
          odecwintally <- odecwintally + 1
        }
        else if (fscore == oscore) {
          draws <- draws + 1
        }
      }
      fights <- fights + 1
      #print(fights)
      if(fwintally + owintally + draws >= 1000){
        break
      }
      
    }
    
    fwinchance <- fwintally/1000
    #print(fwinchance)
    owinchance <- owintally / 1000
    #print(owinchance)
    drawchance <- (1000- fwintally - owintally)/1000
    #print(drawchance)
    
    fkowinchance <- fkowintally/1000
    okowinchance <- okowintally/1000
    fsubwinchance <- fsubwintally/1000
    osubwinchance <- osubwintally/1000
    fdecwinchance <- fdecwintally/1000
    odecwinchance <- odecwintally/1000
    
    
    
    if(fwinchance > owinchance){
      fighter1 <- paste(fighter,"wins",fwinchance*100,"percent of the time")
      result <- fighter1
    }
    else{
      fighter2 <- paste(opponent,"wins",owinchance*100,"percent of the time")
      result <- fighter2
    }
    
    outvec <- c(fwinchance, owinchance, drawchance, fighter, opponent,result, fkowinchance, okowinchance,
                fsubwinchance,osubwinchance,fdecwinchance, odecwinchance)
    return(outvec)
    
  })
  
  cluster <- reactive({
    fighter <- wrrfighter()
    opponent <- wrropponent()
    
    
    #FIGHTER DATA
    
    individualRecordTableF <- simdata2[which(simdata2$Fighter == fighter),1:7]
    
    clusterNumF <- simdata2[which(simdata2$Opponent == opponent), 6]
    
    fightsAgainstCluster1F <- length(which(individualRecordTableF$Cluster == 1))
    winsAgainstCluster1F <- length(which(individualRecordTableF$fightResultConcat == "win_1"))
    lossAgainstCluster1F <- length(which(individualRecordTableF$fightResultConcat == "loss_1"))
    winPercentage1F <- (winsAgainstCluster1F/fightsAgainstCluster1F)
    lossPercentage1F <-(lossAgainstCluster1F/fightsAgainstCluster1F)
    
    fightsAgainstCluster2F <- length(which(individualRecordTableF$Cluster == 2))
    winsAgainstCluster2F <- length(which(individualRecordTableF$fightResultConcat == "win_2"))
    lossAgainstCluster2F <- length(which(individualRecordTableF$fightResultConcat == "loss_2"))
    winPercentage2F <- (winsAgainstCluster2F/fightsAgainstCluster2F)
    lossPercentage2F <-(lossAgainstCluster2F/fightsAgainstCluster2F)
    
    fightsAgainstCluster3F <- length(which(individualRecordTableF$Cluster == 3))
    winsAgainstCluster3F <- length(which(individualRecordTableF$fightResultConcat == "win_3"))
    lossAgainstCluster3F <- length(which(individualRecordTableF$fightResultConcat == "loss_3"))
    winPercentage3F <- (winsAgainstCluster3F/fightsAgainstCluster3F)
    lossPercentage3F <-(lossAgainstCluster3F/fightsAgainstCluster3F)
    
    fightsAgainstCluster4F <- length(which(individualRecordTableF$Cluster == 4))
    winsAgainstCluster4F <- length(which(individualRecordTableF$fightResultConcat == "win_4"))
    lossAgainstCluster4F <- length(which(individualRecordTableF$fightResultConcat == "loss_4"))
    winPercentage4F <- (winsAgainstCluster4F/fightsAgainstCluster4F)
    lossPercentage4F <-(lossAgainstCluster4F/fightsAgainstCluster4F)
    
    
    fightPercentDataF <- as.data.frame(matrix(ncol=4,nrow = 2," "),stringsAsFactors = F)
    
    colnames(fightPercentDataF)[colnames(fightPercentDataF)=="V1"] <- "Cluster 1"
    colnames(fightPercentDataF)[colnames(fightPercentDataF)=="V2"] <- "Cluster 2"
    colnames(fightPercentDataF)[colnames(fightPercentDataF)=="V3"] <- "Cluster 3"
    colnames(fightPercentDataF)[colnames(fightPercentDataF)=="V4"] <- "Cluster 4"
    rownames(fightPercentDataF)[rownames(fightPercentDataF)=="1"] <- "Win Percentage"
    rownames(fightPercentDataF)[rownames(fightPercentDataF)=="2"] <- "Loss Percentage"
    
    if(is.nan(winPercentage1F)){winPercentage1F <- 0}
    if (is.nan(lossPercentage1F)){lossPercentage1F <- 0}
    if(is.nan(winPercentage2F)){winPercentage2F <- 0}
    if (is.nan(lossPercentage2F)){lossPercentage2F <- 0}
    if(is.nan(winPercentage3F)){winPercentage3F <- 0}
    if (is.nan(lossPercentage3F)){lossPercentage3F <- 0}
    if(is.nan(winPercentage4F)){winPercentage4F <- 0}
    if (is.nan(lossPercentage4F)){lossPercentage4F <- 0}
    
    fightPercentDataF[1,1] <- winPercentage1F
    fightPercentDataF[2,1] <- lossPercentage1F
    fightPercentDataF[1,2] <- winPercentage2F
    fightPercentDataF[2,2] <- lossPercentage2F
    fightPercentDataF[1,3] <- winPercentage3F
    fightPercentDataF[2,3] <- lossPercentage3F
    fightPercentDataF[1,4] <- winPercentage4F
    fightPercentDataF[2,4] <- lossPercentage4F
    
    if(clusterNumF == 1 & !is.nan(winPercentage1F)){
      winPercentageF <- winPercentage1F
    } else if(clusterNumF == 2 & !is.nan(winPercentage2F)){
      winPercentageF <- winPercentage2F
    }else if(clusterNumF == 3 & !is.nan(winPercentage2F)){
      winPercentageF <- winPercentage3F
    }else if(clusterNumF == 4 & !is.nan(winPercentage2F)){
      winPercentageF <- winPercentage4F
    }
    
    if (clusterNumF == 1 & !is.nan(winPercentage1F)){
      lossPercentageF <- lossPercentage1F
    }else if (clusterNumF == 2 & !is.nan(winPercentage1F)){
      lossPercentageF <- lossPercentage2F
    }else if (clusterNumF == 3 & !is.nan(winPercentage1F)){
      lossPercentageF <- lossPercentage3F
    }else if (clusterNumF == 4 & !is.nan(winPercentage1F)){
      lossPercentageF <- lossPercentage4F
    }
    
    FighterResult <- (winPercentageF - lossPercentageF)
    
    
    #OPPONENT Data  
    
    individualRecordTableO <- simdata2[which(simdata2$Fighter == opponent),1:7]
    
    
    clusterNumO <- simdata2[which(simdata2$Opponent == fighter), 6]
    
    fightsAgainstCluster1O <- length(which(individualRecordTableO$Cluster == 1))
    winsAgainstCluster1O <- length(which(individualRecordTableO$fightResultConcat == "win_1"))
    lossAgainstCluster1O <- length(which(individualRecordTableO$fightResultConcat == "loss_1"))
    winPercentage1O <- (winsAgainstCluster1O/fightsAgainstCluster1O)
    lossPercentage1O <-(lossAgainstCluster1O/fightsAgainstCluster1O)
    
    fightsAgainstCluster2O <- length(which(individualRecordTableO$Cluster == 2))
    winsAgainstCluster2O <- length(which(individualRecordTableO$fightResultConcat == "win_2"))
    lossAgainstCluster2O <- length(which(individualRecordTableO$fightResultConcat == "loss_2"))
    winPercentage2O <- (winsAgainstCluster2O/fightsAgainstCluster2O)
    lossPercentage2O <-(lossAgainstCluster2O/fightsAgainstCluster2O)
    
    fightsAgainstCluster3O <- length(which(individualRecordTableO$Cluster == 3))
    winsAgainstCluster3O <- length(which(individualRecordTableO$fightResultConcat == "win_3"))
    lossAgainstCluster3O <- length(which(individualRecordTableO$fightResultConcat == "loss_3"))
    winPercentage3O <- (winsAgainstCluster3O/fightsAgainstCluster3O)
    lossPercentage3O <-(lossAgainstCluster3O/fightsAgainstCluster3O)
    
    fightsAgainstCluster4O <- length(which(individualRecordTableO$Cluster == 4))
    winsAgainstCluster4O <- length(which(individualRecordTableO$fightResultConcat == "win_4"))
    lossAgainstCluster4O <- length(which(individualRecordTableO$fightResultConcat == "loss_4"))
    winPercentage4O <- (winsAgainstCluster4O/fightsAgainstCluster4O)
    lossPercentage4O <-(lossAgainstCluster4O/fightsAgainstCluster4O)
    
    fightPercentDataO <- as.data.frame(matrix(ncol=4,nrow = 2," "),stringsAsFactors = F)
    
    
    colnames(fightPercentDataO)[colnames(fightPercentDataO)=="V1"] <- "Cluster 1"
    colnames(fightPercentDataO)[colnames(fightPercentDataO)=="V2"] <- "Cluster 2"
    colnames(fightPercentDataO)[colnames(fightPercentDataO)=="V3"] <- "Cluster 3"
    colnames(fightPercentDataO)[colnames(fightPercentDataO)=="V4"] <- "Cluster 4"
    rownames(fightPercentDataO)[rownames(fightPercentDataO)=="1"] <- "Win Percentage"
    rownames(fightPercentDataO)[rownames(fightPercentDataO)=="2"] <- "Loss Percentage"
    
    if(is.nan(winPercentage1O)){winPercentage1O <- 0}
    if (is.nan(lossPercentage1O)){lossPercentage1O <- 0}
    if(is.nan(winPercentage2O)){winPercentage2O <- 0}
    if (is.nan(lossPercentage2O)){lossPercentage2O <- 0}
    if(is.nan(winPercentage3O)){winPercentage3O <- 0}
    if (is.nan(lossPercentage3O)){lossPercentage3O <- 0}
    if(is.nan(winPercentage4O)){winPercentage4O <- 0}
    if (is.nan(lossPercentage4O)){lossPercentage4O <- 0}
    
    
    fightPercentDataO[1,1] <- winPercentage1O
    fightPercentDataO[2,1] <- lossPercentage1O
    fightPercentDataO[1,2] <- winPercentage2O
    fightPercentDataO[2,2] <- lossPercentage2O
    fightPercentDataO[1,3] <- winPercentage3O
    fightPercentDataO[2,3] <- lossPercentage3O
    fightPercentDataO[1,4] <- winPercentage4O
    fightPercentDataO[2,4] <- lossPercentage4O
    
    
    if(clusterNumO == 1 & !is.nan(winPercentage1O)){
      winPercentageO <- winPercentage1O
    } else if(clusterNumO == 2 & !is.nan(winPercentage2O)){
      winPercentageO <- winPercentage2O
    }else if(clusterNumO == 3 & !is.nan(winPercentage2O)){
      winPercentageO <- winPercentage3O
    }else if(clusterNumO == 4 & !is.nan(winPercentage2O)){
      winPercentageO <- winPercentage4O
    }
    
    
    if (clusterNumO == 1 & !is.nan(winPercentage1O)){
      lossPercentageO <- lossPercentage1O
    }else if (clusterNumO == 2 & !is.nan(winPercentage1O)){
      lossPercentageO <- lossPercentage2O
    }else if (clusterNumO == 3 & !is.nan(winPercentage1O)){
      lossPercentageO <- lossPercentage3O
    }else if (clusterNumO == 4 & !is.nan(winPercentage1O)){
      lossPercentageO <- lossPercentage4O
    }
    
    OpponentResult <- (winPercentageO - lossPercentageO)
    
    winPercentageF <- round(winPercentageF, digits = 2)
    winPercentageO<- round(winPercentageO, digits = 2)
    winPercentageF <-(winPercentageF * 100)
    winPercentageO <-(winPercentageO * 100)
    
    outvec <- c(FighterResult, OpponentResult, fighter, opponent)
    
    
  })
  
  output$wrr <- renderPlot({
    rankvec <- selectedData()
    simpred <- c()
    simvec <- data2()
    clusvec <- cluster()
    simfighter <- as.numeric(simvec[1])
    simopponent <- as.numeric(simvec[2])
    clusfighter <- as.numeric(clusvec[1])
    clusopp <- as.numeric(clusvec[2])
    simacc <- .667
    clustacc <- .636
    rankacc <- .654
    
    simf <- if(simfighter > simopponent){
      simacc
    }else{0}
    
    
    clusf <-  if(clusfighter > clusopp){
      clustacc
    }else{0}
    
    rankf <- if(as.numeric(rankvec) > .5){
      rankacc
    }
    
    
    simo <- if( simfighter < simopponent) {
      simacc
    }else{0}
    
    
    cluso <-  if(clusfighter < clusopp) {
      clustacc
    }else{0}
    
    ranko <- if( as.numeric(rankvec) < .5){
      rankacc
    }else{0}
    
    wrrf <- clusf + simf + rankf
    wrro <- cluso + simo + ranko
    counts <- c(wrrf, wrro)
    colors <- c("cornflowerblue", "coral3")
    barplot(counts, main = "WRR score barplot", names.arg = c("Fighter 1", "Fighter 2"), col= colors, ylim= c(0,1.5)  )
    
    
  })
  
  output$wrrtext <- renderText({
    
    rankvec <- selectedData()
    simpred <- c()
    simvec <- data2()
    clusvec <- cluster()
    simfighter <- as.numeric(simvec[1])
    simopponent <- as.numeric(simvec[2])
    clusfighter <- as.numeric(clusvec[1])
    clusopp <- as.numeric(clusvec[2])
    simacc <- .667
    clustacc <- .636
    rankacc <- .654
    
    simf <- if(simfighter > simopponent){
      simacc
    }else{0}
    
    
    clusf <-  if(clusfighter > clusopp){
      clustacc
    }else{0}
    
    rankf <- if(as.numeric(rankvec) > .5){
      rankacc
    }
    
    
    simo <- if( simfighter < simopponent) {
      simacc
    }else{0}
    
    
    cluso <-  if(clusfighter < clusopp) {
      clustacc
    }else{0}
    
    ranko <- if( as.numeric(rankvec) < .5){
      rankacc
    }else{0}
    
    wrrf <- clusf + simf + rankf
    wrro <- cluso + simo + ranko
    
    
    print(paste("Fighter 1's WRR score:", wrrf, "vs Fighter 2's WRR score:", wrro))
  })
  
  
  
  
  
  
  
  output$piechartsim <- renderPlot({
    datavec <- data()
    fwinchance <- as.numeric(datavec[1])
    owinchance <- as.numeric(datavec[2])
    drawchance <- as.numeric(datavec[3])
    fighter <- datavec[4]
    opponent <- datavec[5]
    result <- datavec[6]
    
    
    outcomeslices <- c(fwinchance,owinchance,drawchance)
    
    outcomelabels <- c(paste(fighter, "wins", fwinchance*100, "%"), paste(opponent ,"wins", owinchance*100, "%"), paste("Draw",drawchance*100, "%"))
    colors <- c("coral3","cornflowerblue","whitesmoke")
    pie(x = outcomeslices, labels = outcomelabels,col = colors, main="Winner prediction chart")
    mtext(result, side = 1)})
  
  output$barplotsim <- renderPlot({
    datavec <- data()
    fkowinchance <- as.numeric(datavec[7])
    okowinchance <- as.numeric(datavec[8])
    fsubwinchance <- as.numeric(datavec[9])
    osubwinchance <- as.numeric(datavec[10])
    fdecwinchance <- as.numeric(datavec[11])
    odecwinchance <- as.numeric(datavec[12])
    fighter <- datavec[4]
    opponent <- datavec[5]
    result <- datavec[6]
    
    
    typewins <- matrix(c(fkowinchance,fsubwinchance,fdecwinchance,
                         okowinchance,osubwinchance,odecwinchance),nrow = 3, ncol = 2, byrow=TRUE)
    # typelabels <- c("F1 ko", "F1 sub", "F1 dec", "F2 ko", "F2 sub", "F2 dec", "Draw")
    typecolors <- c("coral3","cornflowerblue","whitesmoke")
    types <- c("Knock out", "Submission", "Decision")
    par(mar=c(5, 4, 4, 8))
    barplot(typewins,width = .25,
            names.arg = c(fighter, opponent),
            xlab = "Fighter",
            ylab = "Win chance by type",
            ylim = c(0,max(c( fkowinchance,fsubwinchance,fdecwinchance,
                              okowinchance,osubwinchance,odecwinchance))+.05),
            col = typecolors,
            beside=TRUE, main= "Fight Outcome Barplot")
    legend("topright", types, cex = .8, fill = typecolors)
    
  })
  
  output$simprediction <- renderText({
    datavec <- data()
    fwinchance <- as.numeric(datavec[1])
    owinchance <- as.numeric(datavec[2])
    print(paste(fwinchance,owinchance))
    
  })
  
  createdfighter <- reactive({
    fighter <- creInput1()
    
    ftotalfights <- simdata[which(simdata$Fighter == fighter), 10]
    #ototalfights <- simdata[which(simdata$Fighter == opponent), 10]
    
    ftotalstrikes <- simdata[which(simdata$Fighter == fighter),3]
    #ototalstrikes <- simdata[which(simdata$Fighter == opponent),3]
    
    ftotaltds <- simdata[which(simdata$Fighter == fighter),6]
    #ototaltds <- simdata[which(simdata$Fighter == opponent),6]
    
    frounds <- simdata[which(simdata$Fighter == fighter),4]
    #orounds <- simdata[which(simdata$Fighter == opponent),4]
    
    fstrikedef <- simdata[which(simdata$Fighter == fighter), 2]
    ostrikedef <- crestrikedef()
    fstrikesavg <- (1- ostrikedef) * ceiling(ftotalstrikes / frounds)
    ostrikesavg <- (1 - fstrikedef) * crestrikes()
    ftddf <- simdata[which(simdata$Fighter == fighter), 5]
    otddf <-  cretddef()
    ftdsavg <- (1-otddf) * ceiling(ftotaltds/frounds)
    otdsavg <- (1- ftddf) *  cretds()
    fkochance <- simdata[which(simdata$Fighter == fighter),8] / ftotalfights
    okochance <- crekoratio()
    fsubchance <- simdata[which(simdata$Fighter == fighter),9] / ftotalfights
    osubchance <- cresubratio()
    
    fights <- 0
    ftally <- 0
    otally <- 0
    fwintally <- 0
    owintally <- 0
    draws <- 0
    fdecwintally <- 0
    odecwintally <- 0
    fkowintally <- 0
    okowintally <- 0
    fsubwintally <- 0
    osubwintally <- 0
    
    
    st <- 1
    td <- 2
    
    for (i in seq(1,1000)){
      finishindic <- 0
      fscore <- 0
      oscore <- 0
      fstrikes <- rpois(3,fstrikesavg)
      ostrikes <- rpois(3,ostrikesavg)
      ftds <- rpois(3,ftdsavg)
      otds <- rpois(3,otdsavg)
      
      
      
      ffinishroll = runif(1)
      ofinishroll = runif(1)
      
      if(ffinishroll != ofinishroll){
        
        if(ffinishroll < ofinishroll){
          if(ffinishroll <= fkochance + .05){
            fwintally <- fwintally + 1
            fkowintally <- fkowintally + 1
            finishindic <- 1
          }
          else if(ffinishroll < fsubchance + fkochance + .05 && ffinishroll > fkochance + .05){
            fwintally <- fwintally + 1
            fsubwintally <- fsubwintally + 1
            finishindic <- 1
          }
        }
        
        if(ofinishroll < ffinishroll){
          if(ofinishroll <= okochance + .05){
            owintally <- owintally + 1
            okowintally <- okowintally + 1
            finishindic <- 1
          }
          else if(ofinishroll < osubchance + okochance + .05 && ofinishroll > okochance + .05){
            owintally <- owintally + 1
            osubwintally <- osubwintally + 1
            finishindic <- 1
          }
        }
      }
      
      if (finishindic == 0){
        if (fstrikes[1]*st + ftds[1]*td > ostrikes[1]*st + otds[1]*td){
          fscore <- fscore + 10
          oscore <- oscore + 9
        }
        else if (fstrikes[1]*st + ftds[1]*td < ostrikes[1]*st + otds[1]*td){
          oscore <- oscore + 10
          fscore <- fscore + 9
        }
        else if( fstrikes[1]*st + ftds[1]*td == ostrikes[1]*st + otds[1]*td){
          fscore <- fscore + 10
          oscore <- oscore + 10
        }
        if (st*fstrikes[2] + ftds[2]*td > st*ostrikes[2] + otds[2]*td){
          fscore <- fscore + 10
          oscore <- oscore + 9
        }
        else if (st*fstrikes[2] + ftds[2]*td < st*ostrikes[2] + otds[2]*td){
          oscore <- oscore + 10
          fscore <- fscore + 9
        }
        else if (st*fstrikes[2] + ftds[2]*td == st*ostrikes[2] + otds[2]*td){
          fscore <- fscore + 10
          oscore <- oscore + 10
        }
        if (st*fstrikes[3] + ftds[3]*td > st*ostrikes[3] + otds[3]*td ){
          fscore <- fscore + 10
          oscore <- oscore + 9
        }
        else if (st*fstrikes[3] + ftds[3]*td < st*ostrikes[3] + otds[3]*td){
          oscore <- oscore + 10
          fscore <- fscore + 9
        }
        else if (st*fstrikes[3] + ftds[3]*td == st*ostrikes[3] + otds[3]*td){
          fscore <- fscore + 10
          oscore <- oscore + 10
        }
        
        
        
        if (fscore > oscore){
          fwintally <- fwintally + 1
          fdecwintally <- fdecwintally + 1
        }
        else if(fscore < oscore){
          owintally <- owintally + 1
          odecwintally <- odecwintally + 1
        }
        else if (fscore == oscore) {
          draws <- draws + 1
        }
      }
      fights <- fights + 1
      #print(fights)
      if(fwintally + owintally + draws >= 1000){
        break
      }
      
    }
    
    fwinchance <- fwintally/1000
    #print(fwinchance)
    owinchance <- owintally / 1000
    #print(owinchance)
    drawchance <- draws/1000
    #print(drawchance)
    
    #
    fkowinchance <- fkowintally/1000
    okowinchance <- okowintally/1000
    fsubwinchance <- fsubwintally/1000
    osubwinchance <- osubwintally/1000
    fdecwinchance <- fdecwintally/1000
    odecwinchance <- odecwintally/1000
    
    
    
    outvec <- c(fwinchance, owinchance, drawchance,fighter, fkowinchance, okowinchance,
                fsubwinchance,osubwinchance,fdecwinchance, odecwinchance)
    return(outvec)
    
    
  })
  
  
  
  
  
  
  
  output$createdresults <- renderPlot({
    datavec <- createdfighter()
    fwinchance <- datavec[1]
    owinchance <- datavec[2]
    drawchance <- datavec[3]
    fighter <- datavec[4]
    slices <- c(as.numeric(owinchance),as.numeric(fwinchance))
    lbls <- c(paste("Created Fighter wins", as.numeric(owinchance)* 100, "% of the time"), paste(fighter, "wins", as.numeric(fwinchance)*100, "% of the time"))
    barplot(slices, main = "Results", col = c("coral3", "cornflowerblue"), names.arg = lbls, ylim = c(0,1))
    
  })
  
  #PRINT FIGHT PREDICTION
  output$summary <- renderText({
    fighter <- clustfighter()
    opponent <- clustopponent()
    
    
    #FIGHTER DATA
    
    individualRecordTableF <- simdata2[which(simdata2$Fighter == fighter),1:7]
    
    clusterNumF <- simdata2[which(simdata2$Opponent == opponent), 6]
    
    fightsAgainstCluster1F <- length(which(individualRecordTableF$Cluster == 1))
    winsAgainstCluster1F <- length(which(individualRecordTableF$fightResultConcat == "win_1"))
    lossAgainstCluster1F <- length(which(individualRecordTableF$fightResultConcat == "loss_1"))
    winPercentage1F <- (winsAgainstCluster1F/fightsAgainstCluster1F)
    lossPercentage1F <-(lossAgainstCluster1F/fightsAgainstCluster1F)
    
    fightsAgainstCluster2F <- length(which(individualRecordTableF$Cluster == 2))
    winsAgainstCluster2F <- length(which(individualRecordTableF$fightResultConcat == "win_2"))
    lossAgainstCluster2F <- length(which(individualRecordTableF$fightResultConcat == "loss_2"))
    winPercentage2F <- (winsAgainstCluster2F/fightsAgainstCluster2F)
    lossPercentage2F <-(lossAgainstCluster2F/fightsAgainstCluster2F)
    
    fightsAgainstCluster3F <- length(which(individualRecordTableF$Cluster == 3))
    winsAgainstCluster3F <- length(which(individualRecordTableF$fightResultConcat == "win_3"))
    lossAgainstCluster3F <- length(which(individualRecordTableF$fightResultConcat == "loss_3"))
    winPercentage3F <- (winsAgainstCluster3F/fightsAgainstCluster3F)
    lossPercentage3F <-(lossAgainstCluster3F/fightsAgainstCluster3F)
    
    fightsAgainstCluster4F <- length(which(individualRecordTableF$Cluster == 4))
    winsAgainstCluster4F <- length(which(individualRecordTableF$fightResultConcat == "win_4"))
    lossAgainstCluster4F <- length(which(individualRecordTableF$fightResultConcat == "loss_4"))
    winPercentage4F <- (winsAgainstCluster4F/fightsAgainstCluster4F)
    lossPercentage4F <-(lossAgainstCluster4F/fightsAgainstCluster4F)
    
    
    fightPercentDataF <- as.data.frame(matrix(ncol=4,nrow = 2," "),stringsAsFactors = F)
    
    colnames(fightPercentDataF)[colnames(fightPercentDataF)=="V1"] <- "Cluster 1"
    colnames(fightPercentDataF)[colnames(fightPercentDataF)=="V2"] <- "Cluster 2"
    colnames(fightPercentDataF)[colnames(fightPercentDataF)=="V3"] <- "Cluster 3"
    colnames(fightPercentDataF)[colnames(fightPercentDataF)=="V4"] <- "Cluster 4"
    rownames(fightPercentDataF)[rownames(fightPercentDataF)=="1"] <- "Win Percentage"
    rownames(fightPercentDataF)[rownames(fightPercentDataF)=="2"] <- "Loss Percentage"
    
    if(is.nan(winPercentage1F)){winPercentage1F <- 0}
    if (is.nan(lossPercentage1F)){lossPercentage1F <- 0}
    if(is.nan(winPercentage2F)){winPercentage2F <- 0}
    if (is.nan(lossPercentage2F)){lossPercentage2F <- 0}
    if(is.nan(winPercentage3F)){winPercentage3F <- 0}
    if (is.nan(lossPercentage3F)){lossPercentage3F <- 0}
    if(is.nan(winPercentage4F)){winPercentage4F <- 0}
    if (is.nan(lossPercentage4F)){lossPercentage4F <- 0}
    
    fightPercentDataF[1,1] <- winPercentage1F
    fightPercentDataF[2,1] <- lossPercentage1F
    fightPercentDataF[1,2] <- winPercentage2F
    fightPercentDataF[2,2] <- lossPercentage2F
    fightPercentDataF[1,3] <- winPercentage3F
    fightPercentDataF[2,3] <- lossPercentage3F
    fightPercentDataF[1,4] <- winPercentage4F
    fightPercentDataF[2,4] <- lossPercentage4F
    
    if(clusterNumF == 1 & !is.nan(winPercentage1F)){
      winPercentageF <- winPercentage1F
    } else if(clusterNumF == 2 & !is.nan(winPercentage2F)){
      winPercentageF <- winPercentage2F
    }else if(clusterNumF == 3 & !is.nan(winPercentage2F)){
      winPercentageF <- winPercentage3F
    }else if(clusterNumF == 4 & !is.nan(winPercentage2F)){
      winPercentageF <- winPercentage4F
    }
    
    if (clusterNumF == 1 & !is.nan(winPercentage1F)){
      lossPercentageF <- lossPercentage1F
    }else if (clusterNumF == 2 & !is.nan(winPercentage1F)){
      lossPercentageF <- lossPercentage2F
    }else if (clusterNumF == 3 & !is.nan(winPercentage1F)){
      lossPercentageF <- lossPercentage3F
    }else if (clusterNumF == 4 & !is.nan(winPercentage1F)){
      lossPercentageF <- lossPercentage4F
    }
    
    FighterResult <- (winPercentageF - lossPercentageF)
    
    
    #OPPONENT Data  
    
    individualRecordTableO <- simdata2[which(simdata2$Fighter == opponent),1:7]
    
    
    clusterNumO <- simdata2[which(simdata2$Opponent == fighter), 6]
    
    fightsAgainstCluster1O <- length(which(individualRecordTableO$Cluster == 1))
    winsAgainstCluster1O <- length(which(individualRecordTableO$fightResultConcat == "win_1"))
    lossAgainstCluster1O <- length(which(individualRecordTableO$fightResultConcat == "loss_1"))
    winPercentage1O <- (winsAgainstCluster1O/fightsAgainstCluster1O)
    lossPercentage1O <-(lossAgainstCluster1O/fightsAgainstCluster1O)
    
    fightsAgainstCluster2O <- length(which(individualRecordTableO$Cluster == 2))
    winsAgainstCluster2O <- length(which(individualRecordTableO$fightResultConcat == "win_2"))
    lossAgainstCluster2O <- length(which(individualRecordTableO$fightResultConcat == "loss_2"))
    winPercentage2O <- (winsAgainstCluster2O/fightsAgainstCluster2O)
    lossPercentage2O <-(lossAgainstCluster2O/fightsAgainstCluster2O)
    
    fightsAgainstCluster3O <- length(which(individualRecordTableO$Cluster == 3))
    winsAgainstCluster3O <- length(which(individualRecordTableO$fightResultConcat == "win_3"))
    lossAgainstCluster3O <- length(which(individualRecordTableO$fightResultConcat == "loss_3"))
    winPercentage3O <- (winsAgainstCluster3O/fightsAgainstCluster3O)
    lossPercentage3O <-(lossAgainstCluster3O/fightsAgainstCluster3O)
    
    fightsAgainstCluster4O <- length(which(individualRecordTableO$Cluster == 4))
    winsAgainstCluster4O <- length(which(individualRecordTableO$fightResultConcat == "win_4"))
    lossAgainstCluster4O <- length(which(individualRecordTableO$fightResultConcat == "loss_4"))
    winPercentage4O <- (winsAgainstCluster4O/fightsAgainstCluster4O)
    lossPercentage4O <-(lossAgainstCluster4O/fightsAgainstCluster4O)
    
    fightPercentDataO <- as.data.frame(matrix(ncol=4,nrow = 2," "),stringsAsFactors = F)
    
    
    colnames(fightPercentDataO)[colnames(fightPercentDataO)=="V1"] <- "Cluster 1"
    colnames(fightPercentDataO)[colnames(fightPercentDataO)=="V2"] <- "Cluster 2"
    colnames(fightPercentDataO)[colnames(fightPercentDataO)=="V3"] <- "Cluster 3"
    colnames(fightPercentDataO)[colnames(fightPercentDataO)=="V4"] <- "Cluster 4"
    rownames(fightPercentDataO)[rownames(fightPercentDataO)=="1"] <- "Win Percentage"
    rownames(fightPercentDataO)[rownames(fightPercentDataO)=="2"] <- "Loss Percentage"
    
    if(is.nan(winPercentage1O)){winPercentage1O <- 0}
    if (is.nan(lossPercentage1O)){lossPercentage1O <- 0}
    if(is.nan(winPercentage2O)){winPercentage2O <- 0}
    if (is.nan(lossPercentage2O)){lossPercentage2O <- 0}
    if(is.nan(winPercentage3O)){winPercentage3O <- 0}
    if (is.nan(lossPercentage3O)){lossPercentage3O <- 0}
    if(is.nan(winPercentage4O)){winPercentage4O <- 0}
    if (is.nan(lossPercentage4O)){lossPercentage4O <- 0}
    
    
    fightPercentDataO[1,1] <- winPercentage1O
    fightPercentDataO[2,1] <- lossPercentage1O
    fightPercentDataO[1,2] <- winPercentage2O
    fightPercentDataO[2,2] <- lossPercentage2O
    fightPercentDataO[1,3] <- winPercentage3O
    fightPercentDataO[2,3] <- lossPercentage3O
    fightPercentDataO[1,4] <- winPercentage4O
    fightPercentDataO[2,4] <- lossPercentage4O
    
    
    if(clusterNumO == 1 & !is.nan(winPercentage1O)){
      winPercentageO <- winPercentage1O
    } else if(clusterNumO == 2 & !is.nan(winPercentage2O)){
      winPercentageO <- winPercentage2O
    }else if(clusterNumO == 3 & !is.nan(winPercentage2O)){
      winPercentageO <- winPercentage3O
    }else if(clusterNumO == 4 & !is.nan(winPercentage2O)){
      winPercentageO <- winPercentage4O
    }
    
    
    if (clusterNumO == 1 & !is.nan(winPercentage1O)){
      lossPercentageO <- lossPercentage1O
    }else if (clusterNumO == 2 & !is.nan(winPercentage1O)){
      lossPercentageO <- lossPercentage2O
    }else if (clusterNumO == 3 & !is.nan(winPercentage1O)){
      lossPercentageO <- lossPercentage3O
    }else if (clusterNumO == 4 & !is.nan(winPercentage1O)){
      lossPercentageO <- lossPercentage4O
    }
    
    OpponentResult <- (winPercentageO - lossPercentageO)
    
    winPercentageF <- round(winPercentageF, digits = 2)
    winPercentageO<- round(winPercentageO, digits = 2)
    winPercentageF <-(winPercentageF * 100)
    winPercentageO <-(winPercentageO * 100)
    
    
    if(FighterResult > OpponentResult){
      print(paste("We Predict", fighter, "Will Win.", winPercentageF, "Percent"))
    } else if(FighterResult < OpponentResult){
      print(paste("We Predict",opponent, "Will Win.", winPercentageO, "Percent"))
    } else if(FighterResult == OpponentResult){
      print("50/50 Chance")
    } 
    
    
    
    
  })
  
  #PRINTS OPPONENTS CLUSTER NUM
  output$summary2 <- renderText({
    fighter <- clustfighter()
    opponent <- clustopponent()
    
    individualRecordTable <- simdata2[which(simdata2$Fighter == fighter),1:7]
    
    clusterNum <-simdata2[which(simdata2$Opponent == opponent)[1], 6]
    
    print(paste("Opponents Cluster:",  clusterNum))
  })
  
  #PRINTS FIGHTERS CLUSTER NUM
  output$summary3 <- renderText({
    fighter <- clustfighter()
    opponent <- clustopponent()
    
    individualRecordTable <- simdata2[which(simdata2$Fighter == fighter),1:7]
    
    clusterNum <-simdata2[which(simdata2$Opponent == fighter)[1], 6]
    
    print(paste("Fighters Cluster:",  clusterNum))
  })
  
  #Print individual record table
  
  output$individualTable2 <- renderTable({
    fighter3 <- recfighter()
    
    
    
    individualRecordTable <- simdata2[which(simdata2$Fighter == fighter3),1:7]
    
    print(individualRecordTable)
  })
  
  
  #####
  
  #Print fighters success over all clusters
  output$individualTable <- renderTable({
    fighter3 <- recfighter()
    
    
    
    individualRecordTable <- simdata2[which(simdata2$Fighter == fighter3),1:7]
    
    fightsAgainstCluster1 <- length(which(individualRecordTable$Cluster == 1))
    winsAgainstCluster1 <- length(which(individualRecordTable$fightResultConcat == "win_1"))
    lossAgainstCluster1 <- length(which(individualRecordTable$fightResultConcat == "loss_1"))
    winPercentage1 <- (winsAgainstCluster1/fightsAgainstCluster1)
    lossPercentage1 <-(lossAgainstCluster1/fightsAgainstCluster1)
    
    fightsAgainstCluster2 <- length(which(individualRecordTable$Cluster == 2))
    winsAgainstCluster2 <- length(which(individualRecordTable$fightResultConcat == "win_2"))
    lossAgainstCluster2 <- length(which(individualRecordTable$fightResultConcat == "loss_2"))
    winPercentage2 <- (winsAgainstCluster2/fightsAgainstCluster2)
    lossPercentage2 <-(lossAgainstCluster2/fightsAgainstCluster2)
    
    fightsAgainstCluster3 <- length(which(individualRecordTable$Cluster == 3))
    winsAgainstCluster3 <- length(which(individualRecordTable$fightResultConcat == "win_3"))
    lossAgainstCluster3 <- length(which(individualRecordTable$fightResultConcat == "loss_3"))
    winPercentage3 <- (winsAgainstCluster3/fightsAgainstCluster3)
    lossPercentage3 <-(lossAgainstCluster3/fightsAgainstCluster3)
    
    fightsAgainstCluster4 <- length(which(individualRecordTable$Cluster == 4))
    winsAgainstCluster4 <- length(which(individualRecordTable$fightResultConcat == "win_4"))
    lossAgainstCluster4 <- length(which(individualRecordTable$fightResultConcat == "loss_4"))
    winPercentage4 <- (winsAgainstCluster4/fightsAgainstCluster4)
    lossPercentage4 <-(lossAgainstCluster4/fightsAgainstCluster4)
    
    
    fightPercentData <- as.data.frame(matrix(ncol=4,nrow = 2," "),stringsAsFactors = F)
    
    colnames(fightPercentData)[colnames(fightPercentData)=="V1"] <- "Cluster 1 Win/Loss Percentage"
    colnames(fightPercentData)[colnames(fightPercentData)=="V2"] <- "Cluster 2 Win/Loss Percentage"
    colnames(fightPercentData)[colnames(fightPercentData)=="V3"] <- "Cluster 3 Win/Loss Percentage"
    colnames(fightPercentData)[colnames(fightPercentData)=="V4"] <- "Cluster 4 Win/Loss Percentage"
    rownames(fightPercentData)[rownames(fightPercentData)=="1"] <- "Win Percentage"
    rownames(fightPercentData)[rownames(fightPercentData)=="2"] <- "Loss Percentage"
    
    if(is.nan(winPercentage1)){winPercentage1 <- 0}
    if (is.nan(lossPercentage1)){lossPercentage1 <- 0}
    if(is.nan(winPercentage2)){winPercentage2 <- 0}
    if (is.nan(lossPercentage2)){lossPercentage2 <- 0}
    if(is.nan(winPercentage3)){winPercentage3 <- 0}
    if (is.nan(lossPercentage3)){lossPercentage3 <- 0}
    if(is.nan(winPercentage4)){winPercentage4 <- 0}
    if (is.nan(lossPercentage4)){lossPercentage4 <- 0}
    
    winPercentage1 <- round(winPercentage1, digits = 2)
    winPercentage2 <- round(winPercentage2, digits = 2)
    winPercentage3 <- round(winPercentage3, digits = 2)
    winPercentage4 <- round(winPercentage4, digits = 2)
    
    lossPercentage1 <- round(lossPercentage1, digits = 2)
    lossPercentage2 <- round(lossPercentage2, digits = 2)
    lossPercentage3 <- round(lossPercentage3, digits = 2)
    lossPercentage4 <- round(lossPercentage4, digits = 2)
    
    winPercentage1 <- (winPercentage1*100)
    lossPercentage1 <-(lossPercentage1*100)
    winPercentage2 <- (winPercentage2*100)
    lossPercentage2 <-(lossPercentage2*100)
    winPercentage3 <- (winPercentage3*100)
    lossPercentage3 <-(lossPercentage3*100)
    winPercentage4 <- (winPercentage4*100)
    lossPercentage4 <-(lossPercentage4*100)
    
    fightPercentData[1,1] <- winPercentage1
    fightPercentData[2,1] <- lossPercentage1
    fightPercentData[1,2] <- winPercentage2
    fightPercentData[2,2] <- lossPercentage2
    fightPercentData[1,3] <- winPercentage3
    fightPercentData[2,3] <- lossPercentage3
    fightPercentData[1,4] <- winPercentage4
    fightPercentData[2,4] <- lossPercentage4
    
    print(fightPercentData)
    
  })
  
  # FIGHTER V OPPONENT BAR PLOT 
  output$barPlot <- renderPlot({
    fighter <- clustfighter()
    opponent <- clustopponent()
    
    individualRecordTable <- simdata2[which(simdata2$Fighter == fighter),1:7]
    
    
    clusterNum <- simdata2[which(simdata2$Opponent == opponent), 6]
    
    
    fightsAgainstCluster1 <- length(which(individualRecordTable$Cluster == 1))
    winsAgainstCluster1 <- length(which(individualRecordTable$fightResultConcat == "win_1"))
    lossAgainstCluster1 <- length(which(individualRecordTable$fightResultConcat == "loss_1"))
    winPercentage1 <- winsAgainstCluster1/fightsAgainstCluster1
    lossPercentage1 <-lossAgainstCluster1/fightsAgainstCluster1
    
    fightsAgainstCluster2 <- length(which(individualRecordTable$Cluster == 2))
    winsAgainstCluster2 <- length(which(individualRecordTable$fightResultConcat == "win_2"))
    lossAgainstCluster2 <- length(which(individualRecordTable$fightResultConcat == "loss_2"))
    winPercentage2 <- winsAgainstCluster2/fightsAgainstCluster2
    lossPercentage2 <-lossAgainstCluster2/fightsAgainstCluster2
    
    fightsAgainstCluster3 <- length(which(individualRecordTable$Cluster == 3))
    winsAgainstCluster3 <- length(which(individualRecordTable$fightResultConcat == "win_3"))
    lossAgainstCluster3 <- length(which(individualRecordTable$fightResultConcat == "loss_3"))
    winPercentage3 <- winsAgainstCluster3/fightsAgainstCluster3
    lossPercentage3 <-lossAgainstCluster3/fightsAgainstCluster3
    
    fightsAgainstCluster4 <- length(which(individualRecordTable$Cluster == 4))
    winsAgainstCluster4 <- length(which(individualRecordTable$fightResultConcat == "win_4"))
    lossAgainstCluster4 <- length(which(individualRecordTable$fightResultConcat == "loss_4"))
    winPercentage4 <- winsAgainstCluster4/fightsAgainstCluster4
    lossPercentage4 <-lossAgainstCluster4/fightsAgainstCluster4
    
    fightPercentData <- as.data.frame(matrix(ncol=4,nrow = 2," "),stringsAsFactors = F)
    
    
    colnames(fightPercentData)[colnames(fightPercentData)=="V1"] <- "Cluster 1"
    colnames(fightPercentData)[colnames(fightPercentData)=="V2"] <- "Cluster 2"
    colnames(fightPercentData)[colnames(fightPercentData)=="V3"] <- "Cluster 3"
    colnames(fightPercentData)[colnames(fightPercentData)=="V4"] <- "Cluster 4"
    rownames(fightPercentData)[rownames(fightPercentData)=="1"] <- "Win Percentage"
    rownames(fightPercentData)[rownames(fightPercentData)=="2"] <- "Loss Percentage"
    
    if(is.nan(winPercentage1)){winPercentage1 <- 0}
    if (is.nan(lossPercentage1)){lossPercentage1 <- 0}
    if(is.nan(winPercentage2)){winPercentage2 <- 0}
    if (is.nan(lossPercentage2)){lossPercentage2 <- 0}
    if(is.nan(winPercentage3)){winPercentage3 <- 0}
    if (is.nan(lossPercentage3)){lossPercentage3 <- 0}
    if(is.nan(winPercentage4)){winPercentage4 <- 0}
    if (is.nan(lossPercentage4)){lossPercentage4 <- 0}
    
    
    fightPercentData[1,1] <- winPercentage1
    fightPercentData[2,1] <- lossPercentage1
    fightPercentData[1,2] <- winPercentage2
    fightPercentData[2,2] <- lossPercentage2
    fightPercentData[1,3] <- winPercentage3
    fightPercentData[2,3] <- lossPercentage3
    fightPercentData[1,4] <- winPercentage4
    fightPercentData[2,4] <- lossPercentage4
    
    
    if(clusterNum == 1 & !is.nan(winPercentage1)){
      winPercentage <- winPercentage1
    } else if(clusterNum == 2 & !is.nan(winPercentage2)){
      winPercentage <- winPercentage2
    }else if(clusterNum == 3 & !is.nan(winPercentage2)){
      winPercentage <- winPercentage3
    }else if(clusterNum == 4 & !is.nan(winPercentage2)){
      winPercentage <- winPercentage4
    }
    
    
    if (clusterNum == 1 & !is.nan(winPercentage1)){
      lossPercentage <- lossPercentage1
    }else if (clusterNum == 2 & !is.nan(winPercentage1)){
      lossPercentage <- lossPercentage2
    }else if (clusterNum == 3 & !is.nan(winPercentage1)){
      lossPercentage <- lossPercentage3
    }else if (clusterNum == 4 & !is.nan(winPercentage1)){
      lossPercentage <- lossPercentage4
    }
    
    typeslices <- c(winPercentage, lossPercentage)
    typelabels <- c("Win Percent", "Loss Percent")
    typecolors <- c("cornflowerblue","coral3")
    
    winPercentage10 = 0
    lossPercentage10 = 0
    
    typeslices2 <- c(winPercentage10, lossPercentage10)
    typelabels2 <- c("NO PREVIOUS MATCHES IN OPPONENTS CLUSTER", "Loss Percent")
    typecolors2 <- c("cornflowerblue","coral3")
    
    if(winPercentage == 0 & lossPercentage == 0){
      barplot(typeslices2, width = .25,
              names.arg = c("Win % Against Opponents Cluster", "Loss % Against Opponents Cluster"),
              col = typecolors,
              ylab = "Percentages",
              ylim = c(0,1),
              main="NO PREVIOUS MATCHES IN OPPONENTS CLUSTER")
    }else{
      barplot(typeslices, width = .25,
              names.arg = c("Win % Against Opponents Cluster", "Loss % Against Opponents Cluster"),
              col = typecolors,
              ylab = "Percentages",
              ylim = c(0,1),
              main="Fighters Results")
      # pie(x = typeslices, labels = typelabels,col = typecolors,  main= "Pie chart of Win Percentages")
    }
    
  })
  
  # OPPONENT V FIGHTER BAR PLOT 
  output$barPlot2 <- renderPlot({
    opponent <- clustfighter()
    fighter <- clustopponent()
    
    individualRecordTable <- simdata2[which(simdata2$Fighter == fighter),1:7]
    
    
    clusterNum <- simdata2[which(simdata2$Opponent == opponent), 6]
    
    fightsAgainstCluster1 <- length(which(individualRecordTable$Cluster == 1))
    winsAgainstCluster1 <- length(which(individualRecordTable$fightResultConcat == "win_1"))
    lossAgainstCluster1 <- length(which(individualRecordTable$fightResultConcat == "loss_1"))
    winPercentage1 <- winsAgainstCluster1/fightsAgainstCluster1
    lossPercentage1 <-lossAgainstCluster1/fightsAgainstCluster1
    
    fightsAgainstCluster2 <- length(which(individualRecordTable$Cluster == 2))
    winsAgainstCluster2 <- length(which(individualRecordTable$fightResultConcat == "win_2"))
    lossAgainstCluster2 <- length(which(individualRecordTable$fightResultConcat == "loss_2"))
    winPercentage2 <- winsAgainstCluster2/fightsAgainstCluster2
    lossPercentage2 <-lossAgainstCluster2/fightsAgainstCluster2
    
    fightsAgainstCluster3 <- length(which(individualRecordTable$Cluster == 3))
    winsAgainstCluster3 <- length(which(individualRecordTable$fightResultConcat == "win_3"))
    lossAgainstCluster3 <- length(which(individualRecordTable$fightResultConcat == "loss_3"))
    winPercentage3 <- winsAgainstCluster3/fightsAgainstCluster3
    lossPercentage3 <-lossAgainstCluster3/fightsAgainstCluster3
    
    fightsAgainstCluster4 <- length(which(individualRecordTable$Cluster == 4))
    winsAgainstCluster4 <- length(which(individualRecordTable$fightResultConcat == "win_4"))
    lossAgainstCluster4 <- length(which(individualRecordTable$fightResultConcat == "loss_4"))
    winPercentage4 <- winsAgainstCluster4/fightsAgainstCluster4
    lossPercentage4 <-lossAgainstCluster4/fightsAgainstCluster4
    
    fightPercentData <- as.data.frame(matrix(ncol=4,nrow = 2," "),stringsAsFactors = F)
    
    
    colnames(fightPercentData)[colnames(fightPercentData)=="V1"] <- "Cluster 1"
    colnames(fightPercentData)[colnames(fightPercentData)=="V2"] <- "Cluster 2"
    colnames(fightPercentData)[colnames(fightPercentData)=="V3"] <- "Cluster 3"
    colnames(fightPercentData)[colnames(fightPercentData)=="V4"] <- "Cluster 4"
    rownames(fightPercentData)[rownames(fightPercentData)=="1"] <- "Win Percentage"
    rownames(fightPercentData)[rownames(fightPercentData)=="2"] <- "Loss Percentage"
    
    if(is.nan(winPercentage1)){winPercentage1 <- 0}
    if (is.nan(lossPercentage1)){lossPercentage1 <- 0}
    if(is.nan(winPercentage2)){winPercentage2 <- 0}
    if (is.nan(lossPercentage2)){lossPercentage2 <- 0}
    if(is.nan(winPercentage3)){winPercentage3 <- 0}
    if (is.nan(lossPercentage3)){lossPercentage3 <- 0}
    if(is.nan(winPercentage4)){winPercentage4 <- 0}
    if (is.nan(lossPercentage4)){lossPercentage4 <- 0}
    
    
    fightPercentData[1,1] <- winPercentage1
    fightPercentData[2,1] <- lossPercentage1
    fightPercentData[1,2] <- winPercentage2
    fightPercentData[2,2] <- lossPercentage2
    fightPercentData[1,3] <- winPercentage3
    fightPercentData[2,3] <- lossPercentage3
    fightPercentData[1,4] <- winPercentage4
    fightPercentData[2,4] <- lossPercentage4
    
    
    if(clusterNum == 1 & !is.nan(winPercentage1)){
      winPercentage <- winPercentage1
    } else if(clusterNum == 2 & !is.nan(winPercentage2)){
      winPercentage <- winPercentage2
    }else if(clusterNum == 3 & !is.nan(winPercentage2)){
      winPercentage <- winPercentage3
    }else if(clusterNum == 4 & !is.nan(winPercentage2)){
      winPercentage <- winPercentage4
    }
    
    
    if (clusterNum == 1 & !is.nan(winPercentage1)){
      lossPercentage <- lossPercentage1
    }else if (clusterNum == 2 & !is.nan(winPercentage1)){
      lossPercentage <- lossPercentage2
    }else if (clusterNum == 3 & !is.nan(winPercentage1)){
      lossPercentage <- lossPercentage3
    }else if (clusterNum == 4 & !is.nan(winPercentage1)){
      lossPercentage <- lossPercentage4
    }
    
    typeslices <- c(winPercentage, lossPercentage)
    typelabels <- c("Win Percent", "Loss Percent")
    typecolors <- c("cornflowerblue","coral3")
    
    winPercentage10 = 0
    lossPercentage10 = 0
    
    typeslices2 <- c(winPercentage10, lossPercentage10)
    typelabels2 <- c("NO PREVIOUS MATCHES FIGHTERS CLUSTER", "Loss Percent")
    typecolors2 <- c("cornflowerblue","coral3")
    
    if(winPercentage == 0 & lossPercentage == 0){
      barplot(typeslices2, width = .25,
              names.arg = c("Win % Against Fighters Cluster", "Loss % Against Fighters Cluster"),
              col = typecolors,
              ylab = "Percentages",
              ylim = c(0,1),
              main="NO PREVIOUS MATCHES IN FIGHTERS CLUSTER")
    }else{
      barplot(typeslices, width = .25,
              names.arg = c("Win % Against Fighters Cluster", "Loss % Against Fighters Cluster"),
              col = typecolors,
              ylab = "Percentages",
              ylim = c(0,1),
              main="Opponents Results")
      # pie(x = typeslices, labels = typelabels,col = typecolors,  main= "Pie chart of Win Percentages")
    }
    
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

