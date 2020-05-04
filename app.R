 
#y
# 
source('helpers.R')

library(shiny)
 library(ggplot2)
  
# Define UI ----
ui <- fluidPage(  
  titlePanel('SEIR model to model ICUs demand in Bogota during Covid-19 outbreak'  )  , 
  
  tags$div( tags$p("This   shiny app  fits the SEIR model in", tags$a(href="https://www.medrxiv.org/content/10.1101/2020.04.14.20065466v1", "this paper."),
    "It  can be used to estimate the number of ICU beds for a city or a clinic. 
    The initial values should correspond to 'Date Start Plot'. 
Some of the most important values are   the probabilities  \\(  {p_{ICU}  } , {p_{NoICU}  } , {p_{Home}  } \\).
Note that \\(  {p_{ICU}  } +  {p_{NoICU}  } + {p_{Home}  } =1\\). These probabilites are global probabilites, For example,   \\(  {p_{ICU}  }   \\)  is the probability that an infected (including  asymptomatic and symptomatic) person requires ICU care." , style = "color:#BD0759;"), 
    tags$p("The parameters and initial values should be carefully chosen according to their interpretation.")
  ),
  
 
  
  
  
  
 fluidRow(    tags$head(
   tags$style(type="text/css","label{ display: table-cell; text-align: center;vertical-align: middle; } .form-group { display: table-row;}") 
 ),withMathJax() ,  column(2,  textInput(inputId="R0", label="", "2.5")    ), column(10,  p("\\( R(t_0) \\) :     reproduction number-   Stepwise function (\\(t_0, t_2, t_2\\))"))),  
 fluidRow( withMathJax() ,  column(2,  textInput(inputId="R2", label="", "1.5")    ), column(10,  p("\\( R(t_1) \\) :     reproduction number-   Stepwise function (\\(t_0, t_2, t_2\\))"))),  
 fluidRow( withMathJax() ,  column(2,  textInput(inputId="R3", label="", "2")    ), column(10,  p("\\( R(t_2) \\) :     reproduction number-   Stepwise function (\\(t_0, t_2, t_2\\))"))),  
 fluidRow( withMathJax() ,  column(2,  textInput(inputId="TimeMax", label="", "200")    ), column(10,  p("Time max to be plotted"))),  
 fluidRow(  column(2, textInput(inputId="datestart", label="", "2020-03-28")    ),  column(10,  p("Time start plot"))),
            
  fluidRow(  column(2, textInput(inputId="dateStartV", label="", "2020-03-06")    ),  column(10,  p(" \\( \\ t_0 \\):   Time virus starts"))),
 fluidRow(  column(2, textInput(inputId="lockdownDate", label="", "2020-03-29")    ), column(10, p( " \\( \\ t_1 \\):   Time intervention 1 starts"))),
fluidRow(  column(2, textInput(inputId="lockdownDate2",label="", "2020-04-25")), column(10, p(" \\( \\ t_2 \\):   Time intervention 2 starts"))),
fluidRow(  column(2, textInput(inputId="N0",label="", "8000000")),  column(10, p("\\( N_0\\) : Total population "))),
 fluidRow(  column(2, textInput(inputId="share",label="", "1")),  column(10, p("Market share of clinic- 1 for entire city/town"))),
 fluidRow(  column(2, textInput(inputId="E0",label="", "1262")),  column(10, p("\\( E_0\\) :     Initial exposed at \\( t_0\\)  "))),
 fluidRow(  column(2, textInput(inputId="I_ICU0",label="", "13")), column(10, p(  "\\(I_{I_0}\\) :     Initial infected in ICU  at  \\( t_0\\)   " ))),
 fluidRow(  column(2, textInput(inputId="I_NoICU0",label="", '68')), column(10,  p(  "\\(I_{NoI_0}\\) :     Initial infected in Hospital not  ICU  at  \\( t_0\\)  " ))),  
 fluidRow(  column(2, textInput(inputId="I_Home0",label="", '423.78')), column(10,  p(  "\\(I_{H_0}\\) :     Initial infected  at home \\( t_0\\)   "))),
fluidRow(  column(2, textInput(inputId="Rec0",label="", "39")), column(10,   p(  "\\(R_0\\) :     Initial recovered \\( t_0\\)   "))),
fluidRow(  column(2, textInput(inputId="D0",label="", "17")),  column(10,  p( "\\(D_0\\) :     Initial deaths at \\( t_0\\)   "))),
fluidRow(  column(2, textInput(inputId="alpha",label="", "5.2")), column(10, p(  "\\(\\alpha\\) :     mean incubation period   "))),
fluidRow(  column(2, textInput(inputId="bICU",label="" , "7")), column(10,  p( "\\(b_I\\) :     mean infectious period  for ICU patient "))),
fluidRow(  column(2, textInput(inputId="bNoICU",label="", "5")), column(10,  p(  "\\(b_{NoI}\\) :     mean infectious period  for Not ICU patient "))),
fluidRow(  column(2, textInput(inputId="bHome",label="", "4")),  column(10,  p(  "\\(b_H\\) :     mean infectious period  for Home patient "))),
fluidRow(  column(2, textInput(inputId="pICU",label="","0.026")), column(10,  p( "\\(p_I\\) :     probability fo case being ICU " ))),
fluidRow(  column(2, textInput(inputId="pNoICU",label="", "0.134")), column(10,  p(  "\\(p_{NoI}\\) :    probability of case being  hospital, but not ICU  "))),
fluidRow(  column(2, textInput(inputId="pHome",label="","0.84")), column(10, p( "\\(p_H\\) :     probability of case being Home case"))),
fluidRow(  column(2, textInput(inputId="d",label="", "0.5")), column(10, p( "\\(d\\) :     probability of death if in ICU")),
 mainPanel( h3("Results from SEIR model - All"),  plotOutput(  "coolplot"),  br(),
            h3("Results from SEIR model - Infections and Deaths"),
            plotOutput(  "coolplot2"), br(),
            h3("Results from SEIR model - Infections in Hospital and ICU"),
             plotOutput(  "coolplot3"), 
            br(),
            h3("Tables- Prevalence "),
                   tableOutput( "results") )      
           )) 
 
 

 
 
 
 

# Define server logic ----
server2 <- function(input, output) {
  
  
 
 
  
  
  output$coolplot  <- renderPlot({
    datestart  <-  as.Date(input$datestart)
    TimeMax<-   as.numeric(input$TimeMax )
    dateStartV<- as.Date(input$dateStartV)
    lockdownDate <- as.Date(input$lockdownDate) 
    lockdownDate2 <- as.Date(input$lockdownDate2) 
    
    Kappa <-  ( 1/as.numeric(input$alpha ))
    kappaICU <- as.numeric(input$pICU) *  Kappa
    kappaNoICU<- as.numeric(input$pNoICU) *  Kappa
    kappaHome<- as.numeric(input$pHome) *  Kappa
    gammaICU <-   -(-1/as.numeric(input$bICU ))
    gammaHome <- -(-1/as.numeric(input$bHome ))
    gammaNoICU <--(-1/as.numeric(input$bNoICU ))
    
    
    R0   <-  as.numeric(input$R0 )
    R2   <-  as.numeric(input$R2 )
    R3   <-  as.numeric(input$R3)
 
    
    d <- as.numeric(input$d  )
    share <- as.numeric( input$share )
    N0 <-  share*as.numeric(input$N0 )
    E0 <- as.numeric(input$E0 )
    I_Home0 <- as.numeric(input$I_Home0 )
    I_ICU0 <- as.numeric(input$I_ICU0 )
    I_NoICU0 <- as.numeric(input$I_NoICU0 )
    D0 <- as.numeric(input$D0  )
    Rec0 <- as.numeric(input$Rec0  )
    S0  <- N0 - E0 - I_Home0 -  I_ICU0 - I_NoICU0 -Rec0 -D0
    
    
    
    tMax <- as.numeric(TimeMax)
    tStartplot <- as.Date(datestart )
    tStartVirus <-  as.Date(dateStartV)
    tLock<-  as.Date(lockdownDate) 
    tLock2<-  as.Date(lockdownDate2) 
    
    
    tStartVirus2 <- as.numeric(  tStartVirus ) -  as.numeric(tStartplot) 
    tLocka  <-   as.numeric( tLock) -  as.numeric(tStartplot) 
    tLockb  <-   as.numeric( tLock2) -  as.numeric(tStartplot) 
    initA <-c(S0, E0,   I_ICU0, I_NoICU0, I_Home0, Rec0, D0) /S0
    
    
    beta <-  Beta0(R0, gammaICU, gammaNoICU,gammaHome, kappaICU, kappaNoICU, kappaHome ) 
    
    beta2 <-  Betat(R2, gammaICU, gammaNoICU,gammaHome, kappaICU, kappaNoICU, kappaHome, d, t=(tLocka-1), initA, t0=1, t1=tLocka , t2=tLockb, beta,beta, beta) 
    beta3 <-  Betat(R3, gammaICU, gammaNoICU,gammaHome, kappaICU, kappaNoICU, kappaHome, d, t=(tLockb-1), initA, t0=1, t1=tLocka , t2=tLockb, beta,beta2, beta2) 
    
    par<-c(beta,beta2, beta3,kappaICU,kappaNoICU, kappaHome,   gammaICU, gammaNoICU, gammaHome, d, tLocka,tLockb  ,N=1)
    
    
  
    t<-seq(1,TimeMax)
    
    
    
    
    sol<-lsoda(initA,t,seir_ode,par, jactype = "fullint" )
    
    
    # Plot solution
    
    tDate<- as.Date(1:TimeMax, origin= datestart)
    
    S <- sol[,2]
    E <- sol[,3]
    I_ICU <- sol[,4]
    I_NoICU <- sol[,5]
    I_Home  <- sol[,6]
    R <- sol[,7]
    D <- sol[,8]
    sol[, 2:ncol(sol)] <- N0*  sol[, 2:ncol(sol)] 
    
    tDate<- as.Date(1:TimeMax, origin= datestart)
    
    ## Plot
    sol  <- data.frame(sol)
    
    colnames(sol) <- c('time', 'S', 'E', 'IU', "INU", "IH", 'R', 'D' )
    sol$tDate<-  tDate
    sol$I  <- sol$IU+ sol$INU+ sol$IH 
    
    p <- ggplot(data = sol, mapping = aes(x = tDate, y = S, color='Susceptible') ) +  
      geom_line() +
      geom_line(aes(x = tDate, y = E , color='Exposed') ) + 
      geom_line(aes(x = tDate, y = I , color='Total Infected' )  ) + 
      geom_line(aes(x = tDate, y = INU , color='Infected Hosp, No ICU') ) + 
      geom_line(aes(x = tDate, y = IU, color='Infected ICU' ) ) + 
      geom_line(aes(x = tDate, y = R , color='Recovered') ) + 
      scale_color_manual( values=c('Susceptible'= "green", 'Exposed'= "orange", 
                                   'Total Infected'= "brown",  'Infected ICU'= "red",   
                                   'Infected Hosp, No ICU'= "violet", 'Recovered'= "blue"    ))+
      theme(legend.position="top")+
      labs( x="Time ", y="Number of people", color='')
    
    print(p)
    tDate<- as.Date(1:TimeMax, origin= datestart)
    
     
   
    
  }  )
   
  
  
  
  
  output$coolplot2  <- renderPlot({
    datestart  <-  as.Date(input$datestart)
    TimeMax<-   as.numeric(input$TimeMax )
    dateStartV<- as.Date(input$dateStartV)
    lockdownDate <- as.Date(input$lockdownDate) 
    lockdownDate2 <- as.Date(input$lockdownDate2) 
    
    Kappa <-  ( 1/as.numeric(input$alpha ))
    kappaICU <- as.numeric(input$pICU) *  Kappa
    kappaNoICU<- as.numeric(input$pNoICU) *  Kappa
    kappaHome<- as.numeric(input$pHome) *  Kappa
    gammaICU <-   -(-1/as.numeric(input$bICU ))
    gammaHome <- -(-1/as.numeric(input$bHome ))
    gammaNoICU <--(-1/as.numeric(input$bNoICU ))
    
    
    R0   <-  as.numeric(input$R0 )
    R2   <-  as.numeric(input$R2 )
    R3   <-  as.numeric(input$R3)
    
    
    d <- as.numeric(input$d  )
    share <- as.numeric( input$share )
    N0 <-  share*as.numeric(input$N0 )
    E0 <- as.numeric(input$E0 )
    I_Home0 <- as.numeric(input$I_Home0 )
    I_ICU0 <- as.numeric(input$I_ICU0 )
    I_NoICU0 <- as.numeric(input$I_NoICU0 )
    D0 <- as.numeric(input$D0  )
    Rec0 <- as.numeric(input$Rec0  )
    S0  <- N0 - E0 - I_Home0 -  I_ICU0 - I_NoICU0 -Rec0 -D0
    
    
    
    tMax <- as.numeric(TimeMax)
    tStartplot <- as.Date(datestart )
    tStartVirus <-  as.Date(dateStartV)
    tLock<-  as.Date(lockdownDate) 
    tLock2<-  as.Date(lockdownDate2) 
    
    
    tStartVirus2 <- as.numeric(  tStartVirus ) -  as.numeric(tStartplot) 
    tLocka  <-   as.numeric( tLock) -  as.numeric(tStartplot) 
    tLockb  <-   as.numeric( tLock2) -  as.numeric(tStartplot) 
    
    initA<-c(S0, E0,   I_ICU0, I_NoICU0, I_Home0, Rec0, D0) /S0
    
    beta <-  Beta0(R0, gammaICU, gammaNoICU,gammaHome, kappaICU, kappaNoICU, kappaHome ) 
    
    beta2 <-  Betat(R2, gammaICU, gammaNoICU,gammaHome, kappaICU, kappaNoICU, kappaHome, d, t=(tLocka-1), initA, t0=1, t1=tLocka , t2=tLockb, beta,beta, beta) 
    beta3 <-  Betat(R3, gammaICU, gammaNoICU,gammaHome, kappaICU, kappaNoICU, kappaHome, d, t=(tLockb-1), initA, t0=1, t1=tLocka , t2=tLockb, beta,beta2, beta2) 
    
    par<-c(beta,beta2, beta3,kappaICU,kappaNoICU, kappaHome,   gammaICU, gammaNoICU, gammaHome, d, tLocka,tLockb  ,N=1)
    
    
    
    t<-seq(1,TimeMax)
    
    
    
    
    sol<-lsoda(initA,t,seir_ode,par, jactype = "fullint" )
    
    
    # Plot solution
    
    tDate<- as.Date(1:TimeMax, origin= datestart)
    
    S <- sol[,2]
    E <- sol[,3]
    I_ICU <- sol[,4]
    I_NoICU <- sol[,5]
    I_Home  <- sol[,6]
    R <- sol[,7]
    D <- sol[,8]
    sol[, 2:ncol(sol)] <- N0*  sol[, 2:ncol(sol)] 
    
    tDate<- as.Date(1:TimeMax, origin= datestart)
    
    ## Plot
    sol  <- data.frame(sol)
    
    colnames(sol) <- c('time', 'S', 'E', 'IU', "INU", "IH", 'R', 'D' )
    sol$tDate<-  tDate
    sol$I  <- sol$IU+ sol$INU+ sol$IH 
    
    p <-   ggplot(data = sol) +  
      geom_line(aes(x = tDate, y = I , color='Total Infected' )  ) + 
      geom_line(aes(x = tDate, y = INU , color='Infected Hosp, No ICU') ) + 
      geom_line(aes(x = tDate, y = IU, color='Infected ICU' ) ) +  
      geom_line(aes(x = tDate, y = D, color='Deaths' ) ) +  
      scale_color_manual( values=c( 
        'Total Infected'= "brown",  'Infected ICU'= "red",   
        'Infected Hosp, No ICU'= "violet", 'Deaths'='grey44'    ))+
      theme(legend.position="top")+
      labs(    x="Time ", y="Number of people", color='')
    
    
    print(p)
    tDate<- as.Date(1:TimeMax, origin= datestart)
    
    
    
    
  }  )
 
  
  
   
  output$coolplot3  <- renderPlot({
    datestart  <-  as.Date(input$datestart)
    TimeMax<-   as.numeric(input$TimeMax )
    dateStartV<- as.Date(input$dateStartV)
    lockdownDate <- as.Date(input$lockdownDate) 
    lockdownDate2 <- as.Date(input$lockdownDate2) 
    
    Kappa <-  ( 1/as.numeric(input$alpha ))
    kappaICU <- as.numeric(input$pICU) *  Kappa
    kappaNoICU<- as.numeric(input$pNoICU) *  Kappa
    kappaHome<- as.numeric(input$pHome) *  Kappa
    gammaICU <-   -(-1/as.numeric(input$bICU ))
    gammaHome <- -(-1/as.numeric(input$bHome ))
    gammaNoICU <--(-1/as.numeric(input$bNoICU ))
    
    
    R0   <-  as.numeric(input$R0 )
    R2   <-  as.numeric(input$R2 )
    R3   <-  as.numeric(input$R3)
    
    
    d <- as.numeric(input$d  )
    share <- as.numeric( input$share )
    N0 <-  share*as.numeric(input$N0 )
    E0 <- as.numeric(input$E0 )
    I_Home0 <- as.numeric(input$I_Home0 )
    I_ICU0 <- as.numeric(input$I_ICU0 )
    I_NoICU0 <- as.numeric(input$I_NoICU0 )
    D0 <- as.numeric(input$D0  )
    Rec0 <- as.numeric(input$Rec0  )
    S0  <- N0 - E0 - I_Home0 -  I_ICU0 - I_NoICU0 -Rec0 -D0
    
    
    
    tMax <- as.numeric(TimeMax)
    tStartplot <- as.Date(datestart )
    tStartVirus <-  as.Date(dateStartV)
    tLock<-  as.Date(lockdownDate) 
    tLock2<-  as.Date(lockdownDate2) 
    
    
    tStartVirus2 <- as.numeric(  tStartVirus ) -  as.numeric(tStartplot) 
    tLocka  <-   as.numeric( tLock) -  as.numeric(tStartplot) 
    tLockb  <-   as.numeric( tLock2) -  as.numeric(tStartplot) 
    
    initA<-c(S0, E0,   I_ICU0, I_NoICU0, I_Home0, Rec0, D0) /S0
    
    beta <-  Beta0(R0, gammaICU, gammaNoICU,gammaHome, kappaICU, kappaNoICU, kappaHome ) 
    
    beta2 <-  Betat(R2, gammaICU, gammaNoICU,gammaHome, kappaICU, kappaNoICU, kappaHome, d, t=(tLocka-1), initA, t0=1, t1=tLocka , t2=tLockb, beta,beta, beta) 
    beta3 <-  Betat(R3, gammaICU, gammaNoICU,gammaHome, kappaICU, kappaNoICU, kappaHome, d, t=(tLockb-1), initA, t0=1, t1=tLocka , t2=tLockb, beta,beta2, beta2) 
    
    par<-c(beta,beta2, beta3,kappaICU,kappaNoICU, kappaHome,   gammaICU, gammaNoICU, gammaHome, d, tLocka,tLockb  ,N=1)
    
    
    
    t<-seq(1,TimeMax)
    
    
    
    
    sol<-lsoda(initA,t,seir_ode,par, jactype = "fullint" )
    
    
    # Plot solution
    
    tDate<- as.Date(1:TimeMax, origin= datestart)
    
    S <- sol[,2]
    E <- sol[,3]
    I_ICU <- sol[,4]
    I_NoICU <- sol[,5]
    I_Home  <- sol[,6]
    R <- sol[,7]
    D <- sol[,8]
    sol[, 2:ncol(sol)] <- N0*  sol[, 2:ncol(sol)] 
    
    tDate<- as.Date(1:TimeMax, origin= datestart)
    
    ## Plot
    sol  <- data.frame(sol)
    
    colnames(sol) <- c('time', 'S', 'E', 'IU', "INU", "IH", 'R', 'D' )
    sol$tDate<-  tDate
    sol$I  <- sol$IU+ sol$INU+ sol$IH 
    
    p <-     ggplot(data = sol) +   
      geom_line(aes(x = tDate, y = INU , color='Infected Hosp, No ICU') ) + 
      geom_line(aes(x = tDate, y = IU, color='Infected ICU' ) ) +  
      geom_line(aes(x = tDate, y = 1000, color='Current ICU Beds' ) ) +
      scale_color_manual( values=c(  'Infected ICU'= "red",   
                                     'Infected Hosp, No ICU'= "violet" ,  'Current ICU Beds'= 'yellow'   ))+
      theme(legend.position="top")+
      labs(   x="Time ", y="Number of people", color='')
    
    print(p)
    tDate<- as.Date(1:TimeMax, origin= datestart)
    
    
    
    
  }  )
  
  
  
  
 
  
  output$results  <- renderTable({
    
    datestart  <-  as.Date(input$datestart)
    TimeMax<-   as.numeric(input$TimeMax )
    dateStartV<- as.Date(input$dateStartV)
    lockdownDate <- as.Date(input$lockdownDate) 
    lockdownDate2 <- as.Date(input$lockdownDate2) 
    
    Kappa <-  ( 1/as.numeric(input$alpha ))
    kappaICU <- as.numeric(input$pICU) *  Kappa
    kappaNoICU<- as.numeric(input$pNoICU) *  Kappa
    kappaHome<- as.numeric(input$pHome) *  Kappa
    gammaICU <-   -(-1/as.numeric(input$bICU ))
    gammaHome <- -(-1/as.numeric(input$bHome ))
    gammaNoICU <--(-1/as.numeric(input$bNoICU ))
    
    
    R0   <-  as.numeric(input$R0 )
    R2   <-  as.numeric(input$R2 )
    R3   <-  as.numeric(input$R3)
 
    
    d <- as.numeric(input$d  )
    share <- as.numeric( input$share )
    N0 <-  share*as.numeric(input$N0 )
    E0 <- as.numeric(input$E0 )
    I_Home0 <- as.numeric(input$I_Home0 )
    I_ICU0 <- as.numeric(input$I_ICU0 )
    I_NoICU0 <- as.numeric(input$I_NoICU0 )
    D0 <- as.numeric(input$D0  )
    Rec0 <- as.numeric(input$Rec0  )
    S0  <- N0 - E0 - I_Home0 -  I_ICU0 - I_NoICU0 -Rec0 -D0
    
    tMax <- as.numeric(TimeMax)
    tStartplot <- as.Date(datestart )
    tStartVirus <-  as.Date(dateStartV)
    tLock<-  as.Date(lockdownDate) 
    tLock2<-  as.Date(lockdownDate2) 
    
    
    tStartVirus2 <- as.numeric(  tStartVirus ) -  as.numeric(tStartplot) 
    tLocka  <-   as.numeric( tLock) -  as.numeric(tStartplot) 
    tLockb  <-   as.numeric( tLock2) -  as.numeric(tStartplot) 
    
    initA<-c(S0, E0,   I_ICU0, I_NoICU0, I_Home0, Rec0, D0) /S0

    beta <-  Beta0(R0, gammaICU, gammaNoICU,gammaHome, kappaICU, kappaNoICU, kappaHome ) 
    
    beta2 <-  Betat(R2, gammaICU, gammaNoICU,gammaHome, kappaICU, kappaNoICU, kappaHome, d, t=(tLocka-1), initA, t0=1, t1=tLocka , t2=tLockb, beta,beta, beta) 
    beta3 <-  Betat(R3, gammaICU, gammaNoICU,gammaHome, kappaICU, kappaNoICU, kappaHome, d, t=(tLockb-1), initA, t0=1, t1=tLocka , t2=tLockb, beta,beta2, beta2) 
    
    par<-c(beta,beta2, beta3,kappaICU,kappaNoICU, kappaHome,   gammaICU, gammaNoICU, gammaHome, d, tLocka,tLockb  ,N=1)
    
    t<-seq(1,TimeMax)
    
    sol<-lsoda(initA,t,seir_ode,par, jactype = "fullint" )
    
    
    # Plot solution
    S <- sol[,2]
    E <- sol[,3]
    I_ICU <- sol[,4]
    I_NoICU <- sol[,5]
    I_Home  <- sol[,6]
    R <- sol[,7]
    D <- sol[,8]
    
    tDate<- as.Date(1:TimeMax, origin= datestart)
    
    data.frame(date= as.character(tDate) , N0*data.frame(  Suc=S,  Exposed= E, I_ICU =I_ICU , I_NoICU =I_NoICU, I_Home =I_Home , R =R , D=D ))
    
  }) 
  
  
  
  
  ## For models 
  

  

  
}

  
shinyApp(ui,server2)






 