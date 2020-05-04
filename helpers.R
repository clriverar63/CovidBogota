## Authorize app in shiny io

# 
# Load   library
pac1 <-  'rootSolve'
pac2 <- 'deSolve'
pac3 <- 'rsconnect'
pac4 <- 'OpenImageR'
PAC<-  rownames(installed.packages())

if( ! pac1 %in% PAC){ install.packages(pac1)}
if( ! pac2 %in% PAC){ install.packages(pac2)}
if( ! pac3 %in% PAC){ install.packages(pac3)}
if( ! pac4 %in% PAC){ install.packages(pac4)}

library(deSolve)
library(rootSolve)
library(rsconnect)
library(OpenImageR)




## Plot for the clinic. 
## Suppose that hte clinic had assigned 5% of the population of Bogota.

# Function to return derivatives of SEIR model
   


seir_ode<-function(t,Y,par){
  
  S<- Y[1]
  E<- Y[2]
  IICU   <- Y[3]
  INoICU <- Y[4]
  IHome  <- Y[5]
  R<- Y[6]
  D<- Y[7] 
  
  
  beta<-as.numeric(par[1] )  ## R0= 2.8 before lockdown  - beta= R0*gamma # https://www.ijidonline.com/article/S1201-9712(20)30117-X/fulltext
  beta2<-as.numeric(par[2] )  ## R0= 2  -> beta= R0*gamma
  beta3<-as.numeric(par[3] )  ## R0= 2  -> beta= R0*gamma
  
  sigmaICU<-as.numeric(par[4])   ## m 
  sigmaNoICU<-as.numeric(par[5])    ## m 
  sigmaHome<-as.numeric(par[6]  )  ##   
  gammaICU<-as.numeric(par[7] )   ## Mean infectious period  
  gammaNoICU<-as.numeric(par[8])    ## Mean infectious period  
  gammaHome<-as.numeric(par[9] )   ## Mean infectious period  
  d  <-as.numeric(par[10]   )   ## prop severe cases d =0.2
  lockdownDay  <- par[11]
  lockdownDay2  <- par[12]
  N <- as.numeric(par[13])
  indtime1  <-  t <  lockdownDay  
  indtime2  <-  t <  lockdownDay2 &   t >=  lockdownDay  
  indtime3  <-  t >=  lockdownDay2 
  

  
  if( indtime1) {
    dYdt<-vector(length=7)
    dYdt[1]= -beta* ( IICU +  INoICU +  IHome  ) *S/N            ## S 
    dYdt[2]=beta* ( IICU +  INoICU +  IHome  )*S/N-(sigmaICU + sigmaNoICU + sigmaHome )*E   ## E 
    dYdt[3]= sigmaICU*E - (gammaICU)*IICU      ## IICU
    dYdt[4]= sigmaNoICU*E - (gammaNoICU)*INoICU     ## INoICU
    dYdt[5]= sigmaHome*E - (gammaHome)*IHome      ## IHome
    dYdt[6]=  (1-d)*(gammaICU*IICU) + gammaNoICU*INoICU + gammaHome*IHome         ## Recovered
    dYdt[7]=  d*(gammaICU*IICU )         ## D
  }
  
  
  ## sigmaICU - prop expose that go to ICU
  ## sigmaNoICU - prop expose that go to Hospital, but tnot ICU
  ## sigmaHome - prop expose that get infected but home, but  not ICU 
  ##  gammaICU -  1/ gammaICU mean infectios period for ICU
  ##  gammaNoICU - 1/ gammaNoICU mean infectios period for No ICU
  ##  gammaHome  - 1/ gammaHome mean infectios period for Home
  
  if(  indtime2) {
    dYdt<-vector(length=7)
    dYdt[1]= -beta2* ( IICU +  INoICU +  IHome  ) *S/N            ## S 
    dYdt[2]=beta2* ( IICU +  INoICU +  IHome  )*S/N-(sigmaICU + sigmaNoICU + sigmaHome )*E   ## E 
    dYdt[3]= sigmaICU*E - (gammaICU)*IICU      ## IICU
    dYdt[4]= sigmaNoICU*E - (gammaNoICU)*INoICU     ## INoICU
    dYdt[5]= sigmaHome*E - (gammaHome)*IHome      ## IHome
    dYdt[6]=  (1-d)*(gammaICU*IICU) + gammaNoICU*INoICU + gammaHome*IHome         ## Recovered
    dYdt[7]=  d*(gammaICU*IICU )         ## D
  }
  
  if(  indtime3) {
    dYdt<-vector(length=7)
    dYdt[1]= -beta3* ( IICU +  INoICU +  IHome  ) *S/N            ## S 
    dYdt[2]=beta3* ( IICU +  INoICU +  IHome  )*S/N-(sigmaICU + sigmaNoICU + sigmaHome )*E   ## E 
    dYdt[3]= sigmaICU*E - (gammaICU)*IICU      ## IICU
    dYdt[4]= sigmaNoICU*E - (gammaNoICU)*INoICU     ## INoICU
    dYdt[5]= sigmaHome*E - (gammaHome)*IHome      ## IHome
    dYdt[6]=  (1-d)*(gammaICU*IICU) + gammaNoICU*INoICU + gammaHome*IHome         ## Recovered
    dYdt[7]=  d*(gammaICU*IICU )         ## D
  }
  
  
  
  return(list(dYdt))
}


Beta0  <- function(Rt, gammaICU, gammaNoICU,gammaHome, kappaICU, kappaNoICU, kappaHome  ){
   
  
  aux    <- abs(( (-gammaNoICU *  gammaHome * kappaICU - gammaICU* gammaHome *kappaNoICU - gammaICU* gammaNoICU *kappaHome))/(gammaICU *gammaNoICU *gammaHome* (kappaICU + kappaNoICU + kappaHome)))
  Betat   <-  Rt/aux
  Betat
}
Betat  <- function(Rt, gammaICU, gammaNoICU,gammaHome, kappaICU, kappaNoICU, kappaHome, d,t, init0, t0, t1, t2, beta,beta2, beta3 ){

  TimeMax <- t
  tA<-seq(1,TimeMax)
  
  lock1 <- t1
  lock2 <- t2
  
  
  par<- c(beta,beta2, beta3,kappaICU,kappaNoICU, kappaHome,   gammaICU, gammaNoICU, gammaHome, d, lock1, lock2  ,N=1)
  
  sol<-lsoda(init0,tA,seir_ode,par, jactype = "fullint" )
  
  ## Proportion of suceptibles: 
  sol <-cbind(sol)
  PS <- sol[nrow(sol),2]
 
  
aux    <-  PS*abs(( (-gammaNoICU *  gammaHome * kappaICU - gammaICU* gammaHome *kappaNoICU - gammaICU* gammaNoICU *kappaHome))/(gammaICU *gammaNoICU *gammaHome* (kappaICU + kappaNoICU + kappaHome)))
Betat   <-  Rt/aux
Betat
}

### Function to Calculate Incidence

CalculateIncid  <- function(t, betat, St, Et, IICUt, INoICUt, IHomet, Dt, Rt, kappaICU,kappaNoICU, kappaHome,   gammaICU, gammaNoICU, gammaHome, d, N)
{

  SInc      <-    -betat* ( IICUt +  INoICUt +  IHomet  ) *St/N            ## S 
  EInc      <-  betat* ( IICUt +  INoICUt +  IHomet  )*St/N  ## E 
  IICUInc   <-  kappaICU*Et      ## IICU
  INoICUInc <-  kappaNoICU*Et       ## INoICU
  IHomeInc  <-  kappaHome*Et       ## IHome
  RInc      <- (1-d)*(gammaICU*IICUt) + gammaNoICU*INoICUt + gammaHome*IHomet         ## Recovered
  DInc      <-  d*(gammaICU*IICUt )         ## D  
  
  AA        <- data.frame(S = SInc, E= EInc, IICU =IICUInc, INoICU= INoICUInc, IHome= IHomeInc, R= RInc, D= DInc)
  
  return(AA)
}
  
  
kappa <-   -(-1/5)
gammaICU = -(-1/7)
gammaHome = -(-1/5)
gammaNoICU = -(-1/4)
kappaICU =  0.05/(kappa)
 kappaNoICU=0.14/(kappa)
kappaHome =0.81/(kappa)


N    <- 800000
I0   <- 800## Cases bogota multiply by the share of the clinc  
E0  <- 1200
I_ICU0   <- 22 ## Cases bogota multiply by the share of the clinc  
I_NoICU0   <- 198 ## Cases bogota multiply by the share of the clinc  
I_Home0     <- 400  
D0  <-  17
Rec0 <- 39

S0  <- N - E0 - I0 -Rec0 -D0

t0 <- "2020-03-24"
t1 <- "2020-03-29"
t2 <- "2020-04-10"


#init<-c(S0, E0,   I_ICU0, I_NoICU0, I_Home0, Rec0, D0) /S0
#t  <- 1:10
#par<-c(beta,beta2, beta3,kappaICU,kappaNoICU, kappaHome,   gammaICU, gammaNoICU, gammaHome, d, tLocka,tLockb  ,N=1)

 
 
#
#init<-c(S0, E0,   I_ICU0, I_NoICU0, 10, R0, D0) /S0
#t<-seq(1,100)

#sol<-lsoda(init,t,seir_ode,par, jactype = "fullint" )

#    
#beta <-  Beta0(R0, gammaICU, gammaNoICU,gammaHome, kappaICU, kappaNoICU, kappaHome ) 

#beta2 <-  Betat(1.1, gammaICU, gammaNoICU,gammaHome, kappaICU, kappaNoICU, kappaHome, d, t=tLockb, init, t0=1, t1=tLocka , t2=tLockb, beta,beta, beta) 

#beta3 <-  Betat(1.5, gammaICU, gammaNoICU,gammaHome, kappaICU, kappaNoICU, kappaHome, d, t=tLockb, init, t0=1, t1=tLocka , t2=tLockb, beta,beta2, beta2) 




 