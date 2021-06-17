


initialise=function(file=NULL,data=data,S0)
{
  if(is.null(file))
  {
    output = list(
      S0       = S0,
      E0       = 0.0001,
      I0       = 0,
      beta     = 0.75,
      sig      = 1/5,
      mu       = 1/7,
      gamma    = 0.1,
      N        = S0-2,
      numSteps = length(data),
      interventioneffect=0.6,
      p=0.5,
      threshold=900
    )
  }
}


loglikelihood = function(pars,data)
{
  output = 0
  I_m = pars$seir$dIcalib
  #observed data
  I_o <- data   #Infected observed new cases
  toohigh = I_m > (pars$threshold/pars$p)
  mu = pars$p*I_m*(!toohigh) + pars$threshold*(toohigh)
  out1 = dpois(I_o,mu,log=TRUE)
  out2 = dpois(cumsum(I_o),cumsum(pars$seir$dIcalib),log=TRUE) #calibrate on cum cases
  out3 =  dnorm(0.75,I_m/NFWs,0.05,log=TRUE)
  return(sum(out1)+sum(out2)+sum(out3))
  
  # out1 = dpois(I_o,pars$seir$dIcalib,log=TRUE) #calibrate on new cases
  # out2 = dpois(cumsum(I_o),cumsum(pars$seir$dIcalib),log=TRUE) #calibrate on cum cases
  # output = sum(out1) +sum(out2)
  # return(output)
}


mh=function(olp,nep,data)
{
  reject=FALSE
  if(nep$beta<0)reject=TRUE
  if(nep$sig<0)reject=TRUE
  if(nep$mu<0)reject=TRUE
  # if(nep$mu>1)reject=TRUE
  # if(nep$sig>1)reject=TRUE
  if(nep$E0<0)reject=TRUE
  if(nep$p<0)reject=TRUE
  # if(nep$p>1)reject=TRUE
  if(nep$threshold<0)reject=TRUE
  if(nep$interventioneffect<0)reject=TRUE
  # if(nep$interventioneffect>1)reject=TRUE
  if(!reject)
  {
    nep$LL = loglikelihood(nep,data)
    logacc = nep$LL-olp$LL
    lu = -rexp(1)
    if(lu>logacc)reject=T
  }
  if(reject)return(olp)
  return(nep)
}


SEIR = function(input,scalefactor=0.4)
{
  scalefactor = input$interventioneffect
  
  #initial conditions
  S0 <- input$S0 - input$E0 - input$I0
  E0 <- input$E0
  I0 <- input$I0 
  R0 <- 0
  #parameters
  beta       <- input$beta   #Infection (before rigorous testing)
  sig        <- input$sig     #Incubation
  mu         <- input$mu      #Recovery 
  N          <- input$N       #Population
  numSteps   <- input$numSteps   #length to calibrate on
  
  TP=91 # turning point for epidemics, assumed to be doy=109 or time point 91
  
  #Storage vectors for compartments
  S <- c()
  E <- c()
  I <- c()
  R <- c()
  
  dS <- c()
  dE <- c()
  dI <- c()
  dR <- c()
  dIcalib <- c()
  
  S[1] <- S0 
  E[1] <- E0
  I[1] <- I0
  R[1] <- R0
  
  dS[1] <- 0
  dE[1] <- 0
  dI[1] <- 0 
  dR[1] <- 0
  dIcalib[1] <- 0 
  #Difference equations to simulate
  
  
  
  for (t in 2:numSteps)
  {
    ninfs = beta * S[t-1]*I[t-1]/N
    if(t>TP)
    {
      ninfs = 0.5*beta*S[t-1]*I[t-1]/N # test
      difff = t-TP
      ninfs=scalefactor*ninfs+(ninfs-scalefactor*ninfs)*exp(-0.5*difff)
    }
    dS[t] <-  -ninfs
    dE[t] <-  ninfs - sig * E[t-1]
    dI[t] <-  sig*E[t-1] - mu*I[t-1]
    dR[t] <-  mu*I[t-1]
    
    
    
    S[t] <- S[t-1] + dS[t]
    E[t] <- E[t-1] + dE[t] 
    I[t] <- I[t-1] + dI[t] 
    R[t] <- R[t-1] + dR[t]
    
    if(S[t]<0)S[t]=0
    if(E[t]<0)E[t]=0
    if(I[t]<0)I[t]=0
    if(R[t]<0)R[t]=0
    
    dIcalib[t] <- ninfs
  }
  
  epidemic = list(S=S,
                  E=E,
                  I=I,
                  R=R,
                  dI=dI,
                  dE=dE,
                  dR=dR,
                  dIcalib=dIcalib
  )
  return(epidemic)
}


