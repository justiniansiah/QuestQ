#Here we generate the data for the traditional queue

## load library
library(rstream)

## parameters system
arrival0 = 90     #mean arrival times of customers (non-peak)
arrival1 = 45     #mean arrival times of customers (peak)
service = 45      #mean service times of counter

## Poisson didn't work so I'm trying out exponential (non CRN, testing if this works)
randexpo <- function (lambda) {
  U <- runif(1,0,1)
  X <- -lambda*log(1-U)
  
  return(round(X))
}

#############################################################################
################## For A/R Sampling of normal distribution ##################
#############################################################################

#This function is to generate 1 sample of the proposal PDF (g(x)) using the inverse transform method
#w is the inverse of the CDF aka G_inv(x)
#When called, it will output a sample of g(x)
GenSamplePDFg <- function(g_inv) {
  u <- runif(1,0,1)
  v <- runif(1,0,1)
  w <- g_inv(u)
  
  if (v <=0.5){ w = -w }
  return (w)
}

#This function is to trim the proposal PDF to fit the target CDF ie the Acceptance/Rejection part
#takes in f(x),c, g(x)
#returns a sample of f(x) using the A/R method
GenARSamples <- function(f, c, g, g_inv, n, mean, sd) {
  n.accepts     <- 0
  f.sample <- rep(NA, n)
  
  while (n.accepts < n) {
    y <- GenSamplePDFg(g_inv)      # Step 1: Generate from proposal density g
    u <- runif(1,0,1)        # Step 2: Generate Independent uniform rv
    u = (2*u) - 1
    if (u < f(y)/(c*g(y))) { # step 3 (accept)
      n.accepts <- n.accepts+1
      f.sample[n.accepts] = (y*sd)+mean
    }
  }
  f.sample
}

#Finally this is the function to call a rv from a normal distribution
#Takes in mean and varience 
#outputs a sample from the distribution
randnorm <- function (mean,var){
  sd = sqrt(var)
  c = ( 2/sqrt(2*pi) ) * exp(0.5)
  f <- function(x) { ( 1/sqrt(2*pi) ) * exp(-(x^2)/2) }
  g <- function(x) { (1/2) * exp(-abs(x)) } 
  g_inv <- function(y) { -log(1-y) }
  f2 <- function(x) { ( 1/sqrt(2*pi) ) * exp(-( ((x-mean)/sd) ^2)/2) }
  #g <- function(x) { (1/2) * exp(-abs( ((x-mean)/sd)) )} 
  #g_inv <- function(y) { (sd*(-log(1-y))) + mean }
  n <- 1e5
  
  f.sample <- GenARSamples(f,c,g,g_inv,n,mean,sd)
  hist(f.sample, breaks=50, freq=FALSE)
  curve(f2, -10+mean, 10+mean, col="red",  add=T)
}


## Simulate 1 run to obtain mean queue length (and hopefully later on mean waiting time)
simulateOneRun <- function (QueueStart,Runtime,Interarrivals) {
  CurrentTime = 0
  QueueLength = QueueStart
  TotalCustomers = 0 #total customers served
  mean.Q = QueueStart
  var.Q = 0
  
  StartTime = c()
  WaitTime = c()
  
  
  arrival.flag = 0 #1 when system is waiting for customer, 0 otherwise
  service.flag = 0 #1 when system is processing a customer, 0 otherwise
  
  #First run to initialise queue, here we simulate 1130hrs - 1200hrs
  while (CurrentTime < Runtime*60) {   
    #See time which first customer arrives
    if (arrival.flag == 0){   
      TimetoCustomer = randexpo(Interarrivals)
      arrival.flag = 1
    }
    
    #Add customer to queue if customer arrives
    if (TimetoCustomer == 0){
      QueueLength = QueueLength + 1  #For Average Queue Length Calculations, this value will go up and down
      arrival.flag = 0               #Reset flag so check for next customer on next cycle
      
      
      StartTime = append(StartTime,CurrentTime) #Add in the current time to the list (for avg wait time calc)
      
      #This is to allow for multiple customers to come at once (this keeps time the same as we increment this at the end)
      #If no customer come immediately (ie TimetoCustomer =/= 0 then the simulation proceeds as usual)
      CurrentTime = CurrentTime - 1   
    }
    #If not, decrease time taken for customer to arrive
    else{
      TimetoCustomer = TimetoCustomer - 1
    }
    
    #QueueTracker <<- append(QueueTracker,QueueLength) #For Graphing Purposes COMMENT OUT IF RUNNING MANY REPS
    
    ##Process People in Queue
    #Check if there is people in queue
    if (QueueLength > 0){
      #If currently not serving a customer, serve next customer
      if (service.flag == 0){
        service.flag = 1
        TimetoService = service #for fixed service times
      }
      #else, system is serving a customer
      else{
        #First, check if service is done
        if(TimetoService == 0)
        {
          service.flag = 0 #reset flag
          QueueLength = QueueLength - 1 #release customer
          TotalCustomers = TotalCustomers + 1 #Increment total customers served (completed)
          
          #Here we use the time in the list "StartTime"
          #We take the (current time - time stored) and put it into "WaitTime"
          #If there people in the queue on initialisation, we ignore them 
          WaitTime = append(WaitTime,(CurrentTime - StartTime[TotalCustomers-QueueStart]))
        }
        #if not done, decrease time till complete
        else{
          TimetoService = TimetoService - 1
        }
      }
    }
    
    #Update Queue Length statistics
    if (CurrentTime > 0){
      new_mean.Q = mean.Q + (1/(CurrentTime)) * (QueueLength - mean.Q)
      if (CurrentTime>1){ var.Q = (1 - (1/(CurrentTime-1)) )*var.Q + CurrentTime*((new_mean.Q - mean.Q)^2)} #because 1 sample means var = 0
      mean.Q = new_mean.Q
    }
    
    CurrentTime = CurrentTime + 1
    
  }
  #Before we return, we calculate the mean of the waiting times for this simulation
  AvgWait = sum(WaitTime)/TotalCustomers
  
  #Returns Avg Q Length, Current Q Length, Avg Waiting Time
  return(c(mean.Q,QueueLength,AvgWait))
  #return(TotalCustomers)
}

simulatePeak <- function(){
  InitialQueue = round(simulateOneRun(0,30,arrival0)) #Initialise Queue length by running 1 simulation (non-peak, 30mins)
  Data = simulateOneRun(InitialQueue[2],45,arrival1)  #Use queue length to start off peak simulation (45mins)
  return(Data)
}
#1 for Avg Queue Length
#2 for Final Queue Length
#3 for Avg Waiting Time

#Conduct multiple replications of the simulations
results <- replicate(100,simulatePeak())

#display whole numbers for visual viewing.
message("Average Queue Lengths")
round(results[1,]) 
message("Final Queue Lengths")
round(results[2,]) 
message("Average Waiting Time")
round(results[3,]) 

#See which parameter we are interested in (see above for which # to set)
result.diff <- results[3,] 


#Calculate Conf Interval
ci.diff.rng <- t.test(result.diff,conf.level=0.95)$conf.int
print(ci.diff.rng)
mean(result.diff)

###################################
### stuff below are for testing ###
simulateOneRun(0,30,arrival0)
replicate(100,randexpo(arrival1))


#Graphs 
#Uncomment the QueueTracker in line 60 (remember to comment back after done)
QueueTracker = c() #clears vector
#run 1 simulatePeak() ONLY 1 if not you will hang

QueueTracker = QueueTracker[-c(1:1800)] #remove nonpeak
plot(QueueTracker,type="l") #Full graph of 45mins
plot(QueueTracker,type="l", xlim=c(1000,1600)) #a 5min window




