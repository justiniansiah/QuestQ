#Here we generate the data for the traditional queue

## load library
library(rstream)

## parameters system
arrival0 = 56     #mean arrival times of customers (non-peak)
arrival1 = 43     #mean arrival times of customers (peak)
service = 45      #mean service times of counter

## Poisson didn't work so I'm trying out exponential (non CRN, testing if this works)
randexpo <- function (lambda) {
  U <- runif(1,0,1)
  X <- -lambda*log(1-U)
  
  return(round(X))
  }


## Simulate 1 run to obtain mean queue length (and hopefully later on mean waiting time)
simulateOneRun <- function (QueueStart,Runtime,Interarrivals) {
  QueueStart_track = QueueStart #to keep track of people in queue on start
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
        TimetoService = round(randnorm(50,10,10))
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
          if(QueueStart_track>0){
            QueueStart_track = QueueStart_track - 1
          }
          else{
            WaitTime = append(WaitTime,(CurrentTime - StartTime[TotalCustomers-QueueStart]))
          }
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
  #return(StartTime)
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




