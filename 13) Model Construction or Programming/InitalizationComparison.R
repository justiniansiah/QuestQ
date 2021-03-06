#Here we do a comparison for the average waiting times for different initial queue lengths

## load library
library(rstream)

### Set Parameters ##########################
## initialize streams of random numbers
gendemand <- new("rstream.mrg32k3a")

## parameters system
arrival0 = 56     #mean arrival times of customers (non-peak)
arrival1 = 43     #mean arrival times of customers (peak)
service = 45      #mean service times of counter
normmean = 48

runtime = 45 #mins

### Define Functions ##########################
#Exponential Distribution for interarrivals
randexpo <- function (lambda,rs) {
  #U <- runif(1,0,1)
  U <- rstream.sample(rs,1)
  X <- -lambda*log(1-U)
  
  return(round(X))
}

## Generates a random variable based on the mean and variance
#takes in mean, var and minimum values with default being std norm and no minimum
randnorm <- function (mean = 0,var = 1, min = -Inf) {
  sd = sqrt(var)
  out = max(min,rnorm(1,mean,sd))
  out = min(out,180)
  out
}

## Simulate 1 run of Traditional Queue
simulateOneRun_T <- function (QueueStart,Runtime,Interarrivals) {
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
      TimetoCustomer = randexpo(Interarrivals,gendemand)
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
        TimetoService = round(randnorm(normmean,10,10)) #use normal dis instead
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
  
  return(c(mean.Q,QueueLength,AvgWait))
}

#input max queue length and number of iterations to run
dosim <- function(queue,iter=1){
  #Warning (so you dont crash your com)
  if (queue > 10){message("Queue length of >10; Not really sensible, but if you want, go change the code to allow.")}
  else if (queue < 0){message("Invalid Queue Length.")}
  else if (iter > 1000){message("Running >1000 iterations will take a long time. If you want, go change the code to allow.")}
  #Actual code here
  else{
    #run code
    AvgWaitTimes <- rep(NA,queue+1) #vector to store average wait times for each queue length
    
    #Run 1 iteration
    #Get the average wait times using intial queue length of 0-queue
    rstream.nextsubstream(gendemand);
    for (i in 0:queue){
      temp <- simulateOneRun_T(i,runtime, arrival1)
      AvgWaitTimes[i+1] <- temp[3]
      rstream.resetsubstream(gendemand);
    }
    
    iter = iter - 1
    #Repeat iter-1 times
    rep = 1
    while(iter>0){
      rstream.nextsubstream(gendemand);
      for (i in 0:queue){ 
        temp <- simulateOneRun_T(i,runtime, arrival1)             #Run simulation, get average time
        temp <- temp[3]
        old_mean <- AvgWaitTimes[i+1]                             #Store old avg in temp var
        new_mean <- old_mean + (1/(rep+1)) * (temp - old_mean)    #Calc new avg
        AvgWaitTimes[i+1] <- new_mean                             #Store new avg
        
        rstream.resetsubstream(gendemand);
      }
      rep = rep + 1
      iter = iter - 1  
    }
    
    return(AvgWaitTimes)
  }
}

results = dosim(10,100)
results
# plot(y = results, x=c(0:10)
#      , main="Graph",
#      type = "l",
#      xlab="Initial Queue Lengths", ylab="Average Waiting Times (s)",
#      xlim=c(0, 10))
# text(x=c(0:10), y = results, round(results), cex=0.8)

library(ggplot2)
df <- data.frame(x=c(0:10),y=round(results))
ggplot(data=df, aes(x=df$x,y=df$y)) +
  scale_x_continuous(breaks = seq(0, 10, by = 1))+
  geom_point()+
  geom_line()+
  geom_text(aes(label=df$y),size=4, position = position_nudge(y = 10))+
  labs(x="\nInitial Queue Lengths", y="\nAverage Waiting Times (s)\n")+ 
  theme_classic()


