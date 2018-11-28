#Here we generate the data for the traditional queue

## load library
library(rstream)

### Set Parameters ##########################
## initialize streams of random numbers
gendemand <- new("rstream.mrg32k3a")

## parameters system
arrival0 = 56    #mean arrival times of customers (non-peak)
arrival1 = 43     #mean arrival times of customers (peak)
service = 45      #mean service times of counter

service_ordertime = 20      #Time taken to order/pay for food (fixed at 20s)
foodmean = 20
foodvar = 10
foodmin = 10

### Define Functions ##########################
#Exponential Distribution for interarrivals
randexpo <- function (lambda,rs) {
  #U <- runif(1,0,1)
  U <- rstream.sample(rs,1)
  X <- -lambda*log(1-U)
  
  return(round(X))
}

#Normal Distribution for Food service times
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


## Simulate 1 run to Alternative Queue
simulateOneRun_A <- function (QueueStart_Order,QueueStart_Food,Runtime,Interarrivals){ 
  QueueStart_Order_track = QueueStart_Order #to keep track of people in queue on start
  QueueStart_Food_track = QueueStart_Food #to keep track of people in queue on start
  CurrentTime = 0
  QueueLength_Order = QueueStart_Order  #Length of Order Queue (will increase and decrease)
  QueueLength_Food = QueueStart_Food    #Length of Food Queue (will increase and decrease)
  QueueLength_FoodMax = QueueStart_Food #Length of Food Queue (only increase - for indexing)
  TotalCustomers = 0 #total customers served
  #mean.Q = QueueStart_Order
  
  StartTime = rep(0,1e2)
  Order_Queue_index = 1 #keep track new entries to order queue
  
  TimeToFood = rep(-1,1e2)
  Wait_Queue_index = 1
  CarryOverFoodQueue = QueueStart_Food
  
  WaitTime = rep(0,1e2)
  
  
  arrival.flag = 0 #1 when system is waiting for customer, 0 otherwise
  serviceorder.flag = 0 #1 when system is processing a customer, 0 otherwise
  servicefood.flag = 0 #1 when system is processing a customer, 0 otherwise
  
  #Simulate the time steps from 0 - Runtime minutes
  while (CurrentTime < Runtime*60) {   
    ### ARRIVALS ###
    #See time which first customer arrives
    if (arrival.flag == 0){   
      TimetoCustomer = randexpo(Interarrivals,gendemand)
      arrival.flag = 1
    }
    
    #Add customer to queue if customer arrives
    if (TimetoCustomer == 0){
      QueueLength_Order = QueueLength_Order + 1  #For Average Queue Length Calculations, this value will go up and down
      arrival.flag = 0               #Reset flag so check for next customer on next cycle
      
      
      StartTime[Order_Queue_index] = CurrentTime #Add in the current time to the list (for avg wait time calc)
      Order_Queue_index = Order_Queue_index + 1
      
      #This is to allow for multiple customers to come at once (this keeps time the same as we increment this at the end)
      #If no customer come immediately (ie TimetoCustomer =/= 0 then the simulation proceeds as usual)
      CurrentTime = CurrentTime - 1   
    }
    #If not, decrease time taken for customer to arrive
    else{
      TimetoCustomer = TimetoCustomer - 1
    }
    
    #QueueTracker <<- append(QueueTracker,QueueLength_Order) #For Graphing Purposes COMMENT OUT IF RUNNING MANY REPS
    
    ### Queue 1: Order Queue ###
    ##Process People in Queue
    #Check if there is people in queue
    if (QueueLength_Order > 0){
      #If currently not serving a customer, serve next customer
      if (serviceorder.flag == 0){
        serviceorder.flag = 1
        TimetoService_order = service_ordertime #for fixed service times
      }
      #else, system is serving a customer
      else{
        #First, check if service is done
        if(TimetoService_order == 0)
        {
          serviceorder.flag = 0 #reset flag
          QueueLength_Order = QueueLength_Order - 1 #release customer to food queue
          QueueLength_Food = QueueLength_Food + 1 
          QueueLength_FoodMax = QueueLength_FoodMax + 1
          
          #We ignore the waiting times of customers carried over from the initialization
          #here we omit the customers carried over to the order queue
          if (QueueStart_Order > 0){
            QueueStart_Order = QueueStart_Order - 1
            CarryOverFoodQueue = CarryOverFoodQueue + 1
          }
          
          #here we omit the customers carried over to the food queue
          if (CarryOverFoodQueue == 0){
            TimeToFood[Wait_Queue_index] = round(randnorm(foodmean,foodvar,foodmin)) + 1 #Generate a random service time for food service # + 1 so it will not be decreased later
            WaitTime[Wait_Queue_index] = CurrentTime - StartTime[Wait_Queue_index] + TimeToFood[Wait_Queue_index] - 1  #Customer's Wait time # -1 to account for +1 above
            Wait_Queue_index = Wait_Queue_index + 1
          }
          else{
            CarryOverFoodQueue = CarryOverFoodQueue - 1
          }
          
        }  
        #if not done, decrease time till complete
        else{
          TimetoService_order = TimetoService_order - 1
        } 
      } 
    }
    
    ### Queue 2: Food Queue ###
    ##Process People in Queue
    #Check if there is people in queue
    if (QueueLength_Food > 0){
      for (i in 1:QueueLength_FoodMax){
        #Serve Food to those whose food arrives (t=0)
        if (TimeToFood[i] == 0){
          #Serve food
          QueueLength_Food = QueueLength_Food - 1 #decrease Queue length
          TotalCustomers = TotalCustomers + 1 #Increment total customers served (completed)
        }
        #Decrease all values by 1
        if (TimeToFood[i] >= 0){
          TimeToFood[i] = TimeToFood[i] - 1 #if its recently added, time will not decrease (due to the +1 above), the older items will all decrease.
        }
      }
    }
    
    
    # #Update Queue Length statistics
    # if (CurrentTime > 0){
    #   new_mean.Q = mean.Q + (1/(CurrentTime)) * (QueueLength_Order - mean.Q)
    #   if (CurrentTime>1){ var.Q = (1 - (1/(CurrentTime-1)) )*var.Q + CurrentTime*((new_mean.Q - mean.Q)^2)} #because 1 sample means var = 0
    #   mean.Q = new_mean.Q
    # }
    
    CurrentTime = CurrentTime + 1
    
  }
  
  #Before we return, we calculate the mean of the waiting times for this simulation
  #We want to exclude those customers who are still waiting for their food
  for (i in length(TimeToFood)){
    if (TimeToFood[i] > 0){
      WaitTime[i] = 0
    }
  }
  AvgWait = sum(WaitTime)/TotalCustomers
  
  #Returns Avg Q Length, Current Q Length, Avg Waiting Time
  #return(StartTime)
  return(c(QueueLength_Order,QueueLength_Food,AvgWait))
  #return(TotalCustomers)
}


##Get Simulate 1 round of each model and get the difference
simulateDiff <- function(){
  ## skip to beginning of next substream
  rstream.nextsubstream(gendemand);
  InitialQueue_T = round(simulateOneRun_T(0,30,arrival0))
  ## reset to beginning of current substream
  rstream.resetsubstream(gendemand);
  InitialQueue_A = round(simulateOneRun_A(0,0,30,arrival0))
  
  ## skip to beginning of next substream
  rstream.nextsubstream(gendemand);
  Data_T = simulateOneRun_T(InitialQueue_T[2],45,arrival1)
  ## reset to beginning of current substream
  rstream.resetsubstream(gendemand);
  Data_A = simulateOneRun_A(InitialQueue_A[1],InitialQueue_A[2],45,arrival1) 
  
  return(Data_T[3]-Data_A[3])
  #1 for Avg Queue Length
  #2 for Final Queue Length
  #3 for Avg Waiting Time
}


### Run Code ##########################
dosim <- function(){
  #Conduct multiple replications of the simulations
  results <- replicate(100,simulateDiff())
  #display whole numbers for visual viewing.
  round(results) 
  
  #Calculate Conf Interval
  ci.diff.rng <- t.test(results,conf.level=0.95)$conf.int
  print(ci.diff.rng)
  mean(results)
}
dosim()




