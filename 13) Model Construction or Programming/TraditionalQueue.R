#Here we generate the data for the traditional queue

## load library
library(rstream)

## parameters system
arrival0 = 80     #mean arrival times of customers (non-peak)
arrival1 = 40     #mean arrival times of customers (peak)
service = 45      #mean service times of counter

## simple generator for Poisson distribution (non CRN, testing if this works)
randpoisson <- function (lambda) {
  X <- 0
  sum <- exp(-lambda)
  prod <- exp(-lambda)
  U <- runif(1)
  while (U > sum) {
    X <- X + 1
    prod <- prod * lambda / X
    sum <- sum + prod
  }
  return (X)
}


## Simulate 1 run to obtain mean queue length (and hopefully later on mean waiting time)
simulateOneRun <- function () {
  CurrentTime = 0
  QueueLength = 0
  TotalCustomers = 0 #total customers served
  mean.Q = 0
  var.Q = 0
  
  arrival.flag = 0 #1 when system is waiting for customer, 0 otherwise
  service.flag = 0 #1 when system is processing a customer, 0 otherwise
  
  #First run to initialise queue, here we simulate 1130hrs - 1200hrs
  while (CurrentTime < 30*60) {   
    #See time which first customer arrives
    if (arrival.flag == 0){   
      TimetoCustomer = randpoisson(arrival1)
      arrival.flag = 1
    }
    #Add customer to queue if customer arrives
    if (TimetoCustomer == 0){
      QueueLength = QueueLength + 1
      arrival.flag = 0
    }
    else{
      TimetoCustomer = TimetoCustomer - 1
    }
    
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
  #message("Average Length, Varience, Time passed (Min)")
  #return(c(mean.Q,var.Q,CurrentTime/60))
  return(mean.Q)
}

simulateOneRun()

result.diff <- replicate(100,simulateOneRun())
ci.diff.rng <- t.test(result.diff,conf.level=0.95)$conf.int
print(ci.diff.rng)
