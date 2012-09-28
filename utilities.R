library("Brobdingnag")
library("testthat")

CalculateAlpha <- function(slack, n=10) {
  # alpha * n log n = slack;
  n <- 2 ** n
  nlogn <- n * log(n)
  alpha <- slack / nlogn
  return(alpha)
}

CalculateInitialCost <- function(slack, beta, n=10) {
  # Compute the total initial cost of the market. 
  # 
  # Args:
  #   slack;: the total price slack;erence from 1 that will be allowed
  #   beta: the initial desired beta
  #   n: the number of events in the market
  # 
  # Returns: 
  #   The total liability of the market maker
  n                   <- 2 ** n
  nlogn               <- n * log(n)
  
  # slack; = alpha * n log n
  alpha               <- slack / nlogn
  
  # init_events is the total number of events that must be "seeded"
  # beta (beta) = alpha * sum_i q_i
  init.events         <- beta / alpha
  init.event.quantity <- init.events / n
  
  # the initial exp q_i/b(q) value
  init.event.exp      <- exp(init.event.quantity / beta)
  
  # the cost function is C(q) = b(q) * log sum_i exp q_i/b(q)
  init.cost           <- beta * log (n * init.event.exp)
  return(init.cost)
}

CostOfMarket <- function(alpha, vector) {
  sum   <- sum(vector)
  beta  <- alpha * sum
  cost  <- beta * log(
    Reduce(
      function(x,y){x+y},
      lapply(vector, 
             function(x){as.brob(exp(as.brob(x / beta)))}), 
      0)
  )
  return(cost)
}
