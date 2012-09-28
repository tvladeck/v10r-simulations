library("Brobdingnag")
library("testthat")

CalculateAlpha <- function(delta, n=10) {
  # alpha * n log n = delta;
  n <- 2 ** n
  nlogn <- n * log(n)
  alpha <- delta / nlogn
  return(alpha)
}

CalculateInitialCost <- function(delta, beta, n=10) {
  # Compute the total initial cost of the market. 
  # 
  # Args:
  #   delta;: the total price delta;erence from 1 that will be allowed
  #   beta: the initial desired beta
  #   n: the number of events in the market
  # 
  # Returns: 
  #   The total liability of the market maker
  n                   <- 2 ** n
  nlogn               <- n * log(n)
  
  # delta; = alpha * n log n
  alpha               <- delta / nlogn
  
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

NoRiskVolume <- function(delta, beta, base, fee, avg.price=0.0012) {
  # Calculates the volume at which there is no risk to the market maker
  # because the maker has collected enough in fees to cover the maximum
  # risk
  # Args:
  #   delta: the amount above 1 prices may rise
  #   beta: initial liquidity
  #   base: number of base events
  #   fee: the fee for a one condition position
  #   avg.price: the assumed average price of a single position 
  #     should be slightly greater than 1 / 2**base
  # Returns:
  #   the monetary volume required to reach the "no risk" state
  alpha                    <- CalculateAlpha(delta, beta)
  risk                     <- CalculateInitialCost(delta, beta, base)
  charge                   <- fee / (2**(base-1))
  break.even.positions     <- risk / charge
  necessary.vol            <- break.even.positions * avg.price
  return(necessary.vol)    
}
