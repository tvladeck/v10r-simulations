library("gmp")
library("Brobdingnag")

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
  n   								<- 2 ** n
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

SimulateMarket <- function(slack, beta, base, fee, mean, sd) {
  # Simulates a path dependent market
  # Args:
  #   slack: the amount above one prices may rise
  #   beta: the initial beta
  #   base: the number of base events
  #   fee: the average fee charged by the market
  #   mean: the mean of the final distribution
  #   sd: the standard deviation of the final distribution
  # Returns a vector of important parameters for the market
  charge          <- fee / (2**(base - 1))
  atoms           <- 2 ** base
  alpha           <- CalculateAlpha(slack, base)
  initial.events  <- (beta / alpha) / atoms
  initial.cost    <- CalculateInitialCost(slack, beta, base)
  position.vector <- round(rnorm(atoms, mean, sd)) + initial.events
  d.beta          <- alpha * sum(position.vector)
  d.cost          <- d.beta * log(
    Reduce(function(x,y){x+y},lapply(position.vector, function(x){as.numeric(exp(as.brob(x / d.beta)))}), 0)
  )
  intake          <- d.cost - initial.cost
  outlay          <- max(position.vector) - initial.events
  positions       <- sum(position.vector) - atoms * initial.events
  volume          <- positions / atoms
  market.profit   <- intake - outlay
  maker.profit    <- market.profit + charge * positions
  summary         <- data.frame(
    "Total Risk"         = initial.cost,
    "Naked Profit"       = market.profit,
    "Market Profit"      = maker.profit,
    "Payout"             = outlay,
    "Intake"             = intake,
    "Charge"             = charge * positions,
    "Volume"             = volume,
    "New beta"           = d.beta
  )
  return(round(summary))
}

SimulateMarket(0.1, 1000, 10, 0.5, 0, 0)