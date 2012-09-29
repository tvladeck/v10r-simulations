library("Brobdingnag")

SimulateStaticMarket <- function(delta, beta, base, fee, mean, sd) {
  # Simulates a path dependent market
  # Args:
  #   delta: the amount above one prices may rise
  #   beta: the initial beta
  #   base: the number of base events
  #   fee: the average fee charged by the market
  #   mean: the mean of the final distribution
  #   sd: the standard deviation of the final distribution
  # Returns a vector of important parameters for the market
  
  # working out that a bet w/ just one condition gets this
  # charge
  charge          <- fee / (2**(base - 1))
  
  # working out some basic details of the market
  atoms           <- 2 ** base
  alpha           <- CalculateAlpha(delta, base)
  initial.events  <- (beta / alpha) / atoms
  initial.cost    <- CalculateInitialCost(delta, beta, base)
  
  # generate a position vector with a gaussian dist with parameters given in the function
  position.vector <- round(rnorm(atoms, mean, sd)) + initial.events
  
  expect_true(min(position.vector) > 0)
  
  # calculate the new beta
  d.beta          <- alpha * sum(position.vector)
  
  # calculate the new cost of the market
  # formula is beta * log sum_i exp q_i / beta
  d.cost          <- CostOfMarket(alpha, position.vector)
  
  # delta in cost at each position is how much money the market takes in. the assumption
  # is that in one block the market takes in approximately as it would over pieces. 
  # to be tested in another iteration
  intake          <- d.cost - initial.cost
  
  # the max outlay is just the # of positions on the winning bet
  outlay          <- max(position.vector) - initial.events
  
  # the total # of positions taken in the market
  positions       <- sum(position.vector) - atoms * initial.events
  
  # assuming each position was purchased at a price of 1/atoms
  volume          <- positions / atoms
  
  # the market's profit/loss w/out per-transaction charges
  market.profit   <- intake - outlay
  
  # w/ per-transaction charges
  maker.profit    <- market.profit + charge * positions
  
  # price of last element
  next.last.vec      <- position.vector
  pos                <- which.max(next.last.vec)
  next.last.vec[pos] <- next.last.vec[pos] - 1
  last.price         <- CostOfMarket(alpha, position.vector) - 
                          CostOfMarket(alpha, next.last.vec)
  
  # roll-up of summary statistics
  summary         <- data.frame(
    "Total Risk"         = initial.cost,
    "Naked Profit"       = market.profit,
    "Market Profit"      = maker.profit,
    "Payout"             = outlay,
    "Intake"             = intake,
    "Average Price"      = intake / positions,
    "Last Price"         = last.price,
    "Charge"             = charge * positions,
    "Volume"             = volume,
    "New beta"           = d.beta
  )
  return(summary)
}

SimulateStaticMarket(0.1, 1000, 10, 0.5, 1000000, 1000000)
