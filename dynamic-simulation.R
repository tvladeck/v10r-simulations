
SimulateDynamicMarket <- function(delta, beta, base, fee, mean, sd, steps) {
  # Simulates a path dependent market
  # Args:
  #   delta: the amount above one prices may rise
  #   beta: the initial beta
  #   base: the number of base events
  #   fee: the average fee charged by the market
  #   mean: the mean of the final distribution
  #   sd: the standard deviation of the final distribution
  #   steps: the number of transactional steps the simulation takes
  # Returns a vector of important parameters for the market
  
  # working out that a bet w/ just one condition gets this
  # charge
  charge          <- fee / (2**(base - 1))
  
  # working out some basic details of the market
  atoms           <- 2 ** base
  alpha           <- CalculateAlpha(delta, base)
  initial.events  <- (beta / alpha) / atoms
  initial.cost    <- CalculateInitialCost(delta, beta, base)
  
  # this is the initial vector at desired liquidity levels
  initial.vector  <- seq(0, 0, length=atoms) + initial.events
  
  # generate a position vector with a gaussian dist with parameters given in the function
  position.vector        <- rnorm(atoms, mean, sd)
  
  # norm this by the # of steps we'll be using
  normed.position.vector <- round(position.vector / steps)
  
  # calculate how much $ the market takes in
  intake <- 0
  for (i in 1:steps) {
    before.vector <- initial.vector + ((i-1) * normed.position.vector)
    after.vector  <- before.vector + normed.position.vector
    before.cost   <- CostOfMarket(alpha, before.vector)
    after.cost    <- CostOfMarket(alpha, after.vector)
    period.intake <- after.cost - before.cost
    intake        <- intake + period.intake   
  }
  
  # the max outlay is just the # of positions on the winning bet
  outlay          <- max(position.vector)
  
  # the total # of positions taken in the market
  positions       <- sum(round(position.vector)) - atoms * initial.events
  
  # the new beta
  d.beta          <- alpha * positions
  
  # assuming each position was purchased at a price of 1/atoms
  volume          <- positions / atoms
  
  # the market's profit/loss w/out per-transaction charges
  market.profit   <- intake - outlay
  
  # w/ per-transaction charges
  maker.profit    <- market.profit + charge * positions
  
  # roll-up of summary statistics
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

