library("Brobdingnag")
library("testthat")

SimulateWorstCaseMarket <- function(slack, beta, base, fee, volume, delta) {
  # Simulates a market wherein everyone has exited their position in all but
  # the winning position
  # Args:
  #   slack: level above 1 which prices may rise
  #   beta: initial liquidity
  #   base: # of base events
  #   fee: fee charged for single-conditoin transaction
  #   volume: total monetary volume estimated
  #   delta: distance (in money) winning position over rest of market
  
  # number of atoms in the market  
  atoms              <- 2 ** base
  
  # assuming positions / atoms = volume
  # i.e., that each position costs ~ 1 / atoms
  total.positions    <- atoms * volume
  position.delta     <- atoms * delta
  
  alpha              <- CalculateAlpha(slack, base)
  init.cost          <- CalculateInitialCost(slack, beta, base)
  
  # total # of positions taken by participants * charge
  charge             <- fee / (2 ** (base-1))
  charge.intake      <- charge * total.positions
  
  initial.events     <- (beta / alpha) / atoms
  initial.vector     <- seq(0, 0, length=atoms) + initial.events
  expect_that(init.cost, equals(CostOfMarket(alpha, initial.vector)))
  
  base.position      <- (total.positions - position.delta) / atoms
  position.vector    <- initial.vector + base.position
  position.vector[1] <- position.vector[1] + position.delta
  
  expect_that(sum(position.vector), equals(sum(initial.vector) + total.positions))
  
  final.cost         <- CostOfMarket(alpha, position.vector)
  expect_false(final.cost == Inf)
  
  market.intake      <- final.cost - init.cost
  expect_true(total.positions > market.intake)
  
  average.price      <- market.intake / total.positions
  expect_false(average.price > (1 + slack))
  
  payout             <- max(position.vector) - initial.events
  profit             <- market.intake + charge.intake
  new.beta           <- alpha * sum(position.vector)
  summary            <- data.frame(
    "Total Risk"      = init.cost,
    "Naked Profit"    = market.intake,
    "Market Profit"   = profit,
    "Average Price"   = average.price,
    "Payout"          = payout,
    "Volume"          = volume,
    "New Beta"        = new.beta
  )
  
  return(summary) 
}