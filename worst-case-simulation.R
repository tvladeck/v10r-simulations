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
  #   volume: total volume estimated
  #   delta: distance (in positions) winning position over rest of market
  atoms              <- 2 ** base
  alpha              <- CalculateAlpha(slack, base)
  init.cost          <- CalculateInitialCost(slack, beta, base)
  charge             <- fee / (2 ** (base-1))
  charge.intake      <- charge * volume
  initial.events     <- (beta / alpha) / atoms
  initial.vector     <- seq(0, 0, length=atoms) + initial.events
  base.position      <- (volume-delta) / atoms
  position.vector    <- initial.vector + base.position
  position.vector[1] <- position.vector[1] + delta
  final.cost         <- CostOfMarket(alpha, position.vector)
  market.intake      <- final.cost - init.cost
  payout             <- max(position.vector) - initial.events
  profit             <- market.intake + charge.intake
  new.beta           <- alpha * volume
  summary            <- data.frame(
    "Total Risk"      = init.cost,
    "Naked Profit"    = market.intake,
    "Market Profit"   = profit,
    "Payout"          = payout,
    "Volume"          = volume,
    "New Beta"        = new.beta
  )
  
  return(round(summary)) 
}