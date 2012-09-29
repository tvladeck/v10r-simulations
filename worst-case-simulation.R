SimulateWorstCaseMarket <- function(delta, beta, base, fee, volume, margin) {
  # Simulates a market wherein everyone has exited their position in all but
  # the winning position
  # Args:
  #   delta: level above 1 which prices may rise
  #   beta: initial liquidity
  #   base: # of base events
  #   fee: fee charged for single-conditoin transaction
  #   volume: total positional volume estimated
  #   margin: distance (in positions) winning position over rest of market
  
  # calculating basic parameters
  atoms                <- 2 ** base
  alpha                <- CalculateAlpha(delta, base)
  init.cost            <- CalculateInitialCost(delta, beta, base)
  charge               <- fee / (2 ** (base - 1))
  
  init.position.level  <- (beta / alpha) / atoms
  init.vector          <- seq(0, 0, length=atoms) + init.position.level
  
  base.position.level  <- (volume - margin) / atoms
  position.vector      <- seq(base.position.level, base.position.level, length=atoms)
  position.vector[1]   <- position.vector[1] + margin
  
  expect_equal(sum(position.vector), volume)
  
  total.vector         <- init.vector + position.vector
  final.cost           <- CostOfMarket(alpha, total.vector)
  new.beta             <- alpha * sum(total.vector)
  
  expect_true(final.cost > init.cost)
  
  payout               <- max(position.vector)
  
  expect_equal(payout, base.position.level + margin)
  
  total.intake         <- final.cost - init.cost
  average.price        <- total.intake / volume
  
  expect_false(average.price > (1 + delta))
  
  naked.profit         <- total.intake - payout
  
  fee.collected        <- charge * volume
  
  market.profit        <- naked.profit + fee.collected
  
  # price of last element
  next.last.vec      <- total.vector
  pos                <- which.max(next.last.vec)
  next.last.vec[pos] <- next.last.vec[pos] - 1
  last.price         <- CostOfMarket(alpha, total.vector) - 
    CostOfMarket(alpha, next.last.vec)
  
  summary <- data.frame(
    "Total Risk" = init.cost,
    "Naked Profit" = naked.profit,
    "Market Profit" = market.profit,
    "Average price" = average.price,
    "Last Price" = last.price,
    "New beta" = new.beta,
    "Total intake" = total.intake,
    "Payout" = payout
  )
 
  return(summary)
}