############################
### Simulate penny game ####

nsim <- 100
win <- 5  # number of spinner outcomes needed to win the round
spinner <- c("P", "P", "P", "N", "N", "N")  # simulate the spinner

### simulated solution
penny <- rep(NA, nsim) # initialize a vector to store whether the penny wins or not for each round

for (i in 1:nsim){
  # initialize a game with `win` rounds
  game <- rep(NA, win)

  # initialize first round, and loop through rounds until a coin wins the set number of trials per round
  j <- 1
  while ( sum(game == "P", na.rm = TRUE) < win & sum(game == "N", na.rm = TRUE) < win  ) { 
    game[j] <- sample(spinner, size = 1)  
    j <- j + 1
  }
  
  # declare if penny wins round `i`
  if (sum(game=="P", na.rm = TRUE) == win) {
    penny[i] <- 1
  } else {
    penny[i] <- 0
  }
}

sum(penny, na.rm = TRUE) / nsim 

### analytical solution--treat all trials as "best of" series wlog
spinnerProp <- sum(spinner == "P") / length(spinner)
trials <- 2 * win - 1

P <- sum(dbinom(x = win:trials, size = trials, prob = spinnerProp)); P

