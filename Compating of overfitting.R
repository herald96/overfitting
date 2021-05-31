library(readxl)
library(data.table)
library(princurve)


# Download the data
data <- read_excel("~/Desktop/Code and data/data_raw.xlsx")
setDT(data)

### Prepare the dataset
# p - open price (midpoint)
data[ , p := (ask + bid)/2]

# Logarithmic yield and previous value
data[ , r := c(NA, log(tail(p, -1)/head(p, -1)))]
data[ , r_lag_1 := c(NA, head(r, -1))]

data[ , longs_lag_1 := c(NA, head(longs, -1))]

# Change in the proportion of long positions and lag
data[ , r_longs := c(NA, log(tail(longs, -1)/head(longs, -1)))]
data[ , r_longs_lag_1 := c(NA, head(r_longs, -1))]

# Binary variable that determines the sign of the course change
data[ , r_bin := as.factor(ifelse(r>=0, 1, -1))]

data_temp <- data[, .(p, r_lag_1, longs_lag_1, r_longs_lag_1, r_bin)]

data_temp <- na.omit(data_temp)



### Calculation of strategy returns for different lengths of the moving period
W_vals <- seq(24, 120)

# Matrix in which we will save the profitability of the strategy at each step
data_r <- data.frame(matrix(NA, ncol = length(W_vals), nrow = nrow(data_temp)))

# Сalculation of returns
for (i in 1:length(W_vals)) { 
  pnl_temp <- rep(0, nrow(data_temp)) 
  for (t in 123:nrow(data_temp)) { 
    temp <- data_temp[(t-W_vals[i]):(t-1), ] 
    m <- glm(r_bin ~ r_lag_1 + longs_lag_1 + r_longs_lag_1, data = temp, family = 'binomial') 
    prob_temp_is <- predict(m, newdata = temp, type = 'response') 
    thres <- (mean(prob_temp_is[which(temp$r_bin == 1)]) +
                mean(prob_temp_is[which(temp$r_bin == -1)]))/2 # Determine the threshold value of the probability for making a decision
    prob_temp <- predict(m, newdata = data_temp[t, ], type = 'response') # Predicted probability for a new observation (t)
    if (prob_temp > thres) {
      pnl_temp[t] <- data_temp$p[t] - data_temp$p[t-1] # If the probability is greater than the threshold value, we get the long position profitability
    } else if (prob_temp < thres) {
      pnl_temp[t] <- data_temp$p[t - 1] - data_temp$p[t] # If the probability is less than the threshold value, we get the profitability of the short position
    }
  }
  data_r[ , i] <- pnl_temp
  print(i)
}



### Probability of overfitting
d <- 1:1440
# Break observations into 12 parts
ind <- split(d, ceiling(seq_along(d)/120))
# Find all possible options with which you can choose 6 parts out of 12
is_chunks <- combn(1:12, 6)

# The number of the last observation for which the calculation is carried out
N <- 3440

for (t in 1565:N) {
  data_pbo <- data_r[(t - 1440):(t - 1), ] 
  
  ranks_list <- rep(NA, 924) # Vector to store the selected ranks
  ranks_list_pc <- rep(NA, 924)

  for (i in 1:924) { 
    oos_chunks <- setdiff(1:12, is_chunks[, i]) # Find of the out-of-sample parts
    is_ind <- unlist(ind[is_chunks[, i]], use.names = F) # Determine the ordinal numbers of observations that need to be selected
    oos_ind <- unlist(ind[oos_chunks], use.names = F) 
    is_sharpe <- colMeans(data_pbo[is_ind, ])/apply(data_pbo[is_ind, ], 2, sd) # Сount Sharpe кфешщ аor each column of the selected part of data_pbo 
    oos_sharpe <- colMeans(data_pbo[oos_ind, ])/apply(data_pbo[oos_ind, ], 2, sd) 
    
    fit <- principal_curve(cbind(is_sharpe, oos_sharpe))
    best_fit_ind <- which.min(abs(is_sharpe - fit$s[,1][which.max(fit$s[,2])]))  # Find of which point from the sample corresponds to the max of the principal curve
    ranks_list_pc[i] <- rank(is_sharpe, ties.method = 'first')[best_fit_ind] # Determine the rank of the found point
    
    best_ind <- which.max(oos_sharpe) # Maximum without smoothing using principal curve
    ranks_list[i] <- rank(is_sharpe, ties.method = 'first')[best_ind] # The rank that corresponds to the maximum
  }
 
   # Save the results
  ranks_output[[t]] <- ranks_list 
  ranks_pc_output[[t]] <- ranks_list_pc
  
  print(t)
}


### Trade strategy
# Create vectors to save the results
pnl_nopc <- rep(NA, N)
pnl_pc <- rep(NA, N)
pnl_max <- rep(NA, N)
ind_nopc <- rep(NA, N)
ind_pc <- rep(NA, N)
ind_max <- rep(NA, N)

for (t in 2000:N) { # For each moment in time
  current_sharpe <- colMeans(data_r[(t - 720):(t-1), ])/apply(data_r[(t - 720):(t-1), ], 2, sd) # count Sharpe ratio on the last 720 observations for all window lengths
  # Without principal curve
  hist_no_pc <- hist(ranks_list[i], plot = F) # Hist of ranks
  int_no_pc <- which.max(hist_no_pc$density) # Find the max frequency
  ind_nopc[t] <- which(rank(current_sharpe, ties.method = 'first') ==
                         floor(hist_no_pc$mids[int_no_pc])) # find the length of the window gives Sharpe ratio with the optimal rank
  # With principal curve
  hist_pc <- hist(ranks_list_pc[i], plot = F) # Hist of ranks
  int_pc <- which.max(hist_pc$density)  # Find the max frequency
  ind_pc[t] <- which(rank(current_sharpe, ties.method = 'first') ==
                       floor(hist_pc$mids[int_pc])) # find the length of the window gives Sharpe ratio with the optimal rank

  ind_max[t] <- which.max(current_sharpe) # Standart aproach (max Sharpe ratio)
  # Result of strategy on step t
  pnl_nopc[t] <- data_r[t, ind_nopc[t]] # without principal curve
  pnl_pc[t] <- data_r[t, ind_pc[t]] # with principal curve
  pnl_max[t] <- data_r[t, ind_max[t]]  # Naive (maximization Sharp ratio)
}


# Portfolio charts
plot(cumsum(pnl_pc[2000:N]), type = 'l', ylim = c(-0.011, 0.036))
lines(cumsum(pnl_max[2000:N]), col = 'blue')
lines(cumsum(pnl_nopc[2000:N]), col = 'red')

# Graph of selected periods
plot(ind_pc[2000:N], type = 'l')
lines(ind_nopc[2000:N], col = 'blue')
lines(ind_max[2000:N], col = 'red')


# Sharpe ratio
sqrt(252*72) * mean(pnl_pc[2000:N], na.rm = T)/sd(pnl_pc[2000:N], na.rm = T)
sqrt(252*72) * mean(pnl_nopc[2000:N], na.rm = T)/sd(pnl_nopc[2000:N], na.rm = T)
sqrt(252*72) * mean(pnl_max[2000:N], na.rm = T)/sd(pnl_max[2000:N], na.rm = T)


# Cumulative excess
plot(cumsum(pnl_pc[2000:N]) - cumsum(pnl_max[2000:N]), type = 'l')
plot(cumsum(pnl_pc[2000:N]) - cumsum(pnl_nopc[2000:N]), type = 'l')

