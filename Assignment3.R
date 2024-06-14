
setwd("C:\\Users\\Niels\\Documents\\R\\Asset Pricing\\Assignment3")
rm(list = ls())

data = read.csv('Assignment3.csv', sep = ',')

# Function to lag a vector x by k amounts.
lag <- function(x, k=1) {
  if (k > 0)  return (c(rep(NA, k), x)[1 : length(x)] )
  return( c(x[(-k+1) : length(x)], rep(NA, -k)))
}

kperiodReturn <- function(x, k = 1) {
  result = lag(x, -1)
  if (k == 1) return(result)
  for (j in 2:k) {
     result = result * lag(x, -j)
  }
  return(result)
}

summary(lm(data = data, R ~ lag(DG, 1)))
summary(lm(data = data, R ~ lag(R, 1)))
summary(lm(data = data, R ~ lag(DP, 1)))

summary(lm(data = data, DG ~ lag(DG, 1)))
summary(lm(data = data, DG ~ lag(R, 1)))
summary(lm(data = data, DG ~ lag(DP, 1)))

summary(lm(data = data, DP ~ lag(DG, 1)))
summary(lm(data = data, DP ~ lag(R, 1)))
summary(lm(data = data, DP ~ lag(DP, 1)))

# 3 a
summary(lm(data = data, kperiodReturn(R, 1) ~ DP))
summary(lm(data = data, kperiodReturn(R, 3) ~ DP))
summary(lm(data = data, kperiodReturn(R, 5) ~ DP))
summary(lm(data = data, kperiodReturn(R, 7) ~ DP))

summary(lm(data = data, kperiodReturn(DG, 1) ~ DP))
summary(lm(data = data, kperiodReturn(DG, 3) ~ DP))
summary(lm(data = data, kperiodReturn(DG, 5) ~ DP))
summary(lm(data = data, kperiodReturn(DG, 7) ~ DP))

summary(lm(data = data, lag(DP, 1) ~ DP))
summary(lm(data = data, lag(DP, 3) ~ DP))
summary(lm(data = data, lag(DP, 5) ~ DP))
summary(lm(data = data, lag(DP, 7) ~ DP))

#3b
summary(lm(data = data, kperiodReturn(R, 7) ~ DP))

ggplot(data = data.frame(Year = data$year,
                         Predicted = 1.0023 + 27.6612 * data$DP,
                         Actual = kperiodReturn(data$R, 7))) + 
  geom_line(aes(x=Year, y= Predicted, col = 'Predicted')) + 
  geom_point(aes(x=Year, y= Predicted, col = 'Predicted')) + 
  geom_line(aes(x=Year, y= Actual, col = 'Actual'),) + 
  geom_point(aes(x=Year, y= Actual, col = 'Actual')) + 
  ylab("Seven year return") + ggtitle("Predicted vs actual 7-year return")

#3c

summary(lm(data = data[seq(1, 86, by=5),], kperiodReturn(R, 5) ~ DP))
summary(lm(data = data[seq(1, 86, by=5),], kperiodReturn(DG, 5) ~ DP))
summary(lm(data = data[seq(1, 86, by=5),], lag(DP, 5) ~ DP))


# 4a

data$r = log(data$R)
data$deltad = log(data$DG)
data$d = log(data$DP)


br = summary(lm(data = data, r ~ lag(d)))$coefficients[2]
bd = summary(lm(data = data, deltad ~ lag(d)))$coefficients[2]
bdp= summary(lm(data = data, d ~ lag(d)))$coefficients[2]

br - bd + (0.96^k) * bdp

#4b

iterateForward = function(x, k){
  res = lag(x, -1)
  for (j in 2:(k)) {
    res = res + lag(x, k=-(j-1)) * 0.96 ^ (j-1)
  }
  return(res)
}

br = summary(lm(data = data, iterateForward(r, 15) ~ lag(d)))$coefficients[2]
bd = summary(lm(data = data, iterateForward(deltad, 15) ~ lag(d)))$coefficients[2]
bdp= summary(lm(data = data, lag(d, 15) ~ lag(d)))$coefficients[2]

br - bd + (0.96 ^ 15) * bdp
