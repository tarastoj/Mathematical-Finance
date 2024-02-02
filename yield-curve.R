# setup
library(stringr)
library(RColorBrewer)

maturity_dates <- c("2024-03-01", "2024-09-01", "2025-03-01", "2025-09-01",
           "2026-03-01", "2026-09-01", "2027-03-01", "2027-09-01",
           "2028-03-01", "2028-09-01", "2029-03-01")
last_bond_payment_date <- as.Date("2023-09-01")
colours <- brewer.pal(10, "Spectral")

id_info <- read.csv("BondPrices.csv", skip=1, header=TRUE)
other_info <- read.csv("all-bonds.csv", header=TRUE)
other_info$maturity.date <- as.Date(other_info$maturity.date, "%m/%d/%Y")
maturity_dates <- as.Date(maturity_dates)
num_bonds <- length(maturity_dates)
num_dates <- 10
isins <- rep(0,num_bonds)
coupon_rates <- rep(0,num_bonds)
for (i in 1:num_bonds){
  row <- other_info[other_info$maturity.date == maturity_dates[i],]
  isins[i] <- row$isin
  coupon_rates[i] <- as.numeric(str_sub(row$coupon,1,-2))
}
ids <- rep(0,num_bonds)
for (i in 1:num_bonds){
  ids[i] <- id_info[id_info$isin == isins[i],]$ids
}
close_prices <- array(dim=c(num_bonds,num_dates)) # [i,j] bond i, date j
dates <- rep(0,num_bonds)
for (i in 1:num_bonds){
  url <- paste("https://markets.businessinsider.com/Ajax/Chart_GetChartData?instrumentType=Bond&tkData=1,",
               ids[i],
               ",1330,184&from=20231219&to=20240119"
               ,sep="")
  data <- jsonlite::fromJSON(url)
  Sys.sleep(10)
  close_prices[i,] <- tail(head(data$Close, -1),10)
  if (i == 1){
    dates <- as.Date(tail(head(data$Date, -1), 10))
  }
}
dirty_prices <- array(dim=c(num_bonds,num_dates))
for (j in 1:num_dates){
  num_days <- difftime(dates[j], last_bond_payment_date)
  for (i in 1:num_bonds){
    dirty_prices[i,j] <- close_prices[i,j] + num_days / 365 * coupon_rates[i]
  }
}
num_days_to_maturity <- array(dim=c(num_bonds,num_dates))
for (i in 1:num_bonds){
  for (j in 1:num_dates){
    num_days_to_maturity[i,j] <- difftime(maturity_dates[i],dates[j])
  }
}

# Question 4(a)
yields <- array(dim=c(num_bonds,num_dates))
for (i in 1:num_bonds){
  for (j in 1:num_dates){
    aux <- dirty_prices[i,j]
    l <- i-1
    if (l > 0){
      for (k in 1:l){
        t <- num_days_to_maturity[k,j]/365
        aux <- aux - coupon_rates[i] * exp(-1 * yields[k,j] * t) / 2
      }
    }
    payout <- 100 + coupon_rates[i] / 2
    T <- num_days_to_maturity[i,j] / 365
    yields[i,j] <- -1 / T * log(aux / payout)
  }
}
plot(num_days_to_maturity[,1]/365, 100*yields[,1], type='l', col=colours[1],
     xlab="Term (years)", ylab="Yield (%)",
     ylim=c(3,5))
for (i in 2:num_dates){
  lines(num_days_to_maturity[,i]/365, 100*yields[,i], type='l', col=colours[i])
}
legend("topright", legend=dates, fill=colours, cex=0.5)

#Question 4(b)
freq <- 2

spot_rates <- array(dim=c(num_bonds,num_dates))
for (i in 1:num_bonds){
  for (j in 1:num_dates){
    aux <- dirty_prices[i,j]
    l <- i-1
    if (l > 0){
      for (k in 1:l){
        t <- num_days_to_maturity[k,j]/365
        aux <- aux - (coupon_rates[i]/2 * (1 + spot_rates[k,j]/freq)^(-t*freq))
      }
    }
    payout <- 100 + coupon_rates[i] / 2
    T <- num_days_to_maturity[i,j] / 365
    spot_rates[i,j] <- freq * ((aux / payout)^(-1/freq/T) - 1)
  }
}
plot(num_days_to_maturity[,1]/365, 100*spot_rates[,1], type='l', col=colours[1],
     xlab="Term (years)", ylab="Spot Rate (%)",
     ylim=c(3,5))
for (i in 2:num_dates){
  lines(num_days_to_maturity[,i]/365, 100*spot_rates[,i], type='l', col=colours[i])
}
legend("topright", legend=dates, fill=colours, cex=0.5)

#Question 4(c)
n = 5

interpolated_yields <- array(dim=c(n,num_dates))
for (i in 1:n){
  for (j in 1:num_dates){
    r1 = yields[2*i,j];
    r2 = yields[2*i+1,j];
    t1 = num_days_to_maturity[2*i,j]/365;
    t2 = num_days_to_maturity[2*i+1,j]/365;
    interpolated_yields[i,j] = (r2 - r1) / (t2 - t1) * (i - t1) + r1;
  }
}

forward_rates <- array(dim=c(n-1,num_dates))
for (i in 1:n-1){
  for (j in 1:num_dates){
    forward_rates[i,j] = (interpolated_yields[i+1,j]*(i+1) - interpolated_yields[1,j]) / i;
  }
}
plot(2:(n), 100*forward_rates[,1], type='l', col=colours[1],
     xlab="Term (years)", ylab="1-year Forward Rate (%)",
     ylim=c(2.75,4.25))
for (i in 2:num_dates){
  lines(2:(n), 100*forward_rates[,i], type='l', col=colours[i])
}
legend("topright", legend=dates, fill=colours, cex=0.5)

# Question 5
X_yield = NULL
for (i in 1:n){
  Xi <- array(dim=num_dates-1)
  for (j in 1:(num_dates-1)){
    Xi[j] = log(interpolated_yields[i,j+1] / interpolated_yields[i,j])
  }
  X_yield <- cbind(X_yield, Xi)
}
covariance_yield <- cov(X_yield)

X_forward = NULL
for (i in 1:(n-1)){
  Xi <- array(dim=num_dates-1)
  for (j in 1:(num_dates-1)){
    Xi[j] = log(forward_rates[i,j+1] / forward_rates[i,j])
  }
  X_forward <- cbind(X_forward, Xi)
}
covariance_forward <- cov(X_forward)

# Question 6
spectral_yield <- eigen(covariance_yield)
eigenvectors_yield <- spectral_yield$vectors
eigenvalues_yield <- spectral_yield$values

spectral_forward <- eigen(covariance_forward)
eigenvectors_forward <- spectral_forward$vectors
eigenvalues_forward <- spectral_forward$values
