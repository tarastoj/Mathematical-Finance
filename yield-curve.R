library(stringr)
library(RColorBrewer)

maturity_dates <- c("2024-03-01", "2024-09-01", "2025-03-01", "2025-09-01",
           "2026-03-01", "2026-09-01", "2027-03-01", "2027-09-01",
           "2028-03-01", "2028-09-01")
last_bond_payment_date <- as.Date("2023-09-01")
colours <- brewer.pal(10, "Spectral")

id_info <- read.csv("BondPrices.csv", skip=1, header=TRUE)
other_info <- read.csv("all-bonds.csv", header=TRUE)
other_info$maturity.date <- as.Date(other_info$maturity.date, "%m/%d/%Y")
maturity_dates <- as.Date(maturity_dates)
n <- length(maturity_dates)
isins <- rep(0,n)
coupon_rates <- rep(0,n)
for (i in 1:n){
  row <- other_info[other_info$maturity.date == maturity_dates[i],]
  isins[i] <- row$isin
  coupon_rates[i] <- as.numeric(str_sub(row$coupon,1,-2))
}
ids <- rep(0,n)
for (i in 1:n){
  ids[i] <- id_info[id_info$isin == isins[i],]$ids
}
close_prices <- array(dim=c(n,n)) # [i,j] bond i, date j
dates <- rep(0,n)
for (i in 1:n){
  url <- paste("https://markets.businessinsider.com/Ajax/Chart_GetChartData?instrumentType=Bond&tkData=1,",
               ids[i],
               ",1330,184&from=20231222&to=20240122"
               ,sep="")
  data <- jsonlite::fromJSON(url)
  Sys.sleep(10)
  close_prices[i,] <- tail(data$Close,10)
  if (i == 1){
    dates <- as.Date(tail(data$Date, 10))
  }
}
dirty_prices <- array(dim=c(n,n))
for (j in 1:n){
  num_days <- difftime(dates[j], last_bond_payment_date)
  for (i in 1:n){
    dirty_prices[i,j] <- close_prices[i,j] + num_days / 365 * coupon_rates[i]
  }
}
num_days_to_maturity <- array(dim=c(n,n))
for (i in 1:n){
  for (j in 1:n){
    num_days_to_maturity[i,j] <- difftime(maturity_dates[i],dates[j])
  }
}
yields <- array(dim=c(n,n))
for (i in 1:n){
  for (j in 1:n){
    aux <- dirty_prices[i,j]
    l <- i-1
    if (l > 1){
      for (k in 1:l){
        t <- num_days_to_maturity[k,j]/365
        aux <- aux - coupon_rates[i] * exp(-1 * yields[k,j] * t) / 2
      }
    }
    payout <- 100 + coupon_rates[i] / 2
    yields[i,j] <- -1 * 365 / num_days_to_maturity[i,j] * log(aux/payout)
  }
}
plot(maturity_dates, yields[,1], type='l', col=colours[1])
for (i in 2:n){
  lines(maturity_dates, yields[,i], type='l', col=colours[i])
}
legend("topright", legend=maturity_dates)
