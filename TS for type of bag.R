
###### TIME SERIES FOR TYPE OF BAG ######

pacotes <- c("readr","readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth", "tsibble", "fable","tsibbledata", "fpp3","lubridate",
             "urca", "dygraphs", "quantmod","BETS","tseries","FinTS","feasts",
             "gridExtra", "scales", "caret","xtable", "tsutils","GetBCBData", 
             "quantmod","dgof","seasonal","devtools","transformr","gganimate")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

## Importing dataset 

base <- read_excel("base.xlsx")

## Keeping only wanted columns: Type of Bag, Date and Amount in EUR

base2 <- select(base, 3, 8 ,9)


## Adjusting column's names to avoid errors

names(base2) <- c("date", "amount", "product")


## Transforming it in a tsibble - amount per product, grouped by year and quarter

base2$date <- as.Date(base2$date) 

base2 <- base2 %>%
  mutate(year_quarter = quarter(date, with_year = TRUE))

grouped_base2 <- base2 %>%
  group_by(product, year_quarter) %>%
  summarise(amount = sum(amount))

base_tsbl <- grouped_base2 %>%
  as_tsibble(key = product, index = year_quarter, regular = TRUE)


## Plot tsbl

ggplot(base_tsbl, aes(x = year_quarter, y = amount/1000, color = product)) +
  geom_line() +
  labs(x = "Q-Year", y = "Amount (per K)", title = "Time Series by Product") +
  scale_y_continuous(labels = comma_format(scale = 1)) +
  theme_minimal()


## Creating a time series for each product to decompose them

product_list <- split(base_tsbl$amount, base_tsbl$product)

ts_list <- lapply(product_list, function(x) {
  ts(x, start = c(2016, 1), end = c(2023, 2), frequency = 4)
})

print(ts_list$`3S`)

## Functions to decompose a time series into trend, seasonality, and remainder

## Additive

decompose_add <- function(ts_object) {
  decomposed_ts <- decompose(ts_object)
  return(decomposed_ts)
}

## Multiplicative

decompose_mult <- function(ts_object) {
  decomposed_ts <- decompose(ts_object, type = "multiplicative")
  return(decomposed_ts)
}

## Applying the decomposition to each product's time series in ts_list

decomposed_add <- lapply(ts_list, decompose_add)
decomposed_mult <- lapply(ts_list, decompose_mult)

## Accessing the decomposition components for a specific product 

p2sl <- '2SL'
decomposed_p2sl_add <- decomposed_add[[p2sl]]
decomposed_p2sl_mult <- decomposed_mult[[p2sl]]
plot(decomposed_p2sl_add)
plot(decomposed_p2sl_mult)

p3s <- '3S'
decomposed_p3s_add <- decomposed_add[[p3s]]
decomposed_p3s_mult <- decomposed_mult[[p3s]]
plot(decomposed_p3s_add)
plot(decomposed_p3s_mult)

p3SB_4SB_EPF <- '3SB|4SB|EPF'
decomposed_p3SB_4SB_EPF_add <- decomposed_add[[p3SB_4SB_EPF]]
decomposed_p3SB_4SB_EPF_mult <- decomposed_mult[[p3SB_4SB_EPF]]
plot(decomposed_p3SB_4SB_EPF_add)
plot(decomposed_p3SB_4SB_EPF_mult)

plot(decomposed_p3SB_4SB_EPF_mult) 

p4SL <- '4SL'
decomposed_p4SL_add <- decomposed_add[[p4SL]]
decomposed_p4SL_mult <- decomposed_mult[[p4SL]]
plot(decomposed_4SL_add)
plot(decomposed_4SL_mult)

p4SLB_SBTB_EB <- '4SLB|SBTB|EB'
decomposed_p4SLB_SBTB_EB_add <- decomposed_add[[p4SLB_SBTB_EB]]
decomposed_p4SLB_SBTB_EB_mult <- decomposed_mult[[p4SLB_SBTB_EB]]
plot(decomposed_p4SLB_SBTB_EB_add)
plot(decomposed_p4SLB_SBTB_EB_mult)

p4SLB_SBTB_EPF <- '4SLB|SBTB|EPF'
decomposed_p4SLB_SBTB_EPF_add <- decomposed_add[[p4SLB_SBTB_EPF]]
decomposed_p4SLB_SBTB_EPF_mult <- decomposed_mult[[p4SLB_SBTB_EPF]]
plot(decomposed_p4SLB_SBTB_EPF_add)
plot(decomposed_p4SLB_SBTB_EPF_mult)

pSBT <- 'SBT'
decomposed_pSBT_add <- decomposed_add[[pSBT]]
decomposed_pSBT_mult <- decomposed_mult[[pSBT]]
plot(decomposed_pSBT_add)
plot(decomposed_pSBT_mult)

pSF <- 'SF'
decomposed_pSF_add <- decomposed_add[[pSF]]
decomposed_pSF_mult <- decomposed_mult[[pSF]]
plot(decomposed_pSF_add)
plot(decomposed_pSF_mult)


## MEAN, NAIVE, SNAIVE e RW

base_tsbl



