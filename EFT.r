# Install pacman if missing (helper to install+load packages)
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# Load (and install if missing) required packages in one line
pacman::p_load(
  tidyverse,    # data wrangling & ggplot
  data.table,   # fast large-data handling
  lubridate,    # date handling
  readxl,       # read Excel files
  plm,          # panel data tools
  fixest,       # fast FE regressions
  lmtest,       # tests
  sandwich,     # robust SEs
  broom,        # tidy model outputs
  fastDummies   # create dummies if needed
)

# Quick check: show versions loaded
sessionInfo()$otherPkgs %>% map_chr(~ paste0(.x$Package, " ", .x$Version)) %>% paste(collapse = ", ") %>% print()
#importing the data
#Setting  working directory
setwd("C:/Users/LENOVO/Desktop/EFT ANALYSIS/data")
getwd()

library(data.table)

# fread automatically detects separators and types
bond_char <- fread("C:/Users/LENOVO/Desktop/EFT ANALYSIS/data/新建文件夹 (3)/bond_char_data.csv",
                   nThread = 4, showProgress = TRUE)
bond_char <- fread("C:\\Users\\LENOVO\\Desktop\\EFT ANALYSIS\\data\\新建文件夹 (3)\\bond_char_data.csv",
                   nThread = 4, showProgress = TRUE)


# Peek at basic structure
dim(bond_returns)
dim(bond_char)

head(bond_returns)
head(bond_char)

# Convert DATE columns to Date type
bond_returns[, DATE := as.Date(DATE, "%m/%d/%Y")]
bond_char[, DATE := as.Date(DATE, "%m/%d/%Y")]
head(bond_returns$DATE)
head(bond_char$DATE)
library(data.table)

# Merge bond_char with bond_returns
bond_full <- merge(bond_char, 
                   bond_returns[, .(cusip, DATE, bond_ret)], 
                   by = c("cusip", "DATE"), 
                   all.x = TRUE)  # keep all rows from bond_char


dim(bond_full)   # should be close to bond_char rows
head(bond_full)
# Compute yield-spread volatility (rolling 21 days)
library(zoo)

# Make sure data is ordered
bond_full <- bond_full[order(cusip, DATE)]

# Compute 21-day rolling SD of ys_cont_daily per bond
bond_full[, vol_ys := rollapply(ys_cont_daily, width = 21, FUN = sd, fill = NA, align = "right"), by = cusip]

# Check results
# Check first non-NA vol_ys per bond
bond_full[!is.na(vol_ys), .(cusip, DATE, vol_ys)][1:10]

# Create lagged volatilities per bond
bond_full[, `:=`(
  vol_lag1 = shift(vol_ys, 1, type = "lag"),
  vol_lag2 = shift(vol_ys, 2, type = "lag"),
  vol_lag3 = shift(vol_ys, 3, type = "lag")
), by = cusip]

# Check results
head(bond_full[, .(cusip, DATE, vol_ys, vol_lag1, vol_lag2, vol_lag3)], 10)

#Clean data for regression
# Remove rows with NA in dependent or lagged volatility
bond_reg <- bond_full[!is.na(vol_ys) & !is.na(vol_lag1) & !is.na(vol_lag2) & !is.na(vol_lag3)]

#Check how many rows you have left:
dim(bond_reg)

library(lmtest)
library(sandwich)

# OLS regression
model <- lm(vol_ys ~ ETF_ownership + TTM + age + avg_rating + rel_bidask_daily +
              amt_out + dollar_vol + duration + vol_lag1 + vol_lag2 + vol_lag3,
            data = bond_reg)

# Robust SEs
coeftest(model, vcov = vcovHC(model, type = "HC1"))


#Heterogeneity by bond rating


#split by bond rating.
bond_reg[, rating_group := ifelse(avg_rating <= 7, "Investment_Grade", "High_Yield")]

#Check the split:
table(bond_reg$rating_group)


# Investment Grade
model_IG <- lm(vol_ys ~ ETF_ownership + TTM + age + avg_rating + rel_bidask_daily +
                 amt_out + dollar_vol + duration + vol_lag1 + vol_lag2 + vol_lag3,
               data = bond_reg[rating_group == "Investment_Grade"])
coeftest(model_IG, vcov = vcovHC(model_IG, type = "HC1"))


# High Yield
model_HY <- lm(vol_ys ~ ETF_ownership + TTM + age + avg_rating + rel_bidask_daily +
                 amt_out + dollar_vol + duration + vol_lag1 + vol_lag2 + vol_lag3,
               data = bond_reg[rating_group == "High_Yield"])
coeftest(model_HY, vcov = vcovHC(model_HY, type = "HC1"))

#Heterogeneity by time period


bond_reg[, period := fifelse(DATE >= as.Date("2002-07-01") & DATE <= as.Date("2007-06-30"), "Pre_Crisis",
                             fifelse(DATE >= as.Date("2007-07-01") & DATE <= as.Date("2009-06-30"), "GFC",
                                     fifelse(DATE >= as.Date("2009-07-01") & DATE <= as.Date("2014-12-31"), "Post_Crisis",
                                             fifelse(DATE >= as.Date("2015-01-01") & DATE <= as.Date("2019-12-31"), "Late_Expansion",
                                                     fifelse(DATE >= as.Date("2020-01-01") & DATE <= as.Date("2020-06-30"), "COVID19_Shock",
                                                             NA_character_)))))]

# Check distribution
table(bond_reg$period, useNA = "ifany")

#Runing OLS FOR EACH PERIOD 
periods <- unique(na.omit(bond_reg$period))

for(p in periods){
  cat("\n--- Regression for period:", p, "---\n")
  model_period <- lm(vol_ys ~ ETF_ownership + TTM + age + avg_rating + rel_bidask_daily +
                       amt_out + dollar_vol + duration + vol_lag1 + vol_lag2 + vol_lag3,
                     data = bond_reg[period == p])
  print(coeftest(model_period, vcov = vcovHC(model_period, type = "HC1")))
}
# Heterogeneity by Bond Type + Period Combination.
library(data.table)
library(lmtest)
library(sandwich)

# Define bond types
bond_types <- unique(na.omit(bond_reg$rating_group))

# Create empty list to store results
results <- list()

# Loop over periods and bond types
for (p in periods) {
  for (b in bond_types) {
    
    # Subset data for this period × bond type
    subset_data <- bond_reg[period == p & rating_group == b]
    
    # Skip if empty
    if (nrow(subset_data) == 0) next
    
    # Run OLS
    model <- lm(vol_ys ~ ETF_ownership + TTM + age + avg_rating + rel_bidask_daily +
                  amt_out + dollar_vol + duration + vol_lag1 + vol_lag2 + vol_lag3,
                data = subset_data)
    
    # Compute robust SE
    robust_se <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
    
    # Save only ETF_ownership row as numeric
    results[[paste(p, b, sep = "_")]] <- as.numeric(robust_se["ETF_ownership", ])
  }
}

# Combine results into a single table
results_table <- rbindlist(lapply(names(results), function(x){
  data.table(
    Combination = x,
    Estimate = results[[x]][1],
    Std_Error = results[[x]][2],
    t_value = results[[x]][3],
    p_value = results[[x]][4]
  )
}))

results_table

#Visualization of ETF Ownership Effects on Bond Volatility by Bond Type and Period
library(ggplot2)

library(data.table)
library(ggplot2)
library(stringr)

# Split Combination into Period and Bond_Type
results_table[, c("Period", "Bond_Type") := tstrsplit(Combination, "_", fixed = TRUE, keep = c(1,2))]

# Ensure Period is an ordered factor for plotting
results_table[, Period := factor(Period, levels = c("Pre", "GFC", "Post", "Late", "COVID19"))]

# Correct: use Bond_Type as fill
ggplot(results_table, aes(x = Period, y = Estimate, fill = Bond_Type)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Estimate - Std_Error, ymax = Estimate + Std_Error),
                width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = sig), vjust = -0.5, position = position_dodge(0.9), size = 4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "ETF Ownership Effect on Bond Volatility by Period & Type",
       x = "Period",
       y = "Coefficient Estimate (ETF Ownership)")


#robustness
library(data.table)
library(zoo)
library(lmtest)
library(sandwich)

# 1. Log-volatility
bond_reg[, vol_log := log(vol_ys + 1e-6)]  # small constant to avoid log(0)
model_log <- lm(vol_log ~ ETF_ownership + TTM + age + avg_rating + rel_bidask_daily +
                  amt_out + dollar_vol + duration + vol_lag1 + vol_lag2 + vol_lag3,
                data = bond_reg)
res_log <- coeftest(model_log, vcov = vcovHC(model_log, type = "HC1"))

# 2. Rolling window volatility (10-day and 60-day)
bond_reg[, vol_10 := rollapply(ys_cont_daily, 10, sd, fill = NA, align = "right", na.rm = TRUE), by = cusip]
bond_reg[, vol_60 := rollapply(ys_cont_daily, 60, sd, fill = NA, align = "right", na.rm = TRUE), by = cusip]

# Remove rows with NA
bond_reg_10 <- bond_reg[!is.na(vol_10)]
bond_reg_60 <- bond_reg[!is.na(vol_60)]

# 10-day window
model_10 <- lm(vol_10 ~ ETF_ownership + TTM + age + avg_rating + rel_bidask_daily +
                 amt_out + dollar_vol + duration + vol_lag1 + vol_lag2 + vol_lag3,
               data = bond_reg_10)
res_10 <- coeftest(model_10, vcov = vcovHC(model_10, type = "HC1"))

# 60-day window
model_60 <- lm(vol_60 ~ ETF_ownership + TTM + age + avg_rating + rel_bidask_daily +
                 amt_out + dollar_vol + duration + vol_lag1 + vol_lag2 + vol_lag3,
               data = bond_reg_60)
res_60 <- coeftest(model_60, vcov = vcovHC(model_60, type = "HC1"))

# 3. Drop illiquid bonds (1st–99th percentile of dollar_vol)
q <- quantile(bond_reg$dollar_vol, probs = c(0.01, 0.99), na.rm = TRUE)
bond_liquid <- bond_reg[dollar_vol >= q[1] & dollar_vol <= q[2]]
model_liquid <- lm(vol_ys ~ ETF_ownership + TTM + age + avg_rating + rel_bidask_daily +
                     amt_out + dollar_vol + duration + vol_lag1 + vol_lag2 + vol_lag3,
                   data = bond_liquid)
res_liquid <- coeftest(model_liquid, vcov = vcovHC(model_liquid, type = "HC1"))

# 4. Winsorize ETF ownership (1st–99th percentile)
q_etf <- quantile(bond_reg$ETF_ownership, probs = c(0.01, 0.99), na.rm = TRUE)
bond_reg[, ETF_own_w := pmin(pmax(ETF_ownership, q_etf[1]), q_etf[2])]
model_win <- lm(vol_ys ~ ETF_own_w + TTM + age + avg_rating + rel_bidask_daily +
                  amt_out + dollar_vol + duration + vol_lag1 + vol_lag2 + vol_lag3,
                data = bond_reg)
res_win <- coeftest(model_win, vcov = vcovHC(model_win, type = "HC1"))

# 5. Combine results into a table
robust_results <- data.table(
  Specification = c("Log-Volatility", "Vol_10day", "Vol_60day", "Drop Illiquid", "Winsorized ETF"),
  Estimate = c(res_log["ETF_ownership",1],
               res_10["ETF_ownership",1],
               res_60["ETF_ownership",1],
               res_liquid["ETF_ownership",1],
               res_win["ETF_own_w",1]),
  Std_Error = c(res_log["ETF_ownership",2],
                res_10["ETF_ownership",2],
                res_60["ETF_ownership",2],
                res_liquid["ETF_ownership",2],
                res_win["ETF_own_w",2]),
  t_value = c(res_log["ETF_ownership",3],
              res_10["ETF_ownership",3],
              res_60["ETF_ownership",3],
              res_liquid["ETF_ownership",3],
              res_win["ETF_own_w",3]),
  p_value = c(res_log["ETF_ownership",4],
              res_10["ETF_ownership",4],
              res_60["ETF_ownership",4],
              res_liquid["ETF_ownership",4],
              res_win["ETF_own_w",4])
)

# Add significance stars
robust_results[, sig := fifelse(p_value < 0.01, "***",
                                fifelse(p_value < 0.05, "**",
                                        fifelse(p_value < 0.1, "*", "")))]

# Optional: round numeric columns
robust_results[, `:=`(Estimate = round(Estimate, 4), 
                      Std_Error = round(Std_Error, 4), 
                      t_value = round(t_value, 2), 
                      p_value = round(p_value, 4))]

robust_results
