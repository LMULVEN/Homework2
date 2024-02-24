# Meta --------------------------------------------------------------------

## Title:         Econ 470: Homework 2 
## Author:        Leila Mulveny
## Date Created:  2/5/2022
## Description:   This file renders/runs all relevant R code for the assignment

# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales, Matching, cobalt)

# Read the final HCRIS data
final.hcris.data <- read_rds('data/output/HCRIS_Data.rds')


## Answer Question 1: How many hospitals filed more than one report in the same year? Show your answer as a line graph of the number of hospitals over time.

# Count the number of hospitals that filed more than one report in the same year
num_hospitals_multiple_reports = duplicate.hcris %>% 
  summarise(num_hospitals = n_distinct(provider_number))

print(num_hospitals_multiple_reports)

# Create summary data frame
hospitals_over_time = duplicate.hcris %>%
  group_by(fyear) %>%
  summarise(num_hospitals = n_distinct(provider_number))

# Plot line graph
Q1 <- ggplot(hospitals_over_time, aes(x = fyear, y = num_hospitals)) +
  geom_line() +
  labs(x = "Fiscal Year", y = "Number of Hospitals", 
       title = "Number of Hospitals Filing More Than One Report Over Time")
print(Q1)
# Save the plot as an image
ggsave("plot.png", plot = Q1)

## Answer Question 2: After removing/combining multiple reports, how many unique hospital IDs (medicare provider numbers) exist in the data?

# Count unique hospital IDs
num_unique_hospitals <- final.hcris.data %>% ungroup() %>%
distinct(provider_number) %>% nrow()

print(num_unique_hospitals)

## Answer Question 3: What is the distribution of total charges (tot_charges) in each year?
## Show your results with a 'violin' plot, with charges on the y-axis and years on the x-axis.

# Create violin plot
# Notes: Physically plotting the data. There is a scatter plot, plotting the frequency of x. 
# The width of a violin plot is reflecting the height of the distribution. 
#add an upper limit
#geom_violin(aes(group=cut_width(year,1)))

charge_data <- hcris.data %>%
    group_by(year) %>%
    mutate(tot_charges_low = quantile(tot_charges, probs = 0.01, na.rm = TRUE),
           tot_charges_high = quantile(tot_charges, probs = 0.99, na.rm = TRUE)) %>%
    filter(tot_charges >= tot_charges_low & tot_charges <= tot_charges_high) %>%
    filter(!(year %in% c(2007, 2016)))

custom_upper_limit <- 3000000000
Q3 <- ggplot(charge_data %>% ungroup(), aes(x = as.factor(year), y = tot_charges / 1e6)) +
  geom_violin() +
  labs(x = "Year", y = "Total Charges (in millions)", 
       title = "Distribution of Total Charges Each Year") + 
       theme_minimal() + 
       scale_y_continuous(labels = function(x) paste0(x, "M"), limits = c(0, custom_upper_limit / 1e6))

print(Q3)
# Save the plot as an image
ggsave("Q3.png", plot = Q3)


## Answer Question 4: Create the variable "price". What is the distribution of estimated prices in each year? 
## Present your results with a violin plot. Be sure to do something about outliers and/or negative prices in the data.

hcris.data <- final.hcris.data %>%
  mutate( discount_factor = 1-tot_discounts/tot_charges,
          price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
          price_denom = tot_discharges - mcare_discharges,
          price = price_num/price_denom)

Q4 <- hcris.data %>% 
  group_by(year) %>% 
  filter(price_denom > 100, !is.na(price_denom), 
         price_num > 0, !is.na(price_num),
         price < 100000) %>%   
  ggplot(aes(x = as.factor(year), y = price)) + 
  geom_violin() +
  labs(
    x = "Year",
    y = "Hospital Price",
    title = "Distribution of Hospital Prices per Year"
  ) + 
  scale_y_continuous(labels = comma) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(Q4)

# Save the plot as an image
ggsave("Q4.png", plot = Q4)

save.image("submission 1/Hwk2_workspace.Rdata")

## Answer Question 5:
hcris.data <- read_rds(here("data/output/HCRIS_Data.rds"))

hcris.data <- hcris.data %>%
  mutate( discount_factor = 1-tot_discounts/tot_charges,
          price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
          price_denom = tot_discharges - mcare_discharges,
          price = price_num/price_denom)

final.hcris <- hcris.data %>% ungroup() %>%
  filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         price<100000, 
         beds>30, year==2012) %>%  
  dplyr::mutate( hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
          hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)), 
    penalty = (hvbp_payment-hrrp_payment<0)) 

final.hcris %>% group_by(penalty) %>%
  summarize(mean_price = mean(price))

# Calculate the average price among penalized hospitals
average_price_penalized <- final.hcris %>%
  filter(penalty == TRUE) %>%
  summarize(mean_price = mean(price)) %>%
  pull(mean_price)

# Calculate the average price among non-penalized hospitals
average_price_non_penalized <- final.hcris %>%
  filter(penalty == FALSE) %>%
  summarize(mean_price = mean(price)) %>%
  pull(mean_price)

# Calculate the difference between the two averages
price_difference <- average_price_penalized - average_price_non_penalized

# Print the results
print(paste("Average price among penalized hospitals: ", average_price_penalized))
print(paste("Average price among non-penalized hospitals: ", average_price_non_penalized))
print(paste("Difference between the two averages: ", price_difference))

## Answer Question 6:
# Step 1: Split hospitals into quartiles based on bed size
final.hcris <- final.hcris %>%
mutate(quartile = ntile(beds, 4)) # Split into quartiles based on bed size

# Step 2: Create indicator variables for each quartile
final.hcris <- final.hcris %>%
mutate(quartile_1 = ifelse(quartile == 1, 1, 0),
quartile_2 = ifelse(quartile == 2, 1, 0),
quartile_3 = ifelse(quartile == 3, 1, 0),
quartile_4 = ifelse(quartile == 4, 1, 0))

# Step 3: Calculate the average price among treated/control groups for each quartile
average_prices <- final.hcris %>%
group_by(quartile) %>%
summarize(mean_price_treated = mean(price[penalty == TRUE]), # Treated group
mean_price_control = mean(price[penalty == FALSE])) # Control group

# Step 4: Rename the columns for clarity
colnames(average_prices) <- c("Quartile", "Average Price (Treated)", "Average Price (Control)")

# Create a pander table
pander(average_prices, style = "rmarkdown")


#Answer Question 7:
lp.vars <- final.hcris %>% 
  dplyr::select(beds, quartile_1, quartile_2, penalty, quartile_3, 
         quartile_4, price) %>%
  dplyr::filter(complete.cases(.))
lp.covs <- lp.vars %>% dplyr::select(-c("penalty","price"))
#love <- love.plot(bal.tab(lp.covs, treat=lp.vars$penalty, s.d.denom="control"), colors="black", shapes="circle", threshold=0.1) + 
  #theme_bw() + theme(legend.position="none")

v.name=data.frame(new=c("Beds","Quartile 1", "Penalty", "Quartile 2",
                   "Quartile 3", "Quartile 4", "Price"))

# Part 1: Nearest Neighbor Matching (Inverse Variance Distance)
m.nn.var2 <- Matching::Match(Y=lp.vars$price,
                             Tr=lp.vars$penalty,
                             X=lp.covs,
                             M=1,   #<<
                             Weight=1,
                             estimand="ATE")
                             
plot1 <- love.plot(bal.tab(m.nn.var2, covs = lp.covs, treat = lp.vars$penalty), 
          threshold=0.1, 
          var.names=v.name,
          grid=FALSE, sample.names=c("Unmatched", "Matched"),
          position="top", shapes=c("circle","triangle"),
          colors=c("black","blue")) + 
  theme_bw()
#print(plot1)

#Part 2: 
m.nn.md <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=lp.covs,
                           M=1,
                           Weight=2,
                           estimand="ATE")                           
plot2 <- love.plot(bal.tab(m.nn.md, covs = lp.covs, treat = lp.vars$penalty), 
          threshold=0.1, 
          var.names=v.name,
          grid=FALSE, sample.names=c("Unmatched", "Matched"),
          position="top", shapes=c("circle","triangle"),
          colors=c("black","blue")) + 
  theme_bw()
#print(plot2)

#Part 3
logit.model <- glm(penalty ~ beds + quartile_1 + quartile_2 + quartile_3 + 
         quartile_4 + price, family=binomial, data=lp.vars)
ps <- fitted(logit.model)
m.nn.ps <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=ps,
                           M=1,
                           estimand="ATE")
plot3 <- love.plot(bal.tab(m.nn.ps, covs = lp.covs, treat = lp.vars$penalty), 
          threshold=0.1, 
          var.names=v.name,
          grid=FALSE, sample.names=c("Unmatched", "Matched"),
          position="top", shapes=c("circle","triangle"),
          colors=c("black","blue")) + 
  theme_bw()

#print(plot3)

#Part 4
reg.dat <- lp.vars %>% ungroup() %>% filter(complete.cases(.)) %>%
  mutate(beds_diff = penalty*(beds - mean(beds)))
reg <- lm(price ~ penalty + beds + quartile_1 + quartile_2 + quartile_3 + quartile_4 + 
            beds_diff,
          data=reg.dat)
summary(reg)

# Extract ATE estimates
ATE_nn_var <- bal.tab(m.nn.var2, covs = lp.covs, treat = lp.vars$penalty)$ATE
ATE_nn_md <- bal.tab(m.nn.md, covs = lp.covs, treat = lp.vars$penalty)$ATE
ATE_nn_ps <- bal.tab(m.nn.ps, covs = lp.covs, treat = lp.vars$penalty)$ATE
ATE_reg <- coef(summary(reg))["penaltyTRUE", "Estimate"]

# Create a data frame for the results
library(knitr)

results_table <- data.frame(
  Estimator = c("Nearest Neighbor (Inverse Variance Distance)", 
                "Nearest Neighbor (Mahalanobis Distance)", 
                "Inverse Propensity Weighting", 
                "Simple Linear Regression"),
  ATE = c(ATE_nn_var, ATE_nn_md, ATE_nn_ps, ATE_reg)
)

# Create a pander table for the results
pander(results_table, style = "rmarkdown")