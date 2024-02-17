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
num_unique_hospitals <- final.hcris.data %>% 
  summarise(num_unique_hospitals = n_distinct(provider_number))

print(num_unique_hospitals)

## Answer Question 3: What is the distribution of total charges (tot_charges) in each year?
## Show your results with a 'violin' plot, with charges on the y-axis and years on the x-axis.

# Create violin plot
Q3 <- ggplot(final.hcris.data, aes(x = year, y = tot_charges)) +
  geom_violin() +
  labs(x = "Year", y = "Total Charges", 
       title = "Distribution of Total Charges Each Year")

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

filtered.hcris.data <- hcris.data %>%
  filter(price >= 0) %>%
  group_by(year) %>%
  mutate(IQR = IQR(price, na.rm = TRUE),
         Q1 = quantile(price, 0.25, na.rm = TRUE),
         Q3 = quantile(price, 0.75, na.rm = TRUE)) %>%
  filter(price >= (Q1 - 1.5 * IQR), price <= (Q3 + 1.5 * IQR))

Q4 <- ggplot(filtered.hcris.data, aes(x = factor(year), y = price)) +
  geom_violin(trim = FALSE) +
  labs(x = "Year", y = "Price", 
       title = "Distribution of Estimated Prices in Each Year")

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


# Display the table of average prices
print(average_prices)

#Answer Question 7:
lp.vars <- final.hcris %>% 
  dplyr::select(beds, mcaid_discharges, penalty, ip_charges, 
         mcare_discharges, tot_mcare_payment, price) %>%
  filter(complete.cases(.))
lp.covs <- lp.vars %>% dplyr::select(-c("penalty","price"))
love <- love.plot(bal.tab(lp.covs, treat=lp.vars$penalty, s.d.denom="control"), colors="black", shapes="circle", threshold=0.1) + 
  theme_bw() + theme(legend.position="none")

v.name=data.frame(new=c("Beds","Medicaid Discharges", "Inaptient Charges",
                   "Medicare Discharges", "Medicare Payments"))

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
logit.model <- glm(penalty ~ beds + mcaid_discharges + ip_charges + mcare_discharges +
            tot_mcare_payment, family=binomial, data=lp.vars)
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
  mutate(beds_diff = penalty*(beds - mean(beds)),
         mcaid_diff = penalty*(mcaid_discharges - mean(mcaid_discharges)),
         ip_diff = penalty*(ip_charges - mean(ip_charges)),
         mcare_diff = penalty*(mcare_discharges - mean(mcare_discharges)),
         mpay_diff = penalty*(tot_mcare_payment - mean(tot_mcare_payment)))
reg <- lm(price ~ penalty + beds + mcaid_discharges + ip_charges + mcare_discharges + tot_mcare_payment + 
            beds_diff + mcaid_diff + ip_diff + mcare_diff + mpay_diff,
          data=reg.dat)
summary(reg)

# Extract ATE estimates
ATE_nn_var <- bal.tab(m.nn.var2, covs = lp.covs, treat = lp.vars$penalty)$ATE
ATE_nn_md <- bal.tab(m.nn.md, covs = lp.covs, treat = lp.vars$penalty)$ATE
ATE_nn_ps <- bal.tab(m.nn.ps, covs = lp.covs, treat = lp.vars$penalty)$ATE
ATE_reg <- coef(summary(reg))["penaltyTRUE", "Estimate"]

# Create a data frame for the results
results_table <- data.frame(
  Estimator = c("Nearest Neighbor (Inverse Variance Distance)", 
                "Nearest Neighbor (Mahalanobis Distance)", 
                "Inverse Propensity Weighting", 
                "Simple Linear Regression"),
  ATE = c(ATE_nn_var, ATE_nn_md, ATE_nn_ps, ATE_reg)
)

# Print the results table
print(results_table)
