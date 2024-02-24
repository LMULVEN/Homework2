# Meta --------------------------------------------------------------------
## Author:        Leila Mulveny
## Date Created:  2/10/2024
## Question 1

# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate)


# Read and combine data ---------------------------------------------------
source('submission 1/data-code/H1_HCRISv1996.R')
source('submission 1/data-code/H2_HCRISv2010.R')

final.hcris.v1996=read_rds('data/output/HCRIS_Data_v1996.rds')
final.hcris.v2010=read_rds('data/output/HCRIS_Data_v2010.rds')

## create missing variables for columns introduced in v2010 of hcris forms
final.hcris.v1996 = final.hcris.v1996 %>%
  mutate(hvbp_payment=NA, hrrp_payment=NA)

## combine v1996 and v2010 hcris forms, and sort by provider_number/year
final.hcris=rbind(final.hcris.v1996,final.hcris.v2010) %>%
  mutate(fy_end=mdy(fy_end),fy_start=mdy(fy_start),
         date_processed=mdy(date_processed),date_created=mdy(date_created),
         tot_discounts=abs(tot_discounts), hrrp_payment=abs(hrrp_payment)) %>%
  mutate(fyear=year(fy_end)) %>%
  arrange(provider_number,fyear) %>%
  select(-year)

## count of hospitals/provider_number by year
final.hcris %>% group_by(fyear) %>% count()

# Clean data --------------------------------------------------------------

## create count of reports by hospital fiscal year
final.hcris =
  final.hcris %>% 
  add_count(provider_number, fyear, name="total_reports")

## create running total of reports
final.hcris =
  final.hcris %>% 
  group_by(provider_number, fyear) %>%
  mutate(report_number=row_number())

## identify hospitals with only one report per fiscal year 
## this will be the first set of hospitals in the final dataset
unique.hcris1 =
  final.hcris %>%
  filter(total_reports==1) %>%
  select(-report, -total_reports, -report_number, -npi, -status) %>%
  mutate(source='unique reports')


## identify hospitals with multiple reports per fiscal year
duplicate.hcris = 
  final.hcris %>%
  filter(total_reports>1) %>%
  mutate(time_diff=fy_end-fy_start)

## Answer Question 1: How many hospitals filed more than one report in the same year? Show your answer as a line graph of the number of hospitals over time.
## Use Count function on duplicate.hcris object. Group by (provider_number, fyear) and display as a line graph

num_hospitals_multiple_reports = duplicate.hcris %>% 
  summarise(num_hospitals = n_distinct(provider_number))

print(num_hospitals_multiple_reports)

# Create summary data frame
hospitals_over_time = duplicate.hcris %>%
  group_by(fyear) %>%
  summarise(num_hospitals = n_distinct(provider_number))

# Plot line graph
p <- ggplot(hospitals_over_time, aes(x = fyear, y = num_hospitals)) +
  geom_line() +
  labs(x = "Fiscal Year", y = "Number of Hospitals", 
       title = "Number of Hospitals Filing More Than One Report Over Time")

# Save the plot to a file
ggsave("Question1.png", plot = p)