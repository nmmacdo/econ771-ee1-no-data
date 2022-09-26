# Title: Assignment 1 Analysis (main.R)
# Project: assignment-1-small
# Author: Noah MacDonald
# Date Created: September 19, 2022
# Last Edited: September 24, 2022

# Loading required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate,
               crosswalkr, fixest, modelsummary, did, DRDID)

# Loading data
HCRIS <- read_tsv("data/output/HCRIS_Data.txt")
POS <- read_tsv("data/output/pos-data-combined.txt")
ACS <- read_tsv("data/output/acs_medicaid.txt")

# Keeping only necessary years
HCRIS <- subset(HCRIS, HCRIS$year >= 2003 & HCRIS$year <= 2019)
POS <- subset(POS, POS$year >= 2003 & POS$year <= 2019)

# Fixing state variable in ACS to match HCRIS and POS
ACS <- ACS %>% rename(state = State)
cw <- get(data(stcrosswalk))
ACS$state <- encodefrom(ACS, state, cw, stname, stabbr, stname)
# Note that Puerto Rico is still in ACS and is not abbreviated.
rm(cw); rm(stcrosswalk)

# Merging files to create master dataset
POS <- POS %>% rename(provider_number = provider)
master <- merge(HCRIS, POS[,c("provider_number", "year", "profit_status")], 
                by=c("provider_number","year"))
master <- merge(master, ACS, by=c("state", "year"), all.x=TRUE)
# Sorting data to make viewing easier
master <- master[order(master$provider_number, master$state, master$year),]
# Keeping only the necessary variables and reordering
master <- select(master, state, year, provider_number, tot_pat_rev,
                 uncomp_care, tot_uncomp_care_charges, 
                 tot_uncomp_care_partial_pmts, bad_debt, profit_status, 
                 expand_ever, date_adopted, expand_year, expand)
master <- master[, c(1,2,3,4,5,10,12,13,11,6,7,8,9)]
# Removing territories where medicaid expansion data is not available
master <- master[!(master$state=="PR" | master$state=="GU" | 
                     master$state=="MP" | master$state=="VI"),]

### Question 1 ----------------------------------------------------------------

# Creating table of summary statistics
table1a <- master %>% group_by(year) %>%
  mutate(tot_pat_rev = tot_pat_rev/1000000) %>%
  summarize(mean = mean(tot_pat_rev, na.rm = T),
            sd = sd(tot_pat_rev, na.rm = T),
            min = min(tot_pat_rev, na.rm = T),
            max = max(tot_pat_rev, na.rm = T))
table1b <- master %>% group_by(year) %>% 
  mutate(uncomp_care = uncomp_care/1000000) %>%
  summarize(mean = mean(uncomp_care, na.rm = T),
            sd = sd(uncomp_care, na.rm = T),
            min = min(uncomp_care, na.rm = T),
            max = max(uncomp_care, na.rm = T))
table1 <- merge(table1a, table1b, by = "year")
table1 <- table1 %>% mutate(across(where(is.numeric), round, 2))
knitr::kable(table1, 
             col.names=c("Year","Mean","SD","Min","Max","Mean","SD","Min","Max"), 
             format="latex")
rm(table1a); rm(table1b); rm(table1)

### Question 2 ----------------------------------------------------------------

# Isolating data needed for Q2 plot
plot1.dat <- master %>% 
  filter(profit_status=="For Profit" | profit_status=="Non Profit") %>%
  group_by(year, profit_status) %>% summarize(mean=mean(uncomp_care, na.rm = T))

plot1 <- ggplot(data=plot1.dat, mapping=aes(x=year,y=mean/1000000, 
                group=profit_status,linetype=profit_status)) + 
  geom_line() + geom_point() + theme_bw() + guides(linetype="none") +
  geom_text(data = plot1.dat %>% filter(year == 2017), 
            aes(label = c("For Profit","Non Profit"),
                x = year,
                y = mean/1000000 + 2.5)) +
  labs(
    x="Year",
    y="Uncompensated Care (Millions of USD)",
    title="For Profit vs. Non Profit Uncompensated Care"
  )
print(plot1)
rm(plot1.dat); rm(plot1)

### Question 3 ----------------------------------------------------------------

# Filling expand_ever variable by state
master <- master %>% group_by(state) %>% fill(expand_ever, .direction="updown")

# Getting cleaner expand_year variable to merge with master
expansion <- select(ACS, state, year, expand_year) %>% group_by(state) %>%
  summarize(mean(expand_year))
expansion <- rename(expansion, expand_year = `mean(expand_year)`)
expansion <- expansion[!(expansion$state=="Puerto Rico"),]

# Rm all expansion vars except expand_ever and replacing with new expand_year
master <- select(master, state, year, provider_number, tot_pat_rev, uncomp_care,
                 expand_ever, tot_uncomp_care_charges, 
                 tot_uncomp_care_partial_pmts, bad_debt, profit_status)
master <- merge(master, expansion, by="state", all.x=TRUE)

# Creating full sample post and treat following Ian's first DD slides
twfe1.dat <- master %>%
  filter(expand_year >= 2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(post = (year >= 2014), treat = post*expand_ever)

# TWFE with full sample
twfe1 <- feols(uncomp_care/1000000 ~ treat | provider_number + year, data=twfe1.dat)

# Dropping uncompensated care outliers (no longer necessary I don't believe)
# twfe1b.dat <- twfe1.dat %>% 
#   mutate(uncomp_care = 
#            ifelse(uncomp_care < quantile(uncomp_care,0.25, na.rm = T) - 1.5*IQR(uncomp_care, na.rm = T) | 
#                     uncomp_care > quantile(uncomp_care,0.75, na.rm = T) + 1.5*IQR(uncomp_care, na.rm = T), 
#                   NA, uncomp_care))

# Making datasets for 2014, 2015, and 2016 expansions only
twfe2.dat <- master %>%
  filter(expand_year == 2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(post = (year >= 2014), treat = post*expand_ever)
twfe3.dat <- master %>%
  filter(expand_year == 2015 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(post = (year >= 2014), treat = post*expand_ever)
twfe4.dat <- master %>%
  filter(expand_year == 2016 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(post = (year >= 2014), treat = post*expand_ever)

# Running TWFE with these subsamples and collecting results
twfe2 <- feols(uncomp_care/1000000 ~ treat | provider_number + year, data=twfe2.dat)
twfe3 <- feols(uncomp_care/1000000 ~ treat | provider_number + year, data=twfe3.dat)
twfe4 <- feols(uncomp_care/1000000 ~ treat | provider_number + year, data=twfe4.dat)

# Collecting results into one table
q3table <- etable(twfe1, twfe2, twfe3, twfe4)
knitr::kable(q3table,format="latex")
rm(q3table)

### Question 4 ----------------------------------------------------------------

# Formatting data for full sample / staggered adoption event study
es1.dat <- master %>% 
  filter(!is.na(expand_ever)) %>%
  mutate(post = (year>=2014), 
         treat=post*expand_ever,
         time_to_treat = ifelse(expand_ever==FALSE, 0, year-expand_year),
         time_to_treat = ifelse(time_to_treat < -3, -3, time_to_treat))
# Event Study for full sample / staggered adoption
es1 <- feols(uncomp_care/1000000~i(time_to_treat, expand_ever, ref=-1) | 
               provider_number + year,
                  cluster=~provider_number,
                  data=es1.dat)
summary(es1)
iplot(es1, 
      xlab = 'Time to treatment',
      ylab = 'Uncompensated Care (Milliions of USD)',
      main = 'Event Study (Full Sample)')

# Formatting data for 2014 adoption only
es2.dat <- master %>%
  filter(expand_year == 2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(post = (year>=2014), 
         treat=post*expand_ever,
         time_to_treat = ifelse(expand_ever==FALSE, 0, year-expand_year),
         time_to_treat = ifelse(time_to_treat < -3, -3, time_to_treat))
# Event study for 2014 adopters vs never adopted
es2 <- feols(uncomp_care/1000000~i(time_to_treat, expand_ever, ref=-1) | 
               provider_number + year,
             cluster=~provider_number,
             data=es2.dat)
summary(es2)
iplot(es2, 
      xlab = 'Time to treatment',
      ylab = 'Uncompensated Care (Milliions of USD)',
      main = 'Event Study (2014 Treatment)')

options("modelsummary_format_numeric_latex" = "plain")
modelsummary(list(es1, es2), stars = TRUE, output = "latex")

### Question 5 ----------------------------------------------------------------

# Disaggregated SA Event Study for 2014/15/16 adopters
sa.dat <- master %>% 
  filter(expand_year == 2014 | expand_year == 2015 | 
           expand_year == 2016 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(post = (year>=2014), 
         treat=post*expand_ever,
         expand_year = ifelse(expand_ever==FALSE, 10000, expand_year),
         time_to_treat = ifelse(expand_ever==FALSE, 0, year-expand_year),
         time_to_treat = ifelse(time_to_treat < -3, -3, time_to_treat))

sa <- feols(uncomp_care~sunab(expand_year, time_to_treat, no_agg = T) | 
              provider_number + year,
                cluster=~provider_number,
                data=sa.dat)
summary(sa)
# Cannot plot all three on one figure using iplot. Plotting aggregation below.

# Aggregated SA Event Study for 2014/15/16 adopters
sa_agg <- feols(uncomp_care/1000000~sunab(expand_year, time_to_treat, no_agg = F) | 
              provider_number + year,
            cluster=~provider_number, 
            data=sa.dat)
summary(sa_agg)
iplot(sa_agg, 
      xlab = 'Time to treatment',
      ylab = 'Uncompensated Care (Milliions of USD)',
      main = 'SA Event Study (Aggregation)')

# LaTeX table for aggregated SA Event Study only
modelsummary(sa_agg, output = "latex")

### Question 6 ----------------------------------------------------------------

# Redoing for 2014 vs never adopt
sa2014.dat <- master %>% 
  filter(expand_year == 2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(post = (year>=2014),
         expand_year = ifelse(expand_ever==FALSE, 10000, expand_year),
         treat=post*expand_ever,
         time_to_treat = ifelse(expand_ever==FALSE, 0, year-expand_year),
         time_to_treat = ifelse(time_to_treat < -3, -3, time_to_treat))
sa2014 <- feols(uncomp_care/1000000~sunab(expand_year, time_to_treat) | 
                  provider_number + year,
                cluster=~provider_number,
                data=sa2014.dat)
iplot(sa2014, 
      xlab = 'Time to treatment',
      ylab = 'Uncompensated Care (Milliions of USD)',
      main = 'SA Event Study (2014 Treatment)')

# Redoing for 2015 vs never adopt
sa2015.dat <- master %>% 
  filter(expand_year == 2015 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(post = (year>=2014),
         expand_year = ifelse(expand_ever==FALSE, 10000, expand_year),
         treat=post*expand_ever,
         time_to_treat = ifelse(expand_ever==FALSE, 0, year-expand_year),
         time_to_treat = ifelse(time_to_treat < -3, -3, time_to_treat))
sa2015 <- feols(uncomp_care/1000000~sunab(expand_year, time_to_treat) | 
                  provider_number + year,
                cluster=~provider_number,
                data=sa2015.dat)
iplot(sa2015, 
      xlab = 'Time to treatment',
      ylab = 'Uncompensated Care (Milliions of USD)',
      main = 'SA Event Study (2015 Treatment)')

# Redoing for 2016 vs never adopt
sa2016.dat <- master %>% 
  filter(expand_year == 2016 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(post = (year>=2014),
         expand_year = ifelse(expand_ever==FALSE, 10000, expand_year),
         treat=post*expand_ever,
         time_to_treat = ifelse(expand_ever==FALSE, 0, year-expand_year),
         time_to_treat = ifelse(time_to_treat < -3, -3, time_to_treat))
sa2016 <- feols(uncomp_care/1000000~sunab(expand_year, time_to_treat) | 
                  provider_number + year,
                cluster=~provider_number,
                data=sa2016.dat)
iplot(sa2016, 
      xlab = 'Time to treatment',
      ylab = 'Uncompensated Care (Milliions of USD)',
      main = 'SA Event Study (2016 Treatment)')

# LaTeX table for aggregated SA Event Study + 2014 + 2015 + 2016
modelsummary(list("Aggregated" = sa_agg,
                  "2014" = sa2014,
                  "2015" = sa2015,
                  "2016" = sa2016),
             stars = TRUE,
             output = "latex")

### Question 7 ----------------------------------------------------------------

# Preparing data for Callaway & Sant'Anna approach
cs.dat <- master %>%
  filter(!is.na(expand_ever)) %>%
  mutate(post = (year>=2014),
         treat = post*expand_ever,
         expand_year=ifelse(is.na(expand_year),0,expand_year),
         uncomp_care = uncomp_care/1000000) %>%
  filter(!is.na(uncomp_care)) %>%
  group_by(provider_number) %>%
  mutate(providergroup = cur_group_id()) %>% ungroup()

# Getting CS group-time average treatment effects
cs <- att_gt(yname="uncomp_care", tname="year", idname="providergroup",
                 gname="expand_year",
                 data=cs.dat, panel=TRUE, est_method="dr",
                 allow_unbalanced_panel=TRUE)
# Aggregating CS group-time average treatment effects
cs.event <- aggte(cs, type="dynamic")
# Plotting results
ggdid(cs.event, legend=F) + labs(
  x="Time to treatment",
  y="Uncompensated Care (Millions of USD)",
  title="Average Effect by Length of Exposure (CS Full Sample)")

# Preparing alternate data for Callaway & Sant'Anna approach (2014 adopters)
cs2014.dat <- master %>%
  filter(expand_year == 2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(post = (year>=2014),
         treat = post*expand_ever,
         expand_year=ifelse(is.na(expand_year),0,expand_year),
         uncomp_care = uncomp_care/1000000) %>%
  filter(!is.na(uncomp_care)) %>%
  group_by(provider_number) %>%
  mutate(providergroup = cur_group_id()) %>% ungroup()

# Getting CS group-time average treatment effects
cs2014 <- att_gt(yname="uncomp_care", tname="year", idname="providergroup",
             gname="expand_year",
             data=cs2014.dat, panel=TRUE, est_method="dr",
             allow_unbalanced_panel=TRUE)
# Aggregating CS group-time average treatment effects
cs2014.event <- aggte(cs2014, type="dynamic")
# Plotting results
ggdid(cs2014.event, legend=F) + labs(
  x="Time to treatment",
  y="Uncompensated Care (Millions of USD)",
  title="Average Effect by Length of Exposure (CS 2014 Treatment)")

### Question 8 ----------------------------------------------------------------

# I look forward to seeing the solutions!

#source('analysis/Callaway_SantAnna_DiD_with_Rambachan_Roth_honest_DID.R')