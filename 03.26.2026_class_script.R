# 03.26.2024
# JMN
# this is jordans code from 3/24

library(tidyverse)
load("data/RAMLDB v4.66/R Data/DBdata[asmt][v4.66].RData")
 
head(tsmetrics)
head(timeseries)
 
timeseries_tsmetrics = left_join(
    timeseries,
    tsmetrics,
    by = c("tsid" = "tsunique")
)
 
dim(timeseries)
dim(timeseries_tsmetrics)
head(timeseries_tsmetrics)
 
head(timeseries_values_views)
head(taxonomy)
glimpse(stock)
 
# join timeseries_values_views and taxonomy tables into one table
# to do this, we must first join stock and timeseries_values_views bc stockid is how they will link
# select only the columns you want to investigate so you dont have to manipulate 69 columns all the time
fish = timeseries_values_views %>%
  left_join(stock, by = c("stockid", "stocklong")) %>%
  left_join(taxonomy, by = c("tsn", "scientificname")) %>%
  select(stockid, stocklong, year, TCbest, tsn, scientificname, commonname, region, FisheryType, taxGroup)
 
dim(fish)
glimpse(fish)
 
 
##########################################################################################
# a collapse of a fishery is defined as catches dropping below 10% of the recorded maximum
# Boris Worm - 2006
##########################################################################################
 
fish %>%
  arrange(desc(TCbest))
 
# color = stockid will give so many color lines that the whole figure will be a legend
# theme(legend.position) will remove the legend and print all the lines on the graph
ggplot() +
  geom_line(data = fish,
    aes(x = year, y = TCbest, color = stockid)) +
  theme(legend.position = "none")
 
# this filters for only the highest caught fish and thus can handle a legend
ggplot() +
  geom_line(data = fish %>% filter(TCbest > 3e6), aes(x = year, y = TCbest, color = stocklong))
 
# investigate Canadian Cod collapse
# for species, cod, there are 4 distinct regions cod occur in this dataset
# we care about Canada East Coast
fish %>%
  filter(scientificname == "Gadus morhua") %>%
  distinct(region)
 
cod_can = fish %>%
  filter(scientificname == "Gadus morhua",
        region == "Canada East Coast",
        !is.na(TCbest))
head(cod_can)
 
ggplot(data = cod_can) +
  geom_line(aes(x = year, y = TCbest, color = stocklong)) +
  theme_bw() +
  ylab("Total catch in MT")
 
# add all cod stocks together to get one line
# would not typically do this bc want different management units
cod_can_total = cod_can %>%
  group_by(year) %>%
  summarize(total_catch = sum(TCbest))
head(cod_can_total)
 
ggplot(data = cod_can_total) +
  geom_line(aes(x = year, y = total_catch))
 
# play with the cumulative functions in dplyr
dat = c(1, 3, 6, 2, 3, 9, -1)
dat_max = cummax(dat)
dat_sum = cumsum(dat)
test_cum = data.frame(dat, dat_max, dat_sum)
test_cum
 
# cum_max = from this point in my data frame examining all procedding rows, what is the max number i have encountered so far
# cum_sum = adds all proceeding values to get a total sum of all proceeding rows
 
# using boris worm def, has cod collapsed? and when?
head(cod_can_total)
cod_collapse = cod_can_total %>%
  mutate(historical_max_catch = cummax(total_catch),
        collapse = total_catch <= 0.1 * historical_max_catch)
head(cod_collapse)
tail(cod_collapse)
 
# identify the year that the cod collapse occured
# pull() pulls a vector out the data frame so it can be used as a number
cod_collapse_yr = cod_collapse %>%
  filter(collapse == TRUE) %>%
  summarize(year = min(year)) %>%
  pull()
class(cod_collapse_yr)
 
# color = collapse will be red when false and blue when true
ggplot() +
  geom_line(data = cod_collapse, aes(x = year, y = total_catch, color = collapse)) +
  geom_vline(xintercept = cod_collapse_yr) +
  theme_classic()
 
# apply collapse to full data set
# super data frame for all fish
 
head(fish)
collapse = fish %>%
  filter(!is.na(TCbest)) %>%
  group_by(stockid) %>%
  mutate(historical_max_catch = cummax(TCbest),
        current_collapse = TCbest <= 0.1 * historical_max_catch,
        collapsed_yet = cumsum(current_collapse) > 0) %>%
  ungroup()
glimpse(collapse)
 
collapse_yr = collapse %>%
  group_by(stockid, stocklong, region) %>%
  filter(collapsed_yet == TRUE) %>%
  summarize(first_collapse_yr = min(year)) %>%
  ungroup()
 
head(collapse_yr)
 
# time series of shame
n_stocks = length(unique(collapse$stockid))
 
collapse_ts = collapse_yr %>%
  group_by(first_collapse_yr) %>%
  summarize(n = n()) %>%
  mutate(cum_first_collapse_yr = cumsum(n),
         ratio_collapsed_yet = cum_first_collapse_yr / n_stocks)
 
ggplot(data = collapse_ts) +
  geom_line(aes(x = first_collapse_yr, y = ratio_collapsed_yet))

###############################################################
# 03.26.2026
# Unit 4.3
# Generalized Linear Models

# we're gonna use the RAM legacy database; i loaded this in earlier in the script
# load('data/RAMLDB v4.66/R Data/DBdata[asmt][v4.66].RData')
# library(tidyverse)

# source() command allows you to run a function on a set of data from another script
# good for re using functions again and again, and is good for if you update the function for all of your scripts
source('build_collapse_table.R')

# linear regression is when you think your data fits a line
# logistic regressions are when your data fits a logarithmic line; probability your data is bound to variable 0 or 1
    # can be fit with glm() using the binomial family generalized linear models

glimpse(collapse)

# what characteristic makes it more likley for a fishery to collapse?

# step 1: remove time series information and collapse data to "has this stock EVER collapsed?"
model_data = collapse %>%
  group_by(stockid, stocklong) %>% # stocklong will drop out if i dont group with it
  summarize(ever_collapsed = any(current_collapse)) %>%
  ungroup() %>%
  left_join(metadata, by = c("stockid", "stocklong")) %>%
  mutate(FisheryType = as.factor(FisheryType))

glimpse(model_data)

# step 2: make sure we didnt add or subtract any rows
length(unique(collapse$stockid))
dim(model_data)

# step 3: run a logistic regression
model_1 = glm(ever_collapsed ~ FisheryType, data = model_data, family = "binomial")
summary(model_1)

# step 4: arrange fishery type alphabetically to see that intercept represents "Flatfish"
model_data %>% distinct(FisheryType) %>% arrange(FisheryType)
    # model is based on flatfish, so you correct the model to the fish youre looking at based on teh estimate std.

# we are going to generate predictions on the probability of a stock collapse by fishery type
FisheryType = model_data %>% distinct(FisheryType)
# FisheryType = model_data %>% filter(primary_country == "Canada") %>% distinct(FisheryType)
model_1_predict = predict(model_1, newdata = FisheryType, type = "response", se.fit = TRUE)

# Organize predictions into a tidy table
collapse_fishery_type_predictions = cbind(FisheryType, model_1_predict)

# plot predictions and SE bars
ggplot(data = collapse_fishery_type_predictions) + 
  geom_bar(aes(x = FisheryType, y = fit, fill = FisheryType), 
          stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = fit-se.fit, ymax = fit+se.fit, x = FisheryType), width = 0.2) +
  coord_flip() +
  ylab("Probability of Stock Collapse") +
  xlab("Fishery Type") +
  theme_bw()


#### Poissan models
# just like a linear model but expects your y to have a count distribution
# test data over disperssion with AER::dispersiontest()

# question: how many years have my stocks been in a collapsed state 
    # controlled for the total number of years we have data for
    # is the amount of time of collapse a function of fishing pressure or a function of biomass

tsmetrics %>% filter(tsshort == "UdivUmsypref")
tsmetrics %>% filter(tsshort == "BdivBmsypref")

u_summary = timeseries_values_views %>%
  #left_join(stock, by = c("stockid", "stocklong")) %>%
  filter(!is.na(UdivUmsypref),
          !is.na(BdivBmsypref)) %>%
  group_by(stockid, stocklong) %>%
  summarize(yrs_data = n(),
            ratio_yrs_overfished = sum(UdivUmsypref > 1)/yrs_data, # is ratio greater than 1? - high fishing rate
            ratio_yrs_low_stock = sum(BdivBmsypref < 1)/yrs_data) %>% #is ratio less than 1? - low stock
  select(-yrs_data) %>% 
  ungroup() %>%
  left_join(metadata %>% select(stockid, FisheryType))

head(u_summary)

# count number of years each stock is collapsed; join with counts of overfished years and low stock years
collapse_summary = collapse %>% 
  group_by(stockid) %>%
  summarize(yrs_data = n(), 
            yrs_collapsed = sum(current_collapse)) %>%
  inner_join(u_summary, by = "stockid") # only keep stocks that have collapse data and u_summary

glimpse(collapse_summary)

# do we have zero-inflation?
hist(collapse_summary$yrs_collapsed)
table(collapse_summary$yrs_collapsed)

# create a zero-truncated data set to demonstrate poissan model
collapse_summary_zero_trunc = collapse_summary %>% filter(yrs_collapsed>0)
summary(collapse_summary_zero_trunc)

# build poisson model
model_p = glm(yrs_collapsed ~ ratio_yrs_overfished + ratio_yrs_low_stock + FisheryType, 
          offset(log(yrs_data)), data = collapse_summary_zero_trunc, family = "poisson")
summary(model_p)

# do we have an overdispersion?
library(AER)
AER::dispersiontest(model_p)$p.value < 0.5 # TRUE = overdispersed; FALSE = not overdispersed

# address overdispersion with a quasipoisson or negative binomial model
model_qp = glm(yrs_collapsed ~ ratio_yrs_low_stock + ratio_yrs_overfished + FisheryType,
            offset(log(yrs_data)), data = collapse_summary_zero_trunc, family = "quasipoisson")
summary(model_qp)

# we could dump ratio_yrs_low_stock which was not significant
# model_qp2 = glm(yrs_collapsed ~ ratio_yrs_overfished + FisheryType, offset(log(yrs_data)), data = collapse_summary_zero_trunc, family = "quasipoisson")
# summary(model_qp2)

