rm(list=ls())


require(ggplot2)
require(tigris)
require(sf)
require(dplyr)
require(tibble)
library(ggplot2)
library(dplyr)

#call setwd() if needed

deer_data <-read.csv('./Master_CWD_Data_CLEAN.csv')


table(deer_data$status)

deer_matrix = data.matrix(deer_data)

# matrices for numerical summary
table(deer_data$county)


c_matrix = unique(matrix(deer_data[, "County"]))


county <- c(unique(as.character(deer_data[,"County"])))

cases <- vector("numeric", length = length(county))

total <- vector("numeric", length = length(county))


# currently set row names to county, alternatively could have county as 
# another column, but i found this easier
df <- data.frame(cases, total)
row.names(df) <- county



for (i in 1:nrow(deer_data)) {
  df[as.character(deer_data[i,"County"]),2] = df[as.character(deer_data[i,"County"]),2] + 1
  if (deer_data[i,"status"] == 1) {
    df[as.character(deer_data[i,"County"]),1] = df[as.character(deer_data[i,"County"]),1] + 1
  }
}

# StateChoropleth

deer_frame <- data.frame(county, cases)


virginia <- tigris::counties(state = 'VA', cb = T, class = 'sf')


#ideally do this in a loop or something, so each row is entered if more are added

# Number of infections
infData <- tribble(
  ~County, ~State, ~Infections,
  county[1], 'Virginia', df[1,1],
  county[2], 'Virginia', df[2,1],
  county[3], 'Virginia', df[3,1],
  county[4],  'Virginia', df[4,1],
  county[5],  'Virginia', df[5,1],
  county[6], 'Virginia', df[6,1],
)


chart1 <- virginia %>% 
  left_join(infData, by = c("NAME" = "County"))


ggplot(chart1) +
  geom_sf(aes(fill = Infections))


# Ratio of infected deer

ratios <- c(df[1,1]/df[1,2], df[2,1]/df[2,2], df[3,1]/df[3,2], df[4,1]/df[4,2], df[5,1]/df[5,2], df[6,1]/df[6,2])

ratioData <- tribble(
  ~County, ~State, ~Infected,
  county[1], 'Virginia', ratios[1],
  county[2], 'Virginia', ratios[2],
  county[3], 'Virginia', ratios[3],
  county[4],  'Virginia', ratios[4],
  county[5],  'Virginia', ratios[5],
  county[6], 'Virginia', ratios[6],
)


chart2 <- virginia %>% 
  left_join(ratioData, by = c("NAME" = "County"))

# Will likely change this graph so that 0% and 100% infection rates are different colors to make others more distinguished
ggplot(chart2) +
  geom_sf(aes(fill = Infected))