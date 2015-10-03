library(dplyr)
library(ggplot2)

nut <- read.csv("~/Desktop/Nutrient_databases/nut_sept22_lwr_dec3.csv", comment.char="#", stringsAsFactors=FALSE)

str(nut)
summary(nut)
View(nut)
nut$ASFIS.Scientific.name
ntbl <- tbl_df(nut)
ntbl
glimpse(ntbl)
(snippet <- subset(nut, ASFIS.Scientific.name == "Abramis brama"))

filter(ntbl, TL < 3)
filter(ntbl, ASFIS.Scientific.name == "Abramis brama")
filter(ntbl, ASFIS.Scientific.name %in% c("Abramis brama", "Thymallus arcticus"))
nut %>% head
nut %>% head(3)

select(ntbl, ASFIS.Scientific.name, CA_mg)



###STAT 545 Homework 3

#1. Get the min and max of x nutrient/body size, for each species
#2. Look at the spread of x within each habitat (grouping variable)
#3. Get a trimmed mean, i.e. mean of some sort of subset of the data
#4. How does nutrient content vary with body size?
#5. Create some sort of benchmark from the data (i.e. mean, median, quantile, etc.), then determine how many species have a nutrient content less than that
#6. For each table, make sure to include a relevant figure. 
# Your figure does not have to depict every last number from the data aggregation result. It just needs to complement the table, add context, and allow for some sanity checking both ways. 
# Notice which figures are easy/hard to make, which data formats make better inputs for plotting functions vs. for human-friendly tables.
