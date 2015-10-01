library(dplyr)
library(ggplot2)

nut <- read.csv("~/Desktop/Nutrient_databases/nut_sept22_lwr_dec3.csv", comment.char="#", stringsAsFactors=FALSE)

str(nut)
summary(nut)
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
