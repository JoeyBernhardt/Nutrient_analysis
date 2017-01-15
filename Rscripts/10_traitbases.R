### trying out the encyclopedia of life for trait data


install.packages("traits")
devtools::install_github("ropensci/reol")
devtools::install_github("ropensci/traits")



library(Reol)
library(listviewer)
library(traits)


acanth <- APItaxon("Acanthina monodon")


res <- DownloadSearchedTaxa(c("Acanthina monodon"), to.file=FALSE, exact=TRUE)
out <- GatherDataObjectInformation(res)
out[1:6, 1:6]
View(out)

str(out)
str(res)

names(out)

out$description


4726160
