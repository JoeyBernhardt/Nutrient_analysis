# install dependencies
for (p in c('devtools', 'zoo', 'psych')){
	if (!require(p, character.only=T)){
		install.packages(p)
		require(p, character.only=T)
	}
}

library(devtools)

# install packages
install.packages(c('zoo', 'psych', 'tidyr'))
install_github('ohi-science/rCharts')
install_github('ohi-science/ohicore')
require(ohicore)

library(rCharts)
??rCharts
?PlotFlower

## Not run: 
# generate some fake data
set.seed(1)
scores <- sample(1:10)
weights <- sample(1:10)
labels <- paste(LETTERS[1:10], "X", sep="")

# do some plots
par(mfrow=c(2,2), xpd=NA)
aster(lengths=scores, widths=weights, disk=0, main="Example 1",
			plot.outline=FALSE)
aster(lengths=scores, widths=weights, labels=labels, main="Example 2",
			lty=2, fill.col="gray", plot.outline=FALSE)
aster.legend(labels=labels, widths=weights)
aster(lengths=scores, widths=weights, disk=0.5, main="Example 3",
			center="Hello world")
aster(lengths=scores, widths=weights, disk=0.5, main="Example 3",
			center="Hello world")

## End(Not run)

PlotFlower(lengths=scores, widths=weights)
??ohicore
