#### Nutrient_analysis

Hi Mary!


For our Nov 17 meeting:

* [Slides](https://docs.google.com/presentation/d/1yBjuN8BPX3dm4EXpsVULtySWWB433Re6pppF3uW3OOU/edit#slide=id.gd3e5c3894_0_194) (draft!!) and plan for committee meeting on Friday.

* [Navigating the multiple meanings of beta diversity](http://onlinelibrary.wiley.com/doi/10.1111/j.1461-0248.2010.01552.x/abstract). This paper is helping me understand how to think about using beta diversity to measure the variability among species (i.e. vs. turnover etc)

* This vegan [tutorial](https://cran.r-project.org/web/packages/vegan/vignettes/diversity-vegan.pdf), in particular, the section on using the slope (z) of the species accumulation curve to measure beta diversity?

* Should I standardize the trait values???

* How to assess the signifiance of BC values among species? PERMANOVA? How?? Compare to null distribution? Again, how??

* For functional diversity, group by taxon/country?

For our Nov 13 meeting, please check out:

* My [code](https://github.com/JoeyBernhardt/Nutrient_analysis/blob/master/Functional_Diversity/FD.null.Rmd) for generating the null distribution of FD values. The main thing I'd like your feedback on here, is: sample WITH or WITHOUT replacement? Oh, and also how to compare my observed FD values to the null distribution...if something falls outside the 95% percentile it's different from the null?

* This [blog post](http://www.r-bloggers.com/on-functional-diversity-metrics/), which compares different FD approaches.

* Owen Petchey's [advice](http://www.thetrophiclink.org/resources/calculating-fd/) and [code](https://github.com/opetchey/ttl-resources/tree/master/functional_diversity) for calculating FD.

* This [fundiv package](https://github.com/ibartomeus/fundiv), which supposedly  helps compare the two approaches.

* Let's discuss the main differences between distance based metrics of FD and space based approaches, and which is more appropriate for the type of data I have...i.e. some very small 'species richness' values for some countries. Or should I just try both and compare?

* Finally, let's talk about some other ways of getting at functional redundancy, ie. something akin to rarefaction curves? See this [paper](http://onlinelibrary.wiley.com/store/10.1111/j.2041-210X.2011.00178.x/asset/j.2041-210X.2011.00178.x.pdf;jsessionid=7F185D0F8E4890891857EB8C60F53497.f01t04?v=1&t=igwds46p&s=20739ebd17f769aa704a0a6822b7575ac8413968)





For our Nov 6 meeting, please check out:

* This [paper](http://www.esajournals.org/doi/abs/10.1890/08-2244.1) by Laliberte and Legendre which describes the [FD](https://cran.r-project.org/web/packages/FD/index.html) package and approach.


* [rfishbase](https://github.com/ropensci/rfishbase)

* my [attempts](https://github.com/JoeyBernhardt/Nutrient_analysis/tree/master/Functional_Diversity) at calculating nutritional FD, inspired by this [paper](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0021235) about nutritional diversity in African agricultural systems.





#### What's in this repo?
(an attempt at organizing things!)

Code for plots and figures in the trait nutrient paper. 


* [Data files](https://github.com/JoeyBernhardt/Nutrient_analysis/tree/master/data)



* [Functional Diversity analyses](https://github.com/JoeyBernhardt/Nutrient_analysis/tree/master/Functional_Diversity)


* [rfishbase stuff](https://github.com/JoeyBernhardt/Nutrient_analysis/tree/master/rfishbase)



* [plots and models](https://github.com/JoeyBernhardt/Nutrient_analysis/tree/master/R_Plots_models)

