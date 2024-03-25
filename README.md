# Influence of material wealth on age at first reproduction

Here, you can find all the files related to the data wrangling and statistical analyses that aim to understand the relationship between material wealth with the probability that women have their first child, using data from Pimbwe women from Tanzania.

At the beginning you can find two folders, and some files. The files are:
- dataf.csv = contains the sample of information from Pimbwe women used for this analyses.
- firstbaby1.rds = contains the output of the model with only age as predictor, fitting simulated data.
- firstbaby2.rds = contains the output of the model with age and absolute wealth as predictors, fitting simulated data.
- firstbaby3.rds = contains the output of the model with age, absolute, and variability of wealth as predictors, fitting simulated data.
- firstbaby1_real.rds = contains the output of the model with only age as predictor, fitting real data.
- firstbaby2_real.rds = contains the output of the model with age and absolute wealth as predictors, fitting real data.
- firstbaby3_real.rds = contains the output of the model with age, absolute, and variability of wealth as predictors, fitting real data.
- single_abswealth_plot.pdf = plot with all the predictions of the model with absolute wealth in one plot.
- multiple_abswealth_plot.pdf = plot with all the predictions of the model with absolute wealth in multiple plots.
- single_diffwealth_plot.pdf = plot with all the predictions of the model with wealth variability in one plot.
- multiple_diffwealth_plot.pdf = plot with all the predictions of the model with wealth variability in multiple plots.

The folder "Model_code" contains the files related to the data cleaning, wrangling, and analyses. Here, the files are:
- Wealth_AFR_code.R = contains the code for data cleaning, wrangling, and exploration.
- firstbaby.R = contains the code for data wrangling, simulation, and analysis of the model with only age as a predictor.
- firstbaby.stan = contains the code for the Stan model with only age as a predictor.
- firstbaby_abswealth.R = contains the code for data wrangling, simulation, and analysis of the model with age and absolute wealth as predictors.
- firstbaby_abswealth.stan = contains the code for the Stan model with age and absolute wealth as predictors.
- firstbaby_diffwealth.R = contains the code for data wrangling, simulation, and analysis of the model with age, absolute and wealth variability as predictors.
- firstbaby_diffwealth.stan = contains the code for the Stan model with age, absolute and wealth variability.

The folder "Misc" contains files from previous attempts of analyses, and unfinished work. Mainly to keep the files there, until a final clean later on.
