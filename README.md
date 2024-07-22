# Influence of material wealth on age at first reproduction

Here, you can find all the files related to the data wrangling and statistical analyses that aim to understand the relationship between material wealth with the probability that women have their first child, using data from Pimbwe women from Tanzania.

At the beginning you can find two folders. One contains the data used for the analyses ("Data"), while the other one contains the code for the statistical analyses ("Model_code").

In the folder "Data" you can find the file:
- "dataf.csv" which contains the data necessary for the analysis in a csv file. The data consists of:
    - ID and order, which are variables regarding the personal identification of each women.
    - afr, which is the age at which a woman had her first birth.
    - dob, which is the year of birth.
    - aoc, which is the age of censoring.
    - absw95, absw98, absw00, absw02, absw04,absw06, absw10 are the amount of wealth a woman had for each year of census.
    - age_absw95, age_absw98, age_absw00, age_absw02, age_absw04, age_absw06, age_absw10, are the ages of each woman at each census.

In the folder "Model_code" you can find the files:
- "fit_firstbaby_threewealth_real.R", which contains the code for all the statistical analyses
- "firstbaby_threewealth.stan", which contains the code for the Stan model.
