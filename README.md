# Influence of material wealth on age at first reproduction

Here, you can find all the files related to the data wrangling and statistical analyses that aim to understand the relationship between material wealth with the probability that women have their first child, using data from Pimbwe women from Tanzania.

At the beginning you can find two folders. One contains the data used for the analyses ("Data"), while the other one contains the code for the statistical analyses ("Model_code").

In the folder "Data" you can find the file:
- "dataf.csv" which contains the data necessary for the analysis in a csv file. The data consists of:
    - ID, which is the number to identify a woman in the sample.
    - afr, which is the age at which a woman had her first birth.
    - dob, which is the year of birth.
    - aoc, which is the age of censoring.
    - absw95, absw98, absw00, absw02, absw04,absw06, absw10 are the amount of wealth a woman had for each year of census.
    - age_absw95, age_absw98, age_absw00, age_absw02, age_absw04, age_absw06, age_absw10, are the ages of each woman at each census.

In the folder "Model_code" you can find the files:
- "fit_firstbaby_threewealth_real.R", which contains the code for all the statistical analyses of the results in the manuscript.
- "firstbaby_threewealth.stan", which contains the code for the Stan model for the results in the manuscript.

In the folder "Offset" you can find the files:
- "fit_firstbaby_offset.R", which contains the code for all the statistical analyses with the wealth predictors one year before age at first birth.
- "firstbaby_offset.stan", which contains the code for the Stan model with the wealth predictors one year before age at first birth.

In the folder "Interaction" you can find the files:
- "fit_firstbaby_interaction_abs_diff_real.R", which contains the code for all the statistical analyses with interaction between absolute wealth and short-term wealth variability.
- "firstbaby_interaction_abs_diff.stan", which contains the code for the Stan model with interaction between absolute wealth and short-term wealth variability.
- "fit_firstbaby_interaction_abs_msd_real.R", which contains the code for all the statistical analyses with interaction between absolute wealth and long-term wealth variability.
- "firstbaby_interaction_abs_msd.stan", which contains the code for the Stan model with interaction between absolute wealth and long-term wealth variability.
