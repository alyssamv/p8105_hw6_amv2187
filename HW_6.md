Homework 6
================
Alyssa Vanderbeek
27 November 2018

### Problem 1

``` r
homicide_data = read_csv('./data/problem1/homicide-data.csv') # read in data
```

    ## Parsed with column specification:
    ## cols(
    ##   uid = col_character(),
    ##   reported_date = col_integer(),
    ##   victim_last = col_character(),
    ##   victim_first = col_character(),
    ##   victim_race = col_character(),
    ##   victim_age = col_character(),
    ##   victim_sex = col_character(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   lat = col_double(),
    ##   lon = col_double(),
    ##   disposition = col_character()
    ## )

``` r
homicide_data_tidy = homicide_data %>%
  mutate(city_state = paste(city, toupper(state), sep = ', '), # create city_state variable
         victim_race = fct_relevel(ifelse(victim_race == 'White', 'white', 'non-white'), 'white'), # dichotomize race as factor (white vs. non-white)
         victim_age = ifelse(victim_age == 'Unknown', NA, as.numeric(victim_age)), # make age numeric
         resolved = as.numeric(disposition == "Closed by arrest")) %>% # create binary variable for whether murder is resolved
  filter(!(city_state %in% c('Tulsa, AL', 'Dallas, TX', 'Phoenix, AZ', 'Kansas City, MO'))) # filter cities
```

    ## Warning in ifelse(victim_age == "Unknown", NA, as.numeric(victim_age)): NAs
    ## introduced by coercion

``` r
str(homicide_data_tidy)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    48507 obs. of  14 variables:
    ##  $ uid          : chr  "Alb-000001" "Alb-000002" "Alb-000003" "Alb-000004" ...
    ##  $ reported_date: int  20100504 20100216 20100601 20100101 20100102 20100126 20100127 20100127 20100130 20100210 ...
    ##  $ victim_last  : chr  "GARCIA" "MONTOYA" "SATTERFIELD" "MENDIOLA" ...
    ##  $ victim_first : chr  "JUAN" "CAMERON" "VIVIANA" "CARLOS" ...
    ##  $ victim_race  : Factor w/ 2 levels "white","non-white": 2 2 1 2 1 1 2 2 1 2 ...
    ##  $ victim_age   : num  78 17 15 32 72 91 52 52 56 43 ...
    ##  $ victim_sex   : chr  "Male" "Male" "Female" "Male" ...
    ##  $ city         : chr  "Albuquerque" "Albuquerque" "Albuquerque" "Albuquerque" ...
    ##  $ state        : chr  "NM" "NM" "NM" "NM" ...
    ##  $ lat          : num  35.1 35.1 35.1 35.1 35.1 ...
    ##  $ lon          : num  -107 -107 -107 -107 -107 ...
    ##  $ disposition  : chr  "Closed without arrest" "Closed by arrest" "Closed without arrest" "Closed by arrest" ...
    ##  $ city_state   : chr  "Albuquerque, NM" "Albuquerque, NM" "Albuquerque, NM" "Albuquerque, NM" ...
    ##  $ resolved     : num  0 1 0 1 0 0 1 1 0 0 ...

``` r
# Logistic regression for all cities
fit_logistic = homicide_data_tidy %>%
  glm(resolved ~ victim_age + victim_race + victim_sex, data = ., family = binomial())

summary(fit_logistic)
```

    ## 
    ## Call:
    ## glm(formula = resolved ~ victim_age + victim_race + victim_sex, 
    ##     family = binomial(), data = .)
    ## 
    ## Deviance Residuals: 
    ##    Min      1Q  Median      3Q     Max  
    ## -1.594  -1.101  -1.080   1.254   1.306  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.9399017  0.0425156  22.107  < 2e-16 ***
    ## victim_age           -0.0014748  0.0006581  -2.241    0.025 *  
    ## victim_racenon-white -0.5774368  0.0289012 -19.980  < 2e-16 ***
    ## victim_sexMale       -0.5096171  0.0266814 -19.100  < 2e-16 ***
    ## victim_sexUnknown    -0.4593052  0.0994307  -4.619 3.85e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 66507  on 47991  degrees of freedom
    ## Residual deviance: 65592  on 47987  degrees of freedom
    ##   (515 observations deleted due to missingness)
    ## AIC: 65602
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# Table of estimated OR and 95% CI for race, holding all else constant
fit_logistic %>% 
  broom::tidy() %>% # tidy output
  filter(term == 'victim_racenon-white') %>% # get coefficient for race only
  summarise(OR = exp(estimate),
            '95% CI lower bound' = exp(estimate - qnorm(0.975)*std.error),
            '95% CI upper bound' = exp(estimate + qnorm(0.975)*std.error)) %>% # calculate 95% CI for OR
  knitr::kable(digits = 3)
```

|     OR|  95% CI lower bound|  95% CI upper bound|
|------:|-------------------:|-------------------:|
|  0.561|                0.53|               0.594|

Across all cities in the dataset, the odds of the murder of a non-white person being resolved is 44% less than if the victim were white (all else constant).

``` r
# Logistic regression for each city
log_reg_cities = homicide_data_tidy %>%
  select(city_state, resolved, victim_age, victim_race, victim_sex) %>% # select only variables to use in logistic regression
  nest(-city_state) %>% # nest all variables to each city
  mutate(logreg = map(data, ~ broom::tidy(glm(resolved ~ victim_age + victim_race + victim_sex, data = ., family = binomial())))) %>% # map logistic regression with nested data, make output a nested tibble
  select(city_state, logreg) %>% # select only city and model
  unnest %>% 
  filter(term == 'victim_racenon-white') %>% # get coefficients for race only
  rowwise %>%
  summarise(city = city_state,
            OR = exp(estimate),
            ci_lower = exp(estimate - qnorm(0.975)*std.error),
            ci_upper = exp(estimate + qnorm(0.975)*std.error)) # OR and 95% CI for each city

# print table
log_reg_cities %>%
  knitr::kable(digits = 3,
               col.names = c('City', 'OR', '95% CI lower bound', '95% CI upper bound'),
               caption = 'Odds of resolved cases for non-white vs. white victims') # write an actual caption
```

| City               |     OR|  95% CI lower bound|  95% CI upper bound|
|:-------------------|------:|-------------------:|-------------------:|
| Albuquerque, NM    |  0.741|               0.451|               1.218|
| Atlanta, GA        |  0.753|               0.432|               1.313|
| Baltimore, MD      |  0.441|               0.313|               0.620|
| Baton Rouge, LA    |  0.668|               0.313|               1.425|
| Birmingham, AL     |  1.039|               0.615|               1.756|
| Boston, MA         |  0.115|               0.047|               0.278|
| Buffalo, NY        |  0.390|               0.213|               0.714|
| Charlotte, NC      |  0.558|               0.321|               0.969|
| Chicago, IL        |  0.562|               0.431|               0.733|
| Cincinnati, OH     |  0.318|               0.184|               0.551|
| Columbus, OH       |  0.855|               0.634|               1.152|
| Denver, CO         |  0.602|               0.359|               1.009|
| Detroit, MI        |  0.651|               0.488|               0.869|
| Durham, NC         |  1.003|               0.404|               2.489|
| Fort Worth, TX     |  0.838|               0.555|               1.266|
| Fresno, CA         |  0.448|               0.231|               0.870|
| Houston, TX        |  0.873|               0.699|               1.090|
| Indianapolis, IN   |  0.505|               0.382|               0.667|
| Jacksonville, FL   |  0.658|               0.502|               0.862|
| Las Vegas, NV      |  0.755|               0.586|               0.973|
| Long Beach, CA     |  0.794|               0.388|               1.626|
| Los Angeles, CA    |  0.666|               0.483|               0.918|
| Louisville, KY     |  0.392|               0.259|               0.593|
| Memphis, TN        |  0.782|               0.524|               1.168|
| Miami, FL          |  0.576|               0.377|               0.880|
| Milwaukee, WI      |  0.632|               0.403|               0.991|
| Minneapolis, MN    |  0.646|               0.345|               1.209|
| Nashville, TN      |  0.899|               0.653|               1.236|
| New Orleans, LA    |  0.466|               0.295|               0.737|
| New York, NY       |  0.531|               0.279|               1.011|
| Oakland, CA        |  0.213|               0.104|               0.435|
| Oklahoma City, OK  |  0.681|               0.478|               0.971|
| Omaha, NE          |  0.169|               0.094|               0.305|
| Philadelphia, PA   |  0.644|               0.486|               0.852|
| Pittsburgh, PA     |  0.282|               0.161|               0.493|
| Richmond, VA       |  0.447|               0.162|               1.238|
| San Antonio, TX    |  0.689|               0.461|               1.030|
| Sacramento, CA     |  0.781|               0.449|               1.359|
| Savannah, GA       |  0.596|               0.280|               1.270|
| San Bernardino, CA |  0.880|               0.393|               1.972|
| San Diego, CA      |  0.483|               0.298|               0.785|
| San Francisco, CA  |  0.458|               0.290|               0.723|
| St. Louis, MO      |  0.577|               0.406|               0.820|
| Stockton, CA       |  0.376|               0.196|               0.719|
| Tampa, FL          |  1.159|               0.587|               2.288|
| Tulsa, OK          |  0.602|               0.413|               0.879|
| Washington, DC     |  0.510|               0.258|               1.010|

``` r
# plot of OR estimate and CI for each city
log_reg_cities %>%
  mutate(city = fct_reorder(city, OR, desc = T)) %>% # order cities according to OR estimate
  ggplot(aes(x = city, y = OR)) + 
  geom_point() +
  geom_errorbar(aes(x = city, ymin = ci_lower, ymax = ci_upper), width = 0.5) + # error bars for CI bounds
  geom_hline(yintercept = 1, lty = 3) + # dotted line at OR=1 to represent no difference in odds of resolution across races
  labs(y = 'Odds of resolved cases (non-white vs. white)',
       x = 'City') +
  coord_flip()
```

![](HW_6_files/figure-markdown_github/unnamed-chunk-3-1.png)

The figure above shows the odds ratios for case resolution of white vs. non-white victims (all else constant). All cities on display except for two - Tampa, FL and Birmingham, AL - exhibit a lower odds of resolving murders of non-white victims, compared to white victims. However, estimates of several more cities do not exhibit a statistically significant difference in murder resolution between races (95% CI includes OR of 1); take, for example, Richmond, VA, where the OR is 0.447, but the CI extends from 0.162 to 1.238, suggesting that we cannot exclude the possibility that the true odds of resolution is equal (OR=1). Conversely, the odds of resolution across races is about equal in Durham, NC, but the confidence interval around the estimate is wide.

### Problem 2

``` r
birthweight = read_csv('./data/problem2/birthweight.csv')
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   gaweeks = col_double(),
    ##   ppbmi = col_double(),
    ##   smoken = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
birthweight_tidy = birthweight %>%
  mutate(babysex = fct_recode(as.character(babysex), 'Male' = '1', 'Female' = '2'),
         mrace = fct_recode(as.character(mrace), 'White' = '1', 'Black' = '2', 'Asian' = '3', 'Puerto Rican' = '4'),
         frace = fct_recode(as.character(frace), 'White' = '1', 'Black' = '2', 'Asian' = '3', 'Puerto Rican' = '4', 'Other' = '8'))

str(birthweight)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    4342 obs. of  20 variables:
    ##  $ babysex : int  2 1 2 1 2 1 2 2 1 1 ...
    ##  $ bhead   : int  34 34 36 34 34 33 33 33 36 33 ...
    ##  $ blength : int  51 48 50 52 52 52 46 49 52 50 ...
    ##  $ bwt     : int  3629 3062 3345 3062 3374 3374 2523 2778 3515 3459 ...
    ##  $ delwt   : int  177 156 148 157 156 129 126 140 146 169 ...
    ##  $ fincome : int  35 65 85 55 5 55 96 5 85 75 ...
    ##  $ frace   : int  1 2 1 1 1 1 2 1 1 2 ...
    ##  $ gaweeks : num  39.9 25.9 39.9 40 41.6 ...
    ##  $ malform : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ menarche: int  13 14 12 14 13 12 14 12 11 12 ...
    ##  $ mheight : int  63 65 64 64 66 66 72 62 61 64 ...
    ##  $ momage  : int  36 25 29 18 20 23 29 19 13 19 ...
    ##  $ mrace   : int  1 2 1 1 1 1 2 1 1 2 ...
    ##  $ parity  : int  3 0 0 0 0 0 0 0 0 0 ...
    ##  $ pnumlbw : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ pnumsga : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ppbmi   : num  26.3 21.3 23.6 21.8 21 ...
    ##  $ ppwt    : int  148 128 137 127 130 115 105 119 105 145 ...
    ##  $ smoken  : num  0 0 1 10 1 0 0 0 0 4 ...
    ##  $ wtgain  : int  29 28 11 30 26 14 21 21 41 24 ...
    ##  - attr(*, "spec")=List of 2
    ##   ..$ cols   :List of 20
    ##   .. ..$ babysex : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ bhead   : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ blength : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ bwt     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ delwt   : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ fincome : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ frace   : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ gaweeks : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
    ##   .. ..$ malform : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ menarche: list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ mheight : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ momage  : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ mrace   : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ parity  : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ pnumlbw : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ pnumsga : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ ppbmi   : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
    ##   .. ..$ ppwt    : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ smoken  : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
    ##   .. ..$ wtgain  : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   ..$ default: list()
    ##   .. ..- attr(*, "class")= chr  "collector_guess" "collector"
    ##   ..- attr(*, "class")= chr "col_spec"

``` r
summary(lm(bwt ~ babysex + bhead + blength + delwt + fincome + frace + malform + menarche + mheight + momage + mrace + parity + pnumlbw + pnumsga + ppbmi + ppwt + smoken + wtgain, data = birthweight_tidy))
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength + delwt + fincome + 
    ##     frace + malform + menarche + mheight + momage + mrace + parity + 
    ##     pnumlbw + pnumsga + ppbmi + ppwt + smoken + wtgain, data = birthweight_tidy)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1099.53  -181.57    -3.71   174.63  2405.55 
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -6076.3566   664.6165  -9.143  < 2e-16 ***
    ## babysexFemale        35.3765     8.4822   4.171 3.10e-05 ***
    ## bhead               136.3203     3.4038  40.050  < 2e-16 ***
    ## blength              77.4232     2.0113  38.494  < 2e-16 ***
    ## delwt                 4.3518     0.3963  10.981  < 2e-16 ***
    ## fincome               0.3172     0.1808   1.755 0.079375 .  
    ## fraceBlack           13.3009    46.4752   0.286 0.774744    
    ## fraceAsian           19.7233    69.7841   0.283 0.777471    
    ## fracePuerto Rican   -50.7499    44.9905  -1.128 0.259377    
    ## fraceOther            6.7109    74.5955   0.090 0.928320    
    ## malform               3.9295    71.1197   0.055 0.955940    
    ## menarche             -3.5514     2.9155  -1.218 0.223246    
    ## mheight               8.8152    10.3835   0.849 0.395950    
    ## momage                1.2954     1.2288   1.054 0.291859    
    ## mraceBlack         -155.2296    46.3673  -3.348 0.000821 ***
    ## mraceAsian          -90.8324    72.4258  -1.254 0.209857    
    ## mracePuerto Rican   -59.8567    45.4529  -1.317 0.187944    
    ## parity               69.4043    40.6275   1.708 0.087651 .  
    ## pnumlbw                   NA         NA      NA       NA    
    ## pnumsga                   NA         NA      NA       NA    
    ## ppbmi                 3.4160    14.9958   0.228 0.819814    
    ## ppwt                 -3.5621     2.6305  -1.354 0.175761    
    ## smoken               -4.6563     0.5907  -7.883 4.01e-15 ***
    ## wtgain                    NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 274.4 on 4321 degrees of freedom
    ## Multiple R-squared:  0.7143, Adjusted R-squared:  0.713 
    ## F-statistic: 540.1 on 20 and 4321 DF,  p-value: < 2.2e-16

``` r
summary(lm(bwt ~ babysex + bhead + blength + delwt + fincome + frace + malform + menarche + mheight + momage + mrace + parity + ppbmi + ppwt + smoken, data = birthweight_tidy)) # remove pnumlbw, pnumsga, wtgain for NA
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength + delwt + fincome + 
    ##     frace + malform + menarche + mheight + momage + mrace + parity + 
    ##     ppbmi + ppwt + smoken, data = birthweight_tidy)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1099.53  -181.57    -3.71   174.63  2405.55 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -6076.3566   664.6165  -9.143  < 2e-16 ***
    ## babysexFemale        35.3765     8.4822   4.171 3.10e-05 ***
    ## bhead               136.3203     3.4038  40.050  < 2e-16 ***
    ## blength              77.4232     2.0113  38.494  < 2e-16 ***
    ## delwt                 4.3518     0.3963  10.981  < 2e-16 ***
    ## fincome               0.3172     0.1808   1.755 0.079375 .  
    ## fraceBlack           13.3009    46.4752   0.286 0.774744    
    ## fraceAsian           19.7233    69.7841   0.283 0.777471    
    ## fracePuerto Rican   -50.7499    44.9905  -1.128 0.259377    
    ## fraceOther            6.7109    74.5955   0.090 0.928320    
    ## malform               3.9295    71.1197   0.055 0.955940    
    ## menarche             -3.5514     2.9155  -1.218 0.223246    
    ## mheight               8.8152    10.3835   0.849 0.395950    
    ## momage                1.2954     1.2288   1.054 0.291859    
    ## mraceBlack         -155.2296    46.3673  -3.348 0.000821 ***
    ## mraceAsian          -90.8324    72.4258  -1.254 0.209857    
    ## mracePuerto Rican   -59.8567    45.4529  -1.317 0.187944    
    ## parity               69.4043    40.6275   1.708 0.087651 .  
    ## ppbmi                 3.4160    14.9958   0.228 0.819814    
    ## ppwt                 -3.5621     2.6305  -1.354 0.175761    
    ## smoken               -4.6563     0.5907  -7.883 4.01e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 274.4 on 4321 degrees of freedom
    ## Multiple R-squared:  0.7143, Adjusted R-squared:  0.713 
    ## F-statistic: 540.1 on 20 and 4321 DF,  p-value: < 2.2e-16

``` r
summary(lm(bwt ~ babysex + bhead + blength + delwt + fincome + frace + menarche + mheight + momage + mrace + parity + ppbmi + ppwt + smoken, data = birthweight_tidy)) # remove malform
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength + delwt + fincome + 
    ##     frace + menarche + mheight + momage + mrace + parity + ppbmi + 
    ##     ppwt + smoken, data = birthweight_tidy)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1099.52  -181.59    -3.72   174.63  2405.46 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -6075.9476   664.4986  -9.144  < 2e-16 ***
    ## babysexFemale        35.3690     8.4801   4.171 3.09e-05 ***
    ## bhead               136.3214     3.4033  40.055  < 2e-16 ***
    ## blength              77.4212     2.0108  38.503  < 2e-16 ***
    ## delwt                 4.3525     0.3960  10.990  < 2e-16 ***
    ## fincome               0.3170     0.1807   1.754  0.07948 .  
    ## fraceBlack           13.2886    46.4693   0.286  0.77492    
    ## fraceAsian           19.7061    69.7753   0.282  0.77763    
    ## fracePuerto Rican   -50.7615    44.9848  -1.128  0.25921    
    ## fraceOther            6.6856    74.5855   0.090  0.92858    
    ## menarche             -3.5532     2.9150  -1.219  0.22292    
    ## mheight               8.8099    10.3819   0.849  0.39616    
    ## momage                1.2968     1.2285   1.056  0.29119    
    ## mraceBlack         -155.2291    46.3620  -3.348  0.00082 ***
    ## mraceAsian          -90.8415    72.4172  -1.254  0.20976    
    ## mracePuerto Rican   -59.8683    45.4472  -1.317  0.18780    
    ## parity               69.3964    40.6226   1.708  0.08765 .  
    ## ppbmi                 3.4099    14.9937   0.227  0.82010    
    ## ppwt                 -3.5617     2.6302  -1.354  0.17575    
    ## smoken               -4.6555     0.5904  -7.885 3.95e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 274.4 on 4322 degrees of freedom
    ## Multiple R-squared:  0.7143, Adjusted R-squared:  0.713 
    ## F-statistic: 568.7 on 19 and 4322 DF,  p-value: < 2.2e-16

``` r
summary(lm(bwt ~ babysex + bhead + blength + delwt + fincome + menarche + mheight + momage + mrace + parity + ppwt + smoken, data = birthweight_tidy)) # remove frace
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength + delwt + fincome + 
    ##     menarche + mheight + momage + mrace + parity + ppwt + smoken, 
    ##     data = birthweight_tidy)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1097.67  -181.50    -3.58   175.77  2404.34 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -5929.4024   139.9578 -42.366  < 2e-16 ***
    ## babysexFemale        35.3339     8.4761   4.169 3.12e-05 ***
    ## bhead               136.3788     3.3988  40.125  < 2e-16 ***
    ## blength              77.3824     2.0095  38.509  < 2e-16 ***
    ## delwt                 4.3503     0.3957  10.994  < 2e-16 ***
    ## fincome               0.3183     0.1803   1.765 0.077583 .  
    ## menarche             -3.6408     2.9108  -1.251 0.211081    
    ## mheight               6.5303     1.8112   3.606 0.000315 ***
    ## momage                1.2977     1.2267   1.058 0.290178    
    ## mraceBlack         -141.9771    10.2642 -13.832  < 2e-16 ***
    ## mraceAsian          -75.1569    43.0182  -1.747 0.080692 .  
    ## mracePuerto Rican  -106.1816    19.4828  -5.450 5.32e-08 ***
    ## parity               69.3553    40.6034   1.708 0.087687 .  
    ## ppwt                 -2.9724     0.4344  -6.842 8.88e-12 ***
    ## smoken               -4.6275     0.5893  -7.853 5.10e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 274.3 on 4327 degrees of freedom
    ## Multiple R-squared:  0.7142, Adjusted R-squared:  0.7132 
    ## F-statistic: 772.2 on 14 and 4327 DF,  p-value: < 2.2e-16

``` r
summary(lm(bwt ~ babysex + bhead + blength + delwt + fincome + mheight + momage + mrace + parity + ppwt + smoken, data = birthweight_tidy)) # remove menarche
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength + delwt + fincome + 
    ##     mheight + momage + mrace + parity + ppwt + smoken, data = birthweight_tidy)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1099.0  -182.5    -4.3   176.7  2415.1 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -5957.0787   138.2066 -43.103  < 2e-16 ***
    ## babysexFemale        35.3594     8.4766   4.171 3.09e-05 ***
    ## bhead               136.3075     3.3986  40.107  < 2e-16 ***
    ## blength              77.4334     2.0092  38.539  < 2e-16 ***
    ## delwt                 4.3715     0.3954  11.057  < 2e-16 ***
    ## fincome               0.3308     0.1800   1.838 0.066173 .  
    ## mheight               6.2509     1.7975   3.478 0.000511 ***
    ## momage                1.0078     1.2047   0.837 0.402882    
    ## mraceBlack         -142.6081    10.2524 -13.910  < 2e-16 ***
    ## mraceAsian          -77.6130    42.9762  -1.806 0.070995 .  
    ## mracePuerto Rican  -107.0132    19.4727  -5.496 4.12e-08 ***
    ## parity               68.9864    40.6050   1.699 0.089398 .  
    ## ppwt                 -2.9532     0.4342  -6.802 1.17e-11 ***
    ## smoken               -4.6422     0.5892  -7.879 4.16e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 274.3 on 4328 degrees of freedom
    ## Multiple R-squared:  0.7141, Adjusted R-squared:  0.7132 
    ## F-statistic: 831.4 on 13 and 4328 DF,  p-value: < 2.2e-16

``` r
summary(lm(bwt ~ babysex + bhead + blength + delwt + fincome + mheight + momage + mrace + ppwt + smoken, data = birthweight_tidy)) # remove parity
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength + delwt + fincome + 
    ##     mheight + momage + mrace + ppwt + smoken, data = birthweight_tidy)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1097.84  -182.51    -4.61   176.68  2412.25 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -5953.7416   138.2227 -43.074  < 2e-16 ***
    ## babysexFemale        35.6743     8.4764   4.209 2.62e-05 ***
    ## bhead               136.3183     3.3993  40.102  < 2e-16 ***
    ## blength              77.3439     2.0089  38.500  < 2e-16 ***
    ## delwt                 4.3869     0.3954  11.096  < 2e-16 ***
    ## fincome               0.3193     0.1799   1.775 0.075999 .  
    ## mheight               6.2259     1.7978   3.463 0.000539 ***
    ## momage                1.1538     1.2019   0.960 0.337113    
    ## mraceBlack         -142.4236    10.2541 -13.889  < 2e-16 ***
    ## mraceAsian          -78.5154    42.9823  -1.827 0.067815 .  
    ## mracePuerto Rican  -107.2586    19.4765  -5.507 3.86e-08 ***
    ## ppwt                 -2.9726     0.4341  -6.848 8.56e-12 ***
    ## smoken               -4.6493     0.5893  -7.889 3.82e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 274.3 on 4329 degrees of freedom
    ## Multiple R-squared:  0.7139, Adjusted R-squared:  0.7131 
    ## F-statistic:   900 on 12 and 4329 DF,  p-value: < 2.2e-16

``` r
summary(lm(bwt ~ babysex + bhead + blength + delwt + mheight + momage + mrace + ppwt + smoken, data = birthweight_tidy)) # remove fincome
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength + delwt + mheight + 
    ##     momage + mrace + ppwt + smoken, data = birthweight_tidy)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1106.25  -181.67    -5.13   177.10  2398.58 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -5962.2284   138.1743 -43.150  < 2e-16 ***
    ## babysexFemale        35.6144     8.4785   4.201 2.72e-05 ***
    ## bhead               136.5623     3.3974  40.196  < 2e-16 ***
    ## blength              77.2240     2.0083  38.452  < 2e-16 ***
    ## delwt                 4.3797     0.3954  11.076  < 2e-16 ***
    ## mheight               6.4658     1.7931   3.606 0.000315 ***
    ## momage                1.6028     1.1752   1.364 0.172680    
    ## mraceBlack         -147.8666     9.7872 -15.108  < 2e-16 ***
    ## mraceAsian          -83.5317    42.8999  -1.947 0.051584 .  
    ## mracePuerto Rican  -111.9845    19.2984  -5.803 6.99e-09 ***
    ## ppwt                 -2.9736     0.4342  -6.848 8.53e-12 ***
    ## smoken               -4.6765     0.5893  -7.936 2.64e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 274.4 on 4330 degrees of freedom
    ## Multiple R-squared:  0.7137, Adjusted R-squared:  0.7129 
    ## F-statistic:   981 on 11 and 4330 DF,  p-value: < 2.2e-16

``` r
summary(lm(bwt ~ babysex + bhead + blength + delwt + mheight + mrace + ppwt + smoken, data = birthweight_tidy)) # remove momage
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength + delwt + mheight + 
    ##     mrace + ppwt + smoken, data = birthweight_tidy)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1108.6  -182.4    -4.8   177.0  2396.3 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -5939.0078   137.1349 -43.308  < 2e-16 ***
    ## babysexFemale        35.3168     8.4765   4.166 3.15e-05 ***
    ## bhead               136.7213     3.3957  40.263  < 2e-16 ***
    ## blength              77.1737     2.0082  38.430  < 2e-16 ***
    ## delwt                 4.3292     0.3937  10.995  < 2e-16 ***
    ## mheight               6.5465     1.7923   3.652 0.000263 ***
    ## mraceBlack         -152.2160     9.2540 -16.449  < 2e-16 ***
    ## mraceAsian          -76.6528    42.6066  -1.799 0.072075 .  
    ## mracePuerto Rican  -114.3346    19.2232  -5.948 2.93e-09 ***
    ## ppwt                 -2.8858     0.4295  -6.720 2.06e-11 ***
    ## smoken               -4.6823     0.5893  -7.945 2.45e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 274.4 on 4331 degrees of freedom
    ## Multiple R-squared:  0.7135, Adjusted R-squared:  0.7129 
    ## F-statistic:  1079 on 10 and 4331 DF,  p-value: < 2.2e-16

``` r
lm(bwt ~ blength + gaweeks, data = birthweight_tidy) # comparison model 1
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ blength + gaweeks, data = birthweight_tidy)
    ## 
    ## Coefficients:
    ## (Intercept)      blength      gaweeks  
    ##    -4347.67       128.56        27.05

``` r
lm(bwt ~ (bhead + blength + babysex)^3, data = birthweight_tidy) # comparison model 2 with all interaction terms 
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ (bhead + blength + babysex)^3, data = birthweight_tidy)
    ## 
    ## Coefficients:
    ##                 (Intercept)                        bhead  
    ##                  -7176.8170                     181.7956  
    ##                     blength                babysexFemale  
    ##                    102.1269                    6374.8684  
    ##               bhead:blength          bhead:babysexFemale  
    ##                     -0.5536                    -198.3932  
    ##       blength:babysexFemale  bhead:blength:babysexFemale  
    ##                   -123.7729                       3.8781
