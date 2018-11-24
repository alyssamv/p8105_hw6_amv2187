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
  mutate(city_state = paste(city, toupper(state), sep = ', '),
         victim_race = fct_relevel(ifelse(victim_race == 'White', 'white', 'non-white'), 'white'),
         victim_age = ifelse(victim_age == 'Unknown', NA, as.numeric(victim_age)),
         resolved = as.numeric(disposition == "Closed by arrest")) %>%
  filter(!(city_state %in% c('Tulsa, AL', 'Dallas, TX', 'Phoenix, AZ', 'Kansas City, MO')))
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
  broom::tidy() %>% 
  filter(term == 'victim_racenon-white') %>%
  summarise(OR = exp(estimate),
            '95% CI lower bound' = exp(estimate - qnorm(0.975)*std.error),
            '95% CI upper bound' = exp(estimate + qnorm(0.975)*std.error)) %>%
  knitr::kable(digits = 3)
```

|     OR|  95% CI lower bound|  95% CI upper bound|
|------:|-------------------:|-------------------:|
|  0.561|                0.53|               0.594|

Across all cities in the dataset, the odds of the murder of a non-white person being resolved is 44% less than if the victim were white (all else constant).

``` r
# Logistic regression for each city
log_reg_cities = homicide_data_tidy %>%
  select(city_state, resolved, victim_age, victim_race, victim_sex) %>%
  nest(-city_state) %>%
  mutate(logreg = map(data, ~ broom::tidy(glm(resolved ~ victim_age + victim_race + victim_sex, data = ., family = binomial())))) %>%
  select(city_state, logreg) %>%
  unnest %>%
  filter(term == 'victim_racenon-white') %>%
  rowwise %>%
  summarise(city = city_state,
            OR = exp(estimate),
            ci_lower = exp(estimate - qnorm(0.975)*std.error),
            ci_upper = exp(estimate + qnorm(0.975)*std.error))

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
log_reg_cities %>%
  mutate(city = fct_reorder(city, OR, desc = T)) %>%
  ggplot(aes(x = city, y = OR)) +
  geom_point() +
  geom_errorbar(aes(x = city, ymin = ci_lower, ymax = ci_upper), width = 0.5) +
  geom_hline(yintercept = 1, lty = 3) +
  labs(y = 'Odds of resolved cases (non-white vs. white)',
       x = 'City') +
  coord_flip()
```

![](HW_6_files/figure-markdown_github/unnamed-chunk-3-1.png)

The figure above shows the odds ratios for case resolution of white vs. non-white victims (all else constant). All cities on display expect for two - Tampa, FL and Birmingham, AL - exhibit a lower odds of resolving murder of non-white victims, compared to white victims. The odds of resolution between races is about equal in Durham, NC, but the confidence interval around the estimate is wide.

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
