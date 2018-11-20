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
  mutate(city_state = paste(city, state, sep = ', '),
         victim_race = fct_relevel(ifelse(victim_race == 'White', 'white', 'non-white'), 'white'),
         victim_age = ifelse(victim_age == 'Unknown', NA, as.numeric(victim_age)),
         resolved = as.numeric(disposition == "Closed by arrest")) %>%
  filter(!(city_state %in% c('Tulsa, AL', 'Dallas, TX', 'Phoenix, AZ', 'Kansas City, MO')))
```

    ## Warning in ifelse(victim_age == "Unknown", NA, as.numeric(victim_age)): NAs
    ## introduced by coercion

``` r
fit_logistic = homicide_data_tidy %>%
  glm(resolved ~ victim_age + victim_race + victim_sex, data = ., family = binomial())
  
fit_logistic %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate)) %>%
  select(term, log_OR = estimate, OR, p.value) %>% 
  knitr::kable(digits = 3)
```

| term                  |  log\_OR|     OR|  p.value|
|:----------------------|--------:|------:|--------:|
| (Intercept)           |    0.940|  2.560|    0.000|
| victim\_age           |   -0.001|  0.999|    0.025|
| victim\_racenon-white |   -0.577|  0.561|    0.000|
| victim\_sexMale       |   -0.510|  0.601|    0.000|
| victim\_sexUnknown    |   -0.459|  0.632|    0.000|
