Mobile Games A/B Testing with Cookie Cats using R
================

This is the R version of the project [Mobile Games A/B Testing with
Cookie Cats](https://www.datacamp.com/projects/184). The project is
originally provided in an interactive jupyter notebook, I just wanted to
check how fast I can replicate it in R. I used RStudio to create this
.md file from RMarkdown.

``` r
library(tidyverse)
library(tidyquant)

# Reading in the data
df <- read_csv("cookie_cats.csv")

# glimpse df
glimpse(df)
```

    ## Observations: 90,189
    ## Variables: 5
    ## $ userid         <dbl> 116, 337, 377, 483, 488, 540, 1066, 1444, 1574,...
    ## $ version        <chr> "gate_30", "gate_30", "gate_40", "gate_40", "ga...
    ## $ sum_gamerounds <dbl> 3, 38, 165, 1, 179, 187, 0, 2, 108, 153, 3, 0, ...
    ## $ retention_1    <lgl> FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FA...
    ## $ retention_7    <lgl> FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, ...

``` r
# Counting the number of players in each AB group.
df %>%
    select(version) %>%
    group_by(version) %>%
    tally()
```

    ## # A tibble: 2 x 2
    ##   version     n
    ##   <chr>   <int>
    ## 1 gate_30 44700
    ## 2 gate_40 45489

``` r
# Counting the number of players for each number of gamerounds 
plot_df <- df %>%
    group_by(sum_gamerounds) %>%
    summarise(userid_cnt = n())

glimpse(plot_df)
```

    ## Observations: 942
    ## Variables: 2
    ## $ sum_gamerounds <dbl> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1...
    ## $ userid_cnt     <int> 3994, 5538, 4606, 3958, 3629, 2992, 2861, 2379,...

``` r
# Plotting the distribution of players that played 0 to 100 game rounds
plot_df %>%
    filter(sum_gamerounds <= 100) %>%
    ggplot(aes(x = sum_gamerounds, y = userid_cnt)) +
        geom_line() +
        xlab(label = "Rounds(1 to 100)") +
        ylab(label = "User count") +
        theme_tq()
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
# The % of users that came back the day after they installed
df %>%
    select(retention_1) %>%
    summarise(return_pct = sum(retention_1)/n())
```

    ## # A tibble: 1 x 1
    ##   return_pct
    ##        <dbl>
    ## 1      0.445

``` r
# Calculating 1-day retention for each AB-group
df %>%
    select(version, retention_1) %>%
    group_by(version) %>%
    summarise(return_pct = sum(retention_1)/n())
```

    ## # A tibble: 2 x 2
    ##   version return_pct
    ##   <chr>        <dbl>
    ## 1 gate_30      0.448
    ## 2 gate_40      0.442

``` r
# Creating a dataframe with bootstrapped means of each AB-group
emp_mtrx <- matrix(nrow = 0, ncol = 2)

colnames(emp_mtrx) <- c("gate_30", "gate_40")

boot_1d <- as_tibble(emp_mtrx)

for(i in 1:10000){
    boot_1d[i, ]  <- df[sample(1:nrow(df), nrow(df), replace = TRUE), ] %>% group_by(version) %>% summarise(mean_gate = mean(retention_1)) %>% ungroup() %>% spread(version, mean_gate)
}

# A Kernel Density Estimate plot of the bootstrap distributions
boot_1d %>%
    gather(key = "version", value = "mean", ... = c(gate_30, gate_40)) %>%
    ggplot(aes(x = mean, color = version, fill = version, alpha = 0.4)) +
        geom_density() +
        theme_tq()
```

![](README_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
# Adding a column with the % difference between the two AB-groups
boot_1d <- boot_1d %>%
    mutate(diff = ((gate_30 - gate_40)/gate_40)*100)

# Ploting the bootstrap % difference
boot_1d %>%
    ggplot(aes(x = diff, color = 'red', fill = 'red', alpha = 0.2)) +
        geom_density() +
        xlab(label = "% diff") +
        theme_tq()
```

![](README_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
# Calculating the probability that 1-day retention is greater when the gate is at level 30
boot_1d %>%
    mutate(gate_30_hgr = ifelse(diff > 0, 1, 0)) %>%
    summarise_all(mean) %>%
    select(gate_30_hgr)
```

    ## # A tibble: 1 x 1
    ##   gate_30_hgr
    ##         <dbl>
    ## 1       0.960

``` r
# Calculating 7-day retention for both AB-groups
df %>%
    select(version, retention_7) %>%
    group_by(version) %>%
    summarise(return_pct = sum(retention_7)/n())
```

    ## # A tibble: 2 x 2
    ##   version return_pct
    ##   <chr>        <dbl>
    ## 1 gate_30      0.190
    ## 2 gate_40      0.182

``` r
# Creating a dataframe with bootstrapped means of each AB-group
emp_mtrx <- matrix(nrow = 0, ncol = 2)

colnames(emp_mtrx) <- c("gate_30", "gate_40")

boot_7d <- as_tibble(emp_mtrx)

for(i in 1:10000){
    boot_7d[i, ]  <- df[sample(1:nrow(df), nrow(df), replace = TRUE), ] %>% group_by(version) %>% summarise(mean_gate = mean(retention_7)) %>% ungroup() %>% spread(version, mean_gate)
}

# A Kernel Density Estimate plot of the bootstrap distributions
boot_7d %>%
    gather(key = "version", value = "mean", ... = c(gate_30, gate_40)) %>%
    ggplot(aes(x = mean, color = version, fill = version, alpha = 0.4)) +
    geom_density() +
    theme_tq()
```

![](README_files/figure-gfm/unnamed-chunk-1-4.png)<!-- -->

``` r
# Adding a column with the % difference between the two AB-groups
boot_7d <- boot_7d %>%
    mutate(diff = ((gate_30 - gate_40)/gate_40)*100)

# Ploting the bootstrap % difference
boot_7d %>%
    ggplot(aes(x = diff, color = 'red', fill = 'red', alpha = 0.2)) +
    geom_density() +
    xlab(label = "% diff") +
    theme_tq()
```

![](README_files/figure-gfm/unnamed-chunk-1-5.png)<!-- -->

``` r
# Calculating the probability that 7-day retention is greater when the gate is at level 30
boot_1d %>%
    mutate(gate_30_hgr = ifelse(diff > 0, 1, 0)) %>%
    summarise_all(mean) %>%
    select(gate_30_hgr)
```

    ## # A tibble: 1 x 1
    ##   gate_30_hgr
    ##         <dbl>
    ## 1       0.960
