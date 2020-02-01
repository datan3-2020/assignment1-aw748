Statistical assignment 1
================
Amy Wonnacott
29th January 2020

## Open data (10 points)

In this assignment you will work with the individual level data from
wave 8 of the Understanding Society survey. First, you need to open the
data set. Please complete the code
    below.

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.6.2

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.2.1     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## Warning: package 'tidyr' was built under R version 3.6.2

    ## Warning: package 'readr' was built under R version 3.6.2

    ## Warning: package 'purrr' was built under R version 3.6.2

    ## Warning: package 'dplyr' was built under R version 3.6.2

    ## Warning: package 'forcats' was built under R version 3.6.2

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
indresp <- read_tsv("C:/Users/User/Documents/GitHub/dataan3/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

Data saved as indresp- individual level data in the wave 8 set.

## Select variables (10 points)

The data for Wave 8 of the Understanding Society was collected in
2016-18. Among other things, people were asked the following question:
“Should the United Kingdom remain a member of the European Union or
leave the European Union?” In this assignment, we will explore how
answers to this question depend on sex and age.

First, you need to select the variables for the analysis. You want to
keep the following variables: cross-wave individual identifier (*pidp*),
support for the UK remaining or leaving the EU (*h\_eumem*), sex
(*h\_sex\_dv*), age (*h\_age\_dv*), and sample origin (*h\_memorig*).

Complete the code below to select those variables from the data frame
and save the result.

``` r
brexit <- indresp %>%
        select(pidp, h_eumem, h_sex_dv, h_age_dv, h_memorig)
```

## Filter observations (10 points)

To make nationally representative estimates from the Understanding
Society data we would need to use weight coefficients. There are many
different types of weight coefficients that can be used depending on the
question and the level of analysis (see the User Guide, pp. 65-71). We
will not do this in this assignment. However, what we want to do is to
keep data from the original Understanding Society sample only (ukhls gb
2009-10), dropping data for Northern Ireland, the BHPS cohort members
and ethnic minority boost samples. This will make data closer to be
representative for Great Britain. You need to choose the observations
where *h\_memorig* has the value of 1.

``` r
gbbrexit <- brexit %>%
        filter(h_memorig=="1")
```

## Recode data (20 points)

Let us tabulate the variables for EU support, sex, and age.

``` r
table(gbbrexit$h_eumem)
```

    ## 
    ##    -9    -8    -7    -2    -1     1     2 
    ##    33   482   879   354   753 11118  9338

``` r
table(gbbrexit$h_sex_dv)
```

    ## 
    ##     0     1     2 
    ##     1 10470 12486

``` r
table(gbbrexit$h_age_dv)
```

    ## 
    ##  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35 
    ## 284 309 290 291 278 295 268 326 287 257 243 234 229 249 274 278 278 293 314 332 
    ##  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55 
    ## 351 332 321 336 320 327 368 404 372 386 435 465 425 447 406 420 427 414 432 422 
    ##  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75 
    ## 408 413 416 434 369 398 358 399 354 412 345 358 412 434 431 334 326 293 275 251 
    ##  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95 
    ## 219 231 211 205 181 162 138 117 117 108  89  78  77  48  41  27  15  18  15   7 
    ##  96  97  98  99 101 102 
    ##   6   2   3   1   1   1

You will see that all these variables are numeric. You can learn what
the numeric codes mean by checking the codebook here:
<https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/datafile/indresp/wave/8>
.

We want to do the following:

1)  Recode the variable for EU support as binary (1 for Remain, 0 for
    Leave), coding all types of missing values (including refusals and
    “don’t know”) as NA.
2)  Recode sex into a character vector with the values “male” or
    “female”.
3)  Recode age into a variable with the following categories: 16 to 25,
    26 to 40, 41 to 55, 56 to 70, over 70.

In all cases, we want to create new variables:

``` r
gbbrexit <- gbbrexit %>%
        mutate(EU = ifelse(h_eumem == 2, "0",
                           ifelse(h_eumem==1, "1", NA))

        ) %>%
        mutate(sex = ifelse(h_sex_dv== 1, "male",
                            ifelse(h_sex_dv==2, "female", NA))
        )%>%
        mutate(age = case_when(
          between(h_age_dv, 16, 25)~ "16-25",
          between(h_age_dv, 26, 40)~ "26-40",
          between(h_age_dv, 41, 55)~ "41-55",
          between(h_age_dv, 56, 70)~ "56-70",
          h_age_dv >= 70 ~ "over 70"
        )
        )
```

## Summarise data (20 points)

Let us **dplyr** to calculate how many people in the sample supported
Remain and Leave, both as absolute numbers and percentages.

``` r
gbbrexit %>%
  count(EU) %>%
  mutate(perc = n / sum(n) *100)
```

    ## # A tibble: 3 x 3
    ##   EU        n  perc
    ##   <chr> <int> <dbl>
    ## 1 0      9338  40.7
    ## 2 1     11118  48.4
    ## 3 <NA>   2501  10.9

(0 is leave, 1 is remain)

Write a couple of sentences with the interpretation of this result. How
this compares with the result of the 2016 referendum? Why?

From these results, we can see that a much larger percentage of people
supported Remain than leave, which is the opposite to the June 2016
referendum. However there is not a large difference between these Remain
support figures (48.53%) and those of the referendum (48.11%). The most
interesting difference is the fact that the leave supporters
dramatically increased: from 40.68% supporting here, the 2016 referendum
figures showed 51.89% people supporting leave.

## Summarise data by sex and age (30 points)

Now let us look at the support for Leave and Remain by sex and age. Use
your newly created variables.

``` r
gbbrexit%>%
  mutate(leavesex= ifelse(EU == 1,0,1 )) %>%
  group_by(sex) %>%
  summarise(
    propLeaveSex = mean(leavesex, na.rm = TRUE) *100
  )
```

    ## # A tibble: 3 x 2
    ##   sex    propLeaveSex
    ##   <chr>         <dbl>
    ## 1 female         43.3
    ## 2 male           48.6
    ## 3 <NA>            0

``` r
gbbrexit%>%
  mutate(leaveage= ifelse(EU == 1,0,1)) %>%
  group_by(age) %>%
  summarise(
    propLeaveAge = mean(leaveage, na.rm= TRUE) *100
  )
```

    ## # A tibble: 5 x 2
    ##   age     propLeaveAge
    ##   <chr>          <dbl>
    ## 1 16-25           30.4
    ## 2 26-40           38.2
    ## 3 41-55           45.4
    ## 4 56-70           50.8
    ## 5 over 70         58.9

``` r
gbbrexit%>%
  mutate(remainsex= ifelse(EU == 0,0,1)) %>%
  group_by(sex) %>%
  summarise(
    propRemainSex = mean(remainsex, na.rm= TRUE) *100
  )
```

    ## # A tibble: 3 x 2
    ##   sex    propRemainSex
    ##   <chr>          <dbl>
    ## 1 female          56.7
    ## 2 male            51.4
    ## 3 <NA>           100

``` r
gbbrexit%>%
  mutate(remainage = ifelse(EU==0,0,1)) %>%
  group_by(age) %>%
  summarise(
    propRemainAge= mean(remainage, na.rm=TRUE) *100
  )
```

    ## # A tibble: 5 x 2
    ##   age     propRemainAge
    ##   <chr>           <dbl>
    ## 1 16-25            69.6
    ## 2 26-40            61.8
    ## 3 41-55            54.6
    ## 4 56-70            49.2
    ## 5 over 70          41.1

propLeaveSex: this shows that a higher proportion of males supported
leave than females propLeaveAge: as age increases, so does the
proportion of leave supporters. The lowest proportion of leave
supporters is in the 16-25 age bracket (30.37), and the highest
proportion is the over 70s (58.91). Positive correlation. propRemainSex:
This shows the opposite to leave sex, with a higher proportion of female
supporting remain than males. propRemainAge: higher proportion of young
people supporting remain than the old people. Negative correlation.
