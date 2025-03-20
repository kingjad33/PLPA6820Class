---
title: "Jaden_King"
output:
  word_document: default
  html_document: default
---
requires tidyverse package

``` r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
## ✔ purrr     1.0.4     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

read data in

``` r
metadata <- read.csv("metadata.csv")
diversity <- read.csv("DiversityData.csv")
```

connect the data frames by column code

``` r
alpha <- data.frame(left_join(metadata, diversity, by = "Code"))

head(alpha)
```

```
##     Code Crop Time_Point Replicate Water_Imbibed  shannon invsimpson   simpson
## 1 S01_13 Soil          0         1            na 6.624921   210.7279 0.9952545
## 2 S02_16 Soil          0         2            na 6.612413   206.8666 0.9951660
## 3 S03_19 Soil          0         3            na 6.660853   213.0184 0.9953056
## 4 S04_22 Soil          0         4            na 6.660671   204.6908 0.9951146
## 5 S05_25 Soil          0         5            na 6.610965   200.2552 0.9950064
## 6 S06_28 Soil          0         6            na 6.650812   199.3211 0.9949830
##   richness
## 1     3319
## 2     3079
## 3     3935
## 4     3922
## 5     3196
## 6     3481
```

Calculate Pielou’s evenness index: Pielou’s evenness is an ecological parameter calculated by the Shannon diversity index (column Shannon) divided by the log of the richness column. 
-	Using mutate, create a new column to calculate Pielou’s evenness index. 
-	Name the resulting dataframe alpha_even.


``` r
alpha_even <- data.frame(mutate(alpha, Peven = shannon/(log(richness))))

head(alpha_even)
```

```
##     Code Crop Time_Point Replicate Water_Imbibed  shannon invsimpson   simpson
## 1 S01_13 Soil          0         1            na 6.624921   210.7279 0.9952545
## 2 S02_16 Soil          0         2            na 6.612413   206.8666 0.9951660
## 3 S03_19 Soil          0         3            na 6.660853   213.0184 0.9953056
## 4 S04_22 Soil          0         4            na 6.660671   204.6908 0.9951146
## 5 S05_25 Soil          0         5            na 6.610965   200.2552 0.9950064
## 6 S06_28 Soil          0         6            na 6.650812   199.3211 0.9949830
##   richness     Peven
## 1     3319 0.8171431
## 2     3079 0.8232216
## 3     3935 0.8046776
## 4     3922 0.8049774
## 5     3196 0.8192376
## 6     3481 0.8155427
```

Using tidyverse language of functions and the pipe, use the summarise function and tell me the mean and standard error evenness grouped by crop over time.
-	Start with the alpha_even dataframe
-	Group the data: group the data by Crop and Time_Point.
-	Summarize the data: Calculate the mean, count, standard deviation, and standard error for the even variable within each group.
-	Name the resulting dataframe alpha_average

``` r
alpha_average<- data.frame(alpha_even %>%
  group_by(Crop, Time_Point) %>%
  summarize(Mean.even = mean(Peven),
            n=n(),
            sd.dev = sd(Peven))%>%
  mutate(std.err = sd.dev/sqrt(n)))
```

```
## `summarise()` has grouped output by 'Crop'. You can override using the
## `.groups` argument.
```

``` r
head(alpha_average)
```

```
##     Crop Time_Point Mean.even n      sd.dev     std.err
## 1 Cotton          0 0.8201559 6 0.005564427 0.002271668
## 2 Cotton          6 0.8045279 6 0.009198591 0.003755309
## 3 Cotton         12 0.7672012 6 0.015668209 0.006396520
## 4 Cotton         18 0.7548807 5 0.016887655 0.007552389
## 5   Soil          0 0.8141333 6 0.007654244 0.003124832
## 6   Soil          6 0.8096008 6 0.005870261 0.002396524
```

Calculate the difference between the soybean column, the soil column, and the difference between the cotton column and the soil column
-	Start with the alpha_average dataframe
-	Select relevant columns: select the columns Time_Point, Crop, and mean.even.
-	Reshape the data: Use the pivot_wider function to transform the data from long to wide format, creating new columns for each Crop with values from mean.even.
-	Calculate differences: Create new columns named diff.cotton.even and diff.soybean.even by calculating the difference between Soil and Cotton, and Soil and Soybean, respectively.
-	Name the resulting dataframe alpha_average2

``` r
alpha_average2 <- data.frame(alpha_average %>%
  select(Time_Point,Crop,Mean.even) %>%
  pivot_wider(names_from = Crop, values_from = Mean.even) %>%
  mutate(diff.cotton.even = Soil - Cotton) %>%
  mutate(diff.soybean.even = Soil - Soybean))

head(alpha_average2)
```

```
##   Time_Point    Cotton      Soil   Soybean diff.cotton.even diff.soybean.even
## 1          0 0.8201559 0.8141333 0.8215344     -0.006022574      -0.007401079
## 2          6 0.8045279 0.8096008 0.7637156      0.005072906       0.045885155
## 3         12 0.7672012 0.7984872 0.6865940      0.031285973       0.111893212
## 4         18 0.7548807 0.7997796 0.7164671      0.044898885       0.083312497
```

Connecting it to plots
- Start with the alpha_average2 dataframe
- Select relevant columns: select the columns Time_Point, diff.cotton.even, and diff.soybean.even.
-	Reshape the data: Use the pivot_longer function to transform the data from wide to long format, creating a new column named diff that contains the values from diff.cotton.even and diff.soybean.even
-	Create the plot: Use ggplot and geom_line() with ‘Time_Point’ on the x-axis, the column ‘values’ on the y-axis, and different colors for each ‘diff’ category. The column named ‘values’ come from the pivot_longer. The resulting plot should look like the one to the right. 

``` r
alpha_average2 %>%
  select(Time_Point, diff.cotton.even, diff.soybean.even) %>%
  pivot_longer(c(diff.cotton.even, diff.soybean.even),names_to="diff") %>%
  ggplot(aes(x= Time_Point, y= value, color= diff)) +
  geom_line() +
  theme_classic() +
  xlab("Time (hrs)") +
  ylab("Difference from soil in Pielou's evenness")
```

![](DataWrangle_CodeChal_files/figure-docx/unnamed-chunk-7-1.png)<!-- -->

