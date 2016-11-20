# Data Wrangling with dplyr and tidyr
# Source: http://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html

rm(list = ls())

library(dplyr)
library(nycflights13)

dim(flights)
head(flights)
View(flights)

# Basic verbs of data manipulation
# filter | slice | arrange | select | rename | distinct | mutate | transmute | summarise | sample_n |sample_frac

# -----------------------------------------------------------------------------------------------------------------
# 1. Row Operations

# selecting a subset of rows
filter(flights, month==1, day==1)
# verbose equivalent
flights[flights$month == 1 & flights$day == 1, ]
# combining criteria using boolean operators
filter(flights, month==1 | month==2)

# selecting rows by position
slice(flights, 1:10)

# reordering rows
arrange(flights, year, month, day)
# using a descending order for certain variables
arrange(flights, desc(arr_time))
# verbose equivalents
flights[order(flights$year, flights$month, flights$day), ]
flights[order(desc(flights$arr_delay)), ]

# finding out which values a set of variables takes
distinct( select(flights, tailnum) )
distinct( select(flights, origin, dest) )
# similar but faster than  base::unique()

# sampling at random, of a number or a fraction of rows
sample_n(flights, 10)
sample_frac(flights, 0.0001)
# additional arguments: "weights" for weighted sampling and "replace=T" for bootstrap sampling 


# -----------------------------------------------------------------------------------------------------------------
# 2. Column Operations

# selecting columns
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

# matching a larger bloc of variables using helper functions
# starts_with() | ends_with() | matches() | contains()

# renaming variables
rename(flights, tail_num=tailnum)

# adding new columns
mutate(flights, gain=arr_delay-dep_delay, speed=distance/air_time*60 )
# main advantage over base::transform() is ability to just-created columns
mutate(flights, gain=arr_delay-dep_delay, gain_per_hour=gain/(air_time/60) )
# only keep new variables
transmute(flights, gain=arr_delay-dep_delay, gain_per_hour=gain/(air_time/60) )


# -----------------------------------------------------------------------------------------------------------------
# 3. Summarizing Data

summarise(flights, delay=mean(dep_delay, na.rm=T) )

# -----------------------------------------------------------------------------------------------------------------
# 4. Grouped Operations

# breaking dataset into groups of rows and applying verbs above
# select: grouping variables always retained
# arrange: orders first by grouping variables
# mutate & filter: most useful in conjuction with window functions
# sample_n & sample_frac: sample the specified number/fraction of rows from each group
# slice: extract rows within each group
# summarise: 
#   - base aggregate functions: min, max, mean, sum, sd, median(), IQR, ..
#   - dplyr aggregate functions: n, n_distinct, first, last, nth, ..

# when several grouping variables are specified, each summary peels off one level of the grouping
daily <- group_by(flights, year, month, day)
( per_day <- summarise(daily, flights=n()) )
( per_month <- summarise(per_day, flights=sum(flights)) )
( per_year <- summarise(per_month, flights=sum(flights)) )
# think about weighting for means and variances when progressively rolling up summaries
# cannot be done exactly for medians

# -----------------------------------------------------------------------------------------------------------------

# 5. Chaining
# x %>% f(y) turns into f(x, y)
# the %>% operator allows reordering chained operations for better readability

filter(
  summarise(
    select(
      group_by(flights, year, month, day),
      arr_delay, dep_delay
    ),
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ),
  arr > 30 | dep > 30
)

flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(arr > 30 | dep > 30)

# -----------------------------------------------------------------------------------------------------------------
# 7. Further reading

# data.table
# https://github.com/Rdatatable/data.table/wiki
# Vignette for database operations with dply
# http://cran.rstudio.com/web/packages/dplyr/vignettes/databases.html
# Vignette for two-table verbs
# http://cran.rstudio.com/web/packages/dplyr/vignettes/two-table.html
# Vignette for window functions and grouped mutate/filter
# http://cran.rstudio.com/web/packages/dplyr/vignettes/window-functions.html







