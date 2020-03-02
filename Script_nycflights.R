# Data: nycflights13
# Link: https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html

# install packages
# install.packages("nycflights13")
# install.packages("tidyverse")
# Call Library
library(nycflights13)
library(tidyverse)
library(tibble)
library(caret)
library(ggplot2)
library(lattice)

# Checking the dimension fo the flights in nycflight13
dim(flights)

# Checking for the flights details
View(flights)

# Basic verbs of Data Manipulation
# filter() --  to select cases based on their values.
# arrange() -- to reorder the cases.
# select() and rename() -- to select variables based on their names.
# mutate() and transmute() -- to add new variables that are functions of existing variables.
# summarise() -- to condense multiple values to a single value.
# sample_n() and sample_frac() --  to take random samples.

# Filter rows with filter()
filter(flights, month == 1, day == 1)
# Same code as the above one
flights[flights$month == 1 & flights$day == 1, ]

# Arrange rows with arrange()
# Very similiar to filter() except that instead of filtering or selecting rows, it reorders them
arrange(flights, year, month, day)
# Arranging in descending order
arrange(flights, desc(arr_delay))

# Select columns with select()
# Select columns by name
select(flights, year, month, day)
# Select all columns between year and day (inclusive)
select(flights, year:day)
# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))
# select(), like starts_with(), ends_with(), matches() and contains()
select(flights, tail_num = tailnum)
# Using rename the column 
rename(flights, tail_num = tailnum)

# Add new columns with mutate()
# selecting sets of existing columns, it’s often useful to add new columns that are functions of existing columns
mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
)
# only want to keep the new variables
transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60)
)

# Summarizes: collapses a data frame to a single row.
summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE)
)

# Grouped operations
# grouped select() is the same as ungrouped select(), except that grouping variables are always retained.
# grouped arrange() is the same as ungrouped; unless you set .by_group = TRUE, in which case it orders first by the grouping variables
# mutate() and filter() are most useful in conjunction with window functions (like rank(), or min(x) == x). 
# They are described in detail in vignette("window-functions").
# sample_n() and sample_frac() sample the specified number/fraction of rows in each group.

# summarise() computes the summary for each group.
by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 2000)

# Interestingly, the average delay is only slightly related to the
# average distance flown by a plane.
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

# summarise() with aggregate functions, which take a vector of values and return a single number. 
# There are many useful examples of such functions in base R like min(), max(), mean(), sum(), sd(), median(), and IQR(). 
# dplyr provides a handful of others:
# n(): the number of observations in the current group
# n_distinct(x):the number of unique values in x.
# first(x), last(x) and nth(x, n) - these work similarly to x[1], x[length(x)], and x[n]
# but give you more control over the result if the value is missing.
destinations <- group_by(flights, dest)
summarise(destinations,
          planes = n_distinct(tailnum),
          flights = n()
)

# Piping
# This doesn’t lead to particularly elegant code, especially if you want to do many operations at once. 
# You either have to do it step-by-step:
a1 <- group_by(flights, year, month, day)
a2 <- select(a1, arr_delay, dep_delay)
a3 <- summarise(a2,
                arr = mean(arr_delay, na.rm = TRUE),
                dep = mean(dep_delay, na.rm = TRUE))
a4 <- filter(a3, arr > 30 | dep > 30)

# Using the filter script 
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

# This is difficult to read because the order of the operations is from inside to out. 
# Thus, the arguments are a long way away from the function
flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(arr > 30 | dep > 30)





















