# Calling the library dplyr
library(dplyr)

#Loading the Dplyr Data
msleep <- read.csv("C:/Users/Santosh/Desktop/msleep_ggplot2.csv")


# To get number of rows inside a data frame
# Option #1
count(x = msleep)

# Option #2
msleep %>% count()

# Summarize all the information to get the mean of the sleep_total variable
# Option #1
summarise(.data = msleep, mean(sleep_total))
# Option #2
msleep %>% summarise(mean(sleep_total))

# 4_Select the right columns 
# Selecting the column "sleep_total"
select(.data = msleep, 7)

# Select all the columns starting with "sl"
select(msleep, starts_with("sl"))

# 5_Find the average sleep of each animal type
# Grouping the coloumn 'Sleep_total' and 'vore' and removing NA in 'vore' column.
group_msleep <- group_by(.data = msleep, sleep_total, is.na(vore))

# Summarising the average of columns: 'Sleep_total' and 'vore'
sleep_summarise <- summarise(group_msleep,
                             avg_sleeptotal = mean((sleep_total), na.rm = TRUE),
                             avg_vore = mean(vore), na.rm = TRUE)

# 6_Filter information inside the dataset
# Filter the animals that sleep more than 2 hours
filter_2 <- filter(msleep, sleep_total <= 2)
head(filter_2)

# Filter the animals that sleep more than 2 hours or less than 19 hours
filter_2_19 <- filter(msleep, between(sleep_total, 2, 19))
head(filter_2_19)

# Filter the that sleep between 2 and 19 hours, and they are NOT domesticated

filter_3 <- filter(msleep, conservation != 'domesticated')
filter_4 <- filter_3(filter(msleep, between(sleep_total, 2, 19)))
head(filter_3)

# Check if your filter is filtering `NA` for the variable **conservation**. If not, filter them.
filter_5 <- filter(msleep, conservation == 'NA')
head(filter_5)

# 7_Add new variables into your data
# Apply the filters you did before and create a new column with their 'brain-to-body mass ratio' (**brainwt** / **bodywt**)
# Summarize the information with the mean of brain-to-body variable for each level of the **vore** variable.
# Add a column with the counter for observations for each row with `n()`
# 8_Order the information in descending order

filter_7 <- msleep %>%
  mutate(braintobody = brainwt / bodywt) %>%
  mutate(counter = n()) #%>%
  # select(brainwt, bodywt, braintobody) %>%
  arrange(desc(braintobody)) %>%
  # filter(brainwt > 2)

filter_7
nrow(filter_7)
ncol(filter_7)

glimpse(filter_7)

# summary(filter_6)
# voremean <- mean(c(filter_6$braintobody, filter_6$vore))
# summary(voremean)
# arrange()
# # filter_6 <- mutate(msleep,
# # braintobody = msleep$brainwt/ msleep$bodywt)
















