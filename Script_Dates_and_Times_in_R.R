# Package "cramer"
install.packages(cramer)

# # The as.Date function allows a variety of input formats through the format= argument. 
# The default format is a four digit year, followed by a month, then a day, separated by either dashes or slashes.
# The following example shows some examples of dates which as.Date will accept by default:
as.Date('1915-6-16')

as.Date('1990/02/17')

# Input dates are not in the standard format, a format string can be composed using the elements
# %d	Day of the month (decimal number)
# %m	Month (decimal number)
# %b	Month (abbreviated)
# %B	Month (full name)
# %y	Year (2 digit)
# %Y	Year (4 digit)

as.Date('1/15/2001',format='%m/%d/%Y')

as.Date('April 26, 2001',format='%B %d, %Y')

as.Date('22JUN01',format='%d%b%y')   # %y is system-specific; use with caution

# The as.numeric function can be used to convert a Date object to its internal form
bdays = c(tukey <- as.Date('1915-06-16'),fisher <- as.Date('1890-02-17'),
          +           cramer <- as.Date('1893-09-25'), kendall <- as.Date('1907-09-06'))
weekdays(bdays)
