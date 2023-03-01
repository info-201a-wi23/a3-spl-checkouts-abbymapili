data <- read.csv("~/desktop/2017-2023-10-Checkouts-SPL-Data 2.csv", stringsAsFactors = FALSE)
library("dplyr")
library("ggplot2")
library("stringr")
library("scales")

#' @brokenCode
#' 'string_detect_subjects <- str_detect(unlist(data$Subjects), "Fiction")'
#' 'relisting_subjects <- as.list(string_detect_subjects)'
#' 'data_fic <- data %>% filter(Subjects == str_subset(Subjects, " fiction"))'
#' Attempt to filter by only using fiction but cannot use stringr functions in 'filter', because they change the vector size.

#' Filter by Fiction
data_fiction <- data %>% filter(grepl( " fiction|Fiction", Subjects))



#' Most Popular Book Every Year
popular_books_by_year <- data_fiction %>% group_by(CheckoutYear) %>% filter(Checkouts == max(Checkouts, na.rm = TRUE))



#' Highest Number of Checkouts
highest_checkout_number <- max(as.vector(popular_books_by_year$Checkouts))
#' 2950

#' Calculate the average difference between the year it was checked out and the publication year:

#' Also change all the publication years to only one year listed or NA
pub_year <- data_fiction %>% summarize(PublicationYearRevised = gsub("[0-9]{8}", "", as.numeric(gsub("\\D", "", data_fiction$PublicationYear )))) %>% summarize(PublicationYearRevised = gsub("1991966", "", PublicationYearRevised)) %>% summarize(PublicationYearRevised = gsub("[0-9]{7}", "",  PublicationYearRevised))
#' * Used to have a bunch of lines but condensed it to one. 
#' * But it used to be if multiple years were listed (it was always two in the data set) that you had one 8 digit long number
#' * I had to change it so I just replaced all the 8 digit ones to NA

#' Add new dataset to data frame
data_fiction <- data_fiction %>% mutate(pub_year) 

#' @Note to self: 
#' * This code only reports one Publication Year, if multiple listed for one book, then that book won't be used in the data.
#' * Some of the most popular older books with multiple publications, might not be in the dataset.
#' * This will be a problem later, but I think I'll just ignore it.

#' Now time to calculate the average difference between years 
checkout_year_vector <- unlist(data_fiction$CheckoutYear)
pub_year_vector <- as.numeric(unlist(data_fiction$PublicationYearRevised), na.rm = TRUE)
differences <- checkout_year_vector - pub_year_vector
is.vector(differences)
average_differance <- mean(differences, na.rm = TRUE)
#' average difference = 4.6880867

#' Max difference between checkout year and publication date
max_difference_in_years <- max(differences, na.rm = TRUE)
#' 1999, which is weird but maybe it's the bible?

average_pub_year <- mean(pub_year_vector, na.rm = TRUE)
#' 2014.907

#' Testing to see why max difference is weird, but makes sense
max_pub_year <- max(pub_year_vector, na.rm = TRUE)
#' 2023
min_pub_year <- min(pub_year_vector, na.rm = TRUE)
#' 20

#' Save differences as column + make a new DF to keep computer from crashing
new_table <- data_fiction %>% select(CheckoutYear, Checkouts, Subjects, PublicationYearRevised) %>% mutate(diff = as.list(differences))
#' there's too much data and it crashed my computer awhile ago

?is.continuous
#' Chart time

is.numeric(pub_year_vector)
is.numeric(new_table$PublicationYearRevised)
min(pub_year_vector, na.rm = TRUE)
number <- list(pub_year_vector)


#' * Second Chart
#' Publication Year vs Number of Checkouts
pub_n_checkout <- new_table %>% select(PublicationYearRevised, Checkouts)
avg_checkout <- pub_n_checkout %>% group_by(PublicationYearRevised) %>% summarize(AverageCheckout = mean(Checkouts, na.rm = TRUE)) # %>% filter(AverageCheckout == AverageCheckout[nrow = c(seq(2, 50, 1), seq(52, 61, 1), seq(63, 86, 1))])



ggplot(data = avg_checkout) +
  geom_point(mapping= aes(x= PublicationYearRevised, y= AverageCheckout)) + 
  labs(
    title = "How does the age of the book affect it's popularity?",
    x = "Publication Year",
    y = "Checkouts"
  ) +
  scale_x_discrete(limits = factor(c(seq(1933, 2023, 5))) )

