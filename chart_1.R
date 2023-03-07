data <- read.csv("~/desktop/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)
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

#' Chart 1: Column graph of popular books by year
ggplot(data = popular_books_by_year) +
  geom_col(mapping= aes(x= CheckoutYear, y= Checkouts, fill = CheckoutYear)) + 
  labs(
    title = "Most Popular Books per Year",
    x = "Year of Checkouts",
    y = "Number of Checkouts",
  ) +
  scale_fill_continuous(name = "Title of Book", labels = c("Two Kinds of Truth", "The Red Pencil", "After the Flood", "Harry Potter vol. 1", "The Vanishing Half", "The House of Broken Angels", "Lessons in Chemistry"))
