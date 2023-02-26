# Load libraries
library("dplyr")
library("stringr")
library("ggplot2")

# Load the data
df <- read.csv("~/Desktop/INFO201/Local_resource/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)
df <- df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth,  "-01" ))
df$date <- as.Date(df$date, format = "%Y-%m-%d")
df <- df %>% filter(CheckoutYear != 2023)

# Chart 1
# Create the data frame that sums up the checkout records based on material types
# See the types with the most records
df1 <- df %>% group_by(MaterialType) %>% summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))
# Most popular type: BOOK
# Second popular type: EBOOK
# Create a data frame with the total checkouts in these two types
top1_2 <- df %>% filter(MaterialType == "BOOK" | MaterialType == "EBOOK") %>% group_by(CheckoutYear, MaterialType) %>% summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))


# Chart 2
# mean checkout 
# Create the data frame that sums up the checkout record based on publishers then find the top 3 publishers' records
df2 <- df %>% group_by(Publisher) %>% summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))
# Publisher who published most products: Random House, Inc.
# Publisher who published second most products: Books on Tape
# Publisher who published third most products: HarperCollins Publishers Inc.
# Create a data frame with total checkouts in these two types
top_publishers <- df %>% filter(Publisher == "Random House, Inc." | Publisher == "Books on Tape" | Publisher == "HarperCollins Publishers Inc.") %>% group_by(CheckoutYear, Publisher) %>% summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))


# Chart 3
# Create a data frame that filters the book category to see only books
# Then get every book's total checkout amounts
most_book <- df %>% filter(MaterialType == "BOOK") %>% group_by(Title) %>% summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))
# The book that's been checked out the most from 2017-2022: Educated : a memoir / Tara Westover.
# Create two data frames so that we could get a trend of the checkout amounts of this book based on date
the_book <- df %>% filter(Title == "Educated : a memoir / Tara Westover.") %>% filter(MaterialType == "BOOK")
the_book_checkouts <- the_book %>% group_by(date) %>% summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))
