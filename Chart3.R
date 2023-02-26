# Load libraries
library("dplyr")
library("stringr")
library("ggplot2")

# Load the data
df <- read.csv("~/Desktop/INFO201/Local_resource/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)
df <- df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth,  "-01" ))
df$date <- as.Date(df$date, format = "%Y-%m-%d")
df <- df %>% filter(CheckoutYear != 2023)

most_book <- df %>% filter(MaterialType == "BOOK") %>% group_by(Title) %>% summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))
# The book that's been checked out the most from 2017-2022: Educated : a memoir / Tara Westover.
the_book <- df %>% filter(Title == "Educated : a memoir / Tara Westover.") %>% filter(MaterialType == "BOOK")
the_book_checkouts <- the_book %>% group_by(date) %>% summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))

ggplot(the_book_checkouts) +
  geom_line(aes(x = date, y = total_checkouts)) +
  labs(title = "Total Checkout Trends for the book 'Educated : a memoir / Tara Westover.'",
       x = "Date", y = "Total Checkouts")
