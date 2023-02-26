# Load libraries
library("dplyr")
library("stringr")
library("ggplot2")

# Load the data
df <- read.csv("~/Desktop/INFO201/Local_resource/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)
df <- df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth,  "-01" ))
df$date <- as.Date(df$date, format = "%Y-%m-%d")
df <- df %>% filter(CheckoutYear != 2023)

# mean checkout 
df2 <- df %>% group_by(Publisher) %>% summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))
# Publisher who published most products: Random House, Inc.
# Publisher who published second most products: Books on Tape
# Publisher who published third most products: HarperCollins Publishers Inc.
top_publishers <- df %>% filter(Publisher == "Random House, Inc." | Publisher == "Books on Tape" | Publisher == "HarperCollins Publishers Inc.") %>% group_by(CheckoutYear, Publisher) %>% summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))

ggplot(top_publishers) +
  geom_line(aes(x = CheckoutYear, y = total_checkouts, color = Publisher)) +
  labs(title = "Total Checkout Trends for the top-3 Publishers",
       x = "Year", y = "Total Checkouts") + 
  scale_x_continuous(breaks = seq(2017, 2022))
