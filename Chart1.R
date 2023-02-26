# Load libraries
library("dplyr")
library("stringr")
library("ggplot2")

# Load the data
df <- read.csv("~/Desktop/INFO201/Local_resource/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)
df <- df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth,  "-01" ))
df$date <- as.Date(df$date, format = "%Y-%m-%d")
df <- df %>% filter(CheckoutYear != 2023)

df1 <- df %>% group_by(MaterialType) %>% summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))
# Most popular type: BOOK
# Second popular type: EBOOK
top1_2 <- df %>% filter(MaterialType == "BOOK" | MaterialType == "EBOOK") %>% group_by(CheckoutYear, MaterialType) %>% summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))


ggplot(top1_2) +
  geom_line(aes(x = CheckoutYear, y = total_checkouts, color = MaterialType)) +
  labs(title = "Total Checkout Trends for the top-2 popular book materials",
       x = "Year", y = "Total Checkouts") + 
  scale_x_continuous(breaks = seq(2017, 2022))
