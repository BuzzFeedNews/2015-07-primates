# load required packages
library(dplyr)
library(readr)
library(ggplot2)
library(scales)

# import data
report <- read_tsv("report.tsv")
facility <- read_tsv("facility.tsv")

# edit field types
report <- mutate(report, cust_num = as.character(cust_num))
facility <- mutate(facility, cust_num = as.character(cust_num))

# filter the annual reports for primates only, and join to facilities table 
primates <- filter(report, col_a == "NONHUMAN PRIMATES") %>%
  inner_join(facility)

# extract the numbers of primates used in 'column E' experiments, by year and facility
distress_not_allev_facils <- filter(primates, col_e > 0) %>%
  select(fiscal_year, name, city, cert_num, state, col_e) %>%
  arrange(desc(fiscal_year), desc(col_e))

# make static version of interactive chart published with article
stacked_bar <- mutate(distress_not_allev_facils, type = ifelse(grepl("PFIZER|MERCK|BOEHRINGER|NOVARTIS|LILLY|GLAXO|SQUIBB|ROCHE", name), "Big pharma", "Other labs")) %>%
  group_by(fiscal_year, type) %>%
  summarise(total = sum(col_e)) 

ggplot(stacked_bar, aes(x=fiscal_year, y=total, fill=type)) + geom_bar(stat="identity") + xlab("Year") + ylab("Primates used") + xlim(c(1998,2015)) + scale_y_continuous(labels = comma)



