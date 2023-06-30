#Avenir Suisse Blog: "Das digitalisierte Bankwesen in der Regulierungsspirale"
#Date:
#Link:
#Authors: Laurenz Grabher, Lukas Schmid, Tim Schäfer, Jürg Müller
#R-Script: Laurenz Grabher, Tim Schäfer



########## FILE STRUCTURE #################

## PART 2: FINMA RUNDSCHREIBEN

## Scrapping Links and Meta Info

## PDF Download

## Counting page numbers

## Visualization


###############################################

#load packages

library(rvest)
library(pdftools)
library(tm)
library(ggplot2)
library(magrittr)
library(readr)
library(RSelenium)
library(gdata)
library(tesseract)
library(tidyr)
library(dplyr)
library(httr)
library(stringr)
library(tidytext)
library(SnowballC)
library(lubridate)
library(tokenizers)



##-------------------PART 2: FINMA RUNDSCHREIBEN-------------------


#-------------------Scrapping Links and Meta Info-------------------


# Launch Selenium driver (Opens firefox - adapt if you want to use another browser)
# If this part fails, it might be related to an issue with java and/or your firewall
rD <- RSelenium::rsDriver(
  browser = "firefox", 
  port = 4957L,
  version = "latest",
  chromever = NULL
)

remDr <- rD[["client"]]

# Load Website
remDr$navigate(paste0("https://www.finma.ch/de/dokumentation/rundschreiben/"))

Sys.sleep(5)

info <- remDr$getPageSource()[[1]]

# Extract the metadata from the page
title_page <- info %>% read_html() %>% html_nodes(".js-results .document-teaser-box-title") %>% html_text()
Erlass_page <- info %>% read_html() %>% html_nodes(".js-results .document-teaser-box-title") %>% html_text()
Rund_page <- info %>% read_html() %>% html_nodes(".js-results .document-teaser-box-title") %>% html_text()
links_page <- info %>% read_html() %>% html_nodes(".js-results .document-teaser-box-title") %>% html_attr("href")

#Close the Selenium driver
rD$server$stop()

# Create Dataframe with all the data
finma_dataframe = data.frame(title_page, Erlass_page, Rund_page, links_page, stringsAsFactors = FALSE)

# Clean Dataframe
# The Links miss the prefix --> add the prefix https://www.finma.ch to all links
finma_dataframe$links_page <- paste0("https://www.finma.ch", finma_dataframe$links_page)
#Clean Dates of "Erlassdatum"
finma_dataframe$Erlass_page <- gsub(".*\\((\\d{1,2}\\.\\d{1,2}\\.\\d{4})\\).*", "\\1", finma_dataframe$Erlass_page)
# Clean Dates of "Rundschreiben"
finma_dataframe$Rund_page <- sub("^(\\d{4}/\\d{2}).*", "\\1", finma_dataframe$Rund_page)
# Clean Title
finma_dataframe$title_page <- sub("^[^\\s]+\\s", "", finma_dataframe$title_page)

#---------------------Download PDF------------------------------------------------------
setwd("C:/Users/lag/OneDrive - Avenir Suisse/Dokumente - 4230 Finanzsystem/02_Daten/Basel III/FinmaPdfs")


#2 PDFs have exact same title: "Rundschreiben 2013/3 Pr?fwesen (06.12.2012)", but are valid from a different point in time onwards.
# Thus rename the one that is valid from 2024 onwards to "FINMA-Rundschreiben 'Pr?fwesen' (06.12.2012) ab 2024"
finma_dataframe$title_page[29] <- "FINMA-Rundschreiben 'Pr?fwesen' (06.12.2012) ab 2024"


# Function to clean names
clean_filename <- function(filename) {
  # Define characters that are not allowed
  invalid_chars <- c("/", "\\\\", ":", "*", "?", "\"", "<", ">", "|") 
  for (char in invalid_chars) {
    filename <- gsub(char, "_", filename, fixed = TRUE)  
  }
  return(filename)
}
# Function to save PDFs
save_pdf_from_link <- function(link, date, title) {
  # Create Filename
  file_name <- paste0(date, "_", substr(title, 1, 45), ".pdf")
  file_name <- clean_filename(file_name)  # Clean Dataframe
  
  # Download PDFs from the links
  response <- GET(link, write_disk(file_name))
  
  # Check if download successful
  if (status_code(response) == 200) {
    message(paste("PDF", file_name, "successfullay downloaded and saved."))
  } else {
    message(paste("ERROR when downloading", file_name))
  }
}

# Download all PDFs and save them
for (i in 1:nrow(finma_dataframe)) {
  save_pdf_from_link(finma_dataframe$links_page[i], finma_dataframe$date_page[i], finma_dataframe$title_page[i])
}
# ----------Count number of Pages-------------------------------------

# Define path to the PDFs
pdf_directory <- "C:/Users/lag/OneDrive - Avenir Suisse/Dokumente - 4230 Finanzsystem/02_Daten/Basel III/FinmaPdfs"


# Function no count page numbers
count_pdf_pages <- function(file_path) {
  pdftools::pdf_info(file_path)$pages
}

# List and sort PDFs 
pdf_files <- list.files(pdf_directory, full.names = TRUE)
pdf_files <- sort(pdf_files, decreasing = TRUE)
# Count page numbers
page_counts <- sapply(pdf_files, count_pdf_pages)

# Add count to
finma_dataframe$num_pages <- page_counts

# ----------Visualizaton-------------------------------------

# Three wrong Date in titles in 2008/04 FINMA-Rundschreiben "Effektenjournal" it should be 20. November 2008 instead of 25.01.2017
# in Rundschreiben 2015/2 Liquidit?tsrisiken - Banken  it has to be 3. Juli 2014 instead of 7.12.2017
# and in Rundschreiben 2016/1 Offenlegung - Banken it has to be 28.10.2015
finma_dataframe[48, "Erlass_page"] <- "20.11.2008"
finma_dataframe[24, "Erlass_page"] <- "03.07.2014"
finma_dataframe[22, "Erlass_page"] <- "28.10.2015"

# New Column: Year
finma_dataframe$Year <- sapply(strsplit(finma_dataframe$Erlass_page, "\\."), function(x) substr(x[3], 1, 4))
# As numeric
finma_dataframe$Year <- as.numeric(finma_dataframe$Year)

#Export to Excel
write.csv(finma_dataframe, file = "Finma Rundschreiben.csv", row.names = FALSE)

# Calculate the total number of pages published per year
page_totals <- finma_dataframe %>%
  group_by(Year) %>%
  summarize(TotalPages = sum(num_pages))

# Manually add missing years with 0 pages
page_totals <- page_totals %>%
  complete(Year = full_seq(Year, 1), fill = list(TotalPages = 0))

# Create the ggplot chart
ggplot(page_totals, aes(x = Year, y = TotalPages)) +
  geom_col() +
  geom_bar(stat = "identity", fill = "orange") +
  labs(x = "Year", y = "Total Pages") +
  ggtitle("Total Pages Published per Year")+
  theme_bw()

# Or export to Excel
write.csv(page_totals, file = "Finma Rundschreiben.csv", row.names = FALSE)

#this code downloaded all "Finma Rundschreiben"
#the publications concerning bank were selected by hand if the publication addresses the "BankG"
