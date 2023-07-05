#Avenir Suisse Blog: "Das digitalisierte Bankwesen in der Regulierungsspirale"
#Date: 10.07.2023
#Link: https://www.avenir-suisse.ch/das-digitalisierte-bankwesen-in-der-regulierungsspirale/
#Authors: Laurenz Grabher, Lukas Schmid, Tim Schäfer, Jürg Müller
#R-Script: Laurenz Grabher, Tim Schäfer



########## FILE STRUCTURE #################

## PART 1: BASEL FRAMEWORK

## Scrapping Links and Meta Info

# (i) standards
# (ii) sound practices
# (iii) guidelines

## Converting PDF to txt

## Analysis

# Pages
# Topics


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

##-------------------PART 1: BASEL FRAMEWORK-------------------


#-------------------Scrapping Links and Meta Info-------------------

#define Links

#first part of the link
link_bis <-"https://www.bis.org/bcbs/publications.htm?bcbspubls_page=" 

#second part of the link with type: "standards" and status: "current", "superseded" and "modified" selected
link_standard <- "&m=2566&bcbspubls=ZnJvbT0mdGlsbD0mYmNic3B1Ymx0eXBlcz0xJmJjYnNwdWJsc3RhdGVzPTgmYmNic3B1YmxzdGF0ZXM9MTUmYmNic3B1YmxzdGF0ZXM9MTYmb2JqaWQ9YmNic3B1YmxzJnBhZ2U9JnBhZ2luZ19sZW5ndGg9MTAmc29ydF9saXN0PWRhdGVfZGVzYyZ0aGVtZT1iY2JzcHVibHMmbWw9ZmFsc2UmbWx1cmw9JmVtcHR5bGlzdHRleHQ9" 

#second part of the link with type: "guidelines" and status: "current", "superseded" and "modified" selected
link_guidelines <- "&m=2566&bcbspubls=ZnJvbT0mdGlsbD0mYmNic3B1Ymx0eXBlcz0yJmJjYnNwdWJsc3RhdGVzPTgmYmNic3B1YmxzdGF0ZXM9MTUmYmNic3B1YmxzdGF0ZXM9MTYmb2JqaWQ9YmNic3B1YmxzJnBhZ2U9JnBhZ2luZ19sZW5ndGg9MTAmc29ydF9saXN0PWRhdGVfZGVzYyZ0aGVtZT1iY2JzcHVibHMmbWw9ZmFsc2UmbWx1cmw9JmVtcHR5bGlzdHRleHQ9"

#second part of the link with type: "sound practices" and status: "current", "superseded" and "modified" selected
link_soundprac <- "&m=2566&bcbspubls=ZnJvbT0mdGlsbD0mYmNic3B1Ymx0eXBlcz03JmJjYnNwdWJsc3RhdGVzPTgmYmNic3B1YmxzdGF0ZXM9MTUmYmNic3B1YmxzdGF0ZXM9MTYmb2JqaWQ9YmNic3B1YmxzJnBhZ2U9MiZwYWdpbmdfbGVuZ3RoPTEwJnNvcnRfbGlzdD1kYXRlX2Rlc2MmdGhlbWU9YmNic3B1YmxzJm1sPWZhbHNlJm1sdXJsPSZlbXB0eWxpc3R0ZXh0PQ%253D%253D" 

# Launch Selenium driver (Opens firefox - adapt if you want to use another browser)
# If this part fails, it might be related to an issue with java and/or your firewall
rD <- RSelenium::rsDriver(
  browser = "firefox", 
  port = 4221L,
  version = "latest",
  chromever = NULL
)
Sys.sleep(2) 

remDr <- rD[["client"]]


# Create empty lists to store the data
dates_list <- list()
type_list <- list()
status_list <- list()
title_list <- list()
links_list <- list()
topics_list <- list()


#-------------------Standards, Sound Practices and Guidelines-------------------

#the next steps scrape webpages to get the necessary hyperlinks and metadata

#-------------------(i) Standards-------------------



#Loop Standards
for (i in 1:4) {  #there are 4 pages of standards per 16.05.2023
  # Navigate to the page
  page_url <- paste0(link_bis, i, link_standard)
  remDr$navigate(page_url)
  
  #give the webpage time to load
  Sys.sleep(7)
  
  # Extract the data from the page
  info <- remDr$getPageSource()[[1]]
  dates_page <- info %>% read_html() %>% html_nodes(".item .item_date") %>% html_text()
  type_page <- info %>% read_html() %>% html_nodes(".item .bcbs_publication_type") %>% html_text()
  status_page <- info %>% read_html() %>% html_nodes(".item .bcbs_publication_status") %>% html_text()
  title_page <- info %>% read_html() %>% html_nodes(".dark") %>% html_text()
  links_page <- info %>% read_html() %>% html_nodes(".dark") %>% html_attr("href")
  topics_page <- info %>% read_html() %>% html_nodes(".item .bcbs_documentinfo") %>% html_text()
  
  # Append the data to the lists
  dates_list[[i]] <- dates_page
  type_list[[i]] <- type_page
  status_list[[i]] <- status_page
  title_list[[i]] <- title_page
  links_list[[i]] <- links_page
  topics_list[[i]] <- topics_page
}

# Combine the data from all pages into one list
dates <- unlist(dates_list)
type <- unlist(type_list)
status <- unlist(status_list)
title <- unlist(title_list)
links <- unlist(links_list)
topics <- unlist(topics_list)


# clean data as "\n\t"is not readable
namesClean<-c()
for (i in 1:length(title)) { namesClean[i] <- gsub('[\n\t/":,\'().]', '', title[i]) }
datesClean<-c()
for(i in 1:length(dates)){datesClean[i]<-gsub(" \n|\n|\t","", dates[i])}
statusClean <- c()
for(i in 1:length(status)){statusClean[i]<-gsub(' \n|\n|\t|/|\"',"", status[i])}
typeClean <- c()
for(i in 1:length(type)){typeClean[i]<-gsub(' \n|\n|\t|/|\"',"", type[i])}


#remove "Status: " and "Type: "from the data
statusClean <- substring(statusClean,10)
typeClean <- substring(typeClean,8)
clean_topics <- gsub(".*Topics:\\s*", "", topics)
clean_topics <- gsub("\\s*\\n.*", "", clean_topics)


#convert months to numbers 
datesNr<-c()
for(i in 1:length(datesClean)){datesNr[i]<-gsub("Jan","01",datesClean[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Feb","02",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Mar","03",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Apr","04",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("May","05",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Jun","06",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Jul","07",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Aug","08",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Sep","09",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Oct","10",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Nov","11",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Dec","12",datesNr[i])}

#convert to R date format
datesFinal<-as.Date(datesNr, format="%d %m %Y")

#adapt links such that the corresponding .pdf is opened (instead of the webpage)
linksPDF<-c()
for(i in 1:length(links)){linksPDF[i]<-gsub(".htm",".pdf", links[i])}
linksFinal<-c()
for(i in 1:length(linksPDF)){linksFinal[i]<-paste0("https://www.bis.org", linksPDF[i], sep="")}


#save infos in a dataframe
type<-rep("standard",length(namesClean))
status<-rep("final",length(namesClean))
documentsBIS_standards_final<-data.frame(namesClean, datesFinal, linksFinal, typeClean, statusClean, clean_topics)


#-------------------(ii) Sound practices-------------------

#Loop Sound practices
for (i in 1:5) { #there are 5 pages of sound practices as of 16.05.2023 - if fewer pages than in previous loop, you need to empty the list
  # Navigate to the page
  page_url <- paste0(link_bis, i, link_soundprac)
  remDr$navigate(page_url)
  
  #give the webpage time to load
  Sys.sleep(7)
  
  # Extract the data from the page
  info <- remDr$getPageSource()[[1]]
  dates_page <- info %>% read_html() %>% html_nodes(".item .item_date") %>% html_text()
  type_page <- info %>% read_html() %>% html_nodes(".item .bcbs_publication_type") %>% html_text()
  status_page <- info %>% read_html() %>% html_nodes(".item .bcbs_publication_status") %>% html_text()
  title_page <- info %>% read_html() %>% html_nodes(".dark") %>% html_text()
  links_page <- info %>% read_html() %>% html_nodes(".dark") %>% html_attr("href")
  topics_page <- info %>% read_html() %>% html_nodes(".item .bcbs_documentinfo") %>% html_text()
  # Append the data to the lists
  dates_list[[i]] <- dates_page
  type_list[[i]] <- type_page
  status_list[[i]] <- status_page
  title_list[[i]] <- title_page
  links_list[[i]] <- links_page
  topics_list[[i]] <- topics_page
}

# Combine the data from all pages into one list
dates <- unlist(dates_list)
type <- unlist(type_list)
status <- unlist(status_list)
title <- unlist(title_list)
links <- unlist(links_list)
topics <- unlist(topics_list)


# clean data as "\n\t"is not readable
namesClean<-c()
for (i in 1:length(title)) { namesClean[i] <- gsub('[\n\t/":,\'().]', '', title[i]) }
datesClean<-c()
for(i in 1:length(dates)){datesClean[i]<-gsub(" \n|\n|\t","", dates[i])}
statusClean <- c()
for(i in 1:length(status)){statusClean[i]<-gsub(' \n|\n|\t|/|\"',"", status[i])}
typeClean <- c()
for(i in 1:length(type)){typeClean[i]<-gsub(' \n|\n|\t|/|\"',"", type[i])}


#remove "Status: " and "Type: "from the data
statusClean <- substring(statusClean,10)
typeClean <- substring(typeClean,8)
clean_topics <- gsub(".*Topics:\\s*", "", topics)
clean_topics <- gsub("\\s*\\n.*", "", clean_topics)

#convert months to numbers 
datesNr<-c()
for(i in 1:length(datesClean)){datesNr[i]<-gsub("Jan","01",datesClean[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Feb","02",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Mar","03",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Apr","04",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("May","05",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Jun","06",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Jul","07",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Aug","08",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Sep","09",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Oct","10",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Nov","11",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Dec","12",datesNr[i])}

#convert to R date format
datesFinal<-as.Date(datesNr, format="%d %m %Y")

#adapt links such that the corresponding .pdf is opened (instead of the webpage)
linksPDF<-c()
for(i in 1:length(links)){linksPDF[i]<-gsub(".htm",".pdf", links[i])}
linksFinal<-c()
for(i in 1:length(linksPDF)){linksFinal[i]<-paste0("https://www.bis.org", linksPDF[i], sep="")}

## sound Practices - final
type<-rep("soundPract",length(namesClean))
status<-rep("final",length(namesClean))
documentsBIS_soundPract_final <-data.frame(namesClean, datesFinal, linksFinal, typeClean, statusClean, clean_topics)




#-------------------(iii) Guidelines-------------------

#Loop Guidelines
for (i in 1:10) {#there are 10 pages of sound practices per 16.05.2023 - if less pages than in previous loop, you need to empty the list
  # Navigate to the page
  page_url <- paste0(link_bis, i, link_guidelines)
  remDr$navigate(page_url)
  
  #give the webpage time to load
  Sys.sleep(7) 
  
  # Extract the data from the page
  info <- remDr$getPageSource()[[1]]
  dates_page <- info %>% read_html() %>% html_nodes(".item .item_date") %>% html_text()
  type_page <- info %>% read_html() %>% html_nodes(".item .bcbs_publication_type") %>% html_text()
  status_page <- info %>% read_html() %>% html_nodes(".item .bcbs_publication_status") %>% html_text()
  title_page <- info %>% read_html() %>% html_nodes(".dark") %>% html_text()
  links_page <- info %>% read_html() %>% html_nodes(".dark") %>% html_attr("href")
  topics_page <- info %>% read_html() %>% html_nodes(".item .bcbs_documentinfo") %>% html_text()
  
  # Append the data to the lists
  dates_list[[i]] <- dates_page
  type_list[[i]] <- type_page
  status_list[[i]] <- status_page
  title_list[[i]] <- title_page
  links_list[[i]] <- links_page
  topics_list[[i]] <- topics_page
}

# Combine the data from all pages into one list
dates <- unlist(dates_list)
type <- unlist(type_list)
status <- unlist(status_list)
title <- unlist(title_list)
links <- unlist(links_list)
topics <- unlist(topics_list)


# clean names, as some additional characters were generated; remove "/" as R cannot handle file download when / in title; replace " by empty, as pdf2text cannot handle " in title
namesClean<-c()
for (i in 1:length(title)) { namesClean[i] <- gsub('[\n\t/":,\'().]', '', title[i]) }
datesClean<-c()
for(i in 1:length(dates)){datesClean[i]<-gsub(" \n|\n|\t","", dates[i])}
statusClean <- c()
for(i in 1:length(status)){statusClean[i]<-gsub(' \n|\n|\t|/|\"',"", status[i])}
typeClean <- c()
for(i in 1:length(type)){typeClean[i]<-gsub(' \n|\n|\t|/|\"',"", type[i])}


#remove "Status: " and "Type: "from the data
statusClean <- substring(statusClean,10)
typeClean <- substring(typeClean,8)
clean_topics <- gsub(".*Topics:\\s*", "", topics)
clean_topics <- gsub("\\s*\\n.*", "", clean_topics)


#convert months to numbers 
datesNr<-c()
for(i in 1:length(datesClean)){datesNr[i]<-gsub("Jan","01",datesClean[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Feb","02",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Mar","03",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Apr","04",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("May","05",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Jun","06",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Jul","07",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Aug","08",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Sep","09",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Oct","10",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Nov","11",datesNr[i])}
for(i in 1:length(datesNr)){datesNr[i]<-gsub("Dec","12",datesNr[i])}

#convert to R date format
datesFinal<-as.Date(datesNr, format="%d %m %Y")

#adapt links such that the corresponding .pdf is opened (instead of the webpage)
linksPDF<-c()
for(i in 1:length(links)){linksPDF[i]<-gsub(".htm",".pdf", links[i])}
linksFinal<-c()
for(i in 1:length(linksPDF)){linksFinal[i]<-paste0("https://www.bis.org", linksPDF[i], sep="")}


## guidelines - final
type<-rep("guideline",length(namesClean))
status<-rep("final",length(namesClean))
documentsBIS_guidelines_final <-data.frame(namesClean, datesFinal, linksFinal, typeClean, statusClean, clean_topics)


# Close the Selenium driver
rD$server$stop()


#-------------------Metadata-------------------
# create file and combine all metadata
documentsBISall<-rbind(documentsBIS_standards_final,
                       documentsBIS_guidelines_final,
                       documentsBIS_soundPract_final)


#------------------Download PDFs------------------------------------------------------------------------

setwd("C:/Users/lag/OneDrive - Avenir Suisse/Dokumente - 4230 Finanzsystem/02_Daten/Basel III/RawPdfs")

#some docuemnts are not available as PDF
#High cost credit protection, Guideline, 16. Dezember 2011: https://www.bis.org/publ/bcbs_nl16.htm
#--> documentBISall[59]
#General Guide to Account Opening and Customer Identification, Guideline, 12. Februar 2003: https://www.bis.org/publ/bcbs85annex.htm
#--> documentBISall[95]

#define the links to the pdfs as NA
documentsBISall[59,3]<-NA
documentsBISall[95,3]<-NA


#create a dataframe with the documents you want to download
documentsToDownload<-documentsBISall

#define the pdf-filenames with the date, 5 characters of the type, 5 characters of the status and 30 characters of the name. The file names cannot be too long as this can be a problem later on
filenames<-c()
for (i in 1:nrow(documentsToDownload)){
  filenames[i]<-paste0(documentsToDownload$datesFinal[i], "_", substr(documentsToDownload$type[i], 1, 5), "_", substr(documentsToDownload$status[i], 1, 5), "_", substr(documentsToDownload$namesClean[i],1,30),".pdf",sep="")
}


#download the corresponding PDFs
for(i in 1:length(documentsToDownload$linksFinal)){
  if(!is.na(documentsToDownload$linksFinal)[i]){download.file(as.vector(documentsToDownload$linksFinal)[i], filenames[i], mode="wb")
  }
  Sys.sleep(3)
}


#order the dataframe with the information by date
documentsBIS_ordered<-documentsBISall[order(documentsBISall$datesFinal, decreasing=F),]


#-------------------.PDF to .Txt-------------------

# Transform .PDF TO .TXT for potential text analysis
input<-"C:/Users/lag/OneDrive - Avenir Suisse/Dokumente - 4230 Finanzsystem/02_Daten/Basel III/RawPdfs"
output_path <- "C:/Users/lag/OneDrive - Avenir Suisse/Dokumente - 4230 Finanzsystem/02_Daten/Basel III/RawTxt"

# get filenames
pdfNames <- list.files(path = input, pattern = "pdf",  full.names = TRUE)


# transform pdf to txt
for (pdf_file in pdfNames) {
  txt_file <- gsub(".pdf$", ".txt", basename(pdf_file))
  txt_path <- file.path(output_path, txt_file)
  pdf_text <- pdf_text(pdf_file)
  writeLines(pdf_text, txt_path)
}

# 2 documents are not available as pdf -> text was manually copied to a .txt file
#filenames[59] https://www.bis.org/publ/bcbs_nl16.htm
#filenames[95] https://www.bis.org/publ/bcbs85annex.htm


setwd("C:/Users/lag/OneDrive - Avenir Suisse/Dokumente - 4230 Finanzsystem/02_Daten/Basel III/RawTxt")

#create empty .txt for the files which are not available as .pdf
name_pdf <- filenames[59]
name_txt <- paste0(substr(name_pdf, 1, nchar(name_pdf) - 4), ".txt")
file.create(name_txt)

#copy the text manually into the .txt file (skip "summary of document history")

#create empty .txt for the files which are not available as .pdf 
name_pdf <- filenames[95]
name_txt <- paste0(substr(name_pdf, 1, nchar(name_pdf) - 4), ".txt")
file.create(name_txt)

#copy the text manually into the .txt file (skip "summary of document history")


#------------------------some files could not be transformed to .txt / the .txt file was empty-----------

#1975: Report on the supervision of banks'foreign establishment - concordat
#1978 Consolidation of banks blance sheets aggregation of risk-bearing assets as a method of supervising bank solvency 
#1994: Stand Super Amendment to the 1998 Capital Accord
#1999: Guide Super Year 2000 the supervisory contingency planning process

#delete empty .txt files and use "tesseract" package [optical character recognition (OCR)]

unlink("C:/Users/lag/OneDrive - Avenir Suisse/Dokumente - 4230 Finanzsystem/02_Daten/Basel III/RawTxt/1975-09-28_Guide_Super_Report on the supervision of b.txt")
unlink("C:/Users/lag/OneDrive - Avenir Suisse/Dokumente - 4230 Finanzsystem/02_Daten/Basel III/RawTxt/1978-10-28_Sound_Super_Consolidation of banks balance.txt")
unlink("C:/Users/lag/OneDrive - Avenir Suisse/Dokumente - 4230 Finanzsystem/02_Daten/Basel III/RawTxt/1994-12-28_Stand_Super_Amendment to the 1988 Capital .txt")
unlink("C:/Users/lag/OneDrive - Avenir Suisse/Dokumente - 4230 Finanzsystem/02_Daten/Basel III/RawTxt/1999-01-28_Guide_Super_Year 2000 the supervisory cont.txt")


#-------- #1975: Report on the supervision of banks'foreign establishment - concordat

# set path to the input PDF file
input_file <- "C:/Users/lag/OneDrive - Avenir Suisse/Dokumente - 4230 Finanzsystem/02_Daten/Basel III/RawPDFs/1975-09-28_Guide_Super_Report on the supervision of b.pdf"

# set path to the output text file
output_file <- "C:/Users/lag/OneDrive - Avenir Suisse/Dokumente - 4230 Finanzsystem/02_Daten/Basel III/RawTxt/1975-09-28_Guide_Super_Report on the supervision of b.txt"

# perform OCR on the input PDF file using tesseract
result <- ocr(input_file)

# write the OCR result to the output text file
writeLines(result, output_file)


#-------#1978 Consolidation of banks blance sheets aggregation of risk-bearing assets as a method of supervising bank solvency 

#pdf contains a table at the last page which is 90 degrees rotated. the individual page was rotated and again processed with ocr and manually included in the .txt

# set path to the input PDF file
input_file <- "C:/Users/lag/OneDrive - Avenir Suisse/Dokumente - 4230 Finanzsystem/02_Daten/Basel III/RawPDFs/1978-10-28_Sound_Super_Consolidation of banks balance.pdf"

# set path to the output text file
output_file <- "C:/Users/lag/OneDrive - Avenir Suisse/Dokumente - 4230 Finanzsystem/02_Daten/Basel III/RawTxt/1978-10-28_Sound_Super_Consolidation of banks balance.txt"

# perform OCR on the input PDF file using tesseract
result <- ocr(input_file)

# write the OCR result to the output text file
writeLines(result, output_file) 


#-------#1994: Stand Super Amendment to the 1998 Capital Accord

# set path to the input PDF file
input_file <- "C:/Users/lag/OneDrive - Avenir Suisse/Dokumente - 4230 Finanzsystem/02_Daten/Basel III/RawPDFs/1994-12-28_Stand_Super_Amendment to the 1988 Capital .pdf"

# set path to the output text file
output_file <- "C:/Users/lag/OneDrive - Avenir Suisse/Dokumente - 4230 Finanzsystem/02_Daten/Basel III/RawTxt/1994-12-28_Stand_Super_Amendment to the 1988 Capital .txt"

# perform OCR on the input PDF file using tesseract
result <- ocr(input_file)

# write the OCR result to the output text file
writeLines(result, output_file)


#-------#1999: Guide Super Year 2000 the supervisory contingency planning process

# set path to the input PDF file
input_file <- "C:/Users/lag/OneDrive - Avenir Suisse/Dokumente - 4230 Finanzsystem/02_Daten/Basel III/RawPDFs/1999-01-28_Guide_Super_Year 2000 the supervisory cont.pdf"

# set path to the output text file
output_file <- "C:/Users/lag/OneDrive - Avenir Suisse/Dokumente - 4230 Finanzsystem/02_Daten/Basel III/RawTxt/1999-01-28_Guide_Super_Year 2000 the supervisory cont.txt"

# perform OCR on the input PDF file using tesseract
result <- ocr(input_file)

# write the OCR result to the output text file
writeLines(result, output_file)

#-------------------Analysis Pages and Years-------------------

#creating a dataframe to save the results

# number of documents
nDocs<-rep_len(1, nrow(documentsBIS_ordered))

# number of pages
pdfCorpus <- list.files(path=input, pattern="*.pdf", full.names=T, recursive=FALSE)

#extract number of pages from the .pdfs in the pdfCorpus and save them in a vector
nPages<-c()
for (i in 1:length(pdfCorpus)){ 
  nPagesi<-length(pdf_text(pdfCorpus[i]))
  nPages<-rbind(nPages, nPagesi)
}


# "PDF error: Expected the default config, but wasn't able to find it, or it isn't a Dictionary"

#Identifying pdfs with: for (i in 1:length(pdfCorpus)){error <- length(pdf_text(pdfCorpus[i])) print(error)}
#for PDFs [97], [98], [101], [102], [104], [110], and [112] the error occurs.
#PDF error: Expected the default config, but wasn't able to find it, or it isn't a Dictionary
#manually checking revealed, that the error can be ignored as the .txt files and the page numbers were correctly interpreted by R

# include the number of pages of the non-pdf documents (html)

documentsBIS_ordered[135,] # 3 pages when copied to Word using Times New Roman 12pt https://www.bis.org/publ/bcbs_nl16.htm
documentsBIS_ordered[74,] # 6 pages when copied to Word using Times New Roman 12pt https://www.bis.org/publ/bcbs85annex.htm

nPages_pdfhtml<-c() #creating empty vector
nPages_pdfhtml[74]<-6 #filling in documentsBIS_ordered[74,] in position 74 at a length of 6 pages
nPages_pdfhtml[135]<-3 #filling in documentsBIS_ordered[135,] in position 74 at a length of 3 pages

nPages_pdfhtml[1:73]<-nPages[1:73] #filling up vector with identified page numbers
nPages_pdfhtml[75:134]<-nPages[74:133] #filling up vector with identified page numbers
nPages_pdfhtml[136:174] <-nPages[134:172] #filling up vector with identified page numbers

#creating a dataframe in order to store the results
BIS_dtf <- cbind(documentsBIS_ordered, nDocs, nPages_pdfhtml)

#-------------------Analysis of the number of pages-------------------

# Convert datesFinal column to date format
BIS_dtf$datesFinal <- as.Date(BIS_dtf$datesFinal)

# Extract the year from the datesFinal column
BIS_dtf$Year <- year(BIS_dtf$datesFinal)


# Calculate the total number of pages published per year
page_totals <- BIS_dtf %>%
  group_by(Year) %>%
  summarize(TotalPages = sum(nPages_pdfhtml))

# Manually add missing years with 0 pages
page_totals <- page_totals %>%
  complete(Year = full_seq(Year, 1), fill = list(TotalPages = 0))

# Export Data to Excel for Visualization 
write.csv(page_totals, file = "Pagenumbers by year plot.csv", row.names = FALSE)

# Or Create the ggplot chart in R
ggplot(page_totals, aes(x = Year, y = TotalPages)) +
  geom_col() +
  geom_bar(stat = "identity", fill = "orange") +
  labs(x = "Year", y = "Total Pages") +
  ggtitle("Total Pages Published per Year")+
  theme_bw()



#-------------------Analysis the different topics-------------------
#Note: A publication can have assigned multiplte topic
#Some publication do not have a topic assigned

df_Topics <- BIS_dtf %>%
  separate_rows(clean_topics, sep = ",\\s*")%>%
  #Select relevant columns
  select(clean_topics, nPages_pdfhtml, Year)%>%
  #additional column that counts how often every topic occurs
  mutate(Count = 1)


# Translate topic names to german
df_Topics <- df_Topics %>%
  mutate(clean_topics = case_when(
    clean_topics == "Credit risk" ~ "Kreditrisiken",
    clean_topics == "Market risk" ~ "Marktrisiken",
    clean_topics == "Liquidity risk" ~ "Liquidit?tsrisiken",
    clean_topics == "Operational risk" ~ "Operationelle Risiken",
    clean_topics == "Supervisory cooperation" ~ "Aufsichtszusammenarbeit",
    clean_topics == "Anti money laundering" ~ "Geldw?schereibek?mpfung",
    clean_topics == "Accounting and auditing" ~ "Rechnungslegung und Pr?fung",
    clean_topics == "Disclosure" ~ "Offenlegung",
    clean_topics == "Resolution and deposit insurance" ~ "Abwicklung und Einlagensicherung",
    clean_topics == "Definition of capital" ~ "Eigenkapitaldefinition",
    clean_topics == "Basel core principles" ~ "Basler Kernprinzipien",
    clean_topics == "Governance" ~ "Governance",
    clean_topics == "Financial conglomerates" ~ "Finanzkonglomerate",
    clean_topics == "Coronavirus" ~ "Coronavirus",
    clean_topics == "Macroprudential / systemic importance" ~ "Makroprudenzielle / systemische Bedeutung",
    clean_topics == "Fintech" ~ "Finanztechnologie",
    clean_topics %in% c("", NA) ~ "Ohne Zuordnung",  # NAs are also renamed
    TRUE ~ clean_topics
  ))

## Calculate how many pages were publsihed per topic (absolute)
df_Topics_Total <- df_Topics %>%
  group_by(Year, clean_topics) %>%
  summarise(Pages = sum(nPages_pdfhtml))

#Calculate the total number of pages per topic over all years
df_Topics_Pages_total %>%
  group_byclean_topics) %>%
  summarise(total_Pages = sum(Pages))

# Export
write.csv(df_Topics_Pages_total, file = "topics_pages.csv", row.names = FALSE)


#Optional: Include missing years and generate a timeline with pages published per topic and year
# All years
all_years <- 1975:2021
# identify missing years
missing_years <- setdiff(all_years, df_Topics_Total$Year)
# create new rows for missing years
missing_rows <- data.frame(Year = missing_years)
# fill all other columns with zero for missing years
missing_rows[, -1] <- 0
# merge frames
df_Excel_missing <- bind_rows(df_Topics_Total, missing_rows)%>%
  arrange(Year)

