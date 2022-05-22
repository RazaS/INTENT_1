library(dplyr)
library(tidyverse)
library(rvest)
library(ggplot2)
library(data.table)
library(readxl)
library(writexl)
library(lubridate)


setwd("C:/Users/Raza/OneDrive - University of Toronto/INTENT/INTENT for Transfusions/INTENT_1/Data_files")



read_xl <- function(fileName, mywd) {
  
  list.files()  
  
  hb_dat <- fileName
  
  fullpath <- file.path(getwd(), hb_dat) #creates the full path for you depending on OS
  
  file.exists(hb_dat)
  
  data_tibble <- read_excel(fullpath)
  
  data_df <- as.data.frame(data_tibble)
  
  #print(names(data_df))
  
  print (ncol(data_df))
  print(names(data_df))
  
  data_df <- data_df[,!grepl("^Post",names(data_df))] #remnove post Hb
  data_df <- data_df[,!grepl("^MRN",names(data_df))]  #remove MRN 
  
  
        
  data_df <- setnames(data_df, c('Site', 'Product', 'IssueDate', 'PreHb', 'PreHbDate', 'IssueLocation', 'Groupings'))
  
  data_df$PreHbDate <- as_datetime(as.numeric(data_df$PreHbDate)*3600*24, origin='1900-01-01')
  
  print ("and afterwards it was")
  print(ncol(data_df))
  print(head(data_df, 2))
  
  
  return (data_df)
  
}



wd = "C:/Users/Raza/OneDrive - University of Toronto/INTENT/INTENT for Transfusions/INTENT_1/Data_files"

fileN = "2018_Q3.xlsx"

filesDict <- list("2016_Q1.xlsx","Q2 2016 new.xlsx", "2016_Q3_new.xlsx", "2016_Q4.xlsx","2017_Q1.xlsx","2017_Q2.xlsx","2017_Q3.xlsx","2017_Q4.xlsx","2018_Q1.xlsx","2018_Q2.xlsx","2018_Q3.xlsx","2018_Q4.xlsx","2019_Q1.xlsx","2019_Q2.xlsx","2020_Q1.xlsx","2020_Q2.xlsx","2020_Q3.xlsx","2020_Q4.xlsx","2021_Q1.xlsx","2021_Q2.xlsx","2021_Q3.xlsx")

filesDictTest <- list("Test1.xlsx", "Test2.xlsx")


total_df <-  data.frame()




for (fileName in filesDict) {


  print(fileName)
  mydf <- read_xl(fileName, wd)

  total_df<-rbind(total_df, mydf)

}



write_xlsx(total_df, file.path(wd, "combined_test.xlsx"))




# mydf1 <- read_xl("2016_Q1.xlsx", wd)
# 
# print("start reading here##########")
# 
# head(mydf1, 4)
# 
# 
# 
# mydf1$PreHbDate <- as_datetime(as.numeric(mydf1$PreHbDate)*3600*24, origin='1900-01-01')
# 
# head(mydf1, 10)


# 2016-03-22 12:16	Urgent Care Clinic Visits-MD 
# 2016-01-01 12:58	PMH 14C Auto Transplant Unit
# 2016-01-01 10:50	PMH 15A Leukemia & Lymphoma Unit
# 2016-01-01 16:42	PMH 17A Breast, Gyn, GI & GU


#my_data <- read.xlsx(file = hb_dat, header=TRUE)
# 
# fullpath <- file.path(getwd(), hb_dat) #creates the full path for you depending on OS
# 
# fullpath
# 
# file.exists(hb_dat)
# 
# hb_data <- read_csv(fullpath)

# hb_df <- as.data.frame(hb_data)
# 
# hb_df <- setNames(hb_df, c('Site', 'Product', 'IssueDate', 'PreHb', 'PreHbDate', 'PostHb', 'PostHbDate', 'IssueLocation', 'Groupings')) #change column names 
# 
# 
# hbdf <- hb_df %>% filter(PreHb != "NULL" )  #remove rows with pre-hb = null 
# 
# hbdf$PreHb<- as.numeric(hbdf$PreHb)