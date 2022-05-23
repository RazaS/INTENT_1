library(dplyr)
library(tidyverse)
library(rvest)
library(ggplot2)
library(data.table)
library(ggthemes) # Load
library(dplyr)
library(rddtools)
library(magrittr)
library(gridExtra)


########Load data files into dataframe and name columns###########

setwd("C:/Users/Raza/OneDrive - University of Toronto/INTENT/INTENT for Transfusions/INTENT_1/Data_files")
list.files()

hb_dat <- "combined_test.csv" 

fullpath <- file.path(getwd(), hb_dat) #creates the full path for you depending on OS


file.exists(hb_dat)

hb_data <- read_csv(fullpath)

hb_df <- as.data.frame(hb_data)

hb_df <- setNames(hb_df, c('Site', 'Product', 'IssueDate', 'PreHb', 'PreHbDate', 'IssueLocation', 'Groupings')) #change column names 


hbdf <- hb_df %>% filter(PreHb != "NULL" )  #remove rows with pre-hb = null 

hbdf$PreHb<- as.numeric(hbdf$PreHb)

hbdf <- hbdf %>% separate(PreHbDate, c("PreHbDate", "PreHbTime"), " ", fill="right")


############ Data parameters AND FOR LOOP #########################

Sample_Size <- c(0)
Hb_Upper_Limit <- c(0)
Hb_Lower_Limit <- c(0)
Hb_Threshold <- c(0)
Effect_Size <- c(0)
P_Value <- c(0)

results_df <- data.frame(Hb_Threshold, Hb_Upper_Limit, Hb_Lower_Limit,Sample_Size, Effect_Size,P_Value)

results_df <- setnames(results_df, c("Hb_Threshold", "Hb_Upper_Limit", "Hb_Lower_Limit","Sample_Size" , "Effect_Size", "P_Value"))


param_list <- list()
#param_list[[1]] <- c(65,75, 69.5)
#param_list[[1]] <- c(75,85, 79.5)
param_list[[1]] <- c(85,95, 89.5)
#param_list[[4]] <- c(67,77, 71.5)
#param_list[[5]] <- c(77,87, 81.5)
#param_list[[6]] <- c(87,97, 91.5)



for (val in seq(1:length(param_list))) {
  
  hb_lower <- param_list[[val]][[1]]
  hb_upper <- param_list[[val]][[2]]
  thold <- param_list[[val]][[3]]
  



########### Process the dataframe ###########



hbdf_f <- hbdf %>% filter(PreHb<= hb_upper & PreHb>=hb_lower)  #filter rows with pre-hb greater than 100 and less than 30 

#sort by day/night column
hbdf_f <- hbdf_f %>% mutate(DayNight = ifelse((PreHbTime>1700 & PreHbTime<700), "Night", "Day")) 

#convert date column

hbdf_f$PreHbDate <- as.POSIXct (hbdf_f$PreHbDate, tz ="UTC")

#sort by quarter

hbdf_f <- hbdf_f %>% mutate (Quarter = ifelse(as.numeric(format(hbdf_f$PreHbDate, "%m")) %in% c(1, 2, 3), "First", ifelse(as.numeric(format(hbdf_f$PreHbDate, "%m"))  %in% c(4, 5, 6), "Second", ifelse (as.numeric(format(hbdf_f$PreHbDate, "%m")) %in% c(7,8,9), "Third" , "Fourth"))))


#sort by july column 

hbdf_f <- hbdf_f %>% mutate (July = ifelse(as.numeric(format(hbdf_f$PreHbDate, "%m")) %in% c(7), "July", "Other"))





##########Data Frame Filters ##################

#all groupings  [1] "PMH IP"     "PMH OP"     "Apheresis"  "ER"         "MedImaging" "MOTP"       "OP/MDU"     "OR"         "Other"     [10] "PMCC"       "TGH ICU"    "TGH SW"     "TWH ICU"    "KNSP"       "TWH Arth"   "HD"         "TGH MIP"    "TWH MIP"   [19] "TGH GIM"    "TWH GIM"    "TG SW"      "Medimaging"

#study main filter excluding cardiology, operating room, outpatient, apheresis, MDU

hbdf_f <- hbdf_f %>% filter(!(Groupings %in% c("Apheresis","OR", "OP/MDU", "PMH OP", "HD", "PM OP", "PMCC",  "TWH Arth", "Other"))) #filter by grouping 

#hbdf_f <- hbdf_f %>% filter(Groupings %in%  c("PMH OP")) #filter by grouping 



#hbdftgh <- hbdf_f %>% filter(Groupings %in% c("TWH ICU", "TGH ICU"))
#hbdf <- hbdf_f %>% filter(Groupings %in% c("Apheresis", "TWH", "OP/MDU"))

#hbdf_f <- hbdf_f %>% filter(Site %in% c("TGH")) #filter by site 

#hbdf_f <- hbdf_f %>% filter(Groupings %in% c("ER", "TGH ICU", "TWH ICU", "TGH MIP", "TWH MIP", "PMH IP", "TGH MOTP", "TGH SW", "TG SW", "KNSP", "TGH GIM", "TWH GIM", "Medimaging")) #filter by grouping 

#hbdf_f <- hbdf_f %>% filter(Groupings %in% c("ER" )) #filter by grouping 

#filtered database removing outpatient data and pre-selected units 
#FULL DATASET FOR PRIMARY ANALYSIS



#excluding a priori: cardiology, Apheresis, MDU, and PMH OP 


#hbdf_f <- hbdf_f %>% filter(Groupings %in%  c("PMH OP", "PMH IP")) #filter by grouping 

#hbdf_f <- hbdf_f %>% filter(Groupings %in% c( "PMCC")) #filter by grouping 


#hbdf_f <- hbdf_f %>% filter(!(IssueLocation %in% c("TGH"))) #filter by site 


#hbdf_f <- hbdf_f %>% filter(IssueLocation %in% c("TG-ES13 General Medicine", "TG-10MB Medical/Surgical ICU")) #filter by issue location 


#hbdf_f <- hbdf_f %>% filter(DayNight %in% c("Night"))
#(night = 1700-700)

#hbdf_f <- hbdf_f %>% filter(!(July %in% c("Other"))) #filter by site 




###### Main Full Data Plots ########### 

# p <- ggplot(hbdf_f, aes(x=PreHb)) +
#   geom_histogram(bins = hb_upper, alpha=1, position="dodge", fill="red") + ggtitle (paste("RBC Transfusion Thresholds UHN,", "Hb Range ", hb_lower, "-", hb_upper, ", n =", nrow(hbdf_f))) + scale_x_continuous(name = "Hemoglobin at Transfusion", breaks = seq(hb_lower, hb_upper, 1),limits=c(hb_lower-1, hb_upper+1)) + ylab ("RBC Units") + theme_bw()
# p



######## Primary Analysis Preparing DFs #####

hb_t <- table(hbdf_f$PreHb)
#create dataframe
hb_t_df <- data.frame(hb_t)
hb_t_df <- setnames(hb_t_df, c('Hb', 'Transfusions'))
hb_t_df$Hb <- as.numeric(as.character(hb_t_df$Hb))
hb_t_df$Transfusions <- as.numeric(as.character(hb_t_df$Transfusions))



############ Back of Envelope analysis #########
# print ("primary analysis")
# 
# (hb_t["68"]+hb_t["69"] ) /(hb_t["70"]+hb_t["71"] )
# (hb_t["78"]+hb_t["79"] ) /(hb_t["80"]+hb_t["81"] )
# (hb_t["88"]+hb_t["89"] ) /(hb_t["90"]+hb_t["91"] )
# print ("benfords tracer")
# 
# (hb_t["61"]+hb_t["62"] ) /(hb_t["63"]+hb_t["64"] )
# (hb_t["71"]+hb_t["72"] ) /(hb_t["73"]+hb_t["74"] )
# (hb_t["81"]+hb_t["82"] ) /(hb_t["83"]+hb_t["84"] )




############ Regression Discontinuity Analysis, linear models #########

# #linear models, one slope
# rdd_data(y = hb_t_df$Transfusions, 
#          x = hb_t_df$Hb, 
#          cutpoint = thold) %>% 
#   rdd_reg_lm(slope = "same") %>% 
#   summary()
# 
# 
# lm_same_slope <- hb_t_df %>% 
#   mutate(threshold = ifelse(Hb >= thold, 1, 0)) %$% 
#   lm(Transfusions ~ threshold + I(Hb - thold))
# 
# summary(lm_same_slope) 
# 
# 
# #linear models, two slopes
# 
# rdd_data(y = hb_t_df$Transfusions, 
#          x = hb_t_df$Hb, 
#          cutpoint = thold) %>% 
#   rdd_reg_lm(slope = "separate") %>% 
#   summary()
# 
# 
# hb_t_df %>%
#   select(Hb, Transfusions) %>%
#   mutate(threshold = as.factor(ifelse(Hb >= thold, 1, 0))) %>%
#   ggplot(aes(x = Hb, y = Transfusions, color = threshold)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE) +
#   scale_color_brewer(palette = "Accent") +
#   guides(color = FALSE) +
#   geom_vline(xintercept = thold, color = "red",
#              size = 1, linetype = "dashed") +
#   labs(y = "Transfusions",
#        x = "Pre-transfusion hemoglobin (binned)")




############ Regression Discontinuity Analysis, quadratic models #########


#quadratic RDD analysis

lm_quadratic <- hb_t_df %>% 
  mutate(threshold = ifelse(Hb >= thold, 1, 0)) %$% 
  lm(Transfusions ~ threshold + I(Hb - thold) + I((Hb -thold)^2) + threshold:I(Hb - thold) +
       threshold:I((Hb - thold)^2))


pval <- round( summary(lm_quadratic)$coefficients["threshold",4],4)
effsize <- round(lm_quadratic$coefficients["threshold"],2)

print (paste("The parameters are: ", "Hb Low:", hb_lower, " ", "Hb High:",hb_upper, " ", "Threshold:", thold))
print ("The results are: ")

print (paste("Effect Size: ", effsize))

print (paste("P-Value: ", pval))

print (paste("Analytic-Sample Size: ", nrow(hbdf_f)))

print ("++++++++++++++++++++++++++++++")

results_df <- rbind(results_df,c(thold,hb_upper,hb_lower,nrow(hbdf_f),effsize,pval))


#quadratic figure 


hb_t_df %>%
  select(Hb, Transfusions) %>%
  mutate(threshold = as.factor(ifelse(Hb >= thold, 1, 0))) %>%
  ggplot(aes(x = Hb, y = Transfusions, color = threshold)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x ^ 2),
              se = FALSE) +
  guides(color = FALSE) +
  geom_vline(xintercept = thold, color = "red",
             size = 1, linetype = "dashed") +
  labs(y = "Transfusions",
       x = "Pre-transfusion Hemoglobin Level") + #theme_calc()+ scale_colour_calc()+#
  ggtitle("Leading Digit Threshold Analysis") +   scale_x_continuous(breaks = round(seq(min(hb_t_df$Hb), max(hb_t_df$Hb), by = 1),0)) + ylim(0,max(hb_t_df$Transfusions)) + theme(axis.text.x = element_text(face="plain", color="black", size=10, angle=0), axis.text.y = element_text(face="plain", color="black", size=10, angle=45))  + theme(panel.border=element_blank(), 
  plot.title = element_text(color="black", size=18, face="bold"),
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold") ) + theme_bw()




} ###for loop closure

#grid.table(results_df)
