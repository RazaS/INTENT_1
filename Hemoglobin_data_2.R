library(dplyr)
library(tidyverse)
library(rvest)
library(ggplot2)
library(data.table)



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



hb_upper <- 97
hb_lower <- 87

hbdf <- hbdf %>% separate(PreHbDate, c("PreHbDate", "PreHbTime"), " ", fill="right")


# 
# hbdf$PreHbDate<- as.Date(hbdf$PreHbDate)



hbdf_f <- hbdf %>% filter(PreHb<= hb_upper & PreHb>=hb_lower)  #remove rows with pre-hb greater than 100 and less than 30 



hbdftgh <- hbdf_f %>% filter(Site=="TGH")
hbdftwh <- hbdf_f %>% filter(Site=="TWH")
hbdfpmh <- hbdf_f %>% filter(Site=="PMH")


#DATA FILTERS

#hbdftgh <- hbdf_f %>% filter(Groupings %in% c("TWH ICU", "TGH ICU"))
#hbdf <- hbdf_f %>% filter(Groupings %in% c("Apheresis", "TWH", "OP/MDU"))

#hbdf_f <- hbdf_f %>% filter(Site %in% c("TGH")) #filter by site 

#hbdf_f <- hbdf_f %>% filter(Groupings %in% c("ER", "TGH ICU", "TWH ICU", "TGH MIP", "TWH MIP", "PMH IP", "TGH MOTP", "TGH SW", "TG SW", "KNSP", "TGH GIM", "TWH GIM", "Medimaging")) #filter by grouping 

#hbdf_f <- hbdf_f %>% filter(Groupings %in% c("ER" )) #filter by grouping 

#filtered database removing outpatient data and pre-selected units 
#FULL DATASET FOR PRIMARY ANALYSIS
hbdf_f <- hbdf_f %>% filter(!(Groupings %in% c("Apheresis","OR", "OP/MDU", "PMH OP", "HD", "PM OP", "PMCC",  "TWH Arth", "Other"))) #filter by grouping 


#excluding a priori: cardiology, Apheresis, MDU, and PMH OP 


#hbdf_f <- hbdf_f %>% filter(Groupings %in%  c("PMH OP", "PMH IP")) #filter by grouping 

#hbdf_f <- hbdf_f %>% filter(Groupings %in% c( "PMCC")) #filter by grouping 


#hbdf_f <- hbdf_f %>% filter(!(IssueLocation %in% c("TGH"))) #filter by site 


#hbdf_f <- hbdf_f %>% filter(IssueLocation %in% c("TG-ES13 General Medicine", "TG-10MB Medical/Surgical ICU")) #filter by issue location 



#MUTATIONS

# #TG-ES13 General Medicine
# 
hbdf_f <- hbdf_f %>% mutate(DayNight = ifelse((PreHbTime>1700 & PreHbTime<700), "Night", "Day")) #split day/night data 

#convert date column

hbdf_f$PreHbDate <- as.POSIXct (hbdf_f$PreHbDate, tz ="UTC")
# 

#sort by quarter

hbdf_f <- hbdf_f %>% mutate (Quarter = ifelse(as.numeric(format(hbdf_f$PreHbDate, "%m")) %in% c(1, 2, 3), "First", ifelse(as.numeric(format(hbdf_f$PreHbDate, "%m"))  %in% c(4, 5, 6), "Second", ifelse (as.numeric(format(hbdf_f$PreHbDate, "%m")) %in% c(7,8,9), "Third" , "Fourth"))))

#  #split data by quarter
# 
# 
# #july effect
# 
# hbdf_f <- hbdf_f %>% mutate (July = ifelse(as.numeric(format(hbdf_f$PreHbDate, "%m")) %in% c(7), "July", "Other"))  
# #split data by quarter



#hbdf_f <- hbdf_f %>% filter(DayNight %in% c("Night"))
#(night = 1700-700)

#hbdf_f <- hbdf_f %>% filter(!(July %in% c("Other"))) #filter by site 



#ggplot(hbdf, aes(PreHb)) + geom_histogram(binwidth=0.5)+   geom_density(color="red", fill="lightblue")

#p <- ggplot(hbdf_f, aes(x=PreHb, fill =July)) +
#p <- ggplot(hbdf_f, aes(x=PreHb, fill=DayNight)) +

  

p <- ggplot(hbdf_f, aes(x=PreHb)) +
  geom_histogram(bins = hb_upper, alpha=1, position="dodge", fill="red") + ggtitle (paste("RBC Transfusion Thresholds UHN,", "Hb Range ", hb_lower, "-", hb_upper, ", n =", nrow(hbdf_f))) + scale_x_continuous(name = "Hemoglobin at Transfusion", breaks = seq(hb_lower, hb_upper, 1),limits=c(hb_lower-1, hb_upper+1)) + ylab ("RBC Units") + theme_bw()
p

#DATA ANALYSIS STARTS HERE


# hbthresh <- sort(table(hbdf_f$PreHb),decreasing=TRUE)[1:8] #find most common values
# plot(hbthresh, xlab="Hb Value" , ylab="RBC Units Transfused", main="OR, Apheresis, MDU")
# 
# #: TGH ICU, TWH ICU, TGH MIP, TWH MIP, PMH IP, TGH MOTP, TGH SW, KNSP"
# 
# plot(na.omit(hbthresh))
# 
# find_peaks <- function (x, m = 3){
#   shape <- diff(sign(diff(x, na.pad = FALSE)))
#   pks <- sapply(which(shape < 0), FUN = function(i){
#     z <- i - m + 1
#     z <- ifelse(z > 0, z, 1)
#     w <- i + m + 1
#     w <- ifelse(w < length(x), w, length(x))
#     if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
#   })
#   pks <- unlist(pks)
#   pks
# }


# d <- density(hbdf_f$PreHb, bw = "sj")
# loc.max <- d$x[localMaxima(d$y)]
# 
# ggplot(hbdf_f, aes(PreHb)) + geom_density(adjust=1/2) +
#   geom_vline(xintercept=find_peaks(hbdf_f$PreHb, 50), col="red") +
#   xlab("Measured values")


# locpeakfinder <- function(rangelow, rangehigh, datavec) {
#   
#   density(datavec)$y[density(datavec)$x < rangehigh & density(datavec)$x > rangelow]
#   MaxY<- max(density(datavec)$y[density(datavec)$x < rangehigh & density(datavec)$x > rangelow])
#   
#   localpeak <- which(density(datavec)$y == MaxY)
#   
#   localpeak
#   
# }
# 
# 
# rangelow = 80
# rangehigh = 100
# 
# 
# localpeak <- locpeakfinder(7,85, hbdf_f$PreHb)
# density(hbdf_f$PreHb)$x[localpeak]
# 
# 
# ggplot(hbdf_f, aes(PreHb)) + geom_histogram(bins = hb_upper) + geom_vline(xintercept = density(hbdf_f$PreHb)$x[localpeak])

##the essential insight is to use rdd data package and the regression discontinuity 


#difference in differences
# hb_t <- table(hbdf_f$PreHb)
# hbdd <- (hb_t[-1] / hb_t[1:(nrow(hb_t)-1)])
# plot(hbdd)
# 
# #make table out of df frequencies
# hbb <- as.data.frame(table(hbdf_f$PreHb))
# hbb$Freq <-as.numeric(as.numeric(hbb$Freq))
# hbb$Var1 <- as.numeric(as.character(hbb$Var1))
# hbb <- hbb %>% mutate(diff=Freq/lag(Freq, default=first(Freq))) #subtract from row above
# plot(hbb$Var1, hbb$diff, type="s", xlab="Pre-transfusion Hemoglobin", ylab="Ratio of Increase", col="red")
# 
# p<-ggplot(data=hbb, aes(x=Var1, y=diff)) +
#  geom_bar(stat="identity")
# p

#difference in differences
hb_t <- table(hbdf_f$PreHb)
#create dataframe
hb_t_df <- data.frame(hb_t)
hb_t_df <- setnames(hb_t_df, c('Hb', 'Transfusions'))
hb_t_df$Hb <- as.numeric(as.character(hb_t_df$Hb))
hb_t_df$Transfusions <- as.numeric(as.character(hb_t_df$Transfusions))


print ("primary analysis")

(hb_t["68"]+hb_t["69"] ) /(hb_t["70"]+hb_t["71"] )
(hb_t["78"]+hb_t["79"] ) /(hb_t["80"]+hb_t["81"] )
(hb_t["88"]+hb_t["89"] ) /(hb_t["90"]+hb_t["91"] )
print ("benfords tracer")

(hb_t["61"]+hb_t["62"] ) /(hb_t["63"]+hb_t["64"] )
(hb_t["71"]+hb_t["72"] ) /(hb_t["73"]+hb_t["74"] )
(hb_t["81"]+hb_t["82"] ) /(hb_t["83"]+hb_t["84"] )


library(dplyr)
library(ggplot2)
library(rddtools)
library(magrittr)

thold=91.5

rdd_data(y = hb_t_df$Transfusions, 
         x = hb_t_df$Hb, 
         cutpoint = 80) %>% 
  rdd_reg_lm(slope = "same") %>% 
  summary()


lm_same_slope <- hb_t_df %>% 
  mutate(threshold = ifelse(Hb >= thold, 1, 0)) %$% 
  lm(Transfusions ~ threshold + I(Hb - thold))

summary(lm_same_slope) 


#separate slopes RDD 

rdd_data(y = hb_t_df$Transfusions, 
         x = hb_t_df$Hb, 
         cutpoint = thold) %>% 
  rdd_reg_lm(slope = "separate") %>% 
  summary()


hb_t_df %>%
  select(Hb, Transfusions) %>%
  mutate(threshold = as.factor(ifelse(Hb >= thold, 1, 0))) %>%
  ggplot(aes(x = Hb, y = Transfusions, color = threshold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  guides(color = FALSE) +
  geom_vline(xintercept = thold, color = "red",
             size = 1, linetype = "dashed") +
  labs(y = "Transfusions",
       x = "Pre-transfusion hemoglobin (binned)")


#quadratic RDD analysis

lm_quadratic <- hb_t_df %>% 
  mutate(threshold = ifelse(Hb >= thold, 1, 0)) %$% 
  lm(Transfusions ~ threshold + I(Hb - thold) + I((Hb -thold)^2) + threshold:I(Hb - thold) +
       threshold:I((Hb - thold)^2))

summary(lm_quadratic)

library(ggthemes) # Load

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
       x = "Pre-transfusion Hemoglobin Level") + theme_calc()+ scale_colour_calc()+
  ggtitle("Leading Digit Threshold Analysis") +   scale_x_continuous(breaks = round(seq(min(hb_t_df$Hb), max(hb_t_df$Hb), by = 1),0)) + ylim(0,max(hb_t_df$Transfusions)) + theme(axis.text.x = element_text(face="plain", color="black", size=10, angle=0), axis.text.y = element_text(face="plain", color="black", size=10, angle=45))  + theme(
  plot.title = element_text(color="black", size=18, face="bold"),
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold") ) 



# ggplot(data=df, aes(x=dose, y=len, group=1)) +
#   geom_line()+
#   geom_point()