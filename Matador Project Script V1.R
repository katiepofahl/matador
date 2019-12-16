#Katie Pofahl
#December 16, 2019
#This is the script for my project to explore data from Matador Ranch, MT, 
#a grassland conservation program which begain 14 years ago. I have divided this data 
#in to 2 parts. Aggregate data is for Matador Ranch and a summary of member ranches (grassbank).
#Individual data shows individual member ranch data over time.
#Conserved lands are lands that the organization has protected in some way through this 
#program.
#Protected lands are lands that have some form of permanent protection. This is a new initiative
#for the program.

#setup
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               readxl,
               plyr)

#set working directory
setwd("~/Dropbox (Yale_FES)/Data Science Project Files")

#AGGREGATE DATA  

#read in data
mydata <- read.csv("Matador Data v2.csv")
                
#cleanup - this was done in excel

#explore
head(mydata)
filter(mydata, type == "matador")
g <- filter(mydata, type == "grassbank")
mean(g$ranch_acres)

#Subset Data
gb_conserved <- select(filter(mydata, type == "grassbank"), c(year,grasslands_conserved))
gb_protected <- select(filter(mydata, type == "grassbank"), c(year,grasslands_protected))
gb_conserved_protected <- select(filter(mydata, type == "grassbank"), 
      c(year,grasslands_protected,grasslands_conserved))

#View data
gb_conserved
gb_protected
gb_conserved_protected

#Construct test plots
lm(gb_conserved) 
plot(gb_conserved)
lm(gb_protected) 
plot(gb_protected)

#Construct ggplot test plot - acres of conserved lands over time
p <- ggplot(data = gb_conserved_protected, 
            mapping = aes(x = year, y = grasslands_conserved))+
            geom_point(alpha=0.2)+
            geom_smooth()+
            theme_bw()
p

#Construct plot 1 - acres of conserved lands and acres of protected lands over time
p <- ggplot(data = gb_conserved_protected, 
            mapping = aes(x = year))+
            geom_smooth(aes(y = grasslands_conserved, colour="conserved"))+
            geom_smooth(aes(y = grasslands_protected, colour="protected"))+
            scale_colour_manual(values=c("blue", "green"))+
            labs(x = "year", y = "acres", title= "Matador Members: Enrolled Lands", 
              caption="Source: TNC Montana")+
            theme(legend.position="top", legend.title = element_blank())
p

#save plot
ggsave(filename = "EDoutput_KP_enrolled.png",
       plot = p,device = "png",
       width = 5,height = 6,units = "in",dpi = 300)

#Construct plot 2 - dollars paid per acre of program land (return on investment)

##Turn NAs into 0s
mydata2 <- mydata %>% replace_na(list(approved_management = 0, 
      grasslands_conserved = 0, grasslands_protected = 0, friendly_fence = 0, discounts = 0))
mydata2

##Create cost_per_acre variable
mydata2 <- mydata2 %>%
  mutate(cost_per_acre = discounts/(approved_management+grasslands_conserved+
      grasslands_protected+prairie_dog))
mydata2

##Construct plot
p <- ggplot(data = mydata2, 
            mapping = aes(x=year, y=cost_per_acre, color=type))+
            geom_smooth(method = "loess") +
            geom_point()+
            scale_colour_manual(values=c("red", "tan"))+
            labs(x = "year", y = "discount per acre ($)", 
              title= "Matador Members: Discount Per Acre Conserved Land", 
              caption="Source: TNC Montana")+
            theme(legend.position="top", legend.title = element_blank())
p

#save plot
ggsave(filename = "EDoutput_KP_ROI.png",
       plot = p,device = "png",
       width = 5,height = 6,units = "in",dpi = 300)

#INDIVIDUAL DATA

#read in data sheets from excel and add year
data18 <- readxl::read_excel("Matador Data v3.xlsx", sheet = 1)
    data18$year <- 2018
data17 <- readxl::read_excel("Matador Data v3.xlsx", sheet = 2)
    data17$year <- 2017
data16 <- readxl::read_excel("Matador Data v3.xlsx", sheet = 3)
    data16$year <- 2016
data15 <- readxl::read_excel("Matador Data v3.xlsx", sheet = 4)
  data15$year <- 2015
data14 <- readxl::read_excel("Matador Data v3.xlsx", sheet = 5)
  data14$year <- 2014
data13 <- readxl::read_excel("Matador Data v3.xlsx", sheet = 6)
  data13$year <- 2013
data12 <- readxl::read_excel("Matador Data v3.xlsx", sheet = 7)
  data12$year <- 2012
data11 <- readxl::read_excel("Matador Data v3.xlsx", sheet = 8)
  data11$year <- 2011
data10 <- readxl::read_excel("Matador Data v3.xlsx", sheet = 9)
  data10$year <- 2010
data09 <- readxl::read_excel("Matador Data v3.xlsx", sheet = 10)
  data09$year <- 2009
data08 <- readxl::read_excel("Matador Data v3.xlsx", sheet = 11)
  data08$year <- 2008
data07 <- readxl::read_excel("Matador Data v3.xlsx", sheet = 12)
  data07$year <- 2007
data06 <- readxl::read_excel("Matador Data v3.xlsx", sheet = 13)
  data06$year <- 2006
data05 <- readxl::read_excel("Matador Data v3.xlsx", sheet = 14)
  data05$year <- 2005
data04 <- readxl::read_excel("Matador Data v3.xlsx", sheet = 15)
  data04$year <- 2004
data03 <- readxl::read_excel("Matador Data v3.xlsx", sheet = 16)
  data03$year <- 2003

#Cleanup data sheets

##bind sheets
mydata3 <- rbind.fill(data18, data17, data16, data15, data14, data13, data12,
           data11, data10, data09, data08, data07, data06, data05,
           data04, data03)
mydata3

##rename column names and remove unnecessary columns
colnames(mydata3) <- c("landowner", "ccaa", "ranch_acres", "mgmt_acres", "conserved_acres", "protected_acres",
                       "pd_acres", "sg_acres", "fence_miles", "discount", "aum_acres", "workshop", "year", 
                       "a", "b", "c", "d", "e", "f", "g", "h", "i")
mydata3$a <- NULL
mydata3$b <- NULL
mydata3$c <- NULL
mydata3$d <- NULL
mydata3$e <- NULL
mydata3$f <- NULL
mydata3$g <- NULL
mydata3$h <- NULL
mydata3$i <- NULL

mydata3

##Replace NAs
mydata3[is.na(mydata3)] <- 0

mydata3
           
#explore
head(mydata3)
tail(mydata3)

#Write csv
write.csv(mydata3, "Matador Member Ranches.csv")

#Construct plot 3 - individual member ranches enrolled acres over time
mydata3$enrolled_acres <- (mydata3$mgmt_acres + mydata3$conserved_acres + 
                          mydata3$protected_acres + mydata3$pd_acres + mydata3$sg_acres)

subset1 <- subset(mydata3, landowner == "S" | landowner == "N"| landowner == "A" 
                  | landowner == "AA" | landowner == "H" | landowner == "J" | landowner == "O" 
                  | landowner == "K" | landowner == "L" | landowner == "CC" 
                  | landowner == "Z" | landowner == "DD" | landowner == "D" | landowner == "T"
                  | landowner == "V" | landowner == "W" | landowner == "X" | landowner == "BB")

p <- ggplot(data = subset1, 
            mapping = aes(x=year, y=enrolled_acres, color=landowner))+
  geom_line() +
  geom_point()+
  labs(x = "year", y = "enrolled acres", 
       title= "Individual Enrolled Acres Over Time", 
       caption="Source: TNC Montana")
p

ggsave(filename = "EDoutput_KP_members.png",
       plot = p,device = "png",
       width = 5,height = 6,units = "in",dpi = 300)
#####
