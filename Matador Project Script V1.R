#Katie Pofahl
#October 25, 2019
#This is the V1 script for my project to make summative descriptions and a plot for data 
#from Matador Ranch, MT, a grassland conservation program which begain 14 years ago.
#Conserved lands are lands that the organization has protected in some way through this 
#program.
#Protected lands are lands that have some form of permanent protection. This is a new initiative
#for the program.

#setup
install.packages("pacman")
pacman::p_load("tidyverse") 

#set working directory
setwd("~/Dropbox (Yale_FES)/Data Science Project Files")
                
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

#Construct final ggplot 1 - acres of conserved lands and acres of protected lands over time
p <- ggplot(data = gb_conserved_protected, 
            mapping = aes(x = year))+
            geom_smooth(aes(y = grasslands_conserved, colour="conserved"))+
            geom_smooth(aes(y = grasslands_protected, colour="protected"))+
            scale_colour_manual(values=c("blue", "green"))+
            labs(x = "year", y = "acres", title= "Matador Partners: Enrolled Lands", 
              caption="Source: TNC Montana")+
            theme(legend.position="top", legend.title = element_blank())
p

#Construct final ggplot 2 - dollars paid per acre of program land (return on investment)

## Turn NAs into 0s
mydata2 <- mydata %>% replace_na(list(approved_management = 0, 
      grasslands_conserved = 0, grasslands_protected = 0, friendly_fence = 0, discounts = 0))
mydata2

## Create cost_per_acre variable
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
              title= "Matador Partners: Discount Per Acre of Conserved Land", 
              caption="Source: TNC Montana")+
            theme(legend.position="top", legend.title = element_blank())
p


#####

