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

#Subset data
gb_conserved <- select(filter(mydata, type == "grassbank"), c(year,grasslands_conserved))
gb_protected <- select(filter(mydata, type == "grassbank"), c(year,grasslands_protected))
gb_conserved_protected <- select(filter(mydata, type == "grassbank"), 
      c(year,grasslands_protected,grasslands_conserved))

#Construct test plots
lm(gb_conserved) 
plot(gb_conserved)
lm(gb_protected) 
plot(gb_protected)
plot(gb_conserved_protected$grasslands_protected,
     gb_conserved_protected$grasslands_conserved)

#Construct alternate plot 1 - acres of conserved lands over time
p <- ggplot(data = gb_conserved_protected, 
            mapping = aes(x = year, y = grasslands_conserved))+
            geom_point(alpha=0.2)+
            geom_smooth()+
            theme_bw()
p

#Construct alternate plot 2 - acres of conserved lands and acres of protected lands over time
p <- ggplot(data = gb_conserved_protected, 
            mapping = aes(year))+
            geom_smooth(aes(y = grasslands_conserved, colour="conserved"))+
            geom_smooth(aes(y = grasslands_protected, colour="protected"))+
            scale_colour_manual(values=c("blue", "green"))+
            labs(x = "year", y = "acres", title= "Matador Partners: Enrolled Lands", 
              caption="Source: TNC Montana")+
            theme(legend.position="top", legend.title = element_blank())
p

#Construct alternate plot 3 - dollars paid per acre of program land (return on investment)
mydata %>% 
  mutate(cost_per_acre = discounts/(approved_management+grasslands_conserved+
    grasslands_protected+prairie_dog))



#####
