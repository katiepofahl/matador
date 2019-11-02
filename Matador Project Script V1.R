#Katie Pofahl
#October 25, 2019
#This is the V1 script for my project to make summary statistics and a plot
#for data from Matador Ranch, MT.

#setup
install.packages("pacman")
pacman::p_load("tidyverse") 
                
#read in data
mydata <- read.csv("Matador Data v2.csv")
                
#cleanup - this was done in excel

#explore
head(mydata)
filter(mydata, type == "matador")
filter(mydata, type == "grassbank")
mean(mydata$ranch_acres)

#Subset data
gb_conserved <- select(filter(mydata, type == "grassbank"), c(year,grasslands_conserved))
gb_protected <- select(filter(mydata, type == "grassbank"), c(year,grasslands_protected))

#Construct plot
lm(gb_conserved) 
plot(gb_conserved)
lm(gb_protected) 
plot(gb_protected)
               
#write data out