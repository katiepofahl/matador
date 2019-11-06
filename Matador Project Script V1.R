#Katie Pofahl
#October 25, 2019
#This is the V1 script for my project to make summative descriptions and a plot
#for data from Matador Ranch, MT, a grassland conservation program which begain 14 years ago.
#Conserved lands are lands that the organization has protected in some way through this 
#program.
#Protected lands are lands that have some form of permanent protection. This is a new initiative
#for the program.

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
gb_conserved_protected <- select(filter(mydata, type == "grassbank"), c(year,grasslands_protected,grasslands_conserved))

#Construct plot
lm(gb_conserved) 
plot(gb_conserved)
lm(gb_protected) 
plot(gb_protected)
plot(gb_conserved_protected$grasslands_protected,gb_conserved_protected$grasslands_conserved)

#Construct alternate plot 1
p <- ggplot(data = gb_conserved_protected, 
            mapping = aes(x = year, y = grasslands_conserved))+
            geom_point(alpha=0.2)+
            geom_smooth()+
            theme_bw()
p

#Construct alternate plot 2
p <- ggplot(data = gb_conserved_protected, 
            mapping = aes(year))+
              geom_line(aes(y = grasslands_conserved))+
              geom_line(aes(y = grasslands_protected))+
              geom_smooth(aes(y = grasslands_conserved))+
              geom_smooth(aes(y = grasslands_protected))+
              labs(x = "year", y = "acres")
              theme_bw()
              #add title
              #add labels for curves
              #add colors scale_color_manual(values=c("blue", "green"))+
p

#####