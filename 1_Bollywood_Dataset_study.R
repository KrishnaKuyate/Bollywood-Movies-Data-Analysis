#$$$$$$$$$$$$$$$$Bollywood Movie Data Analysis -2015 $$$$$$$$$$$$$$$#

#%Part:1
###Add image
library(magick)
image_read("Wallpaper.jpg")->img1
image_resize(img1, "800x800")->img1
image_oilpaint(img1,radius = 2)
##@Import Package and data file
library(readr)
Bollywood_Movie_Data <- read_csv("Bollywood_Movie_Data.csv")
View(Bollywood_Movie_Data)

#%Part:2
###@Understand the datastructure:
##Convert dataset into dataframe
Bollywood_Movie_Data_Df<-as.data.frame(Bollywood_Movie_Data)
##Find out the no. of variable in dataset:
paste("No. of variable in 'Bollywood_Movie_Data ' dataset :",ncol(Bollywood_Movie_Data_Df))
##Find out no. of Observation in dataset:
paste("No. of Observation in 'Bollywood_Movie_Data ' dataset :",nrow(Bollywood_Movie_Data_Df))
##Find out the no. of movies data present:
na.omit(Bollywood_Movie_Data_Df$Movie_Name)->Movie_List
paste("No. of movies data present 'Bollywood_Movie_Data ' dataset :",length(unique(Movie_List)))

##Structure of dataset:
str(Bollywood_Movie_Data_Df)

##Summary
summary(Bollywood_Movie_Data_Df)
