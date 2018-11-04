####################Mini Project : Bollywood Movie Data Analysis######################

####Import the data
library(readr)
Bollywood_Movie_Data <- read_csv("Bollywood_Movie_Data.csv")
View(Bollywood_Movie_Data)

####Convert into Data Frame
Bollywood_Movie_Data_Df<-as.data.frame(Bollywood_Movie_Data)

####Understand the structure of Dataset
str(Bollywood_Movie_Data_Df)
##How many Variable it contain
ncol(Bollywood_Movie_Data_Df)->No_of_variable
print(No_of_variable)
##No of obesrvation enter in data set
nrow(Bollywood_Movie_Data_Df)->No_of_obs
print(No_of_obs)
summary(Bollywood_Movie_Data_Df)

####Data Analysis####

##Check the no of movie details present in Data set.
Movie<-unique(as.character(Bollywood_Movie_Data_Df$Movie_Name))
length(Movie)->Total_Number_Movie
paste0("Total Number of  movies Release in 2015:",Total_Number_Movie)

##How many actor work in 2015
Actor<-unique(as.character(Bollywood_Movie_Data_Df$Acter_Name))
Total_Actor<-length(Actor)
paste0("Total Number of  Actor work in 2015:",Total_Actor)

###Collection
##Total o_collection in 2015
paste0("Total O collection in 2015 is:",sum(na.omit(Bollywood_Movie_Data_Df$Ocollection)),"Milion")
##Total WCollection in 2015
paste0("Total Wcollection in 2015 is:",sum(na.omit(Bollywood_Movie_Data_Df$Wcollection)),"Milion")
##Total Fcollection in 2015
paste0("Total Wcollection in 2015 is:",sum(na.omit(Bollywood_Movie_Data_Df$Fwcollection)),"Milion")
##Total TCollection in 2015
paste0("Total Wcollection in 2015 is:",sum(na.omit(Bollywood_Movie_Data_Df$Tcollection)),"Milion")

##Movie Contain Most Ocollection
#Ocollection
max(na.omit(Bollywood_Movie_Data_Df$Ocollection))
#wcollection
max(na.omit(Bollywood_Movie_Data_Df$Wcollection))
#Fwcollection
max(na.omit(Bollywood_Movie_Data_Df$Fwcollection))
#Tcollection
max(na.omit(Bollywood_Movie_Data_Df$Tcollection))



###Movie Success Status
Rat<-as.vector(unique(Bollywood_Movie_Data_Df$Verdict))
#subset(Bollywood_Movie_Data_Df,Bollywood_Movie_Data_Df$Verdict==Rat[1])->>nm
#nrow(nm)
#paste0("Total count of ",Rat[1],"Movie is:",nrow(nm))

Count=1
for (Count in 1:length(Rat)) 
{
  subset(Bollywood_Movie_Data_Df,Bollywood_Movie_Data_Df$Verdict==Rat[Count])->nm
  nrow(nm)
  paste0("Total count of ",Rat[Count],"Movie is:",nrow(nm))
  
}

###Actor who worked in most no.of movies
##First
sort(table(Bollywood_Movie_Data_Df$Acter_Name),decreasing = TRUE)[1]
##Second
sort(table(Bollywood_Movie_Data_Df$Acter_Name),decreasing = TRUE)[2]
##Third
sort(table(Bollywood_Movie_Data_Df$Acter_Name),decreasing = TRUE)[3]



#Top 10 Superhit Movie
#Bollywood_Movie_Data_Df$Movie_Name
#Bollywood_Movie_Data_Df$Verdict
subset(Bollywood_Movie_Data_Df,Bollywood_Movie_Data_Df$Verdict=='Super Hit')->supm
as.data.frame(supm)->supmo
#order(Bollywood_Movie_Data_Df$Tcollection,decreasing = TRUE)
#supmo[order(Bollywood_Movie_Data_Df$Tcollection),]
na.omit(supmo[order(Bollywood_Movie_Data_Df$Tcollection),])
as.data.frame(na.omit(supmo[order(Bollywood_Movie_Data_Df$Tcollection),]))->dff
dff$Movie_Name
paste0("Top 10 SuperHit Movie is ")
paste0(dff$Movie_Name[1:10])



####Actor who earn most
aggregate(Bollywood_Movie_Data_Df$Tcollection, by=list(Bollywood_Movie_Data_Df$Acter_Name), FUN=sum)->new_df
names(new_df)<-c("Hero","Income")
order(new_df$Income,decreasing = TRUE)
new_df[order(new_df$Income,decreasing = TRUE),]->new_df1
new_df1[1,1]
paste0("Actor who earn most is :",new_df1[1,1])



####Plot Graph

#Bollywood_Movie_Data_Df$Verdict
Bollywood_Movie_Data_Df$Verdict
sort(table(Bollywood_Movie_Data_Df$Verdict))->ver1
as.data.frame(ver1)->ver2
names(ver2)<-c("Success Cat","Total Movie Count")
ver2
barplot(ver2$`Total Movie Count`,names.arg = ver2$`Success Cat`,
        xlab = "Success Category",ylab = "No. of Movies",main = "Movie Success Analysis",col = "red")


##Actor vs Tcollection
Bollywood_Movie_Data_Df$Tcollection
aggregate(Bollywood_Movie_Data_Df$Tcollection, by=list(Bollywood_Movie_Data_Df$Acter_Name), FUN=sum)->TCO
na.omit(TCO)->TCO
names(TCO)<-c("Hero","TIncome")
TCO[order(TCO$TIncome,decreasing = TRUE),]->TCO1
TCO1[c(1,2,3,4,5),]->TCO2

Bollywood_Movie_Data_Df$Ocollection
aggregate(Bollywood_Movie_Data_Df$Ocollection, by=list(Bollywood_Movie_Data_Df$Acter_Name), FUN=sum)->OCO
na.omit(OCO)
names(OCO)<-c("Hero","OIncome")
OCO[order(OCO$OIncome,decreasing = TRUE),]->OCO1
OCO1[1:5,]->OCO2

Bollywood_Movie_Data_Df$Wcollection
aggregate(Bollywood_Movie_Data_Df$Wcollection, by=list(Bollywood_Movie_Data_Df$Acter_Name), FUN=sum)->WCO
na.omit(WCO)
names(WCO)<-c("Hero","WIncome")
WCO[order(WCO$WIncome,decreasing = TRUE),]->WCO1
WCO1[1:5,]->WCO2

Bollywood_Movie_Data_Df$Fwcollection
aggregate(Bollywood_Movie_Data_Df$Fwcollection, by=list(Bollywood_Movie_Data_Df$Acter_Name), FUN=sum)->FwCO
na.omit(FwCO)
names(FwCO)<-c("Hero","FIncome")
FwCO[order(FwCO$FIncome,decreasing = TRUE),]->FwCO1
FwCO1[1:5,]->FwCO2

disply_grid<-matrix(1:4,nrow = 2,ncol = 2)
layout(disply_grid)
barplot(OCO2$OIncome,names.arg = OCO2$Hero,main ="Top 5 Actor:Actor vs OCollection",xlab = "Actor Name",ylab = "Ocollection",col="red")
barplot(WCO2$WIncome,names.arg = WCO2$Hero,main ="Top 5 Actor:Actor vs WCollectio",xlab = "Actor Name",ylab = "Wcollection",col="green")
barplot(FwCO2$FIncome,names.arg = FwCO2$Hero,main ="Top 5 Actor:Actor vs FwOCollectio",xlab = "Actor Name",ylab = "Fwcollection",col = "yellow")
barplot(TCO2$TIncome,names.arg = TCO2$Hero,main ="Top 5 Actor:Actor vs TOCollectio",xlab = "Actor Name",ylab = "Tocollection",col = "blue")



