#%Part:3

####Movie:

##Total number of movie release in 2015:
na.omit(Bollywood_Movie_Data_Df$Movie_Name)->Movie_List
paste("No. of movies data present 'Bollywood_Movie_Data ' dataset :",length(unique(Movie_List)))

##No.Of Movie vs Month Analysis:
library(lubridate)
library(dplyr)
library(ggplot2)
#Bollywood_Movie_Data_Df$Release_date
month(Bollywood_Movie_Data_Df$Release_date,label = TRUE)->month_movie


#Bollywood_Movie_Data_Df$Movie_Name,month_movie
data.frame(Bollywood_Movie_Data_Df$Movie_Name,month_movie)->month_movie_df
month_movie_df%>%count(month_movie)->month_movie_dfc
colnames(month_movie_dfc)<-c("Month of Release","No. of Movie")
#Table:
month_movie_dfc

#Graph:

ggplot(month_movie_dfc,aes(x=`Month of Release`,y=`No. of Movie`,fill=`Month of Release`))+geom_bar(stat = "identity",position="dodge")+ylab("Movies Released")+xlab("Month")+ggtitle("Movies Released:Monthwise Analysis")+theme(plot.title = element_text(face = "bold.italic",colour = "#800080",size =20))+geom_point()
#######################################################

##Movie Collection:
Bollywood_Movie_Data_Df$Ocollection
Bollywood_Movie_Data_Df$Wcollection
Bollywood_Movie_Data_Df$Fwcollection
Bollywood_Movie_Data_Df$Tcollection

#Total O collection
sum(Bollywood_Movie_Data_Df$Ocollection)->smo
paste(smo,"Million")


#Total W collection
na.omit(Bollywood_Movie_Data_Df$Wcollection)
sum(na.omit(Bollywood_Movie_Data_Df$Wcollection))->smw
paste0(smw,"Millions")

#Total F collection
na.omit(Bollywood_Movie_Data_Df$Fwcollection)
sum(na.omit(Bollywood_Movie_Data_Df$Fwcollection))->smf
paste0(smf,"Millions")

#Total T collection
na.omit(Bollywood_Movie_Data_Df$Tcollection)
sum(na.omit(Bollywood_Movie_Data_Df$Tcollection))->smt
paste(smt,"Millions")

#top 5 Movie according to o collections
Bollywood_Movie_Data_Df[order(Bollywood_Movie_Data_Df$Ocollection,decreasing = TRUE),]->ocdf
data.frame("Movie Name"=ocdf$Movie_Name,"O Collection"=ocdf$Ocollection)->ocdf5
ocdf5[1:5,]->ocdf6
as.data.frame(ocdf6)->ocdf6
#Table:
ocdf6

#top 5 Movie according to w collections
Bollywood_Movie_Data_Df[order(Bollywood_Movie_Data_Df$Wcollection,decreasing = TRUE),]->wcdf
data.frame("Movie Name"=wcdf$Movie_Name,"w Collection"=wcdf$Wcollection)->wcdf5
#Table:
wcdf5[1:5,]->wcdf5
as.data.frame(wcdf5)->wcdf6
wcdf6

#Top 5 Movies according to F collection:
Bollywood_Movie_Data_Df[order(Bollywood_Movie_Data_Df$Fwcollection,decreasing = TRUE),]->fcdf
data.frame("Movie Name"=fcdf$Movie_Name,"F Collection"=fcdf$Fwcollection)->fcdf5
#table:
fcdf5[1:5,]->fcdf6
as.data.frame(fcdf6)->fcdf6
fcdf6

#Top 5 Movie accrding to T Collection :
Bollywood_Movie_Data_Df[order(Bollywood_Movie_Data_Df$Tcollection,decreasing = TRUE),]->tcdf
data.frame("Movie Name"=tcdf$Movie_Name,"T Collections"=tcdf$Tcollection)->tcdf5
#table:
tcdf5[1:5,]->tcdf6
as.data.frame(tcdf6)->tcdf6
tcdf6


########Graph:#####################
grid_col<-matrix(1:4 ,ncol = 4)
layout(grid_col)
#o collections
og<-ggplot(ocdf6,aes(x=ocdf6$Movie.Name,y=ocdf6$O.Collection,fill=Movie.Name))+geom_bar(stat = "identity",position="dodge")+ylab("O Collection")+xlab("Movie Name")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ ggtitle("Movie Name  vs O collections")

#W collection
wg<-ggplot(wcdf6,aes(x=wcdf6$Movie.Name,y=wcdf6$w.Collection,fill=Movie.Name))+geom_bar(stat = "identity",position="dodge")+ylab("W Collection")+xlab("Movie Name")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ ggtitle("Movie Name  vs W collections")

#F cOllection
fg<-ggplot(fcdf6,aes(x=fcdf6$Movie.Name,y=fcdf6$F.Collection,fill=Movie.Name))+geom_bar(stat = "identity",position="dodge")+ylab("F Collection")+xlab("Movie Name")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ ggtitle("Movie Name  vs F collections")

#T collection
tg<-ggplot(tcdf6,aes(x=tcdf6$Movie.Name,y=tcdf6$T.Collections,fill=Movie.Name))+geom_bar(stat = "identity",position="dodge")+ylab("T Collection")+xlab("Movie Name")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ ggtitle("Movie Name  vs T collections")


library(gridExtra)

grid.arrange(og,wg,fg,tg,ncol=2)






















