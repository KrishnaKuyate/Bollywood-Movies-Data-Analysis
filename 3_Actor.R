#%Part :5
library(plotrix)

##Total No .of  actors work in 2015:

Bollywood_Movie_Data_Df$Acter_Name
na.omit(Bollywood_Movie_Data_Df$Acter_Name)->Actors
Actors_u<-unique(Actors)
length(Actors_u)
paste0("No. of actors work in 2015 :",length(Actors_u))

##Actor name list:
Actors_u

##Actor  and Movies Count:
Bollywood_Movie_Data_Df%>%count(Bollywood_Movie_Data_Df$Acter_Name)->acto_mov_c
as.data.frame(acto_mov_c)->acto_mov_c
colnames(acto_mov_c)<-c("Actor Name","Total Movies")
#Table:
acto_mov_c


##Top Actor  according to movies count
acto_mov_c[order(acto_mov_c$`Total Movies`,decreasing = TRUE),]->acto_mov_c5
as.data.frame(acto_mov_c5)->acto_mov_c51
#Table:
acto_mov_c51[1,]

#Top 5 Actor accroding to movies count:
#table:
acto_mov_c51[1:5,]

#Graph:
acto_mov_c51[1:5,]->acto_mov_c5_df
as.data.frame(acto_mov_c5_df)->acto_mov_c5_df

par(col.main="blue")
pie3D(acto_mov_c5_df$`Total Movies`,radius=0.8,height=0.1,theta=pi/3,main="Top 5 Actor  Movies Analysis:",labels=acto_mov_c5_df$`Total Movies`,explode=0.2,shade=0.6)
legend("topright",legend=acto_mov_c5_df$`Actor Name`,fill=acto_mov_c5_df$`Total Movies`,cex = 0.6)

##Top 5 Actor and collections:
Bollywood_Movie_Data_Df$Acter_Name
Bollywood_Movie_Data_Df$Ocollection
Bollywood_Movie_Data_Df$Wcollection
Bollywood_Movie_Data_Df$Fwcollection
Bollywood_Movie_Data_Df$Tcollection

acto_coll_df<-data.frame(Actor_cl=Bollywood_Movie_Data_Df$Acter_Name,
           O_Collection_a=Bollywood_Movie_Data_Df$Ocollection,
           w_Collection_a=Bollywood_Movie_Data_Df$Wcollection,
           F_Collection_a=Bollywood_Movie_Data_Df$Fwcollection,
           T_Collection_a=Bollywood_Movie_Data_Df$Tcollection)


#Table Function :
Top_act_for_collection_t<-function(Collection_type,agg_by)
{
  
  aggregate(Collection_type,by=list(agg_by),FUN=sum)->aff_col_df
  as.data.frame(aff_col_df)->aff_col_df
  aff_col_df[order(aff_col_df$x,decreasing = TRUE),]->aff_col_df5
  aff_col_df5[1:5,]->aff_col_df5_t
  as.data.frame(aff_col_df5_t)->aff_col_df5_tdf
  colnames(aff_col_df5_tdf)<-c("Actor","Total Collection 2015")
  #Table:
  aff_col_df5_tdf
  
}


#Graph Function :
Top_act_for_collection_g<-function(Collection_type,agg_by,title,yaxix)
{
  
  aggregate(Collection_type,by=list(agg_by),FUN=sum)->aff_col_df
  as.data.frame(aff_col_df)->aff_col_df
  aff_col_df[order(aff_col_df$x,decreasing = TRUE),]->aff_col_df5
  aff_col_df5[1:5,]->aff_col_df5_t
  as.data.frame(aff_col_df5_t)->aff_col_df5_tdf
  colnames(aff_col_df5_tdf)<-c("Actor","Total Collection 2015")
  #Table:
  aff_col_df5_tdf
  #Graph:
  
  graph_ti=title
  yaxis_g=yaxix
  ggplot(aff_col_df5_tdf,aes(x=Actor,y=`Total Collection 2015`,fill=Actor))+ geom_bar(stat = "identity",position="dodge")+ggtitle(graph_ti)+theme(plot.title = element_text(face = "bold.italic"))+xlab("Actor Name")+ylab(yaxis_g)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

###Top 5 Actor and o collections:

#Table call
Top_act_for_collection_t(acto_coll_df$O_Collection_a,acto_coll_df$Actor_cl)

#Graph call
Top_act_for_collection_g(acto_coll_df$O_Collection_a,acto_coll_df$Actor_cl,"Actor Name Vs O Collection","O Collection")


###Top 5 Actor and f collections:
#Table call
Top_act_for_collection_t(acto_coll_df$F_Collection_a,acto_coll_df$Actor_cl)
#Graph call
Top_act_for_collection_g(acto_coll_df$F_Collection_a,acto_coll_df$Actor_cl,"Actor Name Vs F Collection","F Collection")

###Top 5 Actor and w collections:
#Table 
Top_act_for_collection_t(acto_coll_df$w_Collection_a,acto_coll_df$Actor_cl)
#Graph call
Top_act_for_collection_g(acto_coll_df$F_Collection_a,acto_coll_df$Actor_cl,"Actor Name Vs W Collection","W Collection")

###Top 5 Actor and t collections:
#Table call
Top_act_for_collection_t(acto_coll_df$T_Collection_a,acto_coll_df$Actor_cl)
#Graph call
Top_act_for_collection_g(acto_coll_df$T_Collection_a,acto_coll_df$Actor_cl,"Actor Name Vs T Collection","T Collection")





























