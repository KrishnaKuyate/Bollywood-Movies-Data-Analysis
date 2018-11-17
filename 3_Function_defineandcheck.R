
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
Top_act_for_collection_g(acto_coll_df$O_Collection_a,acto_coll_df$Actor_cl,"Actor Name Vs O Collection","O Collection")



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

Top_act_for_collection_t(acto_coll_df$O_Collection_a,acto_coll_df$Actor_cl)
