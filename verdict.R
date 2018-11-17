#Part:7
#Verdit

Bollywood_Movie_Data_Df$Verdict

Bollywood_Movie_Data_Df%>%count(Bollywood_Movie_Data_Df$Verdict)->ver_df

as.data.frame(ver_df)->ver_df
colnames(ver_df)<-c("Movie Status","Total Count")

#Table :
ver_df

#Graph:

library(ggvis)
ver_df %>% ggvis(~`Movie Status`, ~`Total Count`,fill:="Blue", size := 600, opacity := 0.4) %>% layer_points()%>% add_axis("x",title = "Movie Success Status", title_offset = 50)%>%add_axis("y",title = "Total Movie Count", title_offset = 50)
