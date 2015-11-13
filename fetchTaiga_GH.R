library(httr)
library(jsonlite)
if(file.exists("c:/Users/Suhas Xavier/Desktop/Taiga_GH_DF.csv"))
{
  file.remove("c:/Users/Suhas Xavier/Desktop/Taiga_GH_DF.csv")
}
if(file.exists("c:/Users/Suhas Xavier/Desktop/notification_table.csv"))
{
  file.remove("c:/Users/Suhas Xavier/Desktop/notification_table.csv")
}
notifi_dat=data.frame(user_name=as.character(),notification=as.character())
write.csv(notifi_dat,file="c:/Users/Suhas Xavier/Desktop/notification_table.csv",row.names = F)
towrite_df=data.frame(Week=as.character(),Additions=as.numeric(),Deletions=as.numeric(),Commits=as.numeric(),GH_Handle=as.numeric(), Weight=as.numeric())
write.csv(towrite_df,file="c:/users/Suhas Xavier/Desktop/Taiga_GH_DF.csv",row.names = F)
closeAllConnections()
gh_url="https://api.github.com/repos/ser515asu/"
add_to_gh_url="/stats/contributors"
member_file=read.csv("c:/users/Suhas Xavier/Desktop/membership_table.csv")
teams_all=member_file$name
gh_df_taiga=data.frame(c(Week=as.character(),LinesOfCode=as.numeric(),Deletions=as.numeric(),Commits=as.numeric(),GitHandle=as.character()))
for(i in 1:length(unique(teams_all)))
{
  tempdf_1=member_file[member_file$name==unique(teams_all)[i],]
  temp_a=paste(gh_url,unique(teams_all)[i],sep="")
  temp_b=paste(temp_a,add_to_gh_url,sep="")
  print(temp_b)
  data_gh=fromJSON(temp_b)
  while(length(data_gh[["author"]][[1]])<1)
  {
    data_gh=fromJSON(temp_b)
  }
  print(data_gh)
  gh_handles=tempdf_1$GH_Handle
  for(j in 1:length(gh_handles))
      {
    print(gh_handles[j])
    uname_user=as.character(tempdf_1[tempdf_1$GH_Handle==gh_handles[j],"username"])
    user_tname=as.character(tempdf_1[tempdf_1$GH_Handle==gh_handles[j],"name"])
        for(k in 1:length(data_gh[["author"]][[1]]))
        {
          if(data_gh[["author"]][[1]][k]==gh_handles[j])
          {
            gh_df_taiga=data.frame(data_gh[["weeks"]][k],gh_handles[j])
            gh_df_taiga$w=substr(as.POSIXct(gh_df_taiga$w,tz="America/Phoenix",origin="1970-01-01"),6,10)
            
            #Calculate weight
            for(l in 1:nrow(gh_df_taiga))
            {
              #if LoCA <= LoCD
              if(gh_df_taiga[l,3] >= gh_df_taiga[l,2])
              {
                gh_df_taiga[l,6]=1
                t_df=data.frame(c(as.character(uname_user)),paste(gh_df_taiga[l,1],"NO significant code added!!"))
                write.table(t_df,file="c:/Users/Suhas Xavier/Desktop/notification_table.csv",row.names = F,col.names = F,append = T,sep=",")
                rm(t_df)
              }
              #ideal scenario
              else if(gh_df_taiga[l,2]>500 & gh_df_taiga[l,2]-gh_df_taiga[l,3] >100 & gh_df_taiga[l,4]>=3)
              {
                gh_df_taiga[l,6]=5
                t_df=data.frame(c(as.character(uname_user)),paste(gh_df_taiga[l,1],"Great, Consistent GH activity!!"))
                write.table(t_df,file="c:/Users/Suhas Xavier/Desktop/notification_table.csv",row.names = F,col.names = F,append = T,sep=",")
                rm(t_df)
              }
              # LoCA > 300 
              else if(gh_df_taiga[l,2]<=500 & gh_df_taiga[l,2]>=200)
              {
                gh_df_taiga[l,6]=4
                t_df=data.frame(c(as.character(uname_user)),paste(gh_df_taiga[l,1],"Good GH activity!!"))
                write.table(t_df,file="c:/Users/Suhas Xavier/Desktop/notification_table.csv",row.names = F,col.names = F,append = T,sep=",")
                rm(t_df)
              }
              else if(gh_df_taiga[l,2]<200 & gh_df_taiga[l,2]>=100)
              {
                gh_df_taiga[l,6]=3
                t_df=data.frame(c(as.character(uname_user)),paste(gh_df_taiga[l,1],"Need to write more code!!"))
                write.table(t_df,file="c:/Users/Suhas Xavier/Desktop/notification_table.csv",row.names = F,col.names = F,append = T,sep=",")
                rm(t_df)
              }
              else if(gh_df_taiga[l,2]>500 & gh_df_taiga[l,4]<=3)
              {
                gh_df_taiga[l,6]=3
                t_df=data.frame(c(as.character(uname_user)),paste(gh_df_taiga[l,1],"Need more GH activity"))
                write.table(t_df,file="c:/Users/Suhas Xavier/Desktop/notification_table.csv",row.names = F,col.names = F,append = T,sep=",")
                rm(t_df)
              }
              else if(gh_df_taiga[l,2]<=10)
              {
                gh_df_taiga[l,6]=0
                t_df=data.frame(c(as.character(uname_user)),paste(gh_df_taiga[l,1],"NO GH activity!!"))
                write.table(t_df,file="c:/Users/Suhas Xavier/Desktop/notification_table.csv",row.names = F,col.names = F,append = T,sep=",")
                rm(t_df)
              }
              else if(gh_df_taiga[l,2]<=100 & gh_df_taiga[l,2]>10)
              {
                gh_df_taiga[l,6]=2
                t_df=data.frame(c(as.character(uname_user)),paste(gh_df_taiga[l,1],"Very Low GH activity!!"))
                write.table(t_df,file="c:/Users/Suhas Xavier/Desktop/notification_table.csv",row.names = F,col.names = F,append = T,sep=",")
                rm(t_df)
              }
              #Calculate frequency
              if(gh_df_taiga[l,4] >= 3)
              {
                gh_df_taiga[l,7]=5
              }
              
              else if(gh_df_taiga[l,4]==2)
              {
                gh_df_taiga[l,7]=3.5
              }
              else if(gh_df_taiga[l,4] == 1)
              {
                gh_df_taiga[l,7]=2
              }
              else if(gh_df_taiga[l,4] == 0)
              {
                gh_df_taiga[l,7]=0
              }
            }
            gh_df_taiga$a=(gh_df_taiga$a/1000)
            gh_df_taiga$d=(gh_df_taiga$d/100)
            gh_df_taiga$TeamName=as.character(unique(teams_all)[i])
            write.table(gh_df_taiga,file="C:/Users/Suhas Xavier/Desktop/Taiga_GH_DF.csv",row.names = F,col.names = F,sep=",",append = T)
            
            
          }
          #rm(data_gh)
        }
   
    closeAllConnections()
      }
}
closeAllConnections()