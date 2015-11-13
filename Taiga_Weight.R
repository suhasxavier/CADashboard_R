  m_table=read.csv("/home/sxavier2/all_files/membership_table.csv")
  taiga_table=read.csv("/home/sxavier2/all_files/taiga_data.csv")
  usernames=m_table$username
  teamnames=m_table$name
  #Calculate individual weight
  tmpdf=data.frame(Date=as.character(),username=as.numeric(), Score=as.numeric())
  tmpdf1=data.frame(user_name=as.numeric(),notification=as.character())
  if(!file.exists("/home/sxavier2/all_files/Taiga_Weight.csv"))
  {
    write.csv(tmpdf,file="/home/sxavier2/all_files/Taiga_Weight.csv",row.names = F,col.names = F)
  }
  if(!file.exists("/home/sxavier2/all_files/notification_table.csv"))
  {
    write.csv(tmpdf1,file="/home/sxavier2/all_files/notification_table.csv",row.names = F,col.names = F,sep=",")
  }
  for(i in 1:length(unique(usernames)))
  {
    tname=as.character(m_table[m_table$username==usernames[i],"name"])
    this_user_data=taiga_table[taiga_table$X==as.character(usernames[i]),]
    dates=max(as.character(this_user_data$Date))
    if(nrow(this_user_data)<5)
    {
      return
    }
    df2=tail(this_user_data,5)
    inp1=length(unique(diff(df2$InProgress)))
    tot1=length(unique(diff(df2$ToTest)))
    don1=length(unique(diff(df2$Done)))
    tot_len=(sum(inp1,tot1,don1))/3
    msg=""
    fin_score=0
    if(tot_len==0)
    {
      fin_score=0
      msg=paste(dates,"No Taiga Activity!",sep=" ")
    }
    if(tot_len>0 & tot_len<=1)
    {
      fin_score=2
      msg=paste(dates,"Very Low Taiga Activity",sep=" ")
    }
    if(tot_len>1 & tot_len<=2)
    {
      fin_score=3.5
      msg=paste(dates,"Need more Taiga Activity",sep=" ")
    }
    if(tot_len>2 & tot_len<=3)
    {
      fin_score=4.5
      msg=paste(dates,"Consistent Taiga Activity",sep=" ")
    }
    if(tot_len>3)
    {
      fin_score=5
      msg=paste(dates,"Excellent Taiga Activity",sep=" ")
    }

      df_temp=data.frame(usernames[i],msg)
      df_holder=data.frame(dates,usernames[i],fin_score,tname)
      print(df_holder)
     write.table(df_holder,file="/home/sxavier2/all_files/Taiga_Weight.csv",row.names = F,col.names = F,sep=",",append = T,na="0")
    write.table(df_temp,file="/home/sxavier2/all_files/notification_table.csv",row.names = F,col.names = F,sep=",",append = T)
      print(df_holder)
      print(df_temp)
  }
  
  tmpdf3=data.frame(Date=as.character(),user_mean=as.numeric(), team_mean=as.numeric(),class_mean=as.numeric(),username=as.character())
  if(file.exists("/home/sxavier2/all_files/Taiga_mean.csv"))
  {
    file.remove("/home/sxavier2/all_files/Taiga_mean.csv")
    write.csv(tmpdf3,file="/home/sxavier2/all_files/Taiga_mean.csv",row.names = F,col.names = F,sep=",")
  }
  tg_wt=read.csv("/home/sxavier2/all_files/Taiga_Weight.csv")
  tg_wt_date=unique(tg_wt$Date)
  u_tg_username=unique(as.character(tg_wt$username))
  for(k in 1:length(u_tg_username))
  {
    tg_team_dt=tg_wt[tg_wt$username==as.character(u_tg_username[k]),]
    tg_teamn=unique(as.character(tg_team_dt$Team))
      for(j in 1:length(tg_wt_date))
      {
        if(nchar(as.character(tg_wt_date[j]))>1)
        {
          tg_wt_curr_date_df=tg_wt[tg_wt$Date==tg_wt_date[j],]
          class_mean=mean(tg_wt_curr_date_df$Weight)
          tg_wt_curr_date_team_df=tg_wt_curr_date_df[tg_wt_curr_date_df$Team==tg_teamn,]
          team_mean=mean(tg_wt_curr_date_team_df$Weight)
          user_mean=as.numeric(tg_wt_curr_date_team_df[tg_wt_curr_date_team_df$username==u_tg_username[k],"Weight"])
          print(as.character(tg_wt_date[j]))
          print(user_mean)
          print(team_mean)
          print(class_mean)
          print(as.character(u_tg_username[k]))
          tg_mean_temp_df=data.frame(Date=as.character(tg_wt_date[j]),user_mean,team_mean,class_mean,username=as.character(u_tg_username[k]))
         write.table(tg_mean_temp_df,file="/home/sxavier2/all_files/Taiga_mean.csv",row.names = F,col.names = F,append = T,sep=",",na="0")
          print(tg_mean_temp_df)
          rm(tg_mean_temp_df)
        }
      }
    }
  
  
  closeAllConnections()