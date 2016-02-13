  m_table=read.csv("c:/Users/Suhas Xavier/Desktop/taiga_membership_table.csv")
  taiga_table=read.csv("c:/Users/Suhas Xavier/Desktop/taiga_data1.csv")
  usernames=m_table$email
  teamnames=m_table$project_name
  
  #Calculate individual weight

  #different grading schmes for different modes (Online and f2f)
  for(i in 1:length(unique(usernames)))
  {
    tname=as.character(m_table[m_table$email==usernames[i],"project_name"])
    this_user_data=taiga_table[taiga_table$email==as.character(usernames[i]),]
  dates=as.character(tail(this_user_data$date,1))
    course_val=as.character(m_table[m_table$email==usernames[i],"course"])
    #for f2f)
    if(course_val=="CST316-F2F")
    {
      if(nrow(this_user_data)>=7)
      {
        exp_val=3
      #diff gives difference of all values, the unique values indicate the actual changes, sum them up and average over 3
      df2=tail(this_user_data,7)
      inp1=length(unique(diff(df2$in_progress)))-1
      tot1=length(unique(diff(df2$to_test)))-1
      tot_len=(sum(inp1,tot1))
      msg=""
      fin_score=0
      if(tot_len<=1)
      {
        fin_score=0
        msg=paste(dates,"NO Taiga Activity!!",sep=" ")
      }
      else if(tot_len>1 & tot_len<=2)
      {
        fin_score=3
        msg=paste(dates,"Need more Taiga Activity!!",sep=" ")
      }
      else if(tot_len>2 & tot_len<=3)
      {
        fin_score=5
        msg=paste(dates,"Consistent Taiga Activity",sep=" ")
      }
      else if(tot_len>3)    
        {
        fin_score=3
        msg=paste(dates,"Too many tasks assigned to you!",sep=" ")
      }
  
        df_temp=data.frame(usernames[i],msg)
        df_holder=data.frame(dates,usernames[i],fin_score,tname,exp_val)
        print(df_holder)
       write.table(df_holder,file="C:/Users/Suhas Xavier/Desktop/Taiga_Weight.csv",row.names = F,col.names = F,sep=",",append = T,na="0")
      write.table(df_temp,file="C:/Users/Suhas Xavier/Desktop/notification_table.csv",row.names = F,col.names = F,sep=",",append = T)
      #  print(df_holder)
        print(df_temp)
    }
  }
    
    #for online
   else if(course_val=="CST316 - Online")
    {
      if(nrow(this_user_data)>=7)
      {
        exp_val=2
      #diff gives difference of all values, the unique values indicate the actual changes, sum them up and average over 3
      df2=tail(this_user_data,7)
      inp1=length(unique(diff(df2$in_progress)))-1
      tot1=length(unique(diff(df2$to_test)))-1
      tot_len=(sum(inp1,tot1))
      msg=""
      fin_score=0
      if(tot_len<=1)
      {
        fin_score=0
        msg=paste(dates,"NO Taiga Activity!!",sep=" ")
      }
      else if(tot_len>1 & tot_len<2)
      {
        fin_score=3
        msg=paste(dates,"Need more Taiga Activity!!",sep=" ")
      }
      else if(tot_len>=2 & tot_len<=3)
      {
        fin_score=5
        msg=paste(dates,"Consistent Taiga Activity!!",sep=" ")
      }
      else if(tot_len>3)    {
        fin_score=3
        msg=paste(dates,"Too many tasks assigned to you!",sep=" ")
      }
      
      df_temp=data.frame(usernames[i],msg)
      df_holder=data.frame(dates,usernames[i],fin_score,tname,exp_val)
      print(df_holder)
      write.table(df_holder,file="C:/Users/Suhas Xavier/Desktop/Taiga_Weight.csv",row.names = F,col.names = F,sep=",",append = T,na="0")
      write.table(df_temp,file="C:/Users/Suhas Xavier/Desktop/notification_table.csv",row.names = F,col.names = F,sep=",",append = T)
      #  print(df_holder)
      print(df_temp)
      }
    }
  }
closeAllConnections()