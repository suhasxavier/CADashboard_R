#@#%$@$%##$^#$^#$^#^#
gi_data=read.csv("c:/Users/Suhas Xavier/Desktop/taiga_data1.csv")
m_table=read.csv("c:/users/Suhas Xavier/Desktop/taiga_membership_table.csv")
eval_date=as.character(tail(gi_data,1)$date)
all_users=unique(as.character(gi_data$email))
for(i in 1:length(all_users))
{
  curr_user=all_users[i]
  curr_teamname=unique(as.character(m_table[m_table$email==curr_user,"project_name"]))
  course_name_user=as.character(unique(m_table[m_table$email==curr_user,"course"]))
  user_data_all=gi_data[gi_data$email==curr_user,]
  this_week_data=tail(user_data_all,7)
  if(length(curr_teamname)>0)
  {
  if(course_name_user=="CST316 - Online" )
  {
    exp_comm=2
    exp_loca=150
    this_week_loca=as.numeric(sum(this_week_data$loca))
    this_week_locd=as.numeric(sum(this_week_data$locd))
    this_week_commit=as.numeric(sum(this_week_data$commits))
    
      #begin weight calcualtions
      impact_score=0
      frequency_score=0
      msg=""
      
      if(this_week_loca <= this_week_locd)
      {
        impact_score=2
        msg=paste(eval_date,"NO significant code added!!",sep=" ")
      }
      
      else if(this_week_loca>=250 & this_week_loca-this_week_locd > 50 & this_week_commit>=2)
        {
          impact_score=5
          msg=paste(eval_date, "Great, Consistent GH activity!!",sep = " ")
      }
      else if(this_week_loca >=250)
      {
        impact_score=5
        msg=paste(eval_date,"Good GH activity!!",sep=" ")
      }
      else if(this_week_loca>=100 & this_week_loca<250)
      {
        impact_score=3
        msg=paste(eval_date, "Need to write more code!!", sep=" ")
      }
      else if(this_week_loca<100 & this_week_loca>10)
      {
        impact_score=2
        msg=paste(eval_date,"Very Low GH activity!!",sep=" ")
      }
      else if(this_week_loca<=10)
      {
        impact_score=0
        msg=paste(eval_date,"NO GH activity!!",sep=" ")
      }
      
      
      #calculate frequency score
      if(this_week_commit>=2)
      {
        frequency_score=5
      }
      else if(this_week_commit==1)
      {
        frequency_score=3.5
      }
      else if(this_week_commit==0)
      {
        frequency_score==0
      }
      calc_df=data.frame(eval_date,curr_user,curr_teamname,impact_score,frequency_score,this_week_commit,exp_comm,this_week_loca,exp_loca)
      print(calc_df)
      write.table(calc_df,file="c:/Users/Suhas Xavier/Desktop/GH_Taiga_Weight.csv",row.names = F,col.names = F,sep=",",append = T)
      noti_table_data=data.frame(curr_user,msg)
      write.table(noti_table_data,file="c:/Users/Suhas Xavier/Desktop/notification_table.csv",row.names = F,col.names =F,sep=",",append = T)
  }
  
  else if(course_name_user=="CST316-F2F")
  {
    exp_comm=3
    exp_loca=200
    this_week_loca=as.numeric(sum(this_week_data$loca))
    this_week_locd=as.numeric(sum(this_week_data$locd))
    this_week_commit=as.numeric(sum(this_week_data$commits))
    
    #begin weight calcualtions
    impact_score=0
    frequency_score=0
    msg=""
    if(this_week_loca <= this_week_locd)
    {
      impact_score=2
      msg=paste(eval_date,"NO significant code added!!",sep=" ")
    }
    
    else if(this_week_loca>=200 & this_week_loca-this_week_locd > 50 & this_week_commit>=2)
    {
      impact_score=5
      msg=paste(eval_date, "Great, Consistent GH activity!!",sep = " ")
    }
    else if(this_week_loca >=200)
    {
      impact_score=5
      msg=paste(eval_date,"Good GH activity!!",sep=" ")
    }
    else if(this_week_loca>=100 & this_week_loca<200)
    {
      impact_score=3.5
      msg=paste(eval_date, "Need to write more code!!", sep=" ")
    }
    else if(this_week_loca < 100 & this_week_loca > 10)
    {
      impact_score=2
      msg=paste(eval_date,"Very Low GH activity!!",sep=" ")
    }
    else if(this_week_loca<=10)
    {
      impact_score=0
      msg=paste(eval_date,"NO GH activity!!",sep=" ")
    }
    
    #calculate frequency score
    if(this_week_commit>=3)
    {
      frequency_score=5
    }
    else if(this_week_commit==2)
    {
      frequency_score=3.5
    }
    else if(this_week_commit==1)
    {
      frequency_score==2
    }
    else if(this_week_commit==0)
    {
      frequency_score==0
    }
    calc_df=data.frame(eval_date,curr_user,curr_teamname,impact_score,frequency_score,this_week_commit,exp_comm,this_week_loca,exp_loca)
    print(calc_df)
    write.table(calc_df,file="c:/Users/Suhas Xavier/Desktop/GH_Taiga_Weight.csv",row.names = F,col.names = F,sep=",",append = T)
    noti_table_data=data.frame(curr_user,msg)
    write.table(noti_table_data,file="c:/Users/Suhas Xavier/Desktop/notification_table.csv",row.names = F,col.names =F,sep=",",append = T)
  }
  }
}