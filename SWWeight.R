m_table=read.csv("c:/Users/Suhas Xavier/Desktop/sw_membership_table.csv")
sw_table=read.csv("c:/Users/Suhas Xavier/Desktop/sw_data.csv")
usernames=m_table$email
#Calculate individual weight
tmpdf=data.frame(Date=as.character(),email=as.numeric(), Score=as.numeric())
if(!file.exists("C:/Users/Suhas Xavier/Desktop/sw_weight.csv"))
{
  write.csv(tmpdf,file="C:/Users/Suhas Xavier/Desktop/sw_weight.csv",row.names = F,col.names = F)
}
for(i in 1:length(unique(usernames)))
{
  tname=as.character(m_table[m_table$email==usernames[i],"project_name"])
  exp=3
  this_user_data=sw_table[sw_table$email==as.character(usernames[i]),]
  dates=as.character(tail(this_user_data$date,1))
  if(nrow(this_user_data)>=7)
  {
    #diff gives difference of all values, the unique values indicate the actual changes, sum them up and average over 3
    df2=tail(this_user_data,7)
    inp1=length(unique(diff(df2$in_pogress)))
    tot1=length(unique(diff(df2$to_test)))
    don1=length(unique(diff(df2$done)))
    tot_len=(sum(inp1,tot1,don1))/3
    msg=""
    fin_score=0
    if(tot_len<=1)
    {
      fin_score=1
      msg=paste(dates,"NO Scrumwise Activity!!",sep=" ")
    }
    else if(tot_len>1 & tot_len<=2)
    {
      fin_score=3
      msg=paste(dates,"Low Scrumwise Activity!!",sep=" ")
    }
    else if(tot_len>2 & tot_len<=3)
    {
      fin_score=5
      msg=paste(dates,"Good Scrumwise Activity!!",sep=" ")
    }
    else if(tot_len>3)    {
      fin_score=3
      msg=paste(dates,"Too amny tasks assigned to you!",sep=" ")
    }
    
    df_temp=data.frame(usernames[i],msg)
    df_holder=data.frame(dates,usernames[i],fin_score,tname,exp)
    print(df_holder)
    write.table(df_holder,file="C:/Users/Suhas Xavier/Desktop/sw_Weight.csv",row.names = F,col.names = F,sep=",",append = T,na="0")
    write.table(df_temp,file="C:/Users/Suhas Xavier/Desktop/notification_table.csv",row.names = F,col.names = F,sep=",",append = T)
    #  print(df_holder)
    print(df_temp)
  }
}
closeAllConnections()