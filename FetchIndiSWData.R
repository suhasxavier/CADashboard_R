library(jsonlite)
dat_json_temp=fromJSON("c:/Users/Suhas Xavier/Desktop/SW/SW_DATA.json")
mem_table=read.csv("c:/users/Suhas Xavier/Desktop/sw_membership_table.csv")
#today_date="02-10"
today_date=substr(Sys.Date(),6,10)
temp_sw_df=data.frame(date=as.character(),email=as.character(),team=as.character(),sw_id=as.character(),status=as.character(),stringsAsFactors = F)
class_teams=unique(as.character(mem_table$project_name))
for(i in 1:length(class_teams))
{
  curr_proj_name=class_teams[i]
  print(curr_proj_name)
  for(j in 1:length(dat_json_temp$result$projects$name))
  {
    if(curr_proj_name==dat_json_temp$result$projects$name[[j]])
    {
      print(curr_proj_name)
      curr_students=as.character(mem_table[mem_table$project_name==curr_proj_name,"sw_id"])
      for(k in 1:length(curr_students))
      {
        curr_stud=curr_students[k]
        curr_team=as.character(mem_table[mem_table$sw_id==curr_stud,"project_name"])
        curr_email=as.character(mem_table[mem_table$sw_id==curr_stud,"email"])
        backlog_item_len=length(dat_json_temp$result$projects$backlogItems[[j]])
        if(backlog_item_len>1)
        {
        for(l in 1:length(backlog_item_len))
        {
          if(length(dat_json_temp$result$projects$backlogItems[[j]]$tasks)>1)
          {
          task_len=length(dat_json_temp$result$projects$backlogItems[[j]]$tasks)
          print(task_len)
          for(m in 1:task_len)
          {
            curr_task=dat_json_temp$result$projects$backlogItems[[j]]$tasks[[m]]
            print(curr_task)
            if(length(curr_task)>1)
            {
              curr_task_length=length(curr_task$id)
              print(curr_task_length)
              for(n in 1:curr_task_length)
              {
                if(!length(curr_task$assignedPersonIDs[[n]])==0)
                {
                if(curr_task$assignedPersonIDs[[n]]==curr_stud)
                {
                  temp_sw_df[nrow(temp_sw_df)+1,]=c(today_date,curr_email,curr_team,curr_stud,curr_task$status[[n]])
                }
                }
              }
            }
          }
        }
        }
        }
        }
      }
    }
}
inp_count=0
tot_count=0
done_count=0
tt1=unique(temp_sw_df$sw_id)
for(x in 1:length(tt1))
{
  pp12=temp_sw_df[temp_sw_df$sw_id==tt1[x],]
  inp_count=sum(pp12$status=="In progress")
  tot_count=sum(pp12$status=="To do")
  done_count=sum(pp12$status=="Done")
  pp2=data.frame(unique(pp12$date),unique(pp12$email),unique(pp12$team),inp_count,tot_count,done_count)
  write.table(pp2,file="c:/users/Suhas Xavier/Desktop/sw_data.csv",row.names = F,col.names = F,sep=",",append = T)
}
closeAllConnections()
gc()