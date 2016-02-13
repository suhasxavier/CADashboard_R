library(httr)
mem_file=read.csv("c:/users/Suhas Xavier/Desktop/taiga_membership_table.csv")
tsk_uuid_path="https://api.taiga.io/api/v1/tasks/csv?uuid="
taiga_dat=data.frame(InProgress=as.numeric(),ToTest=as.numeric(),Done=as.numeric(),Ready=as.numeric(),New=as.numeric(),Archived=as.numeric(),Date=as.numeric())
if(!file.exists("c:/Users/Suhas Xavier/Desktop/taiga_data.csv"))
{
  file.create("c:/Users/Suhas Xavier/Desktop/taiga_data.csv")
  write.csv(taiga_dat,"c:/Users/Suhas Xavier/Desktop/taiga_data.csv",row.names = F)
}
teams=unique(as.character(mem_file$project_name))
for(i in 1:length(teams))
{
  tempdf=mem_file[mem_file$project_name==teams[i],]
  team_name=as.character(unique(tempdf$project_name))
  team_tsk_uuid=as.character(unique(tempdf$tasks_csv_uuid))
  tsk_pth=paste(tsk_uuid_path,team_tsk_uuid,sep="")
  team_dat=read.csv(tsk_pth,na.strings = "NA")
  student_unames=tempdf$taiga_fullname
  print(team_name)
    for(j in 1:length(student_unames))
    {
      #print("going in")
      final_table=data.frame(InProgress=as.numeric(),ToTest=as.numeric(),Done=as.numeric())
      print(student_unames[j])
      student_email=as.character(mem_file[mem_file$taiga_fullname==student_unames[j],"email"])
      tempdf1=team_dat[!team_dat$assigned_to=="",]
      tempdf2=tempdf1[tempdf1$assigned_to_full_name==as.character(student_unames[j]),]
      final_table[nrow(final_table)+1,]=c(sum(tempdf2$status=="In progress"),sum(tempdf2$status=="Ready for test"),sum(tempdf2$status=="Closed"))
      #fetch student file
      #pp1="02-12"
       #final_table$Date=pp1
      final_table$Date=substr(Sys.Date(),6,10)
      final_table$User=student_email
      print(final_table)
      write.table(final_table,file="C:/users/Suhas Xavier/Desktop/taiga_data.csv",row.names = F, col.names = F, sep=",",append = T,na="0")
      rm(final_table)
    }
}
closeAllConnections()