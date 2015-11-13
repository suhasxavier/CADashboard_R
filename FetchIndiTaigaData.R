library(httr)
mem_file=read.csv("/home/sxavier2/all_files/membership_table.csv")
tsk_uuid_path="https://api.taiga.io/api/v1/tasks/csv?uuid="
issue_uuid_path="https://api.taiga.io/api/v1/issues/csv?uuid="
taiga_dat=data.frame(InProgress=as.numeric(),ToTest=as.numeric(),Done=as.numeric(),Ready=as.numeric(),New=as.numeric(),Archived=as.numeric(),Date=as.numeric())
if(!file.exists("/home/sxavier2/all_files/taiga_data.csv"))
{
  file.create("/home/sxavier2/all_files/taiga_data.csv")
  write.csv(taiga_dat,"/home/sxavier2/all_files/taiga_data.csv",row.names = F)
}
teams=mem_file$name
for(i in 1:length(unique(teams)))
{
  tempdf=mem_file[mem_file$name==unique(teams)[i],]
  team_name=unique(tempdf$name)
  team_tsk_uuid=unique(tempdf$tasks_csv_uuid)
  team_iss_uuid=unique(tempdf$issues_csv_uuid)
  tsk_pth=paste(tsk_uuid_path,team_tsk_uuid,sep="")
  team_dat=read.csv(tsk_pth,na.strings = "NA")
  student_unames=tempdf$username
  #print(nrow(team_dat))
  #print(team_name)
 # if(nrow(team_dat)>1)
  #{
    for(j in 1:length(student_unames))
    {
      #print("going in")
      #print(length(unique(teams)))
      #print(length(student_unames))
      print(team_name)
      final_table=data.frame(InProgress=as.numeric(),ToTest=as.numeric(),Done=as.numeric(),Ready=as.numeric(),New=as.numeric(),Archived=as.numeric())
      print(student_unames[j])
      tempdf1=team_dat[!team_dat$assigned_to=="",]
      #print(tempdf1)
      tempdf2=tempdf1[tempdf1$assigned_to==as.character(student_unames[j]),]
      #print(tempdf2)
      final_table[nrow(final_table)+1,]=c(sum(tempdf2$status=="In progress"),sum(tempdf2$status=="Ready for test"),sum(tempdf2$status=="Closed"),sum(tempdf2$status=="Ready"),sum(tempdf2$status=="New"),sum(tempdf2$status=="Archived"))
      #fetch student file
      final_table$Date=substr(Sys.Date(),6,10)
      final_table$User=as.character(student_unames[j])
      #t_holder=paste(student_unames[j],".csv",sep="")
      #indifile=paste(indi_file_path,t_holder,sep="")
      write.table(final_table,file="/home/sxavier2/all_files/taiga_data.csv",row.names = F, col.names = F, sep=",",append = T,na="0")
      #write.table(final_table,file=indifile,row.names = F, col.names = F, sep=",",append = T)
      #print(final_table)
      rm(final_table)
      #print(j)
    }
  #}
}
closeAllConnections()
