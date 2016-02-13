library(httr)
library(dplyr)

if(file.exists("c:/users/Suhas Xavier/Desktop/commit_data.csv"))
{
  file.remove("c:/Users/Suhas Xavier/Desktop/commit_data.csv")
}

file_head1=data.frame(date=as.character(),email=as.character(),commit_id=as.character(),loca=as.character(),locd=as.character(),project_name=as.character(),course_name=as.character())
write.table(file_head1,"c:/users/Suhas Xavier/Desktop/commit_data.csv", row.names = F,col.names = T,sep=",")
closeAllConnections()

all_commits_read=read.csv("/users/Suhas Xavier/Desktop/commit_data_temp.csv")
all_commits_mem=all_commits_read[!all_commits_read$gh_id=="kgary" & !all_commits_read$gh_id=="cst316" & !all_commits_read$gh_id=="test" & !all_commits_read$gh_id=="Kevin Gary",]
commit_id=as.character(unique(all_commits_mem$commit_id))


for(i in 1:length(commit_id))
{
  curr_commit_id=commit_id[i]
  p_name=unique(as.character(all_commits_mem[all_commits_mem$commit_id==curr_commit_id,"project_name"]))
  c_name=unique(as.character(all_commits_mem[all_commits_mem$commit_id==curr_commit_id,"course_name"]))
  stat_email=unique(as.character(all_commits_mem[all_commits_mem$commit_id==curr_commit_id,"email"]))
  stat_date=unique(as.character(all_commits_mem[all_commits_mem$commit_id==curr_commit_id,"date"]))
  commit_branch=as.character(unique(all_commits_mem[all_commits_mem$commit_id==curr_commit_id,"curl_text"]))
  temp_5=paste(commit_branch,"commits",sep="/")
  temp_6=paste(temp_5,curr_commit_id,sep="/")
  temp_7=GET(temp_6,add_headers(Authorization= "token 345d0c10e78787aa98f0f402936df61bd26dde4e"))
  commit_stats=content(temp_7)
  locA=commit_stats$stats$additions
  locD=commit_stats$stats$deletions
  print(stat_email)
  stat_df=data.frame(stat_date,stat_email,curr_commit_id,locA,locD,p_name,c_name)
  write.table(stat_df,"c:/users/Suhas Xavier/Desktop/commit_data.csv",row.names = F,col.names=F, sep=",",append = T)
}

closeAllConnections()
gc()

#data cleanse tasks begin!
commit_data=read.csv("c:/users/Suhas Xavier/Desktop/commit_data.csv")
commit_data$email=as.character(commit_data$email)
commit_data$email[commit_data$email=="david scott"]="dmscott5@asu.edu"
commit_data$email[commit_data$email=="dena tapia"]="dtapia4@asu.edu"

commit_data$email[commit_data$email=="hieu pham"]="hqpham@asu.edu"
commit_data$email[commit_data$email=="ghgoforth@asu.edu"]="ghgofort@asu.edu"
commit_data$email[commit_data$email=="ivkim5@gmail.com"]="ivkim@asu.edu"
commit_data$email[commit_data$email=="jackyliang@jackys-macbook-pro.local"]="jsliang@asu.edu"

commit_data$email[commit_data$email=="jessica.l.knotts@gmail.com"]="jlknotts@asu.edu"
commit_data$email[commit_data$email=="kyle.carey@openx.com"]="krcarey1@asu.edu"

commit_data$email[commit_data$email=="lajom@laptop-79ltgcmn.mobile.asu.edu"]="alajom@asu.edu"
commit_data$email[commit_data$email=="mgibson2020@yahoo.com"]="mdgibso2@asu.edu"
commit_data$email[commit_data$email=="miguel.zavala@asu.edu"]="mazaval4@asu.edu"
commit_data$email[commit_data$email=="robert.d.buss@gmail.com"]="rdbuss@asu.edu"

commit_data$email[commit_data$email=="t0mmy@asu.edu"]="temurph1@asu.edu"
commit_data$email[commit_data$email=="therealdavidhenderson@gmail.com"]="cghende1@asu.edu"

commit_data$email[commit_data$email=="travis dudzinski"]="tdudzins@asu.edu"
commit_data$email[commit_data$email=="adityasamant2505@gmail.com"]="ansamant@asu.edu"
commit_data$email=as.factor(commit_data$email)


if(file.exists("c:/users/Suhas Xavier/Desktop/GH_DF.csv"))
{
  file.remove("c:/Users/Suhas Xavier/Desktop/GH_DF.csv")
}
file_head2=data.frame(email=as.character(),date=as.character(),commits=as.character(),loca=as.character(),locd=as.character())
write.table(file_head2,"c:/users/Suhas Xavier/Desktop/GH_DF.csv", row.names = F,col.names = T,sep=",")
closeAllConnections()

c_data=tbl_df(commit_data)
c_count=c_data %>%
  group_by(email,date) %>%
  summarise(commits=n())
comdf=data.frame(c_count)

stat_count=c_data %>%
  group_by(email,date) %>%
  summarise_each(funs(sum),loca,locd)
statdf=data.frame(stat_count)

final_to_writedf=merge(comdf,statdf)

daily_df=data.frame(email=as.character(),date=as.character(),commits=as.character(),loca=as.character(),locd=as.character(),stringsAsFactors = F)

c_date=as.Date(substr(Sys.Date(),6,10),"%m-%d")
start_date=as.Date("01-25","%m-%d")
date_diff=as.numeric(c_date-start_date)-1
all_students=as.character(unique(final_to_writedf$email))
for(j in 0:date_diff)
{
  date_now=start_date+j
  for(k in 1:length(all_students))
  {
    curr_student=all_students[k]
    curr_student_dat=final_to_writedf[final_to_writedf$email==curr_student,]
    if(date_now %in% as.Date(curr_student_dat$date))
    {
      x1=data.frame(final_to_writedf[as.Date(final_to_writedf$date)==date_now & final_to_writedf$email==curr_student,])
      write.table(x1,"c:/users/Suhas Xavier/Desktop/GH_DF.csv",row.names = F,col.names = F,sep=",",append = T)
    }
    else
    {
      x2=data.frame(curr_student,as.character(date_now),0,0,0)
      write.table(x2,"c:/users/Suhas Xavier/Desktop/GH_DF.csv",row.names = F,col.names = F,sep=",",append = T)
    }
  }
}
gc()
closeAllConnections()