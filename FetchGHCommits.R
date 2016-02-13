library(httr)
branches_mem=read.csv("c:/Users/Suhas Xavier/Desktop/gh_branches.csv")
# For every branch retreieve every commit and committer details and write to a table

if(file.exists("c:/users/Suhas Xavier/Desktop/commit_data_temp.csv"))
{
  file.remove("c:/Users/Suhas Xavier/Desktop/commit_data_temp.csv")
}
header_df=data.frame(commit_id=as.character(),gh_id=as.character(),email=as.character(),date=as.character(),curl_text=as.character(),project_name=as.character(),course_name=as.character(),stringsAsFactors = F)
write.table(header_df,"c:/users/Suhas Xavier/Desktop/commit_data_temp.csv",row.names = F,col.names=T,sep=",")
closeAllConnections()

#fetch data
all_br_teams=as.character(unique(branches_mem$curl_text))
for(i in 1:length(all_br_teams))
{
  curr_team_br=all_br_teams[i]
  cproj=as.character(unique(branches_mem[branches_mem$curl_text==curr_team_br,"project_name"]))
  ccourse=unique(as.character(branches_mem[branches_mem$curl_text==curr_team_br,"course_name"]))
  temp_3=paste(curr_team_br,"commits?sha=",sep="/")
  curr_br_list=branches_mem[branches_mem$curl_text==curr_team_br,"branch_name"]
  for(j in 1:length(curr_br_list))
  {
    curr_br_sel=as.character(curr_br_list[j])
    temp_4=paste(temp_3,curr_br_sel,sep="")
    temp_5=GET(temp_4,add_headers(Authorization= "token 345d0c10e78787aa98f0f402936df61bd26dde4e"))
    all_commits=content(temp_5)
    for(k in 1:length(all_commits))
    {
      curr_commit_dat=all_commits[k]
      commit_no=as.character(curr_commit_dat[[1]]$sha)
      commiter_id=as.character(curr_commit_dat[[1]]$commit$committer$name)
      commiter_email=as.character(curr_commit_dat[[1]]$commit$committer$email)
      commit_date=as.Date(substr(curr_commit_dat[[1]]$commit$committer$date,6,10),"%m-%d")
      print(commiter_email)
      comm_df=data.frame(commit_no,commiter_id,commiter_email,commit_date,curr_team_br,cproj,ccourse)
      write.table(comm_df,"c:/users/Suhas Xavier/Desktop/commit_data_temp.csv",row.names = F,col.names = F,sep=",",append = T)
    }
    
  }
}
closeAllConnections()
gc()