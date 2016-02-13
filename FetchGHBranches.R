library(httr)
#fetch all the branches created in every repo using the format expected by GH
all_teams_mem=read.csv("c:/users/Suhas Xavier/Desktop/taiga_membership_table.csv")
get_br_format="https://api.github.com/repos/cst316/"
teams_now=unique(as.character(all_teams_mem$gh_repo))
if(file.exists("c:/users/Suhas Xavier/Desktop/gh_branches.csv"))
{
  file.remove("c:/Users/Suhas Xavier/Desktop/gh_branches.csv")
}

  file_head=data.frame(project_name=as.character(),course_name=as.character(),repo_name=as.character(),branch_name=as.character(),curl_text=as.character())
  write.table(file_head,"c:/users/Suhas Xavier/Desktop/gh_branches.csv",row.names = F, col.names = T,sep=",")
closeAllConnections()

for(i in 1:length(teams_now))
{
  curr_team_val=teams_now[i]
  projn=unique(as.character(all_teams_mem[all_teams_mem$gh_repo==curr_team_val,"project_name"]))
  coursen=unique(as.character(all_teams_mem[all_teams_mem$gh_repo==curr_team_val,"course"]))
  temp_val1=paste(get_br_format,curr_team_val,sep="")
  plus_br=paste(temp_val1,"branches",sep="/")
  temp_val2=GET(plus_br,add_headers(Authorization= "token 345d0c10e78787aa98f0f402936df61bd26dde4e"))
  br_curr_team=content(temp_val2)
  br_len=length(br_curr_team)
  for(j in 1:br_len)
  {
  branches_names=as.character(br_curr_team[[j]]$name)
  br_df=data.frame(projn,coursen,curr_team_val,branches_names,temp_val1)
  write.table(br_df,"c:/users/Suhas Xavier/Desktop/gh_branches.csv",row.names = F,col.names = F,sep=",",append = T)
  }
}
closeAllConnections()
gc()