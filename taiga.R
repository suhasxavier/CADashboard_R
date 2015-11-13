library(httr)
result= GET("https://api.taiga.io/api/v1/projects?member=67772",
            add_headers(Authorization= "Bearer eyJ1c2VyX2F1dGhlbnRpY2F0aW9uX2lkIjo2Nzc3Mn0:1Zeqqy:o-m8J-oBXy660H0K8zXuT2TXXIA"))
res_content=content(result)
# convert list to df (combine lists)
dat=data.frame(t(sapply(res_content,c)))
proj_ids=dat$id
#get project overview json to fetch names, ids and csv uuids
dataset=data.frame(Project_id=as.numeric(),Taiga_id=as.numeric(),Full_Name=as.character(),Team_Name=as.character(),E_mail=as.character(),username=as.character(),uc_csv=as.character(),iss_csv=as.character(),tsk_csv=as.character())
if(file.exists("C:/users/Suhas Xavier/Desktop/membership_table1.csv"))
{
  file.remove("C:/users/Suhas Xavier/Desktop/membership_table1.csv")
  write.table(dataset,"C:/users/Suhas Xavier/Desktop/membership_table1.csv",row.names = F,col.names = F)
}
proj_curl="https://api.taiga.io/api/v1/projects/"
for (i in 1:length(proj_ids))
     {
        projid=proj_ids[i]
        objval=paste(proj_curl,projid,sep="")
        res=GET(objval,add_headers(Authorization="Bearer eyJ1c2VyX2F1dGhlbnRpY2F0aW9uX2lkIjo2Nzc3Mn0:1Zeqqy:o-m8J-oBXy660H0K8zXuT2TXXIA"))
        res_data=content(res)
        dat1=data.frame(t(sapply(res_data,c)))
        #this calculates number of members
        member_count=as.numeric(sapply(dat1$memberships,length))
        #print(member_count)
        #temp df to accomodate all 
        for(j in 1:member_count)
        {
        Project_id=dat1$memberships[[1]][[j]]["project"]
        print(j)
        Taiga_id=dat1$memberships[[1]][[j]]["user"]
        Full_Name=dat1$memberships[[1]][[j]]["full_name"]
        print(Full_Name)
        E_mail=dat1$memberships[[1]][[j]]["email"]
        print(E_mail)
        username=dat1$memberships[[1]][[j]]["username"]
        print(username)
        Team_Name=dat1$name
        print(Team_Name)
        UC_CSV=dat1$userstories_csv_uuid
        print(UC_CSV)
        Task_CSV=dat1$tasks_csv_uuid
        print(Task_CSV)
        Issue_CSV=dat1$issues_csv_uuid
        print(Issue_CSV)
        temp_dataset1=data.frame(c(Project_id,Taiga_id,Full_Name,Team_Name,E_mail,username,UC_CSV,Task_CSV,Issue_CSV))
        dataset=rbind(dataset,temp_dataset1)
        rm(Project_id)
        rm(Taiga_id)
        rm(Full_Name)
        rm(E_mail)
        rm(Team_Name)
        rm(temp_dataset1)
        rm(member_count)
        rm(username)
        rm(UC_CSV)
        rm(Task_CSV)
        rm(Issue_CSv)
        }
     }
membership_table=dataset[!dataset$username=="deleted-user-1712" & !dataset$username=="ser515asu" &!dataset$username=="deleted-user-1740",]
write.table(membership_table,file="C:/users/Suhas Xavier/Desktop/membership_table1.csv",row.names = F,col.names = F,append = T)
closeAllConnections()