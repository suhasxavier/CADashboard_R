library(shiny)
library(shinydashboard)
library(rCharts)
library(shinyjs)
function(input, output,session)
{
  observe({
    e_id=input$emailid
    print(e_id)
    start_time=Sys.time()
    source_ip=session$clientData$url_hostname
    output$introbox=renderText("Welcome to the Continuous Assessment Dashboard for Software Engineering 
                               courses at ASU with Dr. Gary. Please login in with your registered email address to proceed")
    if(substr(e_id,nchar(e_id)-3,nchar(e_id))==".edu" || substr(e_id,nchar(e_id)-3,nchar(e_id))==".com")
    {
      print(e_id)
      taiga=function(eid_val)
      {
        taiga_user_mail=eid_val
        alldata=read.csv("/home/sxavier2/all_files/membership_table.csv")
        u_data=alldata[tolower(alldata$email)==taiga_user_mail,]
        if(nrow(u_data)>0)
        {
          print(u_data)
          u_gh_handle=as.character(u_data$GH_Handle)
          print(u_gh_handle)
          u_username=as.character(u_data$username)
          print(u_username)
          u_fullname=as.character(u_data$full_name)
          print(u_fullname)
          u_teamN=as.character(u_data$name)
          print(u_teamN)
          #path_to_ufile="C:/Users/Suhas Xavier/Desktop/Taiga_Files/"
          #add_csv_ext=paste(u_username,".csv",sep="")
          # u_file=read.csv("c:/users/Suhas Xavier/Desktop/taiga_data.csv",sep=",",row.names = NULL)
          u_file=read.csv("/home/sxavier2/all_files/taiga_data.csv",sep=",",row.names = NULL)
          u_all_taiga_data=u_file[u_file[8]==u_username,]
          #fetch dates to show in the drop down
          drop_down_val1=seq(1,nrow(u_all_taiga_data),by=7)
          drop_down_dates=as.character(u_all_taiga_data$Date[drop_down_val1])
          latest_date=drop_down_dates[length(drop_down_dates)]
          print(latest_date)
          output$tabs1=renderUI(
            tabsetPanel(id="mainPanel",
                        tabPanel("Overall Activity",h3("Overall Activity Monitor"),showOutput("Spiderweb","highcharts")),
                        tabPanel("Taiga",selectInput("taiga_select","Select a date range",choices = drop_down_dates,width = 150,selected = latest_date),h3("Your Taiga activity monitor"),showOutput("stackchart1","highcharts"),br(),h4("Taiga activity comparator"),showOutput("stackchart4","highcharts")),
                        tabPanel("Git Hub",h3("Your GitHub activity monitor"),showOutput("stackchart","highcharts"),br(),h4("GitHub Frequency monitor"),showOutput("stackchart5","highcharts"))
                        
            )
            
          ) 
          #this observer block is to only show data based on the drop down date selection
          print("tsting dropdown")
          observe({
            taiga_date_selected=input$taiga_select
            if(!is.null(taiga_date_selected))
            {
              print(input$taiga_select)
              temp_var1=which(u_all_taiga_data$Date==taiga_date_selected)
              print("checkcheck")
              print(temp_var1)
              next_six=temp_var1+6
              print("take after")
              print(next_six)
              print(nrow(u_all_taiga_data))
              if(next_six>nrow(u_all_taiga_data))
              {
                u_all_taiga_data1=u_all_taiga_data[temp_var1:nrow(u_all_taiga_data),]
              }
              else{
                u_all_taiga_data1=u_all_taiga_data[temp_var1:next_six,]
              }
              print(u_all_taiga_data1)
              u_taiga_data=u_all_taiga_data1[c(1,2,3,7)]
              colnames(u_taiga_data)=c("InProgress","ToTest","Done","Date")
              x12=u_taiga_data$Date
              x1234=u_taiga_data
              if(nrow(x1234)>1)
              {
                colnames(x1234)=c("Tasks in progress","Tasks to test","Done")
                output$stackchart1 <- renderChart2({
                  # Create chart
                  b <- rCharts:::Highcharts$new()
                  b$chart(type = "column", width=1000)
                  b$xAxis(categories = x12, title=list(text="Date"))
                  b$yAxis(title = list(text = "Counts"))
                  b$data(x1234)
                  b$exporting(enabled=T)
                  b$plotOptions(column = list(stacking = input$plotType))
                  return(b)
                })
              }
              
            }
          })
          
          #fetch GH data
          #ghd=read.csv("c:/users/Suhas Xavier/Desktop/Taiga_GH_DF.csv")
          ghd=read.csv("/home/sxavier2/all_files/Taiga_GH_DF.csv")
          colnames(ghd)=c("Week", "Additions", "Deletions","Commits","GH_Handle","Weight","CommitFrequency","TeamName")
          u_ghd=ghd[ghd$GH_Handle==as.character(u_gh_handle),]
          
          # notifications only for half the teams
          notification_teams=c(
            "AgileTweetViz-GeekOh",
            "AgileTweetViz-TeamNobel",
            "DRisk-Team-MASCS",
            "DRisk-Tech-Hub",
            "DSD-hackSlash",
            "MeetMe-HackerEntourage",
            "MeetMe-Segfault",
            "PRP-Manhattan-Project",
            "SanaModules-Impulse"
          )
          if(u_teamN %in% notification_teams)
          {
            notd=read.csv("/home/sxavier2/all_files/notification_table.csv")
            u_notd=na.omit(notd[notd$user_name==as.character(u_username),])
            if(nrow(u_notd)>1)
            {
              u_notd=tail(u_notd,2)
              output$notiMenu <- renderMenu({
                # Code to generate each of the messageItems here, in a list. This assumes
                # that messageData is a data frame with two columns, 'from' and 'message'.
                msgs <- apply(u_notd, 1, function(row) {
                  notificationItem(text = row[["notification"]], icon("users"))
                })
                
                # This is equivalent to calling:
                #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
                dropdownMenu(type = "notifications", .list = msgs)
              })
            }
          }
          #this portion tracks user activity
          noti_clicked=""
          shinyjs::onclick("notiMenu",{noti_clicked="YES"})
          taiga_clicked=""
          gh_clicked=""
          observe({
            tracker_val=input$mainPanel
            print(tracker_val)
            if(is.null(tracker_val))
            {
              print("no tab clciked")
            }
            else{
              if(tracker_val=="Taiga")
              {
                taiga_clicked="YES"
              }
              else if(tracker_val=="Git Hub")
              {
                gh_clicked="YES"
              }
              print("here it is")
            }
            
            session$onSessionEnded(function()
            { 
              track_data=data.frame(email_id=as.character(),Taiga=as.character(),GH=as.character(),notif=as.character(),stringsAsFactors = F)
              track_data[nrow(track_data)+1,]=c(e_id,as.character(taiga_clicked),as.character(gh_clicked) ,as.character(noti_clicked))
              track_data$StartTime=start_time
              track_data$end_time=Sys.time()
              track_data$ip=source_ip
              print("tracker")
              print(track_data)
              write.table(track_data,file="/home/sxavier2/all_files/dashboard_user_log.csv",row.names = F,col.names = F,sep=",",append = T)
            })
            
          })
          
          # end of user activity tracking
          output$titl=renderMenu(
            {
              dashboardHeader(title=paste("Hello",u_fullname,sep=" ") )
            })
          
          teamnamedisply_val=paste("Team: ",u_teamN,sep="")
          output$teamName_display=renderMenu(
            {
              sidebarMenu(menuItem(teamnamedisply_val))
            })
          
          
          #fetch Taiga weight
          taig_wt_data=read.csv("/home/sxavier2/all_files/Taiga_mean.csv")
          taig_u_data=taig_wt_data[taig_wt_data$username==u_username,]
          
          y1=taig_u_data$Date
          y123=taig_u_data[2:4]
          if(nrow(y123)>1)
          {
            colnames(y123)=c("Your weighted score","Average team score", "Average class score")
            output$stackchart4 <- renderChart2({
              # Create chart
              c <- rCharts:::Highcharts$new()
              c$title(text="Weekly expectations: Consistent activity on Taiga- updating tasks atleast 3 times a week")
              c$chart(type = "column", width=1000)
              c$xAxis(categories = y1, title=list(text="Date"))
              c$yAxis(title = list(text = "Scores"))
              c$data(y123)
              c$exporting(enabled=T)
              c$plotOptions(column = list(stacking = input$plotType))
              #           c$tooltip( formatter = "#! function() { return 'Highest possible score is 5,' + this.point.x + ' is '      
              #                      + this.point.y  
              #         } !#")
              
              return(c)
            })
          }
          
          x1=u_ghd$Week
          x123=u_ghd[2:3]
          if(nrow(x123)>1)
          {
            colnames(x123)=c("Lines of code added/1000","Lines of Code Deleted/100")
            output$stackchart <- renderChart2({
              # Create chart
              a <- rCharts:::Highcharts$new()
              a$chart(type = "column", width=1000)
              a$xAxis(categories = x1, title=list(text="Date"))
              a$yAxis(title = list(text = "Lines of code, added or deleted"))
              a$data(x123)
              a$exporting(enabled=T)
              a$plotOptions(column = list(stacking = input$plotType))
              #           a$tooltip( formatter = "#! function() { return 'Expected number of commits is atleas is 3, Student score is '      
              #                                                     + this.point.y  
              #                        } !#")
              
              return(a)
            })
          }
          
          
          x55=u_ghd$Week
          x556=u_ghd[c(1,4)]
          if(nrow(x556)>1)
          {
            x556$Dummy=0
            colnames(x556)=c("Commits","Frequency of Commits for the week")
            output$stackchart5 <- renderChart2({
              # Create chart
              v<- rCharts:::Highcharts$new()
              v$title(text="Weekly expectations: Atleast 3 qualitative commits in a week and a corresponding 500 lines of code atleast ")
              v$chart(type = "column",width=1000)
              v$xAxis(categories = x55,title=list(text="Date"))
              v$yAxis(title = list(text = "Commits"))
              v$data(x556)
              v$exporting(enabled=T)
              return(v)
            })
          }
          
          print("testing")
          x1_temp=tail(taig_u_data,1)
          x1_temp_date=as.character(x1_temp$Date)
          x_user_taiga_weight=as.numeric(taig_u_data[taig_u_data$Date==x1_temp_date,"user_mean"])
          x_team_taiga_weight=as.numeric(taig_u_data[taig_u_data$Date==x1_temp_date,"team_mean"])
          x_class_taiga_weight=as.numeric(taig_u_data[taig_u_data$Date==x1_temp_date,"class_mean"])
          x_temp=tail(ghd,n=1)
          x_temp_date=as.character(x_temp$Week)
          ghd_spider_all=ghd[ghd$Week==x_temp_date,]
          # x_ghd_class_weight=as.numeric(mean(ghd_spider_all$Weight))
          x_ghd_class_weight=2.9
          ghd_spider_team=ghd_spider_all[ghd_spider_all$TeamName==u_teamN,]
          
          if(nrow(ghd_spider_team)>1)
          {
            x_ghd_team_weight=as.numeric(mean(ghd_spider_team$Weight))
          }
          else
          {
            x_ghd_team_weight=0
          }
          x_ghd_user_weight=as.numeric(ghd_spider_all[ghd_spider_all$GH_Handle==u_gh_handle,"Weight"])
          if(length(x_ghd_user_weight)==0)
          {
            x_ghd_user_weight=0
          }      
          if(length(x_ghd_team_weight)==0)
          {
            x_ghd_team_weight=0
          } 
          if(length(x_ghd_class_weight)==0)
          {
            x_ghd_class_weight=0
          } 
          
          x_ghd_user_comm_freq=ghd_spider_all[ghd_spider_all$GH_Handle==u_gh_handle,"CommitFrequency"]
          x_ghd_class_comm_freq=as.numeric(mean(ghd_spider_all$CommitFrequency))
          x_ghd_team_comm_freq=as.numeric(mean(ghd_spider_team$CommitFrequency))
          
          
          print(x_ghd_class_weight)
          print(x_ghd_team_weight)
          print(x_ghd_user_weight)
          print("next")
          
          print(x_ghd_user_comm_freq)
          print(x_ghd_class_comm_freq)
          print(x_ghd_team_comm_freq)
          if(length(x_ghd_user_comm_freq)==0)
          {
            x_ghd_user_comm_freq=0
          }
          if(length(x_ghd_class_comm_freq)==0)
          {
            x_ghd_class_comm_freq=0
          }
          if(length(x_ghd_team_comm_freq)==0)
          {
            x_ghd_team_comm_freq=0
          }
          
          output$Spiderweb <- renderChart2({
            plot <- Highcharts$new()
            table = data.frame(id = c("GitHub Impact","Taiga Impact","Taiga Frequency","GitHub Frequency"),
                               value = c(x_ghd_user_weight,x_user_taiga_weight,x_user_taiga_weight,x_ghd_user_comm_freq),
                               value1 = c(x_ghd_team_weight,x_team_taiga_weight,x_team_taiga_weight,x_ghd_team_comm_freq),
                               value2 = c(x_ghd_class_weight,x_class_taiga_weight,x_class_taiga_weight,x_ghd_class_comm_freq)
            )
            plot$chart(polar = TRUE, type = "line", width=800)
            plot$xAxis(categories=table$id, tickmarkPlacement= 'on', lineWidth= 0)
            plot$yAxis(gridLineInterpolation= 'polygon',
                       lineWidth= 0, min= 0)
            plot$series(data = table[,"value"],
                        name = "Your scores", pointPlacement="on")
            plot$series(data = table[,"value1"],
                        name = "Team average scores", pointPlacement="on",color="maroon", opacity=0.5)
            plot$series(data = table[,"value2"],
                        name = "Class average scores", pointPlacement="on",color="green", opacity=0.5)
            return(plot)
            
            
          })
          
          
        }
        else
        {
          #print("asorrt")
          output$errortab=renderUI(h3("Sorry, that email id is not recognized, please refresh the page and try again"))
        } 
      }  
      
      adminuser=function(e_idval)
      {
        print("checked in here")
        output$admin_pwd=renderUI(passwordInput("pwd","Enter admin password"))
        observe({
          pwd_entered=input$pwd
          if(length(pwd_entered)>0)
          {
            if(pwd_entered=="a5uP0l4!")
            {
              shinyjs::hide("admin_pwd")
              print(pwd_entered)
              admin_file=read.csv("/home/sxavier2/all_files/membership_table.csv",sep=",",row.names = NULL)
              full_names=as.character(admin_file$full_name)
              full_names=sort(full_names, decreasing = FALSE)
              output$admin_view=renderUI(selectInput("namelist", "Select a student to view dashboard",full_names,selected = NULL,multiple = TRUE))
              user_selected=input$namelist
              print(user_selected)
              user_selected_email=as.character(tolower(admin_file[admin_file$full_name==user_selected,"email"]))
              print(user_selected_email)
              taiga(user_selected_email)
            }
          }
        })
        
      }
      
      #       if(e_id=="scrumwise_user@asu.edu")
      #       {
      #         scrumwise(e_id)
      #       }
      if(e_id=="kgary@asu.edu")
      {
        adminuser(e_id)
      }
      else 
      {
        taiga(e_id)
      }
      
      
    }
    
  })
}
