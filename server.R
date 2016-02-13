library(shiny)
library(shinydashboard)
library(rCharts)
library(shinyjs)
library(mailR)

function(input,output,session)
{
  taiga=function(mailid,inst_yes)
  {
	inst=inst_yes
    shinyjs::hide("welcome_tab")
    shinyjs::hide("forgot_pwd")
    shinyjs::hide("userpwd")
    shinyjs::hide("firstpwd")
    shinyjs::hide("secondpwd")
    shinyjs::hide("non_member")
    shinyjs::hide("pwdsubmit")
    shinyjs::hide("forgot_cnfrm")
    useremail=mailid
    print("in taiga")
    #retrieve user primary info
    taiga_mem_table=read.csv("/home/sxavier2/spring16/taiga_membership_table.csv")
    user_fullname=as.character(taiga_mem_table[taiga_mem_table$email==useremail,"full_name"])
    user_team=as.character(taiga_mem_table[taiga_mem_table$email==useremail,"project_name"])
    #user_nick_name=taiga_mem_table[taiga_mem_table$nick_name]
    user_ghhandle=as.character(taiga_mem_table[taiga_mem_table$email==useremail,"gh_handle"])
    
    #retrieve user Taiga info
    all_taiga_data=read.csv("/home/sxavier2/spring16/taiga_data.csv")
    user_taiga_data=all_taiga_data[all_taiga_data$email==useremail,c("in_progress","to_test","done","date")]
    #proceed with inp,tot,done,date,for chart1
    
    #fetch a weekly date to show in taiga and gh date pickers
    temp1=data.frame(unique(all_taiga_data$date))
    weekly_dates_taiga=as.character(temp1[seq(1,nrow(temp1),by=7),])
    latest_date_taiga=as.character(tail(weekly_dates_taiga,1))
    
    #retrieve user's taiga scores
    all_taiga_scores=read.csv("/home/sxavier2/spring16/Taiga_Weight.csv")
    user_taiga_scores=all_taiga_scores[all_taiga_scores$email==useremail,]
    print(user_taiga_scores)
    #proceed with diff,exp,date for chart2
    
    recent_date=as.character(tail(user_taiga_scores$date,1))
    all_taiga_dates=as.character(unique(user_taiga_scores[user_taiga_scores$date!=recent_date,"date"]))
    last_week_date=as.character(tail(all_taiga_dates,1))
    
    #fetch taiga scores for radial chart
    recent_date=as.character(tail(user_taiga_scores$date,1))
    user_recent_taiga_scores=user_taiga_scores[as.character(user_taiga_scores$date)==recent_date,"weight"]
    mean_team_recent_taiga_scores=mean(all_taiga_scores[as.character(all_taiga_scores$team)==user_team & as.character(all_taiga_scores$date)==recent_date,"weight"])
    mean_class_recent_taiga_scores=mean(all_taiga_scores[as.character(all_taiga_scores$date)==recent_date,"weight"])
    
    #ALL Taiga data retrieved!!
    print("taiga set")
    
    #fetch gh data
    
    all_gh_scores_tg=read.csv("/home/sxavier2/spring16/GH_DF.csv")
    all_gh_scores_tg$loca=all_gh_scores_tg$loca/1000
    all_gh_scores_tg$locd=all_gh_scores_tg$locd/100
    user_gh_scores_tg=NULL
    
    user_gh_scores_tg=all_gh_scores_tg[all_gh_scores_tg$email==useremail,]
    print(user_gh_scores_tg)
    gh_recent_date_tg=as.character(tail(user_gh_scores_tg$date,1))
    
    if(nrow(user_gh_scores_tg)>=1)
    {
      locA_tg=sum(user_gh_scores_tg$loca)*1000
      locD_tg=sum(user_gh_scores_tg$locd)*100
      commits_tg=sum(user_gh_scores_tg$commits)
    }else
    {
      locA_tg=0
      locD_tg=0
      commits_tg=0
    }
    
    #fetch dates for date picker
    temp2_tg=data.frame(unique(user_gh_scores_tg$date))
    if(nrow(temp2_tg)>7)
    {
      weekly_dates_gh_tg=as.character(temp2_tg[seq(1,nrow(temp2_tg),by=7),])
      latest_date_gh_tg=tail(weekly_dates_gh_tg,1)
      print(paste("if date is ",latest_date_gh_tg,sep=""))
    }else
    {
      weekly_dates_gh_tg=""
      latest_date_gh_tg=""
      print(paste("else date is: ",latest_date_gh_tg,sep=""))
    }
    
    #fetch gh data for compliance charts
    gh_comp_scores=read.csv("/home/sxavier2/spring16/GH_Taiga_Weight.csv")
    gi_date=tail(gh_comp_scores$date,1)
    gh_comp_user_score=gh_comp_scores[gh_comp_scores$email==useremail,]
    if(nrow(gh_comp_user_score)<1)
    {
      gh_comp_freq_score=0
      gh_comp_impact_score=0
    }
    else if(nrow(gh_comp_user_score)>=1)
    {
      gh_comp_freq_score=gh_comp_user_score[as.character(gh_comp_user_score$email)==useremail & as.character(gh_comp_user_score$date)==gi_date,"frequency_score"]
      gh_comp_impact_score=gh_comp_user_score[as.character(gh_comp_user_score$email)==useremail & as.character(gh_comp_user_score$date)==gi_date,"impact_score"]
    }
    
    
    #fetch gh scores for radial chart
    
    #gh_comp_user_score is the same as users score
    mean_team_recent_gh_impscore=mean(gh_comp_scores[as.character(gh_comp_scores$project_name)==user_team & as.character(gh_comp_scores$date)==gi_date,"impact_score"])
    mean_team_recent_gh_freqscore=mean(gh_comp_scores[as.character(gh_comp_scores$project_name)==user_team & as.character(gh_comp_scores$date)==gi_date,"frequency_score"])
    print("here")
    print(mean_team_recent_gh_freqscore)
    
    mean_class_recent_gh_impscore=mean(gh_comp_scores[gh_comp_scores$date==gi_date,"impact_score"])
    mean_class_recent_gh_freqscore=mean(gh_comp_scores[gh_comp_scores$date==gi_date,"frequency_score"])
    print(mean_class_recent_gh_freqscore)
    print("gh set")
    #display tabs for students with relevenat date pickers on each tab
    output$tabs_tg=renderUI(tabsetPanel(id="mainPanel_1",
                                      tabPanel("Overall_Activity",fluidRow(box(showOutput("Spiderweb_1","highcharts"),title="Overall Activity This Week",width=6),
                                                                           box(showOutput("Spiderweb_2","highcharts"),selectInput("spider1_select","Select a date range",all_taiga_dates,selected = last_week_date)
                                                                               ,title="Overall Activity Last Week",width=6))),
                                      tabPanel("Taiga",h3("Your Daily Taiga Activity Monitor"),selectInput("taiga_select","Select a date range",choices = weekly_dates_taiga,selected = latest_date_taiga,width=150)
                                               , showOutput("taig_chart1","highcharts"),br(),h3("Your Weekly Taiga Compliance Monitor"),showOutput("taigacharta","highcharts")),
                                      tabPanel("Git Hub",h3("Your Daily GitHub Activity Monitor"),
                                               valueBox(commits_tg,"Total number of commits made",color="aqua",icon = icon("credit-card"),width=4),
                                               valueBox(locA_tg,"Total Lines of Code Added",icon = icon("list"),color="aqua",width=4),
                                               valueBox(locD_tg,"Total Lines of Code Deleted",icon = icon("list-alt"),color="aqua",width=4),
                                               selectInput("gh_select_tg","Select a date range",weekly_dates_gh_tg,width=150,selected = latest_date_gh_tg)
                                               ,showOutput("gh_chart1_taiga","highcharts"),br(),h3("Your Weekly GitHub Compliance Monitor"),showOutput("ghcharta","highcharts"))))
    print("so far")
    #  plot first radial chart
    output$Spiderweb_1 <- renderChart2({
      plot <- Highcharts$new()
      table = data.frame(id = c("GitHub Impact","Taiga Impact","Taiga Frequency","GitHub Frequency"),
                         value = c(gh_comp_impact_score,user_recent_taiga_scores,user_recent_taiga_scores,gh_comp_freq_score),
                         value1 = c(mean_team_recent_gh_impscore,mean_team_recent_taiga_scores,mean_team_recent_taiga_scores,mean_team_recent_gh_freqscore),
                         value2 = c(mean_class_recent_gh_impscore,mean_class_recent_taiga_scores,mean_class_recent_taiga_scores,mean_class_recent_gh_freqscore)
      )
      plot$chart(polar = TRUE, type = "line", width=450)
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
    
    #Spiderweb_2
        observe({
          spiderweb_date=input$spider1_select
          print("check date")
          print(spiderweb_date)
          user_recent_taiga_scores1=user_taiga_scores[user_taiga_scores$date==spiderweb_date,"weight"]
          mean_team_recent_taiga_scores1=mean(all_taiga_scores[all_taiga_scores$team==user_team & all_taiga_scores$date==spiderweb_date,"weight"])
          mean_class_recent_taiga_scores1=mean(all_taiga_scores[all_taiga_scores$date==spiderweb_date,"weight"])
          
          spiderweb_date1="1/31/2016"
                    gh_comp_freq_score1=gh_comp_user_score[gh_comp_user_score$email==useremail & gh_comp_user_score$date==spiderweb_date1,"frequency_score"]
                    gh_comp_impact_score1=gh_comp_user_score[gh_comp_user_score$email==useremail &gh_comp_user_score$date==spiderweb_date1,"impact_score"]
                     mean_team_recent_gh_impscore1=mean(gh_comp_scores[gh_comp_scores$project_name==user_team & gh_comp_scores$date==spiderweb_date1,"impact_score"])
                     mean_team_recent_gh_freqscore1=mean(gh_comp_scores[gh_comp_scores$project_name==user_team & gh_comp_scores$date==spiderweb_date1,"frequency_score"])
                     mean_class_recent_gh_impscore1=mean(gh_comp_scores[gh_comp_scores$date==spiderweb_date1,"impact_score"])
                     mean_class_recent_gh_freqscore1=mean(gh_comp_scores[gh_comp_scores$date==spiderweb_date1,"frequency_score"])
                    

          #plot Spiderweb_2
          output$Spiderweb_2 <- renderChart2({
            plot <- Highcharts$new()
            table1 = data.frame(id = c("GitHub Impact","Taiga Impact","Taiga Frequency","GitHub Frequency"),
                                value3 = c(gh_comp_impact_score1,user_recent_taiga_scores1,user_recent_taiga_scores1,gh_comp_freq_score1),
                                value4 = c(mean_team_recent_gh_impscore1,mean_team_recent_taiga_scores1,mean_team_recent_taiga_scores1,mean_team_recent_gh_freqscore1),
                                value5 = c(mean_class_recent_gh_impscore1,mean_class_recent_taiga_scores1,mean_class_recent_taiga_scores1,mean_class_recent_gh_freqscore1)
            )
            plot$chart(polar = TRUE, type = "line", width=450)
            plot$xAxis(categories=table1$id, tickmarkPlacement= 'on', lineWidth= 0)
            plot$yAxis(gridLineInterpolation= 'polygon',
                       lineWidth= 0, min= 0)
            plot$series(data = table1[,"value3"],
                        name = "Your scores", pointPlacement="on")
            plot$series(data = table1[,"value4"],
                        name = "Team average scores", pointPlacement="on",color="maroon", opacity=0.5)
            plot$series(data = table1[,"value5"],
                        name = "Class average scores", pointPlacement="on",color="green", opacity=0.5)
            return(plot)
          })
          
        })
    
    #later after qualtrics
    
    #plot taiga daily chart
    observe({
      if(!is.null(input$taiga_select))
      {
        currdate_sel=input$taiga_select
        taiga_rows=nrow(user_taiga_data)
        curr_row=as.numeric(which(user_taiga_data$date==currdate_sel))
        plus_six=curr_row+6
        if(plus_six>=taiga_rows)
        {
          temp_taiga_data=user_taiga_data[curr_row:taiga_rows,]
          print(temp_taiga_data)
        }
        else
        {
          temp_taiga_data=user_taiga_data[curr_row:plus_six,]
        }
        taig_chart1_date=as.character(temp_taiga_data$date)
        print(taig_chart1_date)
        taig_chart1_data=temp_taiga_data[c("in_progress","to_test","done")]
        print(taig_chart1_data)
        print("in taiga chart1")
        if(nrow(taig_chart1_data)>=1)
        {
          colnames(taig_chart1_data)=c("In Progress","To Test","Done")
          output$taig_chart1 <- renderChart2({
            # Create chart
            b <- rCharts:::Highcharts$new()
            b$chart(type = "column", width=1000)
            b$xAxis(categories = taig_chart1_date, title=list(text="Date"))
            b$yAxis(title = list(text = "Number of Tasks"))
            b$data(taig_chart1_data)
            b$exporting(enabled=T)
            b$plotOptions(column = list(stacking = input$plotType))
            return(b)
          })
        }
      }
      print("whoof")
      # plot taiga compliance chart
      u_ghd=user_taiga_scores
      output$taigacharta <- renderChart2({
        x77=u_ghd$date
        x6666=u_ghd[c(3,5)]
        colnames(x6666) =c("Your Taiga Activity For the Week", "Expected Taiga Activity for the Week")
        # Create charts
        a <- rCharts:::Highcharts$new()
        a$chart(type = "spline",width=1000)
       # a$title(text = "Taiga Compliance Measure")
        a$xAxis(categories = x77,title=list(text="Date"))
        a$yAxis(title = list(text = "Scrumboard task updates"))
        a$data(x6666)
        return(a)
      })
    })
    
    print("entering gh")
    #  plot gh daily chart
    observe({
      
      if(!is.null(input$gh_select_tg))
      {
        if(nchar(input$gh_select_tg)>1)
        {
        currdate_sel_gh=input$gh_select_tg
        print("hold on")
        print(currdate_sel_gh)
        gh_rows=nrow(user_gh_scores_tg)
        print(gh_rows)
      #  print("the val i am looking for")
       # print(user_gh_scores_tg)
        if(gh_rows>1)
        {
          curr_row_gh=as.numeric(which(user_gh_scores_tg$date==currdate_sel_gh))
        #  print(curr_row_gh)
          plus_six_gh=curr_row_gh+6
         # print(plus_six_gh)
          if(plus_six_gh>=gh_rows)
          {
            temp_gh_data=user_gh_scores_tg[curr_row_gh:gh_rows,]
          #  print(temp_gh_data)
          }
          else
          {
            temp_gh_data=user_gh_scores_tg[curr_row_gh:plus_six_gh,]
          }
          gh_chart1_date=as.character(temp_gh_data$date)
          gh_chart1_data=temp_gh_data[c("commits","loca","locd")]
          print("check")
          print(gh_chart1_data)
          if(nrow(gh_chart1_data)>=1)
          {
            colnames(gh_chart1_data)=c("Number of commits","Lines of Code Added/1000","Lines of Code Deleted/100")
            output$gh_chart1_taiga <- renderChart2({
              #   Create chart
              c <- rCharts:::Highcharts$new()
              c$chart(type = "column", width=1000)
              c$xAxis(categories = gh_chart1_date, title=list(text="Date"))
              c$yAxis(title = list(text = "Counts"))
              c$data(gh_chart1_data)
              c$exporting(enabled=T)
              c$plotOptions(column = list(stacking = input$plotType))
              return(c)
            })
          }
        
      }
      
      # plot gh compliance chart
      u_bhd=gh_comp_user_score
      if(nrow(u_bhd)>1)
      {
      output$ghcharta <- renderChart2({
        x771=u_bhd$date
        x666=u_bhd[c(6,7)]
        colnames(x666) =c("Your Code Activity for the Week","Expected Code Activity For the Week")
        #  Create charts
        d <- rCharts:::Highcharts$new()
        d$chart(type = "spline",width=1000)
        #d$title(text = "Git Hub Compliance Measure")
        d$xAxis(categories = x771,title=list(text="Date"))
        d$yAxis(title = list(text = "Git Hub task updates"))
        d$data(x666)
        return(d)
      })
      }
      }
      }
    })

    #display notifications
    notd=read.csv("/home/sxavier2/spring16/notification_table.csv")
    u_notd=tail(na.omit(notd[notd$email==useremail,]),4)
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
    #track only students and  not instructor
			if(inst>0)
			{
                  #track user clicks
                        observe({
                          tracker_val=input$mainPanel_1
                          print(tracker_val)
                          if(is.null(tracker_val))
                          {
                            print("no tab clciked")
                          }
                          else{
                            if(tracker_val=="Taiga")
                            {
                              track_taiga=data.frame(start_time,useremail,"YES")
                              write.table(track_taiga,"/home/sxavier2/spring16/taiga_tracker.csv",row.names = F,col.names = F,sep=",",append = T)
                            }
                            if(tracker_val=="Git Hub")
                            {
                              track_gh=data.frame(start_time,useremail,"YES")
                              write.table(track_gh,"/home/sxavier2/spring16/gh_tracker.csv",row.names = F,col.names = F,sep=",",append = T)
                            }
                          }
                         
                        })
                        
                        
                        noti_clicked="NO"
                        shinyjs::onclick("notiMenu",{noti_clicked="YES"})
                        observe({
                          if (!is.null(input$userleft) && input$userleft == 1) 
                            {
                            source_ip=session$clientData$url_hostname
                            print("user left")
                            end_time=Sys.time()
                            exit_data=data.frame(start_time,end_time,source_ip,noti_clicked,useremail)
                            write.table(exit_data,file="/home/sxavier2/spring16/dashboard_user_log.csv",row.names = F,col.names = F,sep=",",append = T)
                          }
                        })
                        
                }        
                        output$titl=renderMenu({
      dashboardHeader(title=paste("Hey, ",user_fullname,sep=""))
    })
    # write.table(df_1,file="/home/sxavier2/spring16/pp1.csv",row.names = F,col.names = F,append = T)
    
  }

  scrumwise=function(mailid,inst_yes)
  {
	inst1=inst_yes
    shinyjs::hide("welcome_tab")
    shinyjs::hide("forgot_pwd")
    shinyjs::hide("userpwd")
    shinyjs::hide("firstpwd")
    shinyjs::hide("secondpwd")
    shinyjs::hide("non_member")
    shinyjs::hide("pwdsubmit")
    shinyjs::hide("forgot_cnfrm")
    
    useremail=mailid
    sw_mem_table=read.csv("/home/sxavier2/spring16/sw_membership_table.csv")
    user_firstname=as.character(sw_mem_table[sw_mem_table$email==useremail,"first_name"])
    user_lastname=as.character(sw_mem_table[sw_mem_table$email==useremail,"last_name"])
    user_team=as.character(sw_mem_table[sw_mem_table$email==useremail,"project_name"])
    user_ghhandle=as.character(sw_mem_table[sw_mem_table$email==useremail,"gh_handle"])
    user_sw_id=as.character(sw_mem_table[sw_mem_table$email==useremail,"sw_id"])
    
    #get sw data
    all_swdata=read.csv("/home/sxavier2/spring16/sw_data.csv")
    user_swdata=all_swdata[all_swdata$email==useremail,]
    #use this df to create chart1
    
    #fetch a weekly date to show in sw date pickers
    
    
    temp1=data.frame(unique(all_swdata$date))
    weekly_dates_sw=as.character(temp1[seq(1,nrow(temp1),by=7),])
    latest_date_sw=as.character(tail(weekly_dates_sw,1))
    print("chck")
    print(latest_date_sw)
    
    #get data for sw compliance chart
    all_sw_scores=read.csv("/home/sxavier2/spring16/sw_Weight.csv")
    user_sw_scores=all_sw_scores[all_sw_scores$email==useremail,]
    #use this for chart1
    
    recent_date=as.character(tail(all_sw_scores$date,1))
    
    #fetch date picker for radial charts
    recent_date=as.character(tail(user_sw_scores$date,1))
    all_sw_dates=as.character(unique(user_sw_scores[user_sw_scores$date!=recent_date,"date"]))
    last_week_date_sw=as.character(tail(all_sw_dates,1))
    user_recent_sw_scores=user_sw_scores[user_sw_scores$date==recent_date,"score"]
    print(user_recent_sw_scores)
    
    #get sw data for radial chart
    mean_team_recent_sw_scores=mean(all_sw_scores[all_sw_scores$teamname==user_team & all_sw_scores$date==recent_date,"score"])
    mean_class_recent_sw_scores=mean(all_sw_scores[all_sw_scores$date==recent_date,"score"])
    if(is.nan(mean_team_recent_sw_scores) || is.na(mean_team_recent_sw_scores))
    {
      mean_team_recent_sw_scores=0
    }
    
    if(is.nan(mean_class_recent_sw_scores) || is.na(mean_class_recent_sw_scores))
    {
      mean_class_recent_sw_scores=0
    }
    
    #fetch gh data
    all_gh_scores=read.csv("/home/sxavier2/spring16/GI_DF.csv")
    all_gh_stability=read.csv("/home/sxavier2/spring16/GI_DF_stability.csv")
    all_gh_stability_score=all_gh_stability[all_gh_stability$email==useremail,]
    user_gh_scores=NULL
    user_gh_scores=all_gh_scores[all_gh_scores$email==useremail,]
    gh_recent_date=as.character(tail(user_gh_scores$date,1))
    
    if(nrow(user_gh_scores)>=1)
    {
      locA=user_gh_scores[user_gh_scores$date==gh_recent_date,"loca"]*1000
      locD=user_gh_scores[user_gh_scores$date==gh_recent_date,"locd"]*100
      commits=user_gh_scores[user_gh_scores$date==gh_recent_date,"commits"]
    }
    else
    {
      locA=0
      locD=0
      commits=0
    }
    
    if(nrow(all_gh_stability_score)>=1)
    {
      stab_print=all_gh_stability_score[as.character(all_gh_stability_score$date)==gh_recent_date,"stability"]
      print(stab_print)
      stability=""
      stability=paste(stab_print,"%",sep="")
      print(stability)
    }
    else{
      stability="0%"
    }
    
    #fetch dates for date picker
    temp2=data.frame(unique(user_gh_scores$date))
    if(nrow(temp2)>1)
    {
      
      weekly_dates_gh=as.character(temp2[seq(1,nrow(temp2),by=7),])
      latest_date_gh=tail(weekly_dates_gh,1)
      print(paste(latest_date_gh,"latest date in if",sep=" "))
      
    }
    else{
      weekly_dates_gh=""
      latest_date_gh=""
      print(paste(latest_date_gh,"latest date in else",sep=" "))
    }
    #fetch gh data for compliance charts
    gh_comp_scores=read.csv("/home/sxavier2/spring16/GI_Aggregate_scores.csv")
    gi_date=tail(gh_comp_scores$date,1)
    gh_comp_user_score=gh_comp_scores[gh_comp_scores$email==useremail,]
    if(nrow(gh_comp_user_score)<1)
    {
      gh_comp_freq_score=0
      gh_comp_impact_score=0
    }
    else if(nrow(gh_comp_user_score)>=1)
    {
      gh_comp_freq_score=gh_comp_user_score[as.character(gh_comp_user_score$email)==useremail & as.character(gh_comp_user_score$date)==gi_date,"frequency_score"]
      gh_comp_impact_score=gh_comp_user_score[as.character(gh_comp_user_score$email)==useremail & as.character(gh_comp_user_score$date)==gi_date,"impact_score"]
    }
    
    
    #fetch gh scores for radial chart
    
    #gh_comp_user_score is the same as users score
    mean_team_recent_gh_impscore=mean(gh_comp_scores[as.character(gh_comp_scores$teamname)==user_team & as.character(gh_comp_scores$date)==gi_date,"impact_score"])
    mean_team_recent_gh_freqscore=mean(gh_comp_scores[as.character(gh_comp_scores$teamname)==user_team & as.character(gh_comp_scores$date)==gi_date,"frequency_score"])
    print("here")
    print(mean_team_recent_gh_freqscore)
    
    mean_class_recent_gh_impscore=mean(gh_comp_scores[gh_comp_scores$date==gi_date,"impact_score"])
    mean_class_recent_gh_freqscore=mean(gh_comp_scores[gh_comp_scores$date==gi_date,"frequency_score"])
    print(mean_class_recent_gh_impscore)
    print("gh sw set")
    
    
    #display tabs for students with relevenat date pickers on each tab
    output$tabs2=renderUI(tabsetPanel(id="mainPanel",
                                      tabPanel( "Overall Activity",fluidRow(box(showOutput("Spiderweb1","highcharts"),title="Overall Measures This Week",width=6),
                                                                            box(showOutput("Spiderweb2","highcharts"),selectInput("spider_select","Select a date range",all_sw_dates,selected = last_week_date_sw),title="Overall Measures Last Week",width=6))),
                                      tabPanel("Scrumwise",h3("Your daily Scrumwise activity monitor"),selectInput("sw_selector","Select a date range",weekly_dates_sw,selected = latest_date_sw,width = 150),showOutput("sw_chart1","highcharts")
                                               ,br(),showOutput("swcharta","highcharts")),
                                      tabPanel("GitHub",h3("Your GitHub activity monitor"),
                                               valueBox(commits,"Recent Commits",color="aqua",icon = icon("credit-card"),width=3),
                                               valueBox(locA,"Lines of Code Added",icon = icon("list"),color="aqua",width=3),
                                               valueBox(locD,"Lines of Code Deleted",icon = icon("list-alt"),color="aqua",width=3),
                                               valueBox(stability,"Stability",icon = icon("list-alt"),color="aqua",width=3),
                                               selectInput("gh_select1","Select a date range",weekly_dates_gh,width=150,selected = latest_date_gh),
                                               showOutput("gh_chart1","highcharts"),showOutput("ghchartb","highcharts"))))
    
    
    #plot SW first radial chart
    output$Spiderweb1 <- renderChart2({
      plot <- Highcharts$new()
      table = data.frame(id = c("GitHub Impact","Srumwise Impact","Scrumwise Frequency","GitHub Frequency"),
                         value = c(gh_comp_impact_score,user_recent_sw_scores,user_recent_sw_scores,gh_comp_freq_score),
                         value1 = c(mean_team_recent_gh_impscore,mean_team_recent_sw_scores,mean_team_recent_sw_scores,mean_team_recent_gh_freqscore),
                         value2 = c(mean_class_recent_gh_impscore,mean_class_recent_sw_scores,mean_class_recent_sw_scores,mean_class_recent_gh_freqscore)
      )
      plot$chart(polar = TRUE, type = "line", width=450)
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
    
    #plot scrumwise spiderweb2
    #     observe({
    #       spiderweb_date=input$spider_select
    #                 user_recent_sw_scores1=user_sw_scores[user_sw_scores$date==spiderweb_date,]
    #                 mean_team_recent_sw_scores1=mean(all_sw_scores[all_sw_scores$team==user_team & all_sw_scores$date==spiderweb_date,"score"])
    #                 mean_class_recent_sw_scores1=mean(all_sw_scores[all_sw_scores$date==spiderweb_date,"score"])
    #                 
    #                 gh_comp_freq_score1=gh_comp_user_score[gh_comp_user_score$email==useremail & gh_comp_user_score$date==spiderweb_date,"frequency_score"]
    #                 gh_comp_impact_score1=gh_comp_user_score[gh_comp_user_score$email==useremail &gh_comp_user_score$date==spiderweb_date,"impact_score"]
    #                 mean_team_recent_gh_impscore1=mean(gh_comp_scores[gh_comp_scores$teamname==user_team & gh_comp_scores$date==spiderweb_date,"impact_score"])
    #                 mean_team_recent_gh_freqscore1=mean(gh_comp_scores[gh_comp_scores$teamname==user_team & gh_comp_scores$date==spiderweb_date,"frequency_score"])
    #                 mean_class_recent_gh_impscore1=mean(gh_comp_scores[gh_comp_scores$date==spiderweb_date,"impact_score"])
    #                 mean_class_recent_gh_freqscore1=mean(gh_comp_scores[gh_comp_scores$date==spiderweb_date,"frequency_score"])
    #                 
    #       print("all ok 7")
    #       user_recent_sw_scores1=1
    #       mean_team_recent_sw_scores1=2
    #       mean_class_recent_sw_scores1=3
    #       gh_comp_freq_score1=4
    #       gh_comp_impact_score1=5
    #       mean_team_recent_gh_impscore1=4
    #       mean_team_recent_gh_freqscore1=3
    #       mean_class_recent_gh_impscore1=4
    #       mean_class_recent_gh_freqscore1=2
    #       
    
    #       #plot Spiderweb2
    #       output$Spiderweb2 <- renderChart2({
    #         plot <- Highcharts$new()
    #         table = data.frame(id = c("GitHub Impact","Scrumwise Impact","Scrumwise Frequency","GitHub Frequency"),
    #                            value = c(gh_comp_impact_score1,user_recent_sw_scores1,user_recent_sw_scores1,gh_comp_freq_score1),
    #                            value1 = c(mean_team_recent_gh_impscore1,mean_team_recent_sw_scores1,mean_team_recent_sw_scores1,mean_team_recent_gh_freqscore1),
    #                            value2 = c(mean_class_recent_gh_impscore1,mean_class_recent_sw_scores1,mean_class_recent_sw_scores1,mean_class_recent_gh_freqscore1)
    #         )
    #         plot$chart(polar = TRUE, type = "line", width=450)
    #         plot$xAxis(categories=table$id, tickmarkPlacement= 'on', lineWidth= 0)
    #         plot$yAxis(gridLineInterpolation= 'polygon',
    #                    lineWidth= 0, min= 0)
    #         plot$series(data = table[,"value"],
    #                     name = "Your scores", pointPlacement="on")
    #         plot$series(data = table[,"value1"],
    #                     name = "Team average scores", pointPlacement="on",color="maroon", opacity=0.5)
    #         plot$series(data = table[,"value2"],
    #                     name = "Class average scores", pointPlacement="on",color="green", opacity=0.5)
    #         return(plot)
    #       })
    #     })
    #     
    #plot sw chart 1
    
    
    observe({
      if(!is.null(input$sw_selector))
      {
        print("test")
        print(input$sw_selector)
        currdate_sel1=input$sw_selector
        sw_rows=nrow(user_swdata)
        if(sw_rows >1)
        {
          curr_row=as.numeric(which(user_swdata$date==currdate_sel1))
          plus_six=curr_row+6
          print("all ok")
          if(plus_six>=sw_rows)
          {
            temp_sw_data=user_swdata[curr_row:sw_rows,]
          }
          else
          {
            temp_sw_data=user_swdata[curr_row:plus_six,]
          }
          print("this passed")
          sw_chart1_date=temp_sw_data$date
          sw_chart1_data=temp_sw_data[c("in_progress","to_test","done")]
          print(sw_chart1_data)
          if(nrow(sw_chart1_data)>1)
          {
            colnames(sw_chart1_data)=c("In Progress","To Test","Done")
            output$sw_chart1 <- renderChart2({
              # Create chart
              b <- rCharts:::Highcharts$new()
              b$chart(type = "column", width=1000)
              b$xAxis(categories = sw_chart1_date, title=list(text="Date"))
              b$yAxis(title = list(text = "Number of Tasks"))
              b$data(sw_chart1_data)
              b$exporting(enabled=T)
              b$plotOptions(column = list(stacking = input$plotType))
              return(b)
            })
          }
        }
      }
      
      #plot sw compliance chart
      u1_ghd=user_sw_scores
      if(nrow(u1_ghd)>1)
      {
        output$swcharta <- renderChart2({
          x778=u1_ghd$date
          x6660=u1_ghd[c(3,5)]
          colnames(x6660) =c("Your Scrumwise Activity", "Expected Scrumwise Activity")
          # Create charts
          a <- rCharts:::Highcharts$new()
          a$chart(type = "spline",width=1000)
          a$title(text = "Scrumwise Compliance Measure")
          a$xAxis(categories = x778,title=list(text="Date"))
          a$yAxis(title = list(text = "Scrumboard task updates"))
          a$data(x6660)
          return(a)
        })
      }
    })
    
    #plot gh chart1
    #observe({
    if(!is.null(input$gh_select1))
    {
      currdate_sel_gh=as.character(input$gh_select1)
      print(currdate_sel_gh)
      gh_rows=nrow(user_gh_scores)
      if(gh_rows>1)
      {
        print(currdate_sel_gh)
        curr_row_gh=as.numeric(which(user_gh_scores$date==currdate_sel_gh))
        plus_six_gh=curr_row_gh+6
        if(plus_six_gh>=gh_rows)
        {
          temp_gh_data=user_gh_scores[curr_row_gh:gh_rows,]
        }
        else
        {
          temp_gh_data=user_gh_scores[curr_row_gh:plus_six_gh,]
        }
        gh_chart1_date=as.character(temp_gh_data$date)
        gh_chart1_data=temp_gh_data[c("commits","loca","locd")]
        print(gh_chart1_data)
        colnames(gh_chart1_data)=c("Number of Commits","Lines of Code Added/1000","Lines of Code Deleted/100")
        output$gh_chart1 <- renderChart2({
          # Create chart
          c <- rCharts:::Highcharts$new()
          c$chart(type = "column", width=1000)
          c$xAxis(categories = gh_chart1_date, title=list(text="Date"))
          c$yAxis(title = list(text = "Counts"))
          c$data(gh_chart1_data)
          c$exporting(enabled=T)
          c$plotOptions(column = list(stacking = input$plotType))
          return(c)
        })
      }
    }
    
    
    #plot gh compliance chart
    u_bhd1=gh_comp_user_score
    if(nrow(u_bhd1)>1)
    {
      print("going in 3")
      output$ghchartb <- renderChart2({
        x774=u_bhd1$date
        x6664=u_bhd1[c(4,5,6)]
        colnames(x6664) =c("Your Impact scores", "Your frequency scores", "Expected scores")
        # Create charts
        f <- rCharts:::Highcharts$new()
        f$chart(type = "spline",width=1000)
        f$title(text = "Git Hub Compliance Measure")
        f$xAxis(categories = x774,title=list(text="Date"))
        f$yAxis(title = list(text = "Git Hub task updates"))
        f$data(x6664)
        return(f)
      })
    }
    print("exited gh compliance")
    # })

    
    # end of message code
    #display notifications to students who aren't doing well
    notd=read.csv("/home/sxavier2/spring16/notification_table.csv")
    u_notd=tail(na.omit(notd[notd$email==useremail,]),4)
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
    output$titl=renderMenu({
      dashboardHeader(title=paste("Hey, ",user_firstname,sep=""))
    })
    #end scrumwise logic  
  }
  
  
  admin_users=function(instructor_level,eid)
  {
    if(instructor_level=="taiga_instructor")
    {
      shinyjs::hide("welcome_tab")
      shinyjs::hide("forgot_pwd")
      shinyjs::hide("userpwd")
      shinyjs::hide("firstpwd")
      shinyjs::hide("secondpwd")
      shinyjs::hide("non_member")
      shinyjs::hide("forgot_cnfrm")
	  shinyjs::hide("pwd_submit")
      useremail=eid
      instr_all_data=taiga_mem_table[taiga_mem_table$instructor_email==useremail,]
      instr_courses=as.character(unique(instr_all_data$course))
      output$admin_courses=renderUI(selectInput("instcourse","Select a course",c("",instr_courses),selected = NULL,multiple = F))
      observe({
        if(!is.null(input$instcourse))
        {
          if(nchar(input$instcourse)>1)
          {
            chosen_course=as.character(input$instcourse)
            instr_course_teams=as.character(unique(instr_all_data[instr_all_data$course==chosen_course,"project_name"]))
            output$admin_teams=renderUI(selectInput("instteams","Select a team",c("",instr_course_teams),selected = NULL,multiple = F))
            observe({
              if(!is.null(input$instteams) )
              {
                if(nchar(input$instteams)>1)
                {
                  chosen_team=as.character(input$instteams)
                  print("check")
                  print(chosen_team)
                  team_chosen_members=as.character(instr_all_data[instr_all_data$project_name==chosen_team,"full_name"])
                  output$indi_student_view=renderUI(selectInput("indistudents","Select a student",c("",team_chosen_members),selected = NULL,multiple = F))
                  observe({
                    if(!is.null(input$indistudents))
                    {
                      if(nchar(input$indistudents)>1)
                      {
                        output$taiga_admin_get=renderUI(actionButton("getDB","Retrieve"))
                        shinyjs::onclick("getDB",{
                          chosen_student=as.character(input$indistudents)
                          student_email_id=as.character(instr_all_data[instr_all_data$full_name==chosen_student,"email"])
                          print(paste("admin student selected is",student_email_id))
                          taiga(student_email_id,0)
                        })
                      }
                    }
                  })
                }
              }
            })
          }
        }
        
      })
      
    }  else if(instructor_level=="scrumwise_instructor")
    {
      useremail=eid
      shinyjs::hide("welcome_tab")
      shinyjs::hide("forgot_pwd")
      shinyjs::hide("userpwd")
      shinyjs::hide("firstpwd")
      shinyjs::hide("secondpwd")
      shinyjs::hide("non_member")
      shinyjs::hide("forgot_cnfrm")
	  shinyjs::hide("pwd_submit")
	  shinyjs::hide("forgot_cnfrm")
      instr_all_data=sw_mem_table[sw_mem_table$instructor_email==useremail,]
      instr_courses=as.character(unique(instr_all_data$course))
      output$admin_courses=renderUI(selectInput("instcourse","Select a course",c("",instr_courses),selected= NULL,multiple = F))
      observe({
        if(!is.null(input$instcourse))
        {
          if(nchar(input$instcourse)>1)
          {
            chosen_course=as.character(input$instcourse)
            instr_course_teams=as.character(unique(instr_all_data[instr_all_data$course==chosen_course,"project_name"]))
            output$admin_teams=renderUI(selectInput("instteams","Select a team",c("",instr_course_teams),selected = NULL,multiple = F))
            observe({
              if(!is.null(input$instteams))
              {
                if(nchar(input$instteams)>1)
                {
                  chosen_team=as.character(input$instteams)
                  team_members=as.character(instr_all_data[instr_all_data$project_name==chosen_team,"last_name"])
                  output$indi_student_view=renderUI(selectInput("indistudents","Select a student",c("",team_members),selected = NULL,multiple = F))
                  observe({
                    if(!is.null(input$indistudents))
                    {
                      if(nchar(input$indistudents)>1)
                      {
                        output$sw_admin_get=renderUI(actionButton("getDB1","Retrieve"))
                        shinyjs::onclick("getDB1",{
                          chosen_student=as.character(input$indistudents)
                          student_email_id=as.character(instr_all_data[instr_all_data$last_name==chosen_student,"email"])
                          scrumwise(student_email_id,0)
                        })
                      }
                    }
                  })
                }
              }
            })
          }
        }
        
      })
    }
  }
  
  super_user=function()
  {
    shinyjs::hide("forgot_pwd")
    sw_courses=as.character(unique(sw_mem_table$course))
    taiga_courses=as.character(unique(taiga_mem_table$course))
    all_super_courses=c(sw_courses,taiga_courses)
    print(all_super_courses)
    
    output$see_courses=renderUI(selectInput("seladmincourse","Select a course:", choices = c("",all_super_courses),selected = NULL,multiple = F))
    
     observe({
    if(!is.null(input$seladmincourse))
    {
      if(nchar(input$seladmincourse)>1)
      {
        super_selected_course=as.character(input$seladmincourse)
        print(super_selected_course)
        if(super_selected_course=="Capstone SER402")
        {
          super_teams=as.character(unique(sw_mem_table$project_name))
          output$superswteams=renderUI(selectInput("super_sw_teams","Select a team:",c("",super_teams),selected = NULL,multiple = F))
          selected_superstd=""
          observe({
            if(!is.null(input$super_sw_teams))
            {
              if(nchar(input$super_sw_teams)>1)
              {
                super_selected_swteam=input$super_sw_teams
                super_students=as.character(sw_mem_table[sw_mem_table$project_name==super_selected_swteam,"first_name"])
                output$superswstudents=renderUI(selectInput("super_sw_students","Select a student:",c("",super_students),selected = NULL,multiple = F))
                observe({
                  if(!is.null(input$super_sw_students))
                  {
                    if(nchar(input$super_sw_students)>1)
                    {
                      print("name is")
                      print(selected_superstd)
                      output$super_button=renderUI(actionButton("superbutton","Retrieve"))
                      shinyjs::onclick("superbutton",{
                        selected_superstd=as.character(input$super_sw_students)
                        selected_superstd_email=as.character(sw_mem_table[sw_mem_table$first_name==selected_superstd,"email"])
                          print(selected_superstd_email)
                          scrumwise(selected_superstd_email,0)
                      })
                    }
                  }
                })
              }
            }
          })
        }
        else 
        {
          super_teams_taiga=as.character(unique(taiga_mem_table$project_name))
          output$supertaigateams=renderUI(selectInput("super_taiga_teams","Select a team:",c("",super_teams_taiga),selected = NULL,multiple = F))
          observe({
            if(!is.null(input$super_taiga_teams))
            {
              if(nchar(input$super_taiga_teams)>1)
              {
              super_selected_taigateam=input$super_taiga_teams
              super_students1=as.character(taiga_mem_table[taiga_mem_table$project_name==super_selected_taigateam,"full_name"])
              output$supertaigastudents=renderUI(selectInput("super_taiga_students","Select a student:",c("",super_students1),selected = NULL,multiple = F))
              observe({
                if(!is.null(input$super_taiga_students))
                {
                  if(nchar(input$super_taiga_students)>1)
                  {
                    output$retrieve_button=renderUI(actionButton("retrieve","Retrieve"))
                    shinyjs::onclick("retrieve",{
                      selected_superstd1=input$super_taiga_students
                      selected_superstd_email1=as.character(taiga_mem_table[taiga_mem_table$full_name==selected_superstd1,"email"])
                      taiga(selected_superstd_email1,0)
                    })
                  }
                }
              })
            }
            }
          })
        }
      } 
    }
      })
  }
  
  #end of admin and super user functions
  #for demonstration purpose
  demo_user=function(mailid)
  {
    shinyjs::hide("welcome_tab")
    shinyjs::hide("forgot_pwd")
    shinyjs::hide("userpwd")
    shinyjs::hide("firstpwd")
    shinyjs::hide("secondpwd")
    shinyjs::hide("non_member")
    shinyjs::hide("pwdsubmit")
    shinyjs::hide("forgot_cnfrm")
    useremail=mailid
    print("in taiga")
    
    #retrieve user Taiga info
    all_taiga_data1=read.csv("/home/sxavier2/spring16/taiga_data_demo.csv")
    user_taiga_data1=all_taiga_data1[all_taiga_data1$email==useremail,c("in_progress","to_test","done","date")]
    #proceed with inp,tot,done,date,for chart1
    
    #retrieve user's taiga scores
    all_taiga_scores1=read.csv("/home/sxavier2/spring16/Taiga_Weight_Demo.csv")
    user_taiga_scores1=all_taiga_scores1[all_taiga_scores1$email==useremail,]
    print(user_taiga_scores1)
    #proceed with diff,exp,date for chart2
    
    #ALL Taiga data retrieved!!
    print("taiga set")
    
    #fetch gh data
    
    all_gh_scores_tg1=read.csv("/home/sxavier2/spring16/GH_DF_demo.csv")
    all_gh_scores_tg1$loca=all_gh_scores_tg1$loca/1000
    all_gh_scores_tg1$locd=all_gh_scores_tg1$locd/100
    user_gh_scores_tg1=NULL
    
    user_gh_scores_tg1=all_gh_scores_tg1[all_gh_scores_tg1$email==useremail,]
    print(user_gh_scores_tg1)
    locA_tg1=sum(user_gh_scores_tg1$loca)*1000
    locD_tg1=sum(user_gh_scores_tg1$locd)*100
    commits_tg1=sum(user_gh_scores_tg1$commits)
    
    #fetch gh data for compliance charts
    gh_comp_scores1=read.csv("/home/sxavier2/spring16/GH_Taiga_Weight_Demo.csv")
    gh_comp_user_score1=gh_comp_scores1[gh_comp_scores1$email==useremail,]
    
    print("gh set")
    #display tabs for students with relevenat date pickers on each tab
    output$tabs_tg=renderUI(tabsetPanel(id="mainPanel_12",
                                        tabPanel("Overall-Activity",fluidRow(box(showOutput("Spiderweb_12","highcharts"),title="Overall Activity This Week",width=6),
                                                                             box(showOutput("Spiderweb_22","highcharts")
                                                                                 ,title="Overall Activity Last Week",width=6))),
                                        tabPanel("Taiga Activity",h3("Your Daily Taiga Activity Monitor")
                                                 , showOutput("taig_chart12","highcharts"),br(),h3("Your Weekly Taiga Compliance Monitor"),showOutput("taigachartax","highcharts")),
                                        tabPanel("Git Hub Activity",h3("Your Daily GitHub Activity Monitor"),
                                                 valueBox(commits_tg1,"Total number of commits made",color="aqua",icon = icon("credit-card"),width=4),
                                                 valueBox(locA_tg1,"Total Lines of Code Added",icon = icon("list"),color="aqua",width=4),
                                                 valueBox(locD_tg1,"Total Lines of Code Deleted",icon = icon("list-alt"),color="aqua",width=4)
                                                 ,showOutput("gh_chart1_taiga1","highcharts"),br()
												 ,h3("Your Weekly GitHub Compliance Monitor"),
												 showOutput("ghchartay","highcharts"))))
    
    #  plot first radial chart
    output$Spiderweb_12 <- renderChart2({
      plot <- Highcharts$new()
      table = data.frame(id = c("GitHub Impact","Taiga Impact","Taiga Frequency","GitHub Frequency"),
                         value6 = c(4.5,2.5,3,4.6),
                         value7 = c(3.8,4,3.3,3.8),
                         value8 = c(2.6,2.4,3.2,3.1)
      )
      plot$chart(polar = TRUE, type = "line", width=450)
      plot$xAxis(categories=table$id, tickmarkPlacement= 'on', lineWidth= 0)
      plot$yAxis(gridLineInterpolation= 'polygon',
                 lineWidth= 0, min= 0)
      plot$series(data = table[,"value6"],
                  name = "Your scores", pointPlacement="on")
      plot$series(data = table[,"value7"],
                  name = "Team average scores", pointPlacement="on",color="maroon", opacity=0.5)
      plot$series(data = table[,"value8"],
                  name = "Class average scores", pointPlacement="on",color="green", opacity=0.5)
      return(plot)
    })
    
    #plot Spiderweb_2
    output$Spiderweb_22 <- renderChart2({
      plot <- Highcharts$new()
      table1 = data.frame(id = c("GitHub Impact","Taiga Impact","Taiga Frequency","GitHub Frequency"),
                          value11 = c(2.8,2.6,3.2,3.4),
                          value12 = c(3.8,3.6,3.9,4),
                          value13= c(2.6,2.9,1.9,3)
      )
      plot$chart(polar = TRUE, type = "line", width=450)
      plot$xAxis(categories=table1$id, tickmarkPlacement= 'on', lineWidth= 0)
      plot$yAxis(gridLineInterpolation= 'polygon',
                 lineWidth= 0, min= 0)
      plot$series(data = table1[,"value11"],
                  name = "Your scores", pointPlacement="on")
      plot$series(data = table1[,"value12"],
                  name = "Team average scores", pointPlacement="on",color="maroon", opacity=0.5)
      plot$series(data = table1[,"value13"],
                  name = "Class average scores", pointPlacement="on",color="green", opacity=0.5)
      return(plot)
    })
    
    #plot taiga daily chart
    
    taig_chart12_data=user_taiga_data1[c("in_progress","to_test","done")]
    taig_chart12_date=user_taiga_data1$date
    
    colnames(taig_chart12_data)=c("In Progress","To Test","Done")
    output$taig_chart12 <- renderChart2({
      # Create chart
      ao <- rCharts:::Highcharts$new()
      ao$chart(type = "column", width=1000)
      ao$xAxis(categories = taig_chart12_date, title=list(text="Date"))
      ao$yAxis(title = list(text = "Number of Tasks"))
      ao$data(taig_chart12_data)
      ao$exporting(enabled=T)
      ao$plotOptions(column = list(stacking = input$plotType))
      return(ao)
    })
    
    # plot taiga compliance chart
    u_ghdx=user_taiga_scores1
    output$taigachartax <- renderChart2({
      x77x=u_ghdx$date
      x6666x=u_ghdx[c(3,5)]
      colnames(x6666x) =c("Your Taiga Activity For the Week", "Expected Taiga Activity for the Week")
      # Create charts
      bo <- rCharts:::Highcharts$new()
      bo$chart(type = "spline",width=1000)
      # a$title(text = "Taiga Compliance Measure")
      bo$xAxis(categories = x77x,title=list(text="Date"))
      bo$yAxis(title = list(text = "Scrumboard task updates"))
      bo$data(x6666x)
      return(bo)
    })
    
    gh_chart1_date1=as.character(user_gh_scores_tg1$date)
    gh_chart1_data1=user_gh_scores_tg1[c("commits","loca","locd")]
    
    colnames(gh_chart1_data1)=c("Number of commits","Lines of Code Added/1000","Lines of Code Deleted/100")
    output$gh_chart1_taiga1 <- renderChart2({
      #   Create chart
      co <- rCharts:::Highcharts$new()
      co$chart(type = "column", width=1000)
      co$xAxis(categories = gh_chart1_date1, title=list(text="Date"))
      co$yAxis(title = list(text = "Counts"))
      co$data(gh_chart1_data1)
      co$exporting(enabled=T)
      co$plotOptions(column = list(stacking = input$plotType))
      return(co)
    })
    
    # plot gh compliance chart
    u_bhdy=gh_comp_user_score1
    output$ghchartay <- renderChart2({
      x771y=u_bhdy$date
      x666y=u_bhdy[c(6,7)]
      colnames(x666y) =c("Your Code Activity for the Week","Expected Code Activity For the Week")
      #  Create charts
      dy <- rCharts:::Highcharts$new()
      dy$chart(type = "spline",width=1000)
      #d$title(text = "Git Hub Compliance Measure")
      dy$xAxis(categories = x771y,title=list(text="Date"))
      dy$yAxis(title = list(text = "Git Hub task updates"))
      dy$data(x666y)
      return(dy)
    })
    
    #display notifications
    notd=read.csv("/home/sxavier2/spring16/notification_table_demo.csv")
    u_notd=tail(na.omit(notd[notd$email==useremail,]),4)
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
    
    output$titl=renderMenu({
      dashboardHeader(title="Dashboard Demo")
    })
    
    
  }
  
  ### end of all functions. Identify user and redirect to appropriate function
  output$welcome_tab=renderUI(tabsetPanel("welcome",tabPanel("Home",br(),h4("Welcome to the home page of the Continuous Assessment (CA) research project at the Software Engineering program offered at Arizona State University. If you are a registered member, please login with your email id"),h4("The focus of this research project is to create a platform that provides continuos, formative feedback for students working in a project based course. These projects utilize Agile methods, specifically Scrum to conduct the projects. Scrum is an excellent process model for students in an agile learning setup as it embodies a philosophy of establishing a rhythm to weekly and even daily activities."),tags$img(src="regulated_learning.png",align="center"),h4("However, assessing student activity in an agile, project based course can have its own shortcomings including increased costs for the instructor, delayed feedback and possibly a drop in student compliance. The primary motivation of the Continuous Assessment program is is to gauge project activity and provide real time, formative feedback to enhance learning experience and ensure compliance in project-based courses."),h4("The specific innovation evaluated in this project, is the use of visualizations and notifications to ensure student compliance with the learning process. Students can view their progress via online tools on a daily basis and get regular notifications if they fall behind the expected progress. The initial version of the CA platform supports the integration of 3 tools: GitHub for source code management, and Scrumwise or Taiga for Scrumboards. These tools were chosen as they provide the most important collaborative functionality for supporting software products and organizing a team’s work."))
,tabPanel("Continuous Assessment",br(),h4("Continuous assessment in educational research is defined as the practice of performing frequent assessments in a course context. The practice has become popular in constructivist approaches to (project-based) learning, as frequent assessment provides a feedback mechanism ensuring students are properly aligned with a scaffold learning process. It remains an active area of research, with an ongoing debate surrounding the utility of formative versus summative continuous assessment."),h4("Continuous assessment in a university course tends to follow a scaffold learning process, albeit assessing at a much higher frequency than coarse-grained learning processes typical of a traditional, prescriptive university course. We see the Agile concept of continuous integration and testing, realized in a software technology platform, as a direct means to perform continuous assessment."), h4("The current platform is a web-based integration platform (as seen below) composed of two subsystems, each with its own user-facing components. The primary component, CAssess (short for Continuous Assessment), provides features for 1)
integrating data streams from open tool APIs, 2) performing basic statistical analysis, and 3) displaying visualizations and notifications to students and instructors. The second and supporting system is called Nicest (Nicely Integrating Complex Education Software Together), and has primarily responsibilities for user and team management, and for provisioning the various tools being integrated into CAssess, again via open APIs."),tags$a(href="https://swent0linux.asu.edu/nicest","Visit Nicest",target="_blank"),br(),br(),tags$img(src="nicest_casssess.jpg",align="middle"))))
  
  start_time=Sys.time()
  taiga_mem_table=read.csv("/home/sxavier2/spring16/taiga_membership_table.csv")
  sw_mem_table=read.csv("/home/sxavier2/spring16/sw_membership_table.csv")
  users_pwd=read.csv("/home/sxavier2/spring16/user_pwd.csv")
  observe({
    email_entered=input$emailid  
    if(substr(email_entered,nchar(email_entered)-3,nchar(email_entered))==".edu")
    {
      print(email_entered)
      #first time user
      if(!email_entered  %in% users_pwd$email)
      {
        if(email_entered %in% taiga_mem_table$email || email_entered %in% sw_mem_table$email || email_entered =="admin_user@asu.edu" ||email_entered %in% taiga_mem_table$instructor_email || email_entered %in% sw_mem_table$instructor_email)
        {
          output$first_pwd=renderUI(passwordInput("firstpwd","Create a password"))
          output$second_pwd=renderUI(passwordInput("secondpwd","Confirm password"))
          observe({
            if(!is.null(input$firstpwd))
            {
              if(!is.null(input$secondpwd))
              {
                print(input$firstpwd)
                print(input$secondpwd)
                pwd1=as.character(input$firstpwd)
                pwd2=as.character(input$secondpwd)
                print(pwd1)
                print(pwd2)
                print(length(pwd1))
                if(nchar(pwd1) >1 & pwd1==pwd2)
                {
                  print("they match")
                  output$pwd_submit=renderUI(actionButton("pwdsubmit","Create"))
                  shinyjs::onclick("pwdsubmit",{
                    observe({
                      pwd1_entered=input$pwdsubmit
                      if(length(pwd1_entered)>=1)
                      {
                        pwd_df=data.frame(email_entered,pwd1)
                        print(pwd_df)
                        write.table(pwd_df,"/home/sxavier2/spring16/user_pwd.csv",row.names = F,col.names = F,sep=",",append = T)
                        if(email_entered %in% taiga_mem_table$email)
                        {
                          taiga(email_entered,1)
                        }
                        else if(email_entered %in% sw_mem_table$email)
                        {
                          scrumwise(email_entered,1)
                        }
                        else if(email_entered %in% as.character(unique(taiga_mem_table$instructor_email)))
                        {
                          admin_users("taiga_instructor",email_entered)
                        }
                        else if (email_entered %in% as.character(unique(sw_mem_table$instructor_email)))
                        {
                          admin_users("scrumwise_instructor",email_entered)
                        }
                        else if (email_entered=="admin_user@asu.edu")
                        {
                          super_user()
                        }
                        
                      }
                      
                    })
                  })
                }
              }
            }
          })
        }else if(email_entered=="demo_user@asu.edu")
        {
          demo_user(email_entered)
        }
        else
        {
          output$non_member=renderMenu(sidebarMenu(menuItem("That is not a registered email id")))
        }
      }
      
      else if(email_entered  %in% users_pwd$email)
      {
        reg_pwd=as.character(users_pwd[users_pwd$email==email_entered,"password"])
        output$user_pwd=renderUI(passwordInput("userpwd","Enter your password"))
        observe({
          if(!is.null(input$userpwd))
          {
            entered_pwd=as.character(input$userpwd)
            print(entered_pwd)
            if(entered_pwd==reg_pwd)
            {
              if(email_entered %in% taiga_mem_table$email)
              {
                taiga(email_entered,1)
              }
              else if(email_entered %in% sw_mem_table$email)
              {
                scrumwise(email_entered,1)
              }
              else if(email_entered %in% as.character(unique(taiga_mem_table$instructor_email)))
              {
                admin_users("taiga_instructor",email_entered)
              }
              else if (email_entered %in% as.character(unique(sw_mem_table$instructor_email)))
              {
                admin_users("scrumwise_instructor",email_entered)
              }
              else if (email_entered=="admin_user@asu.edu")
              {
                super_user()
              }
              
            }
          }
          
          output$forgot_pwd=renderUI(actionButton("forgotpwd","Forgot Password?"))
          shinyjs::onclick("forgotpwd",{
            observe({
              output$forgot_cnfrm=renderMenu(sidebarMenu(menuItem("Password has been mailed to you")))
                                                                  
              pwd_forgot=input$forgotpwd
              if(length(pwd_forgot)>=1) 
              {
                forgotText=paste("Your password to the Continuous Assessment dashboard is:",reg_pwd,sep=" ")
                send.mail(from = "cst316asu@gmail.com",
                          to = c(email_entered),
                          subject = "Your password to the Continuous Assessment Dashboard",
                          body = forgotText,
                          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                                      user.name = "cst316asu@gmail.com",            
                                      passwd = "a5uP0l4!", ssl = TRUE),
                          authenticate = TRUE,
                          send = TRUE)
              }
            })
          })
        })
      }
    }
  }) 
  
}

