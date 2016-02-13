library(shinydashboard)
jscode="window.onunload =window.onbeforeunload = function() 
{
Shiny.onInputChange('userleft', 1); 
}"

ui <- dashboardPage(
  dashboardHeader(title="CA Dasboard",
                  dropdownMenuOutput("messageMenu")
                  ,dropdownMenuOutput("notiMenu")
                  ,dropdownMenuOutput("titl"),dropdownMenuOutput("team_name_display")),
  dashboardSidebar(
    textOutput("Registered user?"),
    textInput("emailid","Enter your email address"),
               #    uiOutput("instr_course_view"),
                 uiOutput("admin_courses")
                         ,uiOutput("admin_teams")
                     ,uiOutput("indi_student_view")
#                    uiOutput("admin_view"),
#                    uiOutput("drop_down_students"),
#                     textOutput("sideText")
                   , sidebarMenuOutput("feedback_icon")
                    ,sidebarMenuOutput("non_member")
                    
#                  ,  uiOutput("feedback_box") 
#                  ,  uiOutput("submitFeedback")
#                 ,    sidebarMenuOutput("feedback_succes")


            #new ones
            ,uiOutput("first_pwd")
            ,uiOutput("second_pwd")
            ,uiOutput("pwd_submit")
            ,uiOutput("user_pwd")
            ,uiOutput("forgot_pwd")
            ,uiOutput("see_courses")

          #admins
          ,uiOutput("superswteams")
          ,uiOutput("superswstudents")
          ,uiOutput("supertaigateams")
          ,uiOutput("supertaigastudents")
          ,uiOutput("super_button")
          ,uiOutput("retrieve_button")
          ,uiOutput("sw_admin_get")
          ,uiOutput("taiga_admin_get")
          ,sidebarMenuOutput("forgot_cnfrm")
                  ),
  dashboardBody(fluidPage (
    shinyjs::useShinyjs(),
    uiOutput("welcome_tab"),
    uiOutput("tabs1")
    ,uiOutput("tabs2")
    ,uiOutput("tabs_tg")
    ,uiOutput("introbox") 
    ,textOutput("email_error")
 ,   #,uiOutput("errortab")
  tags$head(tags$script(jscode))
)))