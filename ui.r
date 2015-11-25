library(shinydashboard)
library(shinyjs)
dashboardPage(
  
  
  dashboardHeader(title="CA Dashboard",dropdownMenuOutput("meessageMenu"),dropdownMenuOutput("notiMenu"),dropdownMenuOutput("titl"),dropdownMenuOutput("taskMenu")),
  dashboardSidebar(textInput("emailid","Enter your email address"),uiOutput("admin_view"),sidebarMenuOutput("teamName_display"),sidebarMenuOutput("admin_pwd")),
  dashboardBody(shinyjs::useShinyjs(),uiOutput("tabs1"),uiOutput("introbox")
                # ,uiOutput("errortab")
                ,tags$head(includeScript("/home/sxavier2/all_files/google-analytics.js"))
  )
)