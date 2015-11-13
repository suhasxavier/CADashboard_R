library(shinydashboard)
dashboardPage(
  
  
  dashboardHeader(dropdownMenuOutput("meessageMenu"),dropdownMenuOutput("notiMenu"),dropdownMenuOutput("titl"),dropdownMenuOutput("taskMenu")),
  dashboardSidebar(textInput("emailid","Enter your email address"),uiOutput("admin_view")),
  dashboardBody(uiOutput("tabs1"),uiOutput("introbox")
               # ,uiOutput("errortab")
               ,tags$head(includeScript("/home/sxavier2/all_files/google-analytics.js"))
                )
)