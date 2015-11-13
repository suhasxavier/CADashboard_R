# CADashboard_R
Contains all code to create the dashboard with R and Shiny.
# current setup
1. Start with the taiga.R file which will retrieve all the users on Taiga along with relevant details such as project name, mail_ids, and uuids required to access the csv reports. All this data is populated into the master table called membership_table. Currently all data is stored in files; this will continue until we reach a state of stability. Post this, it will pushed onto a RDBMS.
2. The IndiTaigaData.R file runs daily. It is run with a cron job that executes at 10PM everyday. This script reads data from the membership_table and for each user retrieves the uuid for the tasks report and GETs the file and stores count of tasks that are assigned to a student(tasks can be in various statuses)
3. This is followed by the Taiga_Weight.R script. This is run once every week. It calculates the difference in the number of tasks that have changed statuses in the pst 7 days and assigns a score and loads these scores onto a file. Based on the scores, a notification is also created and stored in a seperate file.
4. fetchTaiga_GH.R reads data from the membership_table and uses the GitHub API to get data of all students. It also calculates a score based on the commit count and lines of code added and generates notifications which are also pushed to the notification table.
5. ui.R describes the front end for creation of the dashboard. It is created dynamically based on the email address entered in the text field.
6. server.R is thebackend. Based on the email address entered, it retrieves data from all the files and displays data. 



