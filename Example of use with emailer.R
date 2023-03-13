devtools::install_github("LiamOMueller/UCSanDiego_BILD5")


library(DataSimulatoR)

?DataSimulatoR
data<-read.csv("WI23_2.csv")
DataSimulatoR(data)


library(Microsoft365R)
emaillist<-read.csv("WI23_2.csv")

#Authenticate with browser
my_outlook <- get_business_outlook()


#Names of the csv files to attach
files<-paste(emaillist[,5],".csv",sep="")

#send an email to each student on the list

for(i in 1:length(emaillist[,6])){
  
  
  
  #compose email
  my_email<-my_outlook$create_email("Here is your final project data for BILD 5!",subject="BILD 5 Fall 22_Final project data",to=emaillist[i,6])$
    add_attachment(files[i])
  
  my_email$send()
  
}

