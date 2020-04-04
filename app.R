#
# A. Cristia 
# alecristia@gmail.Com

## TODO
# keep stats of people's interactions (search terms etc) #https://shiny.rstudio.com/articles/google-analytics.html
# add a score board

library(shiny)
library(RCurl)
library(markdown)


docloc='https://docs.google.com/spreadsheets/d/e/2PACX-1vT4ELiQ_bZVdfYjO1g1jCbi4ACy4E2NcHrNo1QFY4oRZgzzyZQpo0W41larKH7S-xStlrAjb-36WPiq/pub?gid=565157252&single=true&output=csv'
myfile <- getURL(docloc, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
raw<- read.csv(textConnection(myfile), header=T)

ui <- shinyUI(
   
   pageWithSidebar(
   headerPanel("Criminal Case App"),
   sidebarPanel(
      selectInput("chosen", "Choose your story:",
                  levels(raw$story_name), selected=as.factor(levels(raw$story_name)[1])),
      textInput("test", "Enter text to grep (RegEx: .*|)",value="zzz"),
      textInput("crime", "Enter 1 word to identify the crime (fixed)"),
      textInput("culprit", "Enter 1 word to identify the culprit (fixed)"),
      textInput("proof", "Enter 1 word to identify the unequivocal proof (fixed)"),
      submitButton(text = "Submit")
   ),
   mainPanel(  
      tabsetPanel(
      tabPanel("Instructions", includeMarkdown("instructions.md")) ,
      tabPanel("Story", verbatimTextOutput("pcdisc"), 
               tableOutput("Variable"))
      
      )
   )
))


server <- function(input, output) {
   
   new_chosen<- reactive({
      chosen<-input$chosen
      
     # print(chosen)
      
      crime_list=unlist(strsplit(as.character(raw$crime[raw$story_name==chosen])," "))
      culprit_list=unlist(strsplit(as.character(raw$culprit[raw$story_name==chosen])," "))
      proof_list=unlist(strsplit(as.character(raw$proof[raw$story_name==chosen])," "))
      author_name=raw$your_name[raw$story_name==chosen]
      
      my_data<<-NULL
      my_data$statement<<-unlist(strsplit(as.character(raw$story_text[raw$story_name==chosen]),"\n+"))
      my_data$discovered<<-F #whether this has been discovered (cumulative)
      my_data$new<<-T #whether this has been discovered just now
      my_data$show<<-F #whether the line should be shown
      my_data<<-data.frame(my_data)
      my_data$nline<<-c(1:dim(my_data)[1])
      my_data<<-my_data[sample(my_data$nline),]
      #print(head(my_data))
      return(list(my_data,chosen,crime_list,culprit_list,proof_list,author_name))      
   })
   
   is_contained <- reactive({
      test <- input$test
      
      temp=grepl(test, my_data$statement, ignore.case = TRUE, perl=TRUE)

      if(sum(temp)>5){
         matches=cbind(temp,1:length(temp))
         selected=tail(matches[order(temp),2],5)
         matches[,1]<-F
         matches[selected,1]<-T
         temp<-matches[,1]
      }
      my_data$show<<-temp
      my_data[,c("new")]<<-ifelse(temp==1 & my_data$discovered==F,TRUE,FALSE)
      my_data[temp==1,c("discovered")]<<-T
      
      return(my_data)
      
   })
   
   is.solved<-reactive({
      crime <- tolower(input$crime)
      culprit <- tolower(input$culprit)
      proof <- tolower(input$proof)
      return(c(crime,culprit,proof))
   })
   
   output$Variable = renderTable({
      x=new_chosen()
      my_data=x[1]
      chosen=x[2]
      my_data=is_contained()
      my_data[my_data$show==1,c("nline","statement","new")]
   })
   
   output$pcdisc <- renderText({ 
      x <- new_chosen()
      my_data=x[1]
      chosen=x[2]
      crime_list=x[3]
      culprit_list=x[4]
      proof_list=x[5]
      author_name=x[6]
      
      my_data <- is_contained()
      solution <- is.solved()
      solved=paste0("You have not yet solved \"",chosen,"\"\n")
      if(solution[1] %in% crime_list & solution[2] %in% culprit_list & solution[3] %in% proof_list) solved=paste0("You have solved \"",chosen,"\"by", author_name, "!\n")
      print(chosen)
      print(head(x[1]))
      print(head(my_data))
      paste(solved,"\nYou have discovered",round(sum(my_data$discovered)/dim(my_data)[1]*100),"% of the statement")
   })
   
}


# Run the application 
shinyApp(ui = ui, server = server)

