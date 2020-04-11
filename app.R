#
# A. Cristia 
# alecristia@gmail.com

## TODO
# fix bug in confirmation
# fix bug in N of searches done

## improvements
# keep stats of people's interactions (search terms etc) #https://shiny.rstudio.com/articles/google-analytics.html
# add a score board

library(shiny)
library(RCurl)
library(markdown)


docloc='https://docs.google.com/spreadsheets/d/e/2PACX-1vT4ELiQ_bZVdfYjO1g1jCbi4ACy4E2NcHrNo1QFY4oRZgzzyZQpo0W41larKH7S-xStlrAjb-36WPiq/pub?gid=565157252&single=true&output=csv'
myfile <- getURL(docloc, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
raw<- read.csv(textConnection(myfile), header=T)

seed=23

n_searches=-1

ui <- shinyUI(
   pageWithSidebar(
      headerPanel("Criminal Case App"),
      sidebarPanel(
         # Only show this panel if in the instructions tab
         conditionalPanel(
            condition="input.tabselected==1",
            selectInput("chosen", "Choose your story:",
                        levels(raw$story_name), selected=as.factor(levels(raw$story_name)[1]))
         ),
         # Only show this panel if in the explore tab
         conditionalPanel(
            condition="input.tabselected==2",
            textInput("test", "Enter text to grep (RegEx: .*|)",value="zzz")
         ),
         # Only show this panel if in the confirm tab
         conditionalPanel(
            condition="input.tabselected==3",
            textInput("crime", "Enter 1 word to identify the crime (fixed)"),
            textInput("culprit", "Enter 1 word to identify the culprit (fixed)"),
            textInput("proof", "Enter 1 word to identify the proof (fixed)")
         ),
      ), #end sidebarpanel
      mainPanel(  
         tabsetPanel(
            tabPanel("Instructions", value=1, includeMarkdown("instructions.md")) ,
            tabPanel("Explore", value=2, verbatimTextOutput("search"),tableOutput("Variable")),
            tabPanel("Confirm", value=3, verbatimTextOutput("pcdisc")), 
            id = "tabselected"
         )
      )#end main panel
   )#end page with side bar
)#end shinyUI


server <- function(input, output) {
   
   new_chosen<- reactive({
      chosen<-input$chosen
      
      # print(chosen)
      
      crime_list=unlist(strsplit(as.character(raw$crime[raw$story_name==chosen])," "))
      culprit_list=unlist(strsplit(as.character(raw$culprit[raw$story_name==chosen])," "))
      proof_list=unlist(strsplit(as.character(raw$proof[raw$story_name==chosen])," "))
      author_name=as.character(raw$your_name[raw$story_name==chosen])
      
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
   
   output$search <- renderText({ 
      my_data <- is_contained()
      n_searches<<-n_searches+1
      paste("You have done ",n_searches," searches and \n discovered",round(sum(my_data$discovered)/dim(my_data)[1]*100),"% of the statement")
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
      paste0("You have guessed ",round(sum(solution[1] %in% crime_list , solution[2] %in% culprit_list , solution[3] %in% proof_list)/3*100), "% of \"",chosen,"\" by ", author_name, "!\n")
     # print(chosen)
      #print(head(x[1]))
      #print(head(my_data))
   })
   
}


# Run the application 
shinyApp(ui = ui, server = server)

