#
# A. Cristia 
# alecristia@gmail.com

## TODO
# fix bug in confirmation ## done
# fix bug in N of searches done ##done

## improvements
# keep stats of people's interactions (search terms etc) #https://shiny.rstudio.com/articles/google-analytics.html ## done, insert own <UA-xxx-1> in 'google-analytics.js'.
# add a score board ## not part of assignment

library(shiny)
library(RCurl)
library(markdown)


docloc='https://docs.google.com/spreadsheets/d/e/2PACX-1vT4ELiQ_bZVdfYjO1g1jCbi4ACy4E2NcHrNo1QFY4oRZgzzyZQpo0W41larKH7S-xStlrAjb-36WPiq/pub?gid=565157252&single=true&output=csv'
myfile <- getURL(docloc, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
raw<- read.csv(textConnection(myfile), header=T)

seed=23

n_searches=0

ui <- fluidPage(
  #tags$head(includeHTML(("google-analytics.html"))),
  tags$head(includeScript("google-analytics.js")),
  titlePanel("Criminal Case App"),
  sidebarLayout(
      sidebarPanel(
         # Only show this panel if in the instructions tab
         conditionalPanel(
            condition="input.tabselected==1",
            selectInput("chosen", "Choose your story:",
                        choices = raw$story_name, selected=raw$story_name[1]),
            actionButton("goStory","Go!")
         ),
         # Only show this panel if in the explore tab
         conditionalPanel(
            condition="input.tabselected==2",
            textInput("test", "Enter text to grep (RegEx: .*|)",value="zzz"),
            actionButton("goSearch","Go!")
         ),
         # Only show this panel if in the confirm tab
         conditionalPanel(
            condition="input.tabselected==3",
            textInput("crime", "Enter 1 word to identify the crime (fixed)"),
            textInput("culprit", "Enter 1 word to identify the culprit (fixed)"),
            textInput("proof", "Enter 1 word to identify the proof (fixed)"),
            actionButton("goGuess","Go!")
         )
      ), #end sidebarPanel
      mainPanel(
        verbatimTextOutput("chosen"),
         tabsetPanel(
            tabPanel("Instructions", value=1, includeMarkdown("instructions.md")) ,
            tabPanel("Explore", value=2, verbatimTextOutput("search"),tableOutput("Variable")),
            tabPanel("Confirm", value=3, verbatimTextOutput("pcdisc")), 
            id = "tabselected"
         )
      ) #end mainPanel
   ) #end sidebarLayout
) #end fluidPage


server <- function(input, output) {
   
  chosenStory <- eventReactive(
    input$goStory,{
      chosen<-input$chosen
      paste0("You have chosen: ",chosen,".")
    }) 
  
  output$chosen <- renderText({
    chosenStory()
  })
   
   new_chosen<- reactive({
      chosen<-input$chosen
      crime_list=unlist(strsplit(tolower(as.character(raw$crime[raw$story_name==chosen]))," "))
      culprit_list=unlist(strsplit(tolower(as.character(raw$culprit[raw$story_name==chosen]))," "))
      proof_list=unlist(strsplit(tolower(as.character(raw$proof[raw$story_name==chosen]))," "))
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
     crime <- ifelse(length(unlist(strsplit(tolower(input$crime)," ")))>0,unlist(strsplit(tolower(input$crime)," "))," ")
     culprit <- ifelse(length(unlist(strsplit(tolower(input$culprit)," ")))>0,unlist(strsplit(tolower(input$culprit)," "))," ")
     proof <- ifelse(length(unlist(strsplit(tolower(input$proof)," ")))>0,unlist(strsplit(tolower(input$proof)," "))," ")
     return(c(crime,culprit,proof))
   })
   
   reTableSearch <- eventReactive(
     input$goSearch,{
       x=new_chosen()
       my_data=x[1]
       chosen=x[2]
       my_data=is_contained()
       my_data[my_data$show==1,c("nline","statement","new")]
     })
   
   output$Variable = renderTable({
      reTableSearch()
   })
   
   reTextSearch <- eventReactive(
     input$goSearch,{
       my_data <- is_contained()
       n_searches<<-n_searches+1
       paste("You have done ",n_searches," searches and \n discovered",round(sum(my_data$discovered)/dim(my_data)[1]*100),"% of the statement")
     })
   
   output$search <- renderText({ 
     reTextSearch()
   })
   
   reTextGuess <- eventReactive(
     input$goGuess,{
       x <- new_chosen()
       my_data=x[1]
       chosen=x[2]
       crime_list=unlist(x[3])
       culprit_list=unlist(x[4])
       proof_list=unlist(x[5])
       author_name=x[6]
       
       my_data <- is_contained()
       solution <- is.solved()
       solution_1 = solution[1] %in% crime_list 
       solution_2 = solution[2] %in% culprit_list
       solution_3 = solution[3] %in% proof_list
       sumSol = length(solution_1) + length(solution_2)+ length(solution_3)
       paste0("You have guessed ",round(sum(solution_1,solution_2,solution_3)/sumSol*100), "% of \"",chosen,"\" by ", author_name, "!\n")
       # print(chosen)
       #print(head(x[1]))
       #print(head(my_data)) 
     })
   
   output$pcdisc <- renderText({ 
     reTextGuess()
   })
   
}


# Run the application 
shinyApp(ui = ui, server = server)

