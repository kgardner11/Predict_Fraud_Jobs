library(shiny)
library(dplyr)
library(tidyverse)
library(tidytext)
library(stringr) 
library(regclass) 
library(caret)
library(pROC)


data <- read.csv("data.csv")
data <- data[complete.cases(data),]
data$fraudulent <- as.factor(data$fraudulent)
data$fraudulent <- ifelse(data$fraudulent=="0","Yes","No")
fitControl <- trainControl(method="cv", number=5)
svmGrid <- expand.grid( C=10^seq(-2.5,-1.5,by=.5) )
SVM <- train(fraudulent~company_profile_length+description_length+requirements_length+benefits_length+candidatesinprofile+
               recruitinginprofile+bonusinprofile+careerinprofile+staffinginprofile+experienceinprofile+industryinprofile+
               dataintitle+entryintitle+assistantintitle+homeintitle+clerkintitle+payrollintitle+administrativeintitle+
               ampindescription+timeindescription+positionindescription+projectindescription+ampinrequirments+computerinrequirements+
               traininginbenefits+environmentinbenefits+lifeinbenefits, data=data, method='svmLinear', tuneGrid = svmGrid,
             trControl=fitControl, verbose=FALSE, preProc = c("center", "scale")) 

ui <- fluidPage(

    # Application title
    titlePanel("Is this job posting legitimate?"),
    
    sidebarLayout(
        sidebarPanel(
            h5("Please copy and paste the job title, description, benefits, requirements, and company profile from the job posting. If any of the items are not specified in the job posting, type N/A into the box."),
            textInput("title", "Job Title", ""),
            verbatimTextOutput("title1"),
            verbatimTextOutput("title2"),
            verbatimTextOutput("title3"),
            verbatimTextOutput("title4"),
            verbatimTextOutput("title5"),
            verbatimTextOutput("title6"),
            verbatimTextOutput("title7"),
            textInput("description","Job Description",""),
            verbatimTextOutput("descr1"),
            verbatimTextOutput("descr2"),
            verbatimTextOutput("descr3"),
            verbatimTextOutput("descr4"),
            verbatimTextOutput("descr5"),
            textInput("benefits","Job Benefits",""),
            verbatimTextOutput("bene1"),
            verbatimTextOutput("bene2"),
            verbatimTextOutput("bene3"),
            verbatimTextOutput("bene4"),
            textInput("requirements","Job Requirements",""),
            verbatimTextOutput("req1"),
            verbatimTextOutput("req2"),
            verbatimTextOutput("req3"),
            textInput("profile","Company Profile",""),
            verbatimTextOutput("pro1"),
            verbatimTextOutput("pro2"),
            verbatimTextOutput("pro3"),
            verbatimTextOutput("pro4"),
            verbatimTextOutput("pro5"),
            verbatimTextOutput("pro6"),
            verbatimTextOutput("pro7"),
            verbatimTextOutput("pro8"),
            actionButton("submit","Submit")
        ) ,

        mainPanel(
          img(src='job_hunt.png',height=400,width=400),
          DT::dataTableOutput("JobTable"),
          h5("This model will predict with 96.96% accuracy whether the job posting is legitimate or fraudulent. If the answer is 'Yes', that means the job posting is predicted to be legitimate!"),
          verbatimTextOutput("pred")
        )
        
    )
)

server <- function(input, output, session) {

  v <- reactiveValues()
        v$df <- data.frame(company_profile_length = numeric(), description_length = numeric(),requirements_length = numeric(), 
                           benefits_length = numeric(), candidatesinprofile = numeric(), recruitinginprofile = numeric(), bonusinprofile = numeric(), 
                           careerinprofile = numeric(), staffinginprofile = numeric(), experienceinprofile = numeric(), industryinprofile = numeric(), 
                           dataintitle = numeric(), entryintitle = numeric(), assistantintitle = numeric(), homeintitle = numeric(), clerkintitle = numeric(),
                           payrollintitle = numeric(), administrativeintitle = numeric(), ampindescription = numeric(), timeindescription = numeric(), 
                           positionindescription = numeric(), projectindescription = numeric(), ampinrequirments = numeric(), computerinrequirements = numeric(),
                           traininginbenefits = numeric(), environmentinbenefits = numeric(), lifeinbenefits = numeric(),
                           stringsAsFactors = FALSE)
 
        observeEvent(input$submit,{
          req(input$title,input$description,input$benefits,input$requirements,input$profile)
          tmp <- data.frame(company_profile_length = nchar(input$profile), description_length = nchar(input$description),requirements_length = nchar(input$requirements), 
                            benefits_length = nchar(input$description), candidatesinprofile = str_count(input$profile,"candidates")/str_count(input$profile, '\\w+'), 
                            recruitinginprofile = str_count(input$profile,"recruiting")/str_count(input$profile, '\\w+'), bonusinprofile = str_count(input$profile,"bonus")/str_count(input$profile, '\\w+'), 
                            careerinprofile = str_count(input$profile,"career")/str_count(input$profile, '\\w+'), staffinginprofile = str_count(input$profile,"staffing")/str_count(input$profile, '\\w+'), 
                            experienceinprofile = str_count(input$profile,"experience")/str_count(input$profile, '\\w+'), industryinprofile = str_count(input$profile,"industry")/str_count(input$profile, '\\w+'), 
                            dataintitle = str_count(input$title,"data")/str_count(input$title, '\\w+'), entryintitle = str_count(input$title,"entry")/str_count(input$title, '\\w+'), assistantintitle = str_count(input$title,"assistant")/str_count(input$title, '\\w+'), 
                            homeintitle = str_count(input$title,"home")/str_count(input$title, '\\w+'), clerkintitle = str_count(input$title,"clerk")/str_count(input$title, '\\w+'), payrollintitle = str_count(input$title,"payroll")/str_count(input$title, '\\w+'), 
                            administrativeintitle = str_count(input$title,"administrative")/str_count(input$title, '\\w+'), ampindescription = str_count(input$description,"amp")/str_count(input$description, '\\w+'), 
                            timeindescription = str_count(input$description,"time")/str_count(input$description, '\\w+'), positionindescription = str_count(input$description,"position")/str_count(input$description, '\\w+'), projectindescription = str_count(input$description,"project")/str_count(input$description, '\\w+'), 
                            ampinrequirments = str_count(input$requirements,"amp")/str_count(input$requirements, '\\w+'), computerinrequirements = str_count(input$requirements,"computer")/str_count(input$requirements, '\\w+'),
                            traininginbenefits = str_count(input$benefits,"training")/str_count(input$benefits, '\\w+'), environmentinbenefits = str_count(input$benefits,"environment")/str_count(input$benefits, '\\w+'), lifeinbenefits = str_count(input$benefits,"life")/str_count(input$benefits, '\\w+'))
          v$df <- rbind(v$df,tmp)
        })
        
      
        output$pred <- renderPrint({paste(predict(SVM,v$df))})
        
    
    }
    


# Run the application 
shinyApp(ui = ui, server = server)
