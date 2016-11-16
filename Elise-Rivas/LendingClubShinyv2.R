library(shiny)
library(doBy)
library(ggplot2)

setwd("C:/Users/elrivas/Documents/Trainings/R Training")
loandata <- read.csv("loandata.csv", header=TRUE)
purpose_df<-summaryBy(Amount.Funded.By.Investors ~ Loan.Purpose, data = loandata, 
                      FUN = function(x) { c(c=length(x), m = mean(x), s = sum(x)) })
state_df<-summaryBy(Amount.Funded.By.Investors ~ State, data = loandata, 
                    FUN = function(x) { c(c=length(x), m = mean(x), s = sum(x)) })
colnames(purpose_df) <- c("Loan.Purpose", "Count", "Average", "Total")
colnames(state_df) <- c("State", "Count", "Average", "Total")
#----------------------------------------------
###### UI ######
ui <- fluidPage(
  titlePanel("Loan Data by State and Loan Purpose"),
  sidebarPanel(
    selectInput("summaryinput", "Sum, Count, or Average", choices=colnames(purpose_df[,2:4])
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("State", plotOutput(outputId = "stategraph")),
      tabPanel("Loan Purpose", plotOutput(outputId = "purposegraph"))
    )
  )
)


#----------------------------------------------
###### SERVER ######
server <- function(input, output){

  #Create summaryby tables of loan purpose and state
 
  
  #State plot
  output$stategraph <- renderPlot({
    barplot(state_df[,input$summaryinput], names.arg=levels(state_df$State))   })
  # Purpose Plot
  output$purposegraph <- renderPlot({
    barplot(purpose_df[,input$summaryinput], names.arg=levels(purpose_df$Loan.Purpose))
    })

 }


#----------------------------------------------
shinyApp(ui=ui, server=server)