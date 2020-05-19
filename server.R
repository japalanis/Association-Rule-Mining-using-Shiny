library(shiny)
library(rsconnect)
library(dplyr)
library(tidyr)
library(tidyverse)
library(caret)
library(arules)
library(arulesViz)
library(ggplot2)


data<- read.csv("employee_attrition.csv")
sapply(data, function(x) sum(is.na(x)))
levels(data$OverTime)[levels(data$OverTime)==""] <- NA
levels(data$Gender)[levels(data$Gender)==""] <- NA

new_data <- na.omit(data)
new_data<-new_data[!duplicated(new_data),]
for(i in c(1,4,6,7,9,10,11,13,14,15,17,19,20,21,24:35)) {new_data[i] <- lapply(new_data[i], as.numeric)}
new_data[,c(9,22,27)]<-NULL

new_data$Age <- discretize(new_data$Age, method = "interval", breaks = 3, labels = c("Young", "Adult", "Old"), order = T)

new_data$DailyRate <- discretize(new_data$DailyRate, method = "interval", breaks = 5, labels = c("Lower","Low", "Medium", "High", "Higher"), order = T)

new_data$DistanceFromHome <- discretize(new_data$DistanceFromHome, method = "interval", breaks = 3, labels = c("Less", "Medium", "More"), order = T)

new_data$Education <- discretize(new_data$Education, method = "interval", breaks = 5, labels = c("High School","Associate", "Bachelors", "Masters", "PhD"), order = T)
new_data$EnvironmentSatisfaction <- discretize(new_data$EnvironmentSatisfaction, method = "interval", breaks = 4, labels = c("Dissatisfied", "Average", "Satisfied", "Extremely Satisfied"), order = T)
new_data$HourlyRate <- discretize(new_data$HourlyRate, method = "interval", breaks = 5, labels = c("Lower","Low", "Medium", "High", "Higher"), order = T)
new_data$JobInvolvement <- discretize(new_data$JobInvolvement, method = "interval", breaks = 4, labels = c("Low", "Medium", "High", "Very High"), order = T)
new_data$JobLevel <- discretize(new_data$JobLevel, method = "interval", breaks = 5, labels = c("Lower", "Low", "Medium", "High", "Very High"), order = T)
new_data$JobSatisfaction <- discretize(new_data$JobSatisfaction, method = "interval", breaks = 4, labels = c("Low", "Medium", "High", "Very High"), order = T)
new_data$MonthlyIncome <- discretize(new_data$MonthlyIncome, method = "frequency", breaks = 5, labels = c("Lower","Low", "Medium", "High", "Higher"), order = T)
new_data$MonthlyRate <- discretize(new_data$MonthlyRate, method = "interval", breaks = 5, labels = c("Lower","Low", "Medium", "High", "Higher"), order = T)
new_data$NumCompaniesWorked <- discretize(new_data$NumCompaniesWorked, method = "interval", breaks = 3, labels = c("Less", "Medium", "More"), order = T)
new_data$PercentSalaryHike <- discretize(new_data$PercentSalaryHike, method = "interval", breaks = 4, labels = c("Low", "Medium", "High", "Very High"), order = T)
new_data$PerformanceRating <- discretize(new_data$PerformanceRating, method = "interval", breaks = 3, labels = c("Low", "Good", "Excellent"), order = T)
new_data$RelationshipSatisfaction <- discretize(new_data$RelationshipSatisfaction, method = "interval", breaks = 4, labels = c("Low", "Medium", "High", "Very High"), order = T)
new_data$StockOptionLevel <- discretize(new_data$StockOptionLevel, method = "interval", breaks = 4, labels = c("Low", "Medium", "High", "Very High"), order = T)
new_data$TotalWorkingYears <- discretize(new_data$TotalWorkingYears, method = "frequency", breaks = 4, labels = c("Less", "Medium", "More", "Maximum"), order = T)
new_data$TrainingTimesLastYear <- discretize(new_data$TrainingTimesLastYear, method = "interval", breaks = 3, labels = c("Less", "Medium", "More"), order = T)
new_data$WorkLifeBalance <- discretize(new_data$WorkLifeBalance, method = "interval", breaks = 4, labels = c("Bad", "Good", "Better", "Best"), order = T)
new_data$YearsAtCompany <- discretize(new_data$YearsAtCompany, method = "frequency", breaks = 3, labels = c("Less", "Medium", "More"), order = T)
new_data$YearsInCurrentRole <- discretize(new_data$YearsInCurrentRole, method = "frequency", breaks = 3, labels = c("Less", "Medium", "More"), order = T)
new_data$YearsSinceLastPromotion <- discretize(new_data$YearsSinceLastPromotion, method = "interval", breaks = 4, labels = c("Less", "Medium", "More", "Maximum"))
new_data$YearsWithCurrManager <- discretize(new_data$YearsWithCurrManager, method = "frequency", breaks = 3, labels = c("Low", "Medium", "High"))

server <- function(input,output) {
 
  rules<-reactive(
    {
      emp_attrition_rules<- apriori(data=new_data, parameter=list(supp=as.numeric(input$sup),conf = as.numeric(input$conf) , minlen= as.numeric(input$len)+1, maxlen = as.numeric(input$mlen),
                                                               maxtime=as.numeric(input$time), target = "rules"), appearance = list(rhs=c("Attrition=Yes", "Attrition=No")))
    }
  )
  output$plot <- renderPlot({
    emp_attrition_rules<-rules()
    att_plot<- plot(emp_attrition_rules)
    print(att_plot)
  },height = 600)
  
  output$rules <- renderTable( {
    emp_attrition_rules <- rules()
    attrition_rules <- DATAFRAME(emp_attrition_rules)
    attrition_rules
   
    
  })
    
  
}


