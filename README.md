# Association-Rule-Mining-using-Shiny

In this report we will be analysing the Employee Attrition dataset with all data mining steps.We will be performing Association Rule Mining to predict the attrition rate and the factors that causes the output. Then through the Shiny App, its been made interactive to users to find the prediction by changing the parameter values.

Shiny App link: https://jpalanis.shinyapps.io/HW01/


### **Data Loading:**
Load all the necessary packages

```{r,eval=TRUE, results='hide', message=FALSE, warning=FALSE}

library(ggplot2)
library(dplyr)
library(gridExtra)
library(arules)
library(tidyr)
library(tidyverse)
library(arulesViz)
```


### **Data Exploration:**
Then we can initialise the dataset and examine the stucture of the dataset with following code:

```{r}
data<- read.csv("employee_attrition.csv")
str(data)
```

```{r}
summary(data)
```


Based on the above details, we could see that there are some blank values in Gender and OverTime columns.On more exploration of those attributes we can re-confirm it with the below code:

```{r}
levels(data$Gender)
levels(data$OverTime)
```

### **Data Preparation:**

##### **Missing Values:**
Lets look at the NA's and missing values in the dataset

```{r}
sapply(data, function(x) sum(is.na(x)))
```

We could see couple of NA's available in few attributes.

Lets assign the NA value to the blank values in OverTime and Gender attribute to ignore the NA values in the dataset 
```{r}
levels(data$OverTime)[levels(data$OverTime)==""] <- NA
levels(data$Gender)[levels(data$Gender)==""] <- NA
```

```{r}
summary(data)
```


Since we have very minimal number of NA's in the dataset, we can omit them. After omitting, lets check whether the NA's are removed in all the attributes

```{r}
new_data <- na.omit(data)
sum(is.na(new_data))
```

##### **Duplicate Values:**
Now lets deal with the duplicate values in the dataset

```{r}
new_data<-new_data[!duplicated(new_data),]
```

Now our dataset is clean with no missing and duplicate values. Lets move on to the further analysis.

```{r}

ggplot(new_data, aes(x = Attrition, y = DistanceFromHome))+ geom_boxplot(color="black", fill="blue")+ xlab("Attrition") + ylab("Distance from Home") + ggtitle("Relationship between Attrition and Distance from Home") + theme(plot.title = element_text(hjust = 0.5))
```

##### **Analysis:**
The average distance travelled by an employee who are in attrition is around 7 and who are not without attrition is 5. Based on this,it is interpreted that employees who travel little more distance from home are with attrition in the company.

```{r}
ggplot(new_data,aes(x=Gender, y=JobSatisfaction, fill=Attrition)) + geom_boxplot(color="black") +xlab("Gender") +ylab("Job Satisfaction")+ ggtitle("Job Satisfaction Vs Gender Vs Attrition") +theme(plot.title = element_text(hjust = 0.5)) + scale_fill_manual(values=c("orange", "maroon"))
```

##### **Analysis:**
It is observed that both female and male without attrition have same level of Job satisfaction. But for male employees with attrition have a minimum job satisfaction of 1 and maximum of 3 whereas female employees have minimum job satisfaction of 2 and maximum of 3.

```{r}
plot1<-ggplot(new_data, aes(x=Age)) + geom_histogram(color="black", fill="yellow",bins = 25) + ggtitle("Age Distribution")+ theme(plot.title = element_text(hjust = 0.5)) + ylab("Count of Age") + xlab("Age")

plot2<-ggplot(new_data, aes(x=Gender)) + geom_bar(color="black", fill="steelblue") + ggtitle("Gender Distribution")+ theme(plot.title = element_text(hjust = 0.5)) + ylab("Count of Gender") + xlab("Gender")

plot3<-ggplot(new_data, aes(x = Attrition, y = Age))+ geom_boxplot(color = "black", fill="green") + xlab("Attrition") + ylab("Age") + ggtitle("Attrition vs Age") + theme(plot.title = element_text(hjust = 0.5))

plot4<-ggplot(new_data, aes(x = Gender, fill = Attrition)) + geom_bar(color="black",position = 'fill') + theme(axis.text.x = element_text(angle = 90, hjust = 1),plot.title = element_text(hjust = 0.5)) + xlab("Gender") + ylab("Ratio") + ggtitle("Attrition rates based on Gender")+scale_fill_manual(values=c("maroon", "pink"))

grid.arrange(plot1,plot2,plot3,plot4,nrow=2,ncol=2)
```


##### **Analysis:**
Based on the above graphs, we can observe the following factors:
(a)From the Age distribution graph, we can see that the most of the employees fall around 25 to 45 years of age group.
(b)From the Gender distribution graph,we can observe there are around 490 female employees and 600 male employees.
(c)From the third graph, it is clear that employees without attrition fall between 30-43 years of age group and employees who leave the company are mostly between 26-39 years of age.
(d)Based on the graph, there is no major difference in the attrition rate of male and female. Just the ratio of male is slightly higher than the female.

```{r}
options(repr.plot.width=8, repr.plot.height=5) 

avg.income <- new_data %>% select(EducationField, MonthlyIncome, Attrition) %>% group_by(Attrition, EducationField) %>%
summarize(average_income=mean(MonthlyIncome)) %>%
ggplot(aes(x=reorder(EducationField, average_income), y=average_income, fill=Attrition)) + geom_bar(stat="identity") + facet_wrap(~Attrition) + theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5)) + 
scale_fill_manual(values=c("maroon", "orange")) + 
labs(y="Average Income", x="Education Field", title="Average Income by Education Field \n and Attrition Status") + 
geom_text(aes(x=EducationField, y=0.01, label= paste0("$ ", round(average_income,2))),
            hjust=-0.5, vjust=0, size=3, colour="black", angle=90)
avg.income
```


##### **Analysis:**
When we compare the employees who leave or stay in company based on the education field, it is clear that employees in Human Resources department get more average income compared to other employees with other education field and so there are very less employees leaving the company in that field.

```{r}
ggplot(new_data, aes(x=Department)) + geom_bar(color="black", fill="pink",) + ggtitle("Distribution of Department")+ theme(plot.title = element_text(hjust = 0.5)) + ylab("Count") + xlab("Department")
```


##### **Analysis:**
From Distribution of Department plot, it is observed that majority of employees work in Research and Development. 

```{r}
plot5<- ggplot(new_data, aes(x = Attrition, y = PerformanceRating))+ geom_boxplot() + xlab("Attrition") + ylab("Performance Rating") + ggtitle("Attrition vs Performance Rating") + theme(plot.title = element_text(hjust = 0.5))
plot6<- ggplot(new_data, aes(x = Attrition, y = PercentSalaryHike))+ geom_boxplot() + xlab("Attrition") + ylab("Percent Salary Hike") + ggtitle("Attrition vs Percent Salary Hike") + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(plot5,plot6,nrow=1)
```


##### **Analysis:**
From the above box plots, it is observed that the performance rating and percentage salary hike is same for employees with or without attrition.

```{r}
ggplot(new_data, aes(x = BusinessTravel, fill = Attrition)) + geom_bar(position = 'fill') + theme(axis.text.x = element_text(angle = 90, hjust = 1),plot.title = element_text(hjust = 0.5)) + xlab("Business Travel") + ylab("Ratio") + ggtitle("Attrition rates based on Business Travel")+scale_fill_manual(values=c("maroon", "lightgreen"))
```


##### **Analysis:**
From the above barplot, we can observe that the propotion of employees who travel frequently are tend to leave the company more than the employees who do not travel.


### **Data Transformation:**

Now,lets convert all the integer values to numeric values to proceed with the discretization. Also, we can remove the attributes that are least important and the one that doesn't contribute much for our analysis.So,'Over18','EmployeeCount' and 'StandardHours' attributes are removed.

```{r}
for(i in c(1,4,6,7,9,10,11,13,14,15,17,19,20,21,24:35)) {new_data[i] <- lapply(new_data[i],as.numeric)}
new_data[,c(9,22,27)]<-NULL
```

Lets discretize the variables to ordinal categorical variables

```{r}
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
```

Lets have a look at the structure of final dataset:
```{r}
str(new_data)
```

### **Association Rule Mining:**

Now lets run the dafault apriori function on the final dataset.

```{r, message=FALSE,warning=FALSE, results="hide"}
emp_attrition_rules<- apriori(data = new_data)
```

Inspect the top 10 rules with high confidence

```{r}
inspect(head(sort(emp_attrition_rules, by='confidence'),10))
```

Plot the generated rules:

```{r, warning=FALSE, message=FALSE}
plot(emp_attrition_rules)
```


##### **Analysis:**
From the above plot, it is observed that with the increase in support value, the lift and confidence becomes lower.

### Tuned Model:
Now let us set the confidence and support value to 0.5 and inspect the model

```{r,warning=FALSE, message=FALSE}
emp_attrition_rules <- apriori(data=new_data, parameter=list (supp=0.5,conf =0.5, minlen= 2,maxtime=10, target = "rules"))
```

```{r,warning=FALSE, message=FALSE}
plot(emp_attrition_rules)
```

##### **Analysis:**
When the minimum length of the rule is set to 2, then the confidence and lift values increase with higher support value.

```{r}
inspect(head(sort(emp_attrition_rules, by='confidence'),10))
```

### **Attrition Prediction:**
Now let us predict the association of attrition with other attributes.We can set the target as "rules" and rhs as "Attrition=Yes" and "Attrition=No"

```{r,message=FALSE,warning=FALSE}
emp_attrition_rules <- apriori(data=new_data, parameter=list (supp=0.5,conf =0.5, minlen= 2, maxtime=10, target = "rules"), appearance = list (rhs=c("Attrition=Yes", "Attrition=No")))
```

```{r}
inspect(head(sort(emp_attrition_rules, by='confidence'),10))
```

```{r,warning=FALSE, message=FALSE}
plot(emp_attrition_rules)
```

#### **Output:**
On fine tuning, we got around 23 rules with highest rule with support of 0.6369, confidence of 0.8929 and lift of 1.0592.The employees are predicted to stay in the company when they doesn't need to work overtime and with the less distance from home. Also based on the factor that they travel rarely.Even though when the company provides a low performance rating, the employees are comfortable with it considering other factors like less distance from home, no overtime and rare business travels.  
