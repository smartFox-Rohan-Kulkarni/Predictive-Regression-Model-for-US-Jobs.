#package module
#install.packages("rjson")
install.packages("jsonlite")
install.packages("shiny")
install.packages("e1071")
install.packages('caret')
install.packages('DT')
install.packages('GGally')
library(GGally)
library(caret)
library(shiny)
library(DT)
library("jsonlite")
library(plyr)
#library("rjson")
library(class)
library(tidyr)
library('Hmisc')
library(e1071)
library(party)
library(ggplot2)


#data module alpha
json_file <- "projectSem2dmdw/Monster_usa_job_listings_dataset_20190601_20190930__20k_data.ldjson"
alpha <- fromJSON(paste("[",paste(readLines(json_file),collapse=","),"]"))

#data module beta
csv_file<- "projectSem2dmdw/monster_com-job_sample.csv"
beta<-read.csv(file = csv_file,header = T)

#data structure
str(alpha)
str(beta)
summary(alpha)
summary(beta)
colnames(alpha)
colnames(beta)
sapply(alpha,class)
sapply(beta,class)

#Data cleaning module(remove noisy and inconsistent data)
#check for NA values/ missing values
na_count_alpha <-sapply(alpha, function(y) sum(length(which(is.na(y)))))
na_count_alpha
na_count_beta <-sapply(beta, function(y) sum(length(which(is.na(y)||y==""))))
na_count_beta

#Data integration module(combine data from both datasets to form a dataWare house)
colnames(beta)[colnames(beta)=="page_url"] <- "url"
colnames(beta)[colnames(beta)=="sector"] <- "category"
colnames(beta)[colnames(beta)=="organization"] <- "company_name"
colnames(beta)[colnames(beta)=="date_added"] <- "post_date"
colnames(beta)[colnames(beta)=="salary"] <- "salary_offered"
beta1=beta
beta1=beta1 %>% separate(location, c("city", "state"), ",")

#city contains state data needs to be rectified
for (row in 1:nrow(beta1)) {
  tcity <- beta1[row, "city"]
  tcountry<-beta1[row,"country"]
  if(tcountry=="Usa" | tcountry=="United States of America"){
    beta1[row, "country"]<-"US"
  }
  if(tcity=="AL" | tcity=="AK" | tcity=="AZ" | tcity=="AR" | tcity=="CA" |tcity=="CO" |tcity=="CT" |tcity=="DE" |tcity=="FL" |tcity=="GA" |tcity=="HI" |tcity=="ID" |tcity=="IL" |tcity=="IN" |tcity=="IA" |tcity=="KS" |tcity=="KY" |tcity=="LA" |tcity=="ME" |tcity=="MD" |tcity=="MA" |tcity=="MI" |tcity=="MN" |tcity=="MS" |tcity=="MO" |tcity=="MT" |tcity=="NE" |tcity=="NV" |tcity=="NH" |tcity=="NJ" |tcity=="NM" |tcity=="NY" |tcity=="NC" |tcity=="ND" |tcity=="OH" |tcity=="OK" |tcity=="OR" |tcity=="PA" |tcity=="RI" |tcity=="SC" |tcity=="SD" |tcity=="TN" |tcity=="TX" |tcity=="UT" |tcity=="VT" |tcity=="VA" |tcity=="WA" |tcity=="WV" |tcity=="WI" |tcity=="WY" |tcity=="DC" |tcity=="MH") {
    beta1[row,"state"]<-tcity
  }
}

for (row in 1:nrow(alpha)) {
  tcountry<-alpha[row,"country"]
  if(tcountry=="Usa"|tcountry=="United States of America"){
    alpha[row, "country"]<-'US'
  }
}
#preDATA selection and Data prepartions.
#common and required variables in alpha and beta are: 
#uniq_id,url,job_title,category,company_name,city,state,country,post_date,job,description,job_board,has_expired,job_type,salary_offered
#hence remming information such has Job_requirment,post_date_in_int format,crawltimestamp,lang,contact are to be ignored and will not 
#be stored in the datawarehouse.As the dataware house is stored as a data.frame and not a list it requires to be of the same length hence adding to this decision.
variable_set<-c("uniq_id","url","job_title","category","company_name","city","state","country","post_date","job_description","job_board","has_expired","job_type","salary_offered")
alpha_clean_1<-alpha[variable_set]
beta_clean_1<-beta1[variable_set]
mainSource<- rbind(alpha_clean_1,beta_clean_1)
#verify there isnt any dublicate values
dim(mainSource[duplicated(mainSource$uniq_id),])[1]
#if any then remove them
mainSourceHtml<-mainSource<-mainSource[!duplicated(mainSource$uniq_id),]
# as the arbitrary (uniq_id,url,job_description,job_board,has_expired) data isn't important in the analysis
# we can remove them
mainSource$uniq_id <- NULL
mainSource$url <- NULL
mainSource$job_description <- NULL
mainSource$job_board <- NULL
mainSource$has_expired<-NULL
mainSource$salary_offered<-NULL
str(mainSource)
#testing......??
mainSource$post_date<-NULL
mainSource$country<-NULL


#dentify the rows without missing data:

na_count_mainSource <-sapply(mainSource, function(y) sum(length(which(is.na(y)))))

na_count_mainSource  
mainSource <- mainSource[complete.cases(mainSource),]
#convert the char data to numeric factors
#Data Transformation covert textual data to factors.
str(mainSource)

test<-levels(as.factor(mainSource$job_title))
View(test)

#mainSource$job_title <- gsub("[:alnum:]*","",mainSource$job_title)
#mainSource$job_title <- gsub("[0:9]","",mainSource$job_title)
#mainSource$job_title <- gsub("[/]","",mainSource$job_title)


#x<-mainSource
#x$job_title <- gsub("[^[:alnum:]\\s]","",x$job_title)
#x$job_title <- as.factor(x$job_title)
#x$category <- gsub("[^[:alnum:]\\s]","",x$category)
#x$category <- as.factor(x$category)
#x$company_name <- gsub("[^[:alnum:]\\s]","",x$company_name)
#x$company_name <- as.factor(x$company_name)
#x$city <- gsub("[^[:alnum:]\\s]","",x$city)
#x$city<- as.factor(x$city)
#x$state <- gsub("[^[:alnum:]\\s]","",x$state)
#x$state <- as.factor(x$state)
#x$country <- gsub("[^[:alnum:]\\s]","",x$country)
#x$country <- as.factor(x$country)
#mainSource$post_date <- as.factor(mainSource$post_date)
#x$job_type <- gsub("[^[:alnum:]\\s]","",x$job_type)
#x$job_type <- as.factor(x$job_type)


mainSource$job_title <- as.factor(mainSource$job_title)
mainSource$category <- as.factor(mainSource$category)
mainSource$company_name <- as.factor(mainSource$company_name)
mainSource$city<- as.factor(mainSource$city)
mainSource$state <- as.factor(mainSource$state)
#mainSource$country <- as.factor(mainSource$country)
mainSource$job_type <- as.factor(mainSource$job_type)
#mainSource$post_date <- as.factor(mainSource$post_date)



#shiny html sites
ui <- fluidPage(
  titlePanel("Graph 1"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins","Number of bins:",min = 1,max = 25,value = 10)),
      mainPanel()
  ),br(),br(),
  plotOutput("distPlot")
  
)

server <- function(input, output) {
  output$distPlot<-renderPlot({
    x1<-count(mainSource$category)
    x1<-x1[order(x1$freq,decreasing = TRUE),]
    x1<-head(x1,25)
    x1
    # draw the histogram with the specified number of bins
    ggplot(x1[1:input$bins,],aes(x=factor(x),fill=factor(x)))+
      geom_bar()+ 
      geom_text(aes(label=..count..),stat="count")
  })
}

shinyApp(ui = ui, server = server)



#(1)Histogram to display the count/trends according to sample of top 25 job category use slider to take input for sample size.

#(2)by using shiny package display clustering of job categories.
#(3)Take company name as input for dropdown list and display the jobs,categories,has exp,country and other info using shiny.
#(4)interactive map :plot the jobs according to the city on us map.
#(5)clustering of how the salary is to be paid(factor the options first)ie after data transformation.
#(6)clustering of job categories according to job types

#Data Selection and prepation end!

#Model Building
#Preform minnig algos and devlop models for 70|30 traingin and testing data.
set.seed(3)
mainSource<-mainSource[1:200,]
sample1<-sample(2,nrow(mainSource),replace=TRUE,prob = c(0.7,0.3))
#train_data<-x[sample1==1,-2]
#test_data<-x[sample1==2,-2]
train_data<-mainSource[sample1==1,]
test_data<-mainSource[sample1==2,]
summary(train_data)

#training_Outcomes <- mainSource[sample1==1,2]
#test_Outcomes <- mainSource[sample1==2,2]
summary(mainSource)

#k_value<-floor(sqrt(nrow(mainSource)))
#predictions <- knn(train = train_data, cl = training_Outcomes, k = k_value,test = test_data)
#ncol(predictions)
#table(test_Outcomes, predictions)

#using window and plot functions display desison tree and crosstable/contengency table.
mainSourcetemp<-mainSource[1:10,]
x<-summary(mainSourcetemp[,-1])
View(x)
g<-ggpairs(data=mainSourcetemp, columns=c(1:5), title="jobs data")
g
set.seed(1)

sample_for_temp1<-sample(2,nrow(mainSourcetemp),replace=TRUE,prob = c(0.7,0.3))
train_datatemp<-mainSourcetemp[sample_for_temp1==1,]
test_datatemp<-mainSourcetemp[sample_for_temp1==2,]

categoryModel <- train(form =  city~., data=mainSourcetemp,method = "knn")
categoryTestPred <- predict( categoryModel, test_datatemp)
confusionMatrix( categoryTestPred, mainSource$category)

#conclude | present final knowledge and findings.

x=levels(train_data$job_type)
View(x)

temps<-mainSource

mainSource<-temps
str(mainSource)
df<-mainSource
for (i in colnames(df[, sapply(df, is.factor)])){
  for (level in unique(df[, i])){
    df[paste(i, level, sep = "_")] = 
      as.integer(ifelse(df[, i] == level, 1, -1))
  }
}

mainSource<-df

