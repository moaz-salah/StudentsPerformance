if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")   
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(ggpubr)


##MMS: STEP 1:  DATA Wrangling 
##############################################################################################################################

# see working directory
getwd()

studentsDF<- read.csv('StudentsPerformance.csv') #File must be under WD
head(studentsDF)
str(studentsDF)
#summary(studentsDF)

####check if any scores of data is missing:
if(any( is.na(studentsDF[,])) )
  print("Missing Data")   
# No Missing data

#change the column names to smaller, more readable names
studentsDF<- studentsDF|> rename("race" = "race.ethnicity"  ,
      "P_education" ="parental.level.of.education" ,
      "Test_Prepare"="test.preparation.course",
      "math" ="math.score"  ,
      "reading" = "reading.score" ,
      "writing" ="writing.score" )


##MMS: STEP 2:  DATA EXPLORATION 
##############################################################################################################################

#####Check correlation between different subjects Results (The dependent variables) : Visually and Calculate Correlation
r_w_plot<- ggplot(studentsDF, aes(x=reading, y=writing)) + 
  geom_point()+
  geom_smooth(method=lm)
r_m_plot<- ggplot(studentsDF, aes(x=reading, y=math)) + 
  geom_point()+
  geom_smooth(method=lm)
w_m_plot<- ggplot(studentsDF, aes(x=writing, y=math)) + 
  geom_point()+
  geom_smooth(method=lm)

ggarrange(r_w_plot, r_m_plot, w_m_plot, 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)

read_write_relation<- cor.test(studentsDF$reading, studentsDF$writing, 
                               method = "pearson")

read_math_relation<- cor.test(studentsDF$reading, studentsDF$math, 
                              method = "pearson")

write_math_relation<- cor.test(studentsDF$writing, studentsDF$math, 
                               method = "pearson")
read_write_relation    #0.9545981 
read_math_relation     #0.8175797 
write_math_relation    #0.802642 

#Benefiting from this high correlation, I will create a new field 'avg_score' representing the average
studentsDF<- studentsDF |> mutate(avg_score=(math+ reading + writing)/3)

read_dist<- studentsDF |>  ggplot(aes( y = reading)) +
  geom_boxplot() 
write_dist<- studentsDF |>  ggplot(aes( y = writing)) +
  geom_boxplot() 
math_dist<- studentsDF |>  ggplot(aes( y = math)) +
  geom_boxplot() 


ggarrange(read_dist, write_dist, math_dist, 
            labels = c("R", "W", "M"),
            ncol = 3, nrow =1)

summary(studentsDF$reading)
summary(studentsDF$writing)
summary(studentsDF$math)


#####Check all inputs are normally distributed + relation between Inputs
#Checking Gender prevalence   
studentsDF |> group_by(gender) %>%
  summarise(n=n()) %>%
  arrange(n) %>%
  ggplot(aes(x=gender,y=n)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=n), vjust=-0.3, size=3.5)+
  ggtitle("Distribution of Gender") +
  xlab("Gender") +
  ylab("Number in Gender Group")  
## Female is little bit more than half (518)


#Checking race distribution  
studentsDF |> group_by(race) %>%
  summarise(n=n()) %>%
  arrange(n) %>%
  ggplot(aes(x=race,y=n)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=n), vjust=-0.3, size=3.5)+
  ggtitle("Distribution of Race") +
  xlab("Race") +
  ylab("Number in Race Group") 


#Checking Gender distribution  per race
studentsDF |> group_by(race, gender) %>%
  summarise(n=n()) %>%
  #  arrange(n) %>%
  ggplot(aes(x=race,y=n, fill=gender)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=n), vjust=-0.3, position = position_dodge(0.9), size=3.5)+
  ggtitle("Distribution of Race") +
  xlab("Race") +
  ylab("Number in Race Group") 
#Data seems not biased by Gender or Race, with little bit more Females over all 

#checking if race (maybe due to economical status of certain race) affect Lunch feature
studentsDF |> group_by(race, lunch) %>%
  summarise(n=n()) %>%
  #  arrange(n) %>%
  ggplot(aes(x=race,y=n, fill=lunch)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=n), vjust=-0.3, position = position_dodge(0.9), size=3.5)+
  ggtitle("Distribution of Race") +
  xlab("Race") +
  ylab("Number in Race Group") 

#checking if race (maybe due to economical status of certain race) affect Educational level
studentsDF |> group_by(race, P_education) %>%
  summarise(n=n()) %>%
  #  arrange(n) %>%
  ggplot(aes(x=race,y=n, fill=P_education)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=n), vjust=-0.3, position = position_dodge(0.9), size=3.5)+
  ggtitle("Distribution of Parents Education per Race") +
  xlab("Race") +
  ylab("Number in Race Group") 

studentsDF |> group_by(race, Test_Prepare) %>%
  summarise(n=n()) %>%
  #  arrange(n) %>%
  ggplot(aes(x=race,y=n, fill=Test_Prepare)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=n), vjust=-0.3, position = position_dodge(0.9), size=3.5)+
  ggtitle("Distribution of Test_Prepare per Race") +
  xlab("Race") +
  ylab("Number in Race Group") 

studentsDF |> group_by(race) %>% summarise(grp_number = n(), stand_lunch = sum(lunch == "standard", na.rm = TRUE), stand_lunch_Perc =stand_lunch/grp_number )
studentsDF |> group_by(race) %>% summarise(Number_in_Grp = n(), has_degree = sum(str_count(P_education,"degree")>0, na.rm = TRUE), degree_Perc =has_degree/Number_in_Grp )
studentsDF |> group_by(race) %>% summarise(grp_number = n(), tst_prepare = sum(Test_Prepare == "completed", na.rm = TRUE), prepare_exam =tst_prepare/grp_number )


library(MASS)

#double check the relation between race and both lunch and Educational level
chisq.test(as.factor(studentsDF$race), as.factor(studentsDF$lunch))  #p-value = 0.4867 ,
chisq.test(as.factor(studentsDF$race), as.factor(studentsDF$P_education)) # p-value = 0.07911
chisq.test(as.factor(studentsDF$race), as.factor(studentsDF$Test_Prepare)) # p-value = 0.2408

#All p-values are larger than 5%, so we cant reject Null hypothesis and we cant say there is a direct relation. This match the visual results. 
#Still, note that this p-Value is much less for Educational Levels.


#checking if gender affect the type of Lunch
studentsDF |> group_by(gender, lunch) %>%
  summarise(n=n()) %>%
  #  arrange(n) %>%
  ggplot(aes(x=gender,y=n, fill=lunch)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=n), vjust=-0.3, position = position_dodge(0.9), size=3.5)+
  ggtitle("Distribution of lunch per Gender") +
  xlab("Gender") +
  ylab("Number in Gender Group") 

#if Gender affect taking the preparation course 
studentsDF |> group_by(gender, Test_Prepare) %>%
  summarise(n=n()) %>%
  #  arrange(n) %>%
  ggplot(aes(x=gender,y=n, fill=Test_Prepare)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=n), vjust=-0.3, position = position_dodge(0.9), size=3.5)+
  ggtitle("Distribution of Test_Prepare per Gender") +
  xlab("Gender") +
  ylab("Number in Gender Group") 

chisq.test(as.factor(studentsDF$gender), as.factor(studentsDF$lunch))  #p-value = 0.5421
chisq.test(as.factor(studentsDF$gender), as.factor(studentsDF$Test_Prepare))  #p-value = 0.9008
#Both p-value > 5%, so we cant reject Null hypothesis and we cant say there is a direct relation. This match the visual results



##### We need to check inputs & outputs relations
#Checking race correlation with Avg score
raceMedian <- studentsDF |> group_by(race) |>  summarise( MD = round(median(avg_score)))
studentsDF |> 
  ggplot(aes(x=race,y=avg_score,  vjust = -1.5)) +
  geom_boxplot() +
  geom_text(data = raceMedian, aes(as.factor(race), MD, label = MD), position = position_dodge(width = 0.8), size = 3, vjust = -0.5)+
  ggtitle("Race affect on Score") +
  xlab("Race") +
  ylab("Average Score") 


#Checking gender correlation with Avg score
genderMedian <- studentsDF |> group_by(gender) |>  summarise( MD = round(median(avg_score)))
studentsDF |> 
  ggplot(aes(x=gender,y=avg_score)) +
  geom_boxplot() +
  geom_text(data = genderMedian, aes(gender, MD, label = MD), position = position_dodge(width = 0.8), size = 3, vjust = -0.5)+
  ggtitle("Gender affect on Score") +
  xlab("Gender") +
  ylab("Average Score") 


#Checking lunch correlation with Avg score
lunchMedian <- studentsDF |> group_by(lunch) |>  summarise( MD = round(median(avg_score)))
studentsDF |> 
  ggplot(aes(x=lunch,y=avg_score)) +
  geom_boxplot() +
  geom_text(data = lunchMedian, aes(lunch, MD, label = MD), position = position_dodge(width = 0.8), size = 3, vjust = -0.5)+
  ggtitle("Lunch affect on Score") +
  xlab("Lunch") +
  ylab("Average Score") 


#Checking Education correlation with Avg score
EducMedian <- studentsDF |> group_by(P_education) |>  summarise( MD = round(median(avg_score)))
studentsDF |> 
  ggplot(aes(x=P_education,y=avg_score)) +
  geom_boxplot() +
  geom_text(data = EducMedian, aes(P_education, MD, label = MD), position = position_dodge(width = 0.8), size = 3, vjust = -0.5)+
  ggtitle("Parent Education affect on Score") +
  xlab("Parent Education" ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("Average Score") 


#Checking Test Prepar correlation with Avg score
testPrepMedian <- studentsDF |> group_by(Test_Prepare) |>  summarise( MD = round(median(avg_score)))
studentsDF |> 
  ggplot(aes(x=Test_Prepare,y=avg_score)) +
  geom_boxplot() +
  geom_text(data = testPrepMedian, aes(Test_Prepare, MD, label = MD), position = position_dodge(width = 0.8), size = 3, vjust = -0.5)+
  ggtitle("Test Preparation affect on Score") +
  xlab("Test Preparation") +
  ylab("Average Score") 


cor.test(as.numeric(as.factor(studentsDF$race)), studentsDF$avg_score, method = "pearson")  #cor 0.1851684  
cor.test(as.numeric(as.factor(studentsDF$gender)), studentsDF$avg_score, method = "pearson")  #cor -0.1308612  
cor.test(as.numeric(as.factor(studentsDF$lunch)), studentsDF$avg_score, method = "pearson")  #cor 0.290064   
cor.test(as.numeric(as.factor(studentsDF$P_education)), studentsDF$avg_score, method = "pearson")  #cor 0.07888339    
cor.test(as.numeric(as.factor(studentsDF$Test_Prepare)), studentsDF$avg_score, method = "pearson")  #cor 0.2567097  




##MMS: STEP 3:  DATA Preparation 
##############################################################################################################################

##MMS: 1- Split Data Set
set.seed (1, sample.kind = "Rounding")
final_test_index <- createDataPartition(y = studentsDF$avg_score, times = 1, p = 0.2, list = FALSE)
final_test_data <- studentsDF[final_test_index,]
temp_data <- studentsDF[-final_test_index,]

work_test_index <- createDataPartition(y = temp_data$avg_score, times = 1, p = 0.2, list = FALSE)
work_test_data <- temp_data[work_test_index,]
work_train_data <- temp_data[-work_test_index,]
rm(temp_data)

##MMS: 2- Prepare Evaluation Function. Used Method is RMSE
Eval_RMSE <- function(true_ratings, predicted_ratings){    sqrt(mean((true_ratings - predicted_ratings)^2))}

##MMS: 3- Prepare Reference Model: THE AVERAGE
mu <- mean(work_train_data$avg_score, na.rm = TRUE)
mu     #67.93093

Ref_rmse <- Eval_RMSE(work_test_data$avg_score, mu)
Ref_rmse   # 13.71996

rmse_results <- tibble(method = "Just the average", RMSE = Ref_rmse)


##MMS: STEP 4:  MODELING 
##############################################################################################################################

### As this is Categorical, I'll try Regression Tree
library(rpart)

# will try first the most significant factors 
#I'm applying 2 pieces of code for prediction part- for learning objectives only
#Method 1:
fit_reg_tree <- rpart(avg_score ~ lunch+Test_Prepare+race, data = work_train_data) 
print(fit_reg_tree)
#summary(fit)
plot(fit_reg_tree, margin = 0.1)
text(fit_reg_tree, use.n=TRUE, cex = 0.75)

y_hat= predict(fit_reg_tree,work_test_data)
Eval_RMSE(work_test_data$avg_score, y_hat)

#Method 2: 
#select best tune parameters using cross validation
train_rpart_top <- train(avg_score ~ lunch+Test_Prepare,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 50)),
                     data = work_train_data)
plot(train_rpart_top)
train_rpart_top$bestTune

y_hat <- predict(train_rpart_top, work_test_data)
Class_tree_top_rmse <- Eval_RMSE(work_test_data$avg_score, y_hat)

rmse_results<- rmse_results %>% add_row(method ="Regression Tree- Top Factors: ", RMSE = Class_tree_top_rmse) #13.2
rmse_results


# Now, we try all factors, except race
train_rpart_all <- train(avg_score ~ lunch+Test_Prepare+gender+P_education,
                       method = "rpart",
                       tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 50)),
                       data = work_train_data)
plot(train_rpart_all)
train_rpart_all$bestTune

y_hat <- predict(train_rpart_all, work_test_data)
Class_tree_all_rmse <- Eval_RMSE(work_test_data$avg_score, y_hat)

rmse_results<- rmse_results %>% add_row(method ="Regression Tree- All Factors: ", RMSE = Class_tree_all_rmse) #13.1
rmse_results

#I tried this and as expected gave worse results
# train_rpart_x <- train(avg_score ~ lunch+Test_Prepare+gender+P_education+ race,
#                        method = "rpart",
#                        tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 50)),
#                        data = work_train_data)



## Now, we try another Model: RandomForest
library(randomForest)
#fit_rf <- randomForest(avg_score~lunch+Test_Prepare, data = work_train_data) 
#fit_rf <- randomForest(avg_score~lunch+Test_Prepare+gender+P_education, data = work_train_data) 
fit_rf <- randomForest(avg_score~lunch+Test_Prepare+gender+P_education+ race, data = work_train_data) 

y_hat_rf <- predict(fit_rf, work_test_data)
RF_rmse <- Eval_RMSE(work_test_data$avg_score, y_hat_rf)
rmse_results<- rmse_results %>% add_row(method ="Random Forest ", RMSE = RF_rmse) #12.6

## Now, we try another Model: Knn

train_knn <- train(avg_score~lunch+Test_Prepare+gender+P_education+ race, method = "knn",
                   data = work_train_data, 
                   tuneGrid = data.frame(k = seq(4, 70, 1)))           # Resulted in 12.5452
# train_knn <- train(avg_score~lunch+Test_Prepare+gender+P_education, method = "knn",
#                    data = work_train_data, 
#                    tuneGrid = data.frame(k = seq(4, 70, 1)))         # Resulted in 12.77193
# train_knn <- train(avg_score~lunch+Test_Prepare+gender, method = "knn",
#                    data = work_train_data, 
#                    tuneGrid = data.frame(k = seq(4, 70, 1)))            # Resulted in 12.96871
train_knn$bestTune

y_hat_knn <- predict(train_knn, work_test_data)
knn_rmse <- Eval_RMSE(work_test_data$avg_score, y_hat_knn)

rmse_results<- rmse_results %>% add_row(method ="KNN", RMSE = knn_rmse)
rmse_results 


#Ensemble
#y_hat_av<- (y_hat+ y_hat_rf + y_hat_knn)/3
y_hat_av<- ( y_hat_rf + y_hat_knn)/2

av_rmse <- Eval_RMSE(work_test_data$avg_score, y_hat_av)
rmse_results<- rmse_results %>% add_row(method ="Average", RMSE = av_rmse)
rmse_results #


##### Final Evaluation of the Model
#y_hat_final <- predict(train_rpart_all, final_test_data)
y_hat_final_rf <- predict(fit_rf, final_test_data)
y_hat_final_knn <- predict(train_knn, final_test_data)

y_hat_final_av  <- ( y_hat_final_rf + y_hat_final_knn)/2

final_rmse <- Eval_RMSE(final_test_data$avg_score, y_hat_final_rf)
final_rmse # 14.38573


####################################### Appendix: Trying the same after removing out-liars points on each subject Exam

#Get Index of out-liars in each subject 
outR<-boxplot.stats(studentsDF$reading)$out
outR_ind <- which(studentsDF$reading %in% c(outR))

outW<-boxplot.stats(studentsDF$writing)$out
outW_ind <- which(studentsDF$writing %in% c(outW))

outM<- boxplot.stats(studentsDF$math)$out
outM_ind <- which(studentsDF$math %in% c(outM))

#outR_ind
#outW_ind
#outM_ind

#Here if removed all outliares
Indexes_to_remove <- (outR_ind)
Indexes_to_remove <- Indexes_to_remove |> append (outW_ind)|> append (outM_ind)
Indexes_to_remove <-Indexes_to_remove[!duplicated(Indexes_to_remove)]
Indexes_to_remove

# #I'll remove only points that appeared as out liar in 2 subjects (I tried it)
# Indexes_to_remove <- intersect(outR_ind,outW_ind)
# Indexes_to_remove <- Indexes_to_remove |> append  (intersect(Indexes_to_remove,outM_ind))
# Indexes_to_remove <-Indexes_to_remove[!duplicated(Indexes_to_remove)]

#Remove the Data Points
set.seed (1, sample.kind = "Rounding")
studentsDF_No_OL <- studentsDF |> filter(!row_number() %in% Indexes_to_remove) 

final_test_index_No_OL <- createDataPartition(y = studentsDF_No_OL$avg_score, times = 1, p = 0.2, list = FALSE)
final_test_data_No_OL <- studentsDF_No_OL[final_test_index_No_OL,]
temp_data <- studentsDF_No_OL[-final_test_index_No_OL,]

work_test_index_No_OL <- createDataPartition(y = temp_data$avg_score, times = 1, p = 0.2, list = FALSE)
work_test_data_No_OL <- temp_data[work_test_index_No_OL,]
work_train_data_No_OL <- temp_data[-work_test_index_No_OL,]
rm(temp_data)

dim(final_test_data_No_OL)
dim(work_test_data_No_OL)
dim(work_train_data_No_OL)


#Apply same models on the new data set
train_rpart_N <- train(avg_score ~  lunch+Test_Prepare+gender+P_education,
                         method = "rpart",
                         tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 50)),
                         data = work_train_data_No_OL)
y_hatX <- predict(train_rpart_N, work_test_data_No_OL)
reg_rmse_No_OL <- Eval_RMSE(work_test_data_No_OL$avg_score, y_hatX)  #11.61568


fit_rf_N <- randomForest(avg_score~lunch+Test_Prepare+gender+P_education+ race, data = work_train_data_No_OL) 

y_hat_rf_N <- predict(fit_rf_N, work_test_data_No_OL)
rf_rmse_No_OL <- Eval_RMSE(work_test_data_No_OL$avg_score, y_hat_rf_N)   #11.41727

train_knn_N <- train(avg_score~lunch+Test_Prepare+gender+P_education+ race, method = "knn",
                   data = work_train_data_No_OL, 
                   tuneGrid = data.frame(k = seq(4, 70, 1)))           
y_hat_knn_N <- predict(train_knn_N, work_test_data_No_OL)
knn_rmse_No_OL <- Eval_RMSE(work_test_data_No_OL$avg_score, y_hat_knn_N)   # 11.57655


mu_NO_OL <- mean(work_train_data_No_OL$avg_score, na.rm = TRUE)

Ref_rmse_NO_OL <- Eval_RMSE(work_test_data_No_OL$avg_score, mu_NO_OL)
rmse_results_NO_OL <- tibble(method = "Just the average, No Out Liars", RMSE = Ref_rmse_NO_OL)  #12.6



rmse_results_NO_OL<- rmse_results_NO_OL %>% add_row(method ="Regression tree, No Out Liars", RMSE = reg_rmse_No_OL)
rmse_results_NO_OL<- rmse_results_NO_OL %>% add_row(method ="Random Forest, No Out Liars", RMSE = rf_rmse_No_OL)
rmse_results_NO_OL<- rmse_results_NO_OL %>% add_row(method ="KNN, No Out Liars", RMSE = knn_rmse_No_OL)
rmse_results_NO_OL


#Ensemble
y_hat_av_No_OL<- ( y_hat_rf_N + y_hat_knn_N)/2

av_rmse_NO_OL <- Eval_RMSE(work_test_data$avg_score, y_hat_av_No_OL)
rmse_results_NO_OL<- rmse_results_NO_OL %>% add_row(method ="Average-No OL", RMSE = av_rmse_NO_OL)
rmse_results_NO_OL #

##### Final Evaluation of the Model
#Here will pick the best model
y_hat_final_knn_NO_OL <- predict(train_knn_N, final_test_data_No_OL)

final_rmse_NO_OL <- Eval_RMSE(final_test_data_No_OL$avg_score, y_hat_final_knn_NO_OL)
final_rmse_NO_OL #






