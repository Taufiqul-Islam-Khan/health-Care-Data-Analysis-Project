library(plyr)
library(dplyr)
library(tidyr)
#get data from healthcare-dataset-stroke-data.csv file
healthcare_data <- read.csv("healthcare-dataset-stroke-data.csv")
print((healthcare_data))

#Data cleaning
# Here we are going to find every null values which should be negligible
# For example we have N/A in bmi column,Unknown in smoking_status column
# The rows which contain those values will be deleted from dataframe
healthcare_data2 <-healthcare_data[!(healthcare_data$bmi =='N/A'),]
print(healthcare_data2)
#healthcare_data2 will have all data without N/A from bmi

healthcare_data3 <-healthcare_data2[!(healthcare_data2$smoking_status =='Unknown'),]
print(healthcare_data3)
#healthcare_data3 will have all data without Unknown from smoking_status

# Now we don't have any missing value so healthcare_data3 is our usable final set of data


# Here we can analyze the maximum minimum and average ages of people in our dataset
mean_age= mean(healthcare_data3[,3])
max_age= max(healthcare_data3$age, na.rm = TRUE)
min_age= min(healthcare_data3$age, na.rm = TRUE)
print(mean_age)
print(max_age)
print(min_age)

#By printing this value we can see, the average age is 48.64594, the highest
#age is 82 and the lowest person's age is 10


# Here we can analyze the maximum and minimum glucose level in our dataset
max_avg_glucose_level= max(healthcare_data3$avg_glucose_level, na.rm = TRUE)
min_avg_glucose_level= min(healthcare_data3$avg_glucose_level, na.rm = TRUE)

print(max_avg_glucose_level)
print(min_avg_glucose_level)
#By printing this value we can see, the highest glucose level is 108.3219 and
#lowest is 55.12


# Here we can analyze the maximum and minimum bmi in our dataset
max_bmi= max(healthcare_data3$bmi, na.rm = TRUE)
min_bmi= min(healthcare_data3$bmi, na.rm = TRUE)
print(max_bmi)
print(min_bmi)
#By printing this value we can see, the highest glucose level is 92 and lowest is 11.55


#Finding how many unique values are possible in each column

unique_value_of_gender<-unique(healthcare_data3[c("gender")])
print(unique_value_of_gender)

unique_value_of_hypertension<-unique(healthcare_data3[c("hypertension")])
print(unique_value_of_hypertension)

unique_value_of_heart_disease<-unique(healthcare_data3[c("heart_disease")])
print(unique_value_of_heart_disease)

unique_value_of_ever_married<-unique(healthcare_data3[c("ever_married")])
print(unique_value_of_ever_married)

unique_value_of_work_type<-unique(healthcare_data3[c("work_type")])
print(unique_value_of_work_type)

unique_value_of_Residence_type<-unique(healthcare_data3[c("Residence_type")])
print(unique_value_of_Residence_type)

unique_value_of_smoking_status<-unique(healthcare_data3[c("smoking_status")])
print(unique_value_of_smoking_status)

unique_value_of_stroke<-unique(healthcare_data3[c("stroke")])
print(unique_value_of_stroke)

# now we will build some correlation regarding the data

# Firstly we will find out how many people have formerly smoked, never smoked or smoke

count(healthcare_data3, "smoking_status")
table(healthcare_data3$smoking_status)


# we can see 837 individuals formerly smoked, 1852 never smoked and 737 smoke

#Looking at stroke vs smoke analysis
smoking_stroke_dataFrame<-select(healthcare_data3,smoking_status, stroke)

smoking_stroke_dataFrame2 <-smoking_stroke_dataFrame[!(smoking_stroke_dataFrame$smoking_status
                                        =='never smoked'),]
smoked <-smoking_stroke_dataFrame2[!(smoking_stroke_dataFrame2$smoking_status
                                                       =='formerly smoked'),]
smoked_stroke <-smoked[!(smoked$stroke
                                     ==0),]
smoked_stroked_people<-count(smoked_stroke, "stroke")
print(smoked_stroked_people)
#In this dataset 39 people who were smokers had a stroke

#stroke vs former smoker

smoking_stroke_dataFrame<-select(healthcare_data3,smoking_status, stroke)

smoking_stroke_dataFrame2 <-smoking_stroke_dataFrame[!(smoking_stroke_dataFrame$smoking_status
                                                       =='smokes'),]
smoked <-smoking_stroke_dataFrame2[!(smoking_stroke_dataFrame2$smoking_status
                                     =='never smoked'),]
smoked_stroke <-smoked[!(smoked$stroke
                         ==0),]
smoked_stroked_people<-count(smoked_stroke, "stroke")
print(smoked_stroked_people)
#In this dataset 57 people who identified as former smokers had a stroke

#finally lets see how many people had a stroke after being a non smoker

smoking_stroke_dataFrame<-select(healthcare_data3,smoking_status, stroke)

smoking_stroke_dataFrame2 <-smoking_stroke_dataFrame[!(smoking_stroke_dataFrame$smoking_status
                                                       =='smokes'),]
smoked <-smoking_stroke_dataFrame2[!(smoking_stroke_dataFrame2$smoking_status
                                     =='formerly smoked'),]
smoked_stroke <-smoked[!(smoked$stroke
                         ==0),]
smoked_stroked_people<-count(smoked_stroke, "stroke")
print(smoked_stroked_people)
#In this dataset 84 people who identified as non smokers had a stroke


#Making an average percentage according to our data

avg_smoker<-(39/737)*100
avg_Nonsmoker<-(84/1852)*100
avg_Formerlysmoker<-(57/837)*100
options(scipen = 100)
typeof(avg_smoker)



# we can make a decision here that according to our dataset
#the stroke tendency for Former smokers is higher than for
#smokers and non smokers


# lets make a bar chart for our findings
var1 <- c(avg_Formerlysmoker,avg_smoker,avg_Nonsmoker)
var2 <- c("Formerly smokes","Smokes","Never Smokes")
png(file = "barchart_Smoking_VS_stroke.png")
barplot(var1,names.arg=var2,xlab="Smoking Status",ylab="stroke in %",col="blue",
        main="Smoking VS stroke chart",border="red")
dev.off()
# the plot is saved is in Files

#Gender analysis
count(healthcare_data3, "gender")
table(healthcare_data3$gender)
# We can see There are 2086 Female, 1339 Male and 1 other person in our dataset

# lets ignore the other as it is not specific about gender, and make a dataframe for
#gender,hypertension, heart_disease and stroke
gender_desease_dataframe<-select(healthcare_data3,gender,hypertension,
                                 heart_disease,stroke)

gender_desease_dataframe <-gender_desease_dataframe[!(gender_desease_dataframe$gender
                                                      =='Other'),]

#Male patient for each disease
gender_desease_dataframe2 <-gender_desease_dataframe[!(gender_desease_dataframe$gender
                                                       =='Female'),]

Male_hypertension <-gender_desease_dataframe2[!(gender_desease_dataframe2$hypertension
                                                       =='0'),]

Male_hypertension_patient<-count(Male_hypertension, "hypertension")
print(Male_hypertension_patient)
#180 Male suffer from  hypertension
Male_heart_disease <-gender_desease_dataframe2[!(gender_desease_dataframe2$heart_disease
                                                =='0'),]

Male_heart_disease_patient<-count(Male_heart_disease, "heart_disease")
print(Male_heart_disease_patient)
#121 Male suffer from heart_disease


Male_stroke <-gender_desease_dataframe2[!(gender_desease_dataframe2$stroke
                                                 =='0'),]

Male_stroke_patient<-count(Male_stroke, "stroke")
print(Male_stroke_patient)
#75 Male suffer from  stroke


#Female patient for each disease
gender_desease_dataframe2 <-gender_desease_dataframe[!(gender_desease_dataframe$gender
                                                       =='Male'),]

FEMale_hypertension <-gender_desease_dataframe2[!(gender_desease_dataframe2$hypertension
                                                =='0'),]

FEMale_hypertension_patient<-count(FEMale_hypertension, "hypertension")
print(FEMale_hypertension_patient)
#228 female suffer from hypertension
FEMale_heart_disease <-gender_desease_dataframe2[!(gender_desease_dataframe2$heart_disease
                                                 =='0'),]

FEMale_heart_disease_patient<-count(FEMale_heart_disease, "heart_disease")
print(FEMale_heart_disease_patient)
#85 female suffer from heart_disease


FEMale_stroke <-gender_desease_dataframe2[!(gender_desease_dataframe2$stroke
                                          =='0'),]

FEMale_stroke_patient<-count(FEMale_stroke, "stroke")
print(FEMale_stroke_patient)
#105 female suffer from stroke




# According to our data lets make an average percentage of male and female disease

avg_male_hypertension<-(180/1339)*100
avg_male_heart_desease<-(121/1339)*100
avg_male_stroke<-(75/1339)*100

avg_female_hypertension<-(228/2086)*100
avg_female_heart_desease<-(85/2086)*100
avg_female_stroke<-(105/2086)*100
options(scipen = 100)

# Lets make a line chart
male <- c(avg_male_stroke,avg_male_hypertension,avg_male_heart_desease)
female <- c(avg_female_stroke,avg_female_hypertension,avg_female_heart_desease)
png(file = "line_chart_male_female_desease.jpg")
plot(male,type = "o",col = "red", xlab = "Male & Female", ylab = "Disease",
     main = "Male Female Disease ratio")
lines(female, type = "o", col = "blue")
dev.off()


# Now for the next analysis lets do an evaluation of work_type
# We saw that we have 'Private' 'Self-employed' 'Govt_job' 'children' 'Never_worked'
#5 types of working people in our dataset

#Lets see how many people are from different sectors of work_type

count(healthcare_data3, "work_type")
table(healthcare_data3$work_type)

# It shows us we have 68 children, 514 Govt Job, 14 Never_worked, Private 2201
#and 629 Self-employed

#lets make a histogram and see what values are present here
worktype <- c(68,514,14,2201,629)
png(file = "histogram_WorkType.png")
hist(worktype,xlab = "work type",col = "green",border = "red", xlim = c(0,3000), ylim = c(0,5),
     breaks = 5)
dev.off()

#it shows the frequency for different worktype employees


#lets predict the diabetes patients in our dataset
count(healthcare_data3, "avg_glucose_level")
# we have 3426 people in total
glucose_level_dataframe<-select(healthcare_data3,avg_glucose_level)

#here we are iterating glucose level of every person
#According to MAYO Clinic, A blood sugar level less than 140 mg/dLis normal.
#A reading of more than 200 mg/dL indicates diabetes.
#A reading between 140 and 199 mg/dL indicates prediabetes.
normal_patient<-0
prediabetes_patient <- 0
diabetes_patient<-0
print (normal_patient)
for(i in 1:nrow(glucose_level_dataframe)) {
  if (glucose_level_dataframe[i, ]<140.00){
    normal_patient<-normal_patient+1
  }
  else if (glucose_level_dataframe[i, ]<199.00){
    prediabetes_patient<-prediabetes_patient+1
  }
  else{
    diabetes_patient<-diabetes_patient+1
  }

}
print(normal_patient)
print(prediabetes_patient)
print(diabetes_patient)

# we have predicted that in our dataset, out of 3426 people
# 2812 people don't have diabetes
# 262 are prediabetic patients
# and 347 are diabetes patients

# lets make a bar chart for our findings
var1 <- c(normal_patient,diabetes_patient,prediabetes_patient)
var2 <- c("Normal People","Diabeties patient","Prediabeties Patient")
png(file = "barchart_diabeties predict.png")
barplot(var1,names.arg=var2,xlab="Diabeties status",ylab="people",col="blue",
        main="Diabeties predict chart",border="red")
dev.off()
# the plot is saved in Files

work_stroke_dataframe<-select(healthcare_data3,work_type,hypertension,
                                 heart_disease,stroke)


private_hypertention<-0
private_heart_disease<-0
private_stroke<-0

Self_employed_hypertention<-0
Self_employed_heart_disease<-0
Self_employed_stroke<-0

Govt_job_hypertention<-0
Govt_job_heart_disease<-0
Govt_job_stroke<-0

Never_worked_hypertention<-0
Never_worked_heart_disease<-0
Never_worked_stroke<-0

children_hypertention<-0
children_heart_disease<-0
children_stroke<-0


for(i in 1:nrow(work_stroke_dataframe)) {
  #for private
  if (work_stroke_dataframe[i,1 ]=='Private' && work_stroke_dataframe[i,2 ]==1 ){
    private_hypertention<-private_hypertention+1
  }
  if (work_stroke_dataframe[i,1 ]=='Private' && work_stroke_dataframe[i,3 ]==1 ){
    private_heart_disease<-private_heart_disease+1
  }
  if (work_stroke_dataframe[i,1 ]=='Private' && work_stroke_dataframe[i,4 ]==1 ){
    private_stroke<-private_stroke+1
  }

  #for self employed
  if (work_stroke_dataframe[i,1 ]=='Self-employed' && work_stroke_dataframe[i,2 ]==1 ){
    Self_employed_hypertention<-Self_employed_hypertention+1
  }
  if (work_stroke_dataframe[i,1 ]=='Self-employed' && work_stroke_dataframe[i,3 ]==1 ){
    Self_employed_heart_disease<-Self_employed_heart_disease+1
  }
  if (work_stroke_dataframe[i,1 ]=='Self-employed' && work_stroke_dataframe[i,4 ]==1 ){
    Self_employed_stroke<-Self_employed_stroke+1
  }

  #for Never_worked
  if (work_stroke_dataframe[i,1 ]=='Never_worked' && work_stroke_dataframe[i,2 ]==1 ){
    Never_worked_hypertention<-Never_worked_hypertention+1
  }
  if (work_stroke_dataframe[i,1 ]=='Never_worked' && work_stroke_dataframe[i,3 ]==1 ){
    Never_worked_heart_disease<-Never_worked_heart_disease+1
  }
  if (work_stroke_dataframe[i,1 ]=='Never_worked' && work_stroke_dataframe[i,4 ]==1 ){
    Never_worked_stroke<-Never_worked_stroke+1
  }
  #for govt_job

  if (work_stroke_dataframe[i,1 ]=='Govt_job' && work_stroke_dataframe[i,2 ]==1 ){
    Govt_job_hypertention<-Govt_job_hypertention+1
  }
  if (work_stroke_dataframe[i,1 ]=='Govt_job' && work_stroke_dataframe[i,3 ]==1 ){
    Govt_job_heart_disease<-Govt_job_heart_disease+1
  }
  if (work_stroke_dataframe[i,1 ]=='Govt_job' && work_stroke_dataframe[i,4 ]==1 ){
    Govt_job_stroke<-Govt_job_stroke+1
  }
  #for children
  if (work_stroke_dataframe[i,1 ]=='children' && work_stroke_dataframe[i,2 ]==1 ){
    children_hypertention<-children_hypertention+1
  }
  if (work_stroke_dataframe[i,1 ]=='children' && work_stroke_dataframe[i,3 ]==1 ){
    children_heart_disease<-children_heart_disease+1
  }
  if (work_stroke_dataframe[i,1 ]=='children' && work_stroke_dataframe[i,4 ]==1 ){
    children_stroke<-children_stroke+1
  }

}

print(private_hypertention)
print(private_heart_disease)
print(private_stroke)

print(Self_employed_hypertention)
print(Self_employed_heart_disease)
print(Self_employed_stroke)

print(Govt_job_hypertention)
print(Govt_job_heart_disease)
print(Govt_job_stroke)

print(Never_worked_hypertention)
print(Never_worked_heart_disease)
print(Never_worked_stroke)

print(children_hypertention)
print(children_heart_disease)
print(children_stroke)

private_hypertention<-(private_hypertention/2201)*100
private_heart_disease<-(private_hypertention/2201)*100
private_stroke<-(private_hypertention/2201)*100

Self_employed_hypertention<-(Self_employed_hypertention/629)*100
Self_employed_heart_disease<-(Self_employed_heart_disease/629)*100
Self_employed_stroke<-(Self_employed_stroke/629)*100


Govt_job_hypertention<-(Govt_job_hypertention/514)*100
Govt_job_heart_disease<-(Govt_job_heart_disease/514)*100
Govt_job_stroke<-(Govt_job_stroke/514)*100

# Lets make a line chart
private <- c(private_hypertention,private_heart_disease,private_stroke)
self <- c(Self_employed_hypertention,Self_employed_heart_disease,Self_employed_stroke)
govt <- c(Govt_job_hypertention,Govt_job_heart_disease,Govt_job_stroke)
child <- c(children_hypertention,children_heart_disease,children_stroke)
never <- c(Never_worked_hypertention,Never_worked_heart_disease,Never_worked_stroke)
png(file = "line_chart_work_type_desease.jpg")
plot(private,type = "o",col = "red", xlab = "work type", ylab = "Disease",
     main = "work type Vs Disease ratio")
lines(self, type = "o", col = "blue")

lines(never, type = "o", col = "yellow")
lines(govt, type = "o", col = "green")
lines(child, type = "o", col = "black")
dev.off()

