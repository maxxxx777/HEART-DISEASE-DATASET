#LINK TO DATASET : https://archive.ics.uci.edu/dataset/45/heart+disease

install.packages("dplyr")
install.packages("ggplot2")
install.packages("RColorBrewer")



library(readr) #for importing datasets
library(dplyr) #for merging datatsets
library(ggplot2) # for making histograms barcharts etc
library(RColorBrewer) # for access to more colours

# PART 1

#imoprting the 4 datasets
va <- read_csv("va.data")
switzerland <- read_csv("switzerland.data")
hungarian <- read_csv("hungarian.data")
cleveland <- read_csv("cleveland.data")

#serttng the column names we will apply to datasets
col_names <- c("Age" , "Sex" , "ChestPainType" , "RestingBP", "SerumCholesterol" , "FastingBS" 
               ,"RestingECG" , "MaxHeartRate" , "ExerciseAngina" , "STdepression" , "SlopeofPeakExceriseST" 
               , "NumberofVessels" , "Thal" , "Diagnosis")

#applying column names to datasets
colnames(va) <- col_names
print(va)

colnames(switzerland) <- col_names
print(switzerland)

colnames(hungarian) <- col_names
print(hungarian)

colnames(cleveland) <- col_names
print(cleveland)

#change "?" in columns to "NA"
va[va == "?"] <- NA
print(va)

switzerland[switzerland == "?"] <- NA
print(switzerland)

hungarian[hungarian == "?"] <- NA
print(hungarian)

cleveland[cleveland == "?"] <- NA
print(cleveland)

#show number of NA counts per column in each dataset
na_counts <- colSums(is.na(va))
print(na_counts)

na_counts <- colSums(is.na(switzerland))
print(na_counts)

na_counts <- colSums(is.na(hungarian))
print(na_counts)

na_counts <- colSums(is.na(cleveland))
print(na_counts)

#remove columns SlopeofPeakExersieST , NumberofVessels and Thal as way too may NA counts

columns_to_remove <- c("SlopeofPeakExceriseST", "NumberofVessels", "Thal")
va1 <- va[, !names(va) %in% columns_to_remove]

columns_to_remove <- c("SlopeofPeakExceriseST", "NumberofVessels", "Thal")
switzerland1<- switzerland[, !names(switzerland) %in% columns_to_remove]

columns_to_remove <- c("SlopeofPeakExceriseST", "NumberofVessels", "Thal")
hungarian1 <- hungarian[, !names(hungarian) %in% columns_to_remove]

columns_to_remove <- c("SlopeofPeakExceriseST", "NumberofVessels", "Thal")
cleveland1 <- cleveland[, !names(cleveland) %in% columns_to_remove]

#remove NA from the datasets
va_clean <-na.omit(va1)

switzerland_clean <-na.omit(switzerland1)

hungarian_clean <-na.omit(hungarian1)

cleveland_clean <-na.omit(cleveland1)




#now that theyre are no missing values I will convert all columns to numeric as some were
#imported as character types
va_clean1 <- mutate_all(va_clean, as.numeric)
switzerland_clean1 <- mutate_all(switzerland_clean, as.numeric)
hungarian_clean1 <- mutate_all(hungarian_clean, as.numeric)
cleveland_clean1 <- mutate_all(cleveland_clean, as.numeric)

#quick summary statistics to see if theyre are any abnormalities
summary(va_clean1)
summary(switzerland_clean1)
summary(hungarian_clean1)
summary(cleveland_clean1)

# check for duplicated rows
duplicated_rows1 <- va_clean1[duplicated(va_clean1), ]

duplicated_rows2 <- switzerland_clean1[duplicated(switzerland_clean1), ]

duplicated_rows3 <- hungarian_clean1[duplicated(hungarian_clean1), ]

duplicated_rows4 <- cleveland_clean1[duplicated(cleveland_clean1), ]

#va has 1 duplicate row. remove 1 duplicated row from va
va_clean2 <- unique(va_clean1)
print(va_clean2)

#swiss data has 0 for all serumcholesterol which is impossible therefore I will leave out
#swiss dataset from merging as data may be incorrect

#merge 3 datasets together
heart_disease_all <- bind_rows(va_clean2, hungarian_clean1, cleveland_clean1)

# checking the columns with binary values to see if there are any abnormal values e.g. cant
# have '200' for Sex column as 1 is male and 0 is female.
for (col_name in colnames(heart_disease_all)) {
  cat("Counts for", col_name, ":\n")
  col_counts <- table(heart_disease_all[[col_name]])
  print(col_counts)
  cat("\n")
}

#Serumcholesterol and RestingBP still have values with 0. need to remove those as it is
#impossible to have 0 serumcholesterol and 0mmhg systolic blood pressure. All binary 
#values seem normal.

columns_with_0 <- c("SerumCholesterol", "RestingBP")
heart_disease_all4 <- heart_disease_all %>%
  filter(across(all_of(columns_with_0), ~ . != 0))


#checking numeric columns for outliers

#checking Age for outliers
boxplot(heart_disease_all4$Age, main="Boxplot of Age")

#checking RestingBP for outliers
boxplot(heart_disease_all4$RestingBP, main="Boxplot of Resting Blood Pressure")

#checking SerumCholesterol for outliers
boxplot(heart_disease_all4$SerumCholesterol, main="Boxplot of Serum Cholesterol")

#checking MaxHeartRate for outliers
boxplot(heart_disease_all4$MaxHeartRate, main="Boxplot of Maximum Heart Rate")

#Resting blood Pressure and Serum Cholesterol had a few high outliers but this is normal as
#some of these people have heart disease in which you would expect high numbers for 
#Serum Cholesterol and Resting Blood Pressure.

#Labels for Binary Values

# 1=Male , 0=Female
heart_disease_all5 <- heart_disease_all4 %>%
  mutate(Sex = ifelse(Sex == 1, "Male", "Female"))

# 1=high Fasting Blood sugar , 0= Low fasting blood sugar
heart_disease_all6 <- heart_disease_all5 %>%
  mutate(FastingBS = ifelse(FastingBS == 1, "High(>120mg/dl)", "Low(<120mg/dl)"))
  
# 1 = yes for exercise induced angina(chest pain) , 0 = no for exercise induced angina
heart_disease_all7 <- heart_disease_all6 %>%
  mutate(ExerciseAngina = case_when(ExerciseAngina == 1 ~ "Yes",
                                   ExerciseAngina == 0 ~ "No",))

# Type of chest pain - 1 = typical angina , 2= atypical angina , 3 = Non-anignal pain 
# 4 = asymptomatic
heart_disease_all8 <- heart_disease_all7 %>%
  mutate(ChestPainType = case_when(
    ChestPainType == 1 ~ "Typical Angina",
    ChestPainType == 2 ~ "Atypical Angina",
    ChestPainType == 3 ~ "Non-Anginal Pain",
    ChestPainType == 4 ~ "Asymptomatic", ))

# Electrocardiogram results - 0 = normal , 1 = Abnormal ST-T wave , 2 = Left Ventricular
# Hypertrophy
heart_disease_all9 <- heart_disease_all8 %>%
  mutate(RestingECG = case_when(
    RestingECG == 0 ~ "Normal",
    RestingECG == 1 ~ "Abnormal (ST-T wave)",
    RestingECG == 2 ~ "Left Ventricular Hypertrophy",))

# Diagnosis - 0= no heart disease , 1,2,3,4= Heart Disease
heart_disease_all10 <- heart_disease_all9 %>%
  mutate(Diagnosis = case_when(
    Diagnosis == 0 ~ "No Heart Disease",
    Diagnosis == 1 ~ "Heart Disease",
    Diagnosis == 2 ~ "Heart Disease",
    Diagnosis == 3 ~ "Heart Disease",
    Diagnosis == 4 ~ "Heart Disease",))

#PART 2

#checking numeric columns for outliers

#checking Age for outliers
boxplot(heart_disease_all10$Age, main="Boxplot of Age")

#checking RestingBP for outliers
boxplot(heart_disease_all10$RestingBP, main="Boxplot of Resting Blood Pressure")

#checking SerumCholesterol for outliers
boxplot(heart_disease_all10$SerumCholesterol, main="Boxplot of Serum Cholesterol")

#checking MaxHeartRate for outliers
boxplot(heart_disease_all10$MaxHeartRate, main="Boxplot of Maximum Heart Rate")

#Resting blood Pressure and Serum Cholesterol had a few high outliers but this is normal as
#some of these people have heart disease in which you would expect high numbers for 
#Serum Cholesterol and Resting Blood Pressure.

#show the structure of the dataset
str(heart_disease_all10)

#summary of the dataset
summary(heart_disease_all10)

#Age

#Histogram of Age
gg1 <- ggplot(heart_disease_all10, aes(x=heart_disease_all10$Age)) + labs(caption="Figure 1: Histogram of Age")

gg1 <- gg1 + labs(x="Age")
gg1 <- gg1 + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg1 <- gg1 + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#adding a normal curve
gg1 <- gg1 + stat_function(fun=dnorm, color="red",args=list(mean=mean(heart_disease_all10$Age, na.rm=TRUE), sd=sd(heart_disease_all10$Age, na.rm=TRUE)))
gg1

#Looks like normal distribution from histogram.

#QQplot of Age. few outliers at high and low ends but age seems to have a parametric 
#distribution
qqnorm(heart_disease_all10$Age, main="Figure 2: QQ Plot of Age" , xlab= "z-score" , ylab = "Age in Years" ) 
qqline(heart_disease_all10$Age, col=2)


#Sex

#Piechart for Sex. A lot more Males in this dataset. 486 Males to 171 Females.
sex_count <- table(heart_disease_all10$Sex)
colour <- brewer.pal(length(sex_count), "Pastel1")
pie(sex_count, labels = paste(names(sex_count), ": ", sex_count), col = colour, main = "Figure 3: Male/Female Distribution")

#Chest pain type

# Pie chart show the distribution of chest pain types. Asymptomatic is by far the most common
Chest_pain_count <- table(heart_disease_all10$ChestPainType)
colour <- brewer.pal(length(Chest_pain_count), "Pastel2")
pie(Chest_pain_count, labels = paste(names(Chest_pain_count), ": ", Chest_pain_count), col = colour, main = "Figure 4 : Chest Pain Distribution")

#Histogram of resting systolic blood pressure.
gg2 <- ggplot(heart_disease_all10, aes(x=heart_disease_all10$RestingBP)) + labs(caption="Figure 5: Histogram of Resting Systolic Blood Pressure")

gg2 <- gg2 + labs(x="Resting Systolic Blood Pressure in mmHg")
gg2 <- gg2 + geom_histogram(binwidth=5, colour="blue", aes(y=..density.., fill=..count..))
gg2 <- gg2 + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#adding normal curve
gg2 <- gg2 + stat_function(fun=dnorm, color="red",args=list(mean=mean(heart_disease_all10$RestingBP, na.rm=TRUE), sd=sd(heart_disease_all10$RestingBP, na.rm=TRUE)))
gg2
                                         
#QQplot for resting systolic blood pressure.There seems to be some high outliers as seen 
#from the qqplot. The qq plot also looks jagged as there was a lot of the same blood pressure
#readings it seems. I would say that the blood pressure looks slightly non-parametric.
#This is okay though as there will be people with high systolic blood pressure that will
#have heart disease as high systolic blood factor is a big risk factor for heart disease.
# I would expect there to be a correaltion between high systolic blood pressure and 
# heart disease.
qqnorm(heart_disease_all10$RestingBP, main="Figure 6: QQ Plot of Resting Systolic Blood Pressure" , ylab = "Systolic BP in mmHg", xlab= "z-score") 
qqline(heart_disease_all10$RestingBP, col=2)

#SerumCholesterol

#Histogram for Serum Cholesterol
gg3 <- ggplot(heart_disease_all10, aes(x=heart_disease_all10$SerumCholesterol)) + labs(caption="Figure 7: Histogram of Serum Cholesterol")

gg3 <- gg3 + labs(x="Serum Cholesterol in mg/dl")
gg3 <- gg3 + geom_histogram(binwidth=15, colour="black", aes(y=..density.., fill=..count..))
gg3 <- gg3 + scale_fill_gradient("Count", low="#87CEEB", high="#1E90FF")

#adding normal curve. can see from histogram that there are some extremely high outliers
gg3 <- gg3 + stat_function(fun=dnorm, color="red",args=list(mean=mean(heart_disease_all10$SerumCholesterol, na.rm=TRUE), sd=sd(heart_disease_all10$SerumCholesterol, na.rm=TRUE)))
gg3

#qqplot of Serum Cholesterol. the qqplot confirms the high outliers seen in the histogram.
# This is okay as there can be high serum cholesterol levels in people with heart disease
# as high serum cholesterol levels can be a risk factor for heart disease. I would expect
# there to be a correlation between high serum cholesterol and heart disease.
qqnorm(heart_disease_all10$SerumCholesterol, col ="darkblue",  main="Figure 8: QQ Plot of Serum Cholesterol" , xlab = "z-score" , ylab= "Serum Cholesterol in mg/dl") 
qqline(heart_disease_all10$SerumCholesterol, col=2)

#Fasting Blood Sugar

#Barplot for fasting blood sugar. There is a low more Low fasting blood sugar which is good.
# A high fasting blood sugar can be an indicator of diabetes or prediabetes. Diabtetes 
# can also be an indicator of heart disease so I would expect there to be a correlation
# between high fasting blood sugar levels and heart disease.
barplot(table(heart_disease_all10$FastingBS), col = c("green", "pink"), main = "Figure 9: Distribution of High/Low Fasting Blood Sugar", xlab = "Fasting Blood Sugar in mg/dl", ylab = "Count")

#Resting ECG

#piechart for restingECG. The majority of the ECG results were normal. the abnormal results
# (Left ventricular hypertrophy and abnormal ST-T wave) can be a indicator of heart disease
# so I would expect an correlation between ECG result and heart disease.
RestingECG_count <- table(heart_disease_all10$RestingECG)
colour <- brewer.pal(length(RestingECG_count), "Set3")
pie(RestingECG_count, labels = paste(names(RestingECG_count), ": ", RestingECG_count), col = colour, main = "Figure 10 : Resting ECG result")

#MaxHeartRate

#Histogram for Max Heart Rate. 
gg6 <- ggplot(heart_disease_all10, aes(x=heart_disease_all10$MaxHeartRate)) + labs(caption="Figure 11: Histogram of Max Heart Rate")

gg6 <- gg6 + labs(x="Max Heart Rate in BPM")
gg6 <- gg6 + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg6 <- gg6 + scale_fill_gradient("Count", low="#87CEEB", high="#1E90FF")

#adding a normal curve
gg6 <- gg6 + stat_function(fun=dnorm, color="red",args=list(mean=mean(heart_disease_all10$MaxHeartRate, na.rm=TRUE), sd=sd(heart_disease_all10$MaxHeartRate, na.rm=TRUE)))
gg6

#qqplot for max heart rate. there are a few high outliers and a lesser amount of low outliers
#I still think this has a nearly normal distribution therefor i think it is parametric
qqnorm(heart_disease_all10$MaxHeartRate, col ="darkgrey",  main="Figure 12: QQ Plot of Max Heart Rate" , xlab = "z-score" , ylab= "Max Heart Rate in BPM") 
qqline(heart_disease_all10$MaxHeartRate, col=1)

#Exercise Angina

#Piechart for exercise Angina. Majority of people didnt have Angina(chest pain) while
# exercising. Angina while exercisng can be a symptom of underlying heart issue including
# heart disease. therefore I would expect a correlation between Angina with exercise and 
# heart disease.
Exerciseangina_count <- table(heart_disease_all10$ExerciseAngina)
colour <- brewer.pal(length(Exerciseangina_count), "Accent")
pie(Exerciseangina_count, labels = paste(names(Exerciseangina_count), ": ", Exerciseangina_count), col = colour, main = "Figure 13 : Angina with Exercise")

#STdepression

#histogram for STdepression. As we can see from the histogram the majority of the values
# for ST depression sit around 0. 0mm of ST depression in a clinical setting is 
# considered normal. A value of more than 1mm is a clinically significant value and
# could indicate a cardiac condition and possibly heart disease. Therefore I would like to 
# see is there is a correlation between ST depression values greater than 1mm and a positive
# heart disease diagnosis.
gg7 <- ggplot(heart_disease_all10, aes(x=heart_disease_all10$STdepression)) + labs(caption="Figure 14: Histogram of ST Depression")

gg7 <- gg7 + labs(x="ST depression in mm")
gg7 <- gg7 + geom_histogram(binwidth=.3, colour="black", aes(y=..density.., fill=..count..))
gg7 <- gg7 + scale_fill_gradient("Count", low="#FF6347", high="#00FF00")

#adding a standard curve
gg7 <- gg7 + stat_function(fun=dnorm, color="red",args=list(mean=mean(heart_disease_all10$STdepression, na.rm=TRUE), sd=sd(heart_disease_all10$STdepression, na.rm=TRUE)))
gg7

#qq plot for st depression. there are a lot of high outliers. there are also a lot of values
# that are the same which gives the qq plot its jagged shape. overall this i think this
# is non-parametric from looking at it.
qqnorm(heart_disease_all10$STdepression, col ="lightblue",  main="Figure 15: QQ Plot of ST Depression" , xlab = "z-score" , ylab= "ST depression in mm") 
qqline(heart_disease_all10$STdepression, col=2)


#Diagnosis

#piechart for diagnosis. just under half of people have heart disease in this dataset.
Diagnosis_count <- table(heart_disease_all10$Diagnosis)
colour <- brewer.pal(length(Diagnosis_count), "Pastel1")
pie(Diagnosis_count, labels = paste(names(Diagnosis_count), ": ", Diagnosis_count), col = colour, main = "Figure 16: Diagnosis of Heart Disease")


#Part 3

#Q1 Is there a relationship between Serum Cholesterol and a positive heart disease diagnosis?
# I wanted to ask this question as high serum cholesterol is a risk factor for heart disease.


# boxplot of Serum Cholesterol v Heart Disease. you can see there is a small difference
# between the two diagnosis' and the mean is slightly higher in the heart disease result
# and has more higher outliers. This makes sense as high serum cholesterol levels are a big
# risk factor for heart disease.From the box plot its looks as though there is a small
# difference in serum cholesterol based on positive and negative heart disease diagnosis
# but to confirm you would first need to assess normality and then do a t test to see if 
# you get a significant result.
ggplot(heart_disease_all10, aes(x = heart_disease_all10$Diagnosis, y = heart_disease_all10$SerumCholesterol , fill = "pink")) +
  geom_boxplot() +
  labs(title = "Figure 17 : Box Plot of Serum Cholesterol vs Heart Disease",
       x = "Heart Disease",
       y = "Serum Cholesterol in mg/dl")


#Q2 Is there a relationship between age and heart disease diagnosis. There should be a
# relationship between these two variables as the older you get the more likely you are
# to get heart disease.

# boxplot of Age v Heart Disease. you can see there is a difference in mean between
# the two diagnosis' and the mean is higher in the heart disease result. This makes sense as
# the older you get the more likely you are to get heart disease. From the box plot
# its looks as though there is a difference in age on heart disease but to confirm you would first need to assess normality and
# you would first need to assess normality and then do a t test to see if you get a 
# significant result.
ggplot(heart_disease_all10, aes(x = heart_disease_all10$Diagnosis, y = heart_disease_all10$Age , fill = "pink")) +
  geom_boxplot() +
  labs(title = "Figure 18 : Box Plot of Age vs Heart Disease",
       x = "Heart Disease",
       y = "Age in years")


#Q3 Is there a realtionship between Exercise induced Angina and heart disease?

# I wanted to ask this question as exercise induced angina is a symptom of heart disease


# barplot of exercise induced angina (chest pain) against the heart disease diagnosis.
# as we can see from the barplot it is clear that the majority of people with heart disease
# have exercise induced angina whereas the vast majority of people without heart disease
# do not have exercise induced angina. this makes sense as exercise induced angina is a
# symptom of heart disease. to confirm this you could make a chi square to see if you 
# get a signifacnt p value.
barplot(table(heart_disease_all10$ExerciseAngina, heart_disease_all10$Diagnosis), beside = TRUE,
        col = c("pink", "purple"), main = "Figure 19 : Exercise Induced Angina vs Heart Disease", 
        legend = TRUE)
  

#Q4 Is there a relationship between restingECG and heart disease?

# I wanted to ask this question as an abnormal ECG result is linked to cardiac conditions
# including heart disease.

#barplot of restingECG agaisnt heart disease diagnosis.As you can see from the barplot there
# is a small difference in resting ECG results with people that have heart diseas comapred 
# to no heart disease. the people with heart disease have more Abnormal and Left ventricular
# hypertrophy results and less normal results than people with no heart disease. this makes
# sense as an abnormal ECG result can be an indication of a cardiac condition including
# heart disease. to confirm this  you could make a chi square to see if you get a 
# signifacnt p value.
barplot(table(heart_disease_all10$RestingECG, heart_disease_all10$Diagnosis), beside = TRUE, 
        main = " Figure 20: RestingECG vs Heart Disease" , col = c("lightgreen", "lightblue" , "orange"), legend = TRUE)


#Q5 Is there a relationship between fasting blood sugar and heart disease diagnosis.

# I wanted to ask this question as people with high fasting blood sugar levels are likely
# diabetic or prediabetic and these conditions are a big risk factor to heart disease.


# barplot of fasting blood sugar levels against heart disease diagnosis. as we can see from
# the barplot there seems to be slightly more people with high fasting blood sugar levels
# that have heart disease and less people with high fasting blood sugar levels that dont
# have heart disease. this makes sense as high fasting blood sugar is an indicator of 
# diabetes and diabetes is a major risk factor for heart disease. to confirm this 
# you could make a chi square to see if you get a signifacnt p value.
barplot(table(heart_disease_all10$FastingBS, heart_disease_all10$Diagnosis), beside = TRUE,
        main = "Figure 21 : Fasting Blood Sugar vs Heart Disease" ,col = c("red", "black" ), legend = TRUE)


#Q6 is there a relationship between Sex and heart disease diagnosis?

# I wanted to ask this question as Men are more likely to get heart disease than women.

# barplot for Sex against heart disease diagnosis. as we can see from the barplot a lot
# more men have heart disease as a ration compared to women in this dataset. This makes
# sense as men are more susceptable to getting heart disease than women as the hormone 
# estrogen helps protects women from heart disease. Also men are more likely to smoke and
# drink which are risk factors for heart disease.
barplot(table(heart_disease_all10$Diagnosis, heart_disease_all10$Sex), beside = TRUE, 
        col = c("blue", "green"), main = "Figure 22 : Sex vs Heart Disease", legend = TRUE)

#Q7 is there a relationship between STdepression and heart disease diagnosis

# I wanted to ask this question as an ST depression of 1mm or more is a clinically significant
# result and could indicate a possible cardiac condition such as heart disease.

# boxplot for ST depression against heart disease diagnosis. as we can see from the boxplot
# it is very obvious that a higher ST depression is highly correlated with heart disease.
# we can see that the ST depression levels are much higher in the heart disease diagnosis
# when compared to the ST depression levels of no heart disease. This makes sense as in a 
# clinical setting a ST depression result of higher than 1mm is clinically significant
# and could point to a cardiac condition including heart disease. to confirm my suspicions 
# I would check to see if STdpression has a parametric distribution and then depending on
# the distribution I would conduct a t test and correaltion test to see if I get a signicant
# result. 
ggplot(heart_disease_all10, aes(x = heart_disease_all10$Diagnosis, y = heart_disease_all10$STdepression , fill = "red")) +
  geom_boxplot() +
  labs(title = "Figure 23 : Box Plot of ST Depression vs Heart Disease",
       x = "Heart Disease",
       y = "ST Depression in mm")


#Q8 Is there a relationship between Serum Cholesterol and Stdepression
  
# I wanted to ask this question as these are both risk factors for heart disease but
# not for eachother so I wanted to see if there was an association between the two.

#scatterplot for Serum Cholesterol against STdepression. from the scatter plot its hard 
# see an association between Serum Cholesterol and STdepression so maybe they arent associated
# with eachother. To be sure you would need to check normality of the two variables and
# conduct a correlation and t test to see if you get significant results.
plot(heart_disease_all10$SerumCholesterol, heart_disease_all10$STdepression, 
     xlab = "Serum Cholesterol in mg/dl", ylab = "STdepression in mm",
     main = "Figure 24: Serum Cholesterol vs STDepression" , col = "blue")
  
  
#Q9 Is there a relationship between Max Heart Rate and Age?

# I wanted to ask this question because I have been told numerous times that your maximum
# heart rate is your age minus 220. Because im 24 theoretically my max heart rate is 196bpm.
# I want to see if this is true and the older you get the lower your maximum heart rate is.

# scatterplot with age against max heart rate.From the scatter plot it seems that the saying
# is correct as we can see a slight correlation between older age and lower Max Heart Rate
# from the line which is the correlation coefficient. The highest Max Heart rate value of the
# whole dataset is someone who is around 29 it seems which makes perfect sense. to confirm
# our suspicions we would assess both variables for normality then find correlation
# coefficient and t test to see if we find significant results.
ggplot(heart_disease_all10, aes(x = heart_disease_all10$MaxHeartRate, y = heart_disease_all10$Age)) +
  geom_point() +
  labs(title = "Figure 25 : Scatter Plot of Age v Max Heart Rate",
       x = "Max Heart Rate", y = "Age") +
  geom_smooth(method = "lm", se = FALSE, color = "red")
  
  
  

# Final comments
# Overall i think this was a nice dataset to do the analysis on and I wish I could probe
# and do more statiscal analysis on it as I think there would've been a lot of statiscally
# significant results. I picked this dataset as I am very much so interested in the 
# medical/pharma side of data science (did my undergrad in medical/pharma science) so I 
# think this was a good dataset to choose for me. i think I  found a lot of interesting 
# assumptions based on the graphs I created. from the graphs I learned there seems to be 
# an relationship between age and MaxHeartRate, there is no realtionship between Serum 
# Cholesterol and STdepression, there is a relationship between STdepression and heart
# disease, there is a relationship between heart disease and sex, there is a relationship
# between fasting blood sugar and heart disease, there is a relationship between resting 
# ECG result and heart disease, there is a relationship between exercised induced angina
# (chest pain) and heart disease, there is a relationship between age and heart disease and
# finally there is a relationship between serum cholesterol and heart disease. In this 
# assignment I also learned how to clean data efficiently and merge datasets together.
# I also learned how to visualise data effectively and got better at labelling I think.
# What ive learned from this dataset is people who are more likely to NOT have heart disease
# are women, young, have asymptomic chest pain, have lower serum cholesterol levels, have 
# low fasting blood sugar levels, have a normal resting ECG result, dont have exercise 
# induced angina and have a lower STdepresion. People with heart disease are more likely
# to be men, older, more likely to have chest pain and less asymptomatic, have higher
# serum cholesterol levels, have high fasting blood sugar levels, more likely to have 
# abnormal or left ventricular hypertrophy ECG results, more likely to have exercise induced
# angina and have a higher STdepression.
# LINK TO DATASET : https://archive.ics.uci.edu/dataset/45/heart+disease