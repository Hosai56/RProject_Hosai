install.packages("wooldridge")
install.packages("dplyr")
library("wooldridge")
library("dplyr")

View(fertil1)
head(fertil1)
str(fertil1)

##Filtering Function 2  qual

Black_kids=filter(fertil1, black== 1 , east== 0)
View(Black_kids)

#### 1 qual 1 quan

Age_black = filter(fertil1, age>=50 , black== 0)
View(Age_black)

####2 quan

Education_kids = filter(fertil1, educ> 10 , kids < 3)
View(Education_kids)


#%in% function to get the max and min of age 
#omitting NA values finding min and max
age=fertil1$age
is.null(fertil1$age)
summary(fertil1$age)
min_max_age=filter(fertil1, age %in% c(35, 54))
View(min_max_age)

#arrange
Education_ascending = arrange(fertil1, educ)
View(Education_ascending)

Kids_descending = arrange(fertil1, desc(kids))
View(Kids_descending)

#select
Qualitative_data= select(fertil1, c(black, east))
View(Qualitative_data)

Qual_Quant_data = select(fertil1, c(age, town))
View(Qual_Quant_data)

Quantitative_data = select(fertil1, c(educ, kids))
View(Quantitative_data)

#rename
Kids_rename=rename(fertil1, number_of_kids=kids)
View(Kids_rename)

educ_rename = rename(fertil1, years_of_education=educ)
View(educ_rename)

#mutate
mutated_data=mutate(fertil1, overall_education=meduc+feduc)
View(mutated_data)

#summarize
summarise(fertil1, median(age), median(educ))
summarise(fertil1,max(meduc), min(feduc))

kids= group_by(fertil1,kids)
summarise(kids, mean(educ), sd(educ))

#pulling a column as a vector
pull(fertil1, feduc)
pull(fertil1, west)

#sample_n
dim(fertil1)
sampledfertil= sample_n(fertil1, 100)
head(sampledfertil)
summary(sampledfertil)
