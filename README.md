# statistical-learning
Final project for UIUC statistical learning course

# Classification of Schools and Analysis of Academic Performance in Portuguese for High School Students
## Contributors: Xiaomian Wu, Yanxuan Wang, Jing Wang, Min Jin
## Date: 5/12/2017

## Data

We obtained this Student Performance Data Set from the UCI website. It contains 33 attributes of 649 students(shown below) from two Portuguese high schools, Gabriel Pereira (GP) and Mousinho da Silveira(MS). The data was collected by school reports and questionaires. It provides information about students’ grades, demographic features, social and school related features. 17 of the 33 attributes are categorical variables and the rest are discrete numeric variables. Among discrete numeric variables, five of them can be treated as continuous numeric variables and three of the five are grades of Portuguese language, which are highly correlated with each other.

Basically, the attributes could be divided into three parts, basic information about students themselves and information about their family and information about their school performance. Attributes about students include school, sex, age, guardian, traveltime, activities, alcohol consumption etc. Attributes about their family include parent education and job, family size, quality of family relationships etc. Attributes about school performance include study time, grades, failures of classes etc.

## Project Goals

In this project, we are going to perform one classification task and one regression task.

First, we will classify or predict which school the students come from based on other attributes. This is a binary classification. Some of the attributes such as reason (the reason to choose school) and address (student address) suggest some difference between the two school and hence their students. We would like to examine whether it is feasible to predict which school a student attends based on provided attributes.

Second, we are interested in predict a student’s first period grade G1 without using the second period grade G2 and final grade G3, which are highly linearly correlated with G1. It would be interesting to examine which attributes affect students’ grades the most and possibly provide suggestions on how to improve students’ performance in Portuguese class.
