#title: "Classification of Schools and Analysis of Academic Performance in Portuguese for High School Students"
#author: "Xiaomian Wu, Yanxuan Wang, Jing Wang, Min Jin"
#date: "5/12/2017"

#script was converted from R Markdown file
## ---- message=FALSE, warning=FALSE, include=FALSE------------------------
# library and helper functions
library(caret)
library(corrplot)
library(rpart.plot)
library(rpart)
library(randomForest)
library(gbm)
library(glmnet)
library(plyr)  
library(klaR)
library(MASS)

oob = trainControl(method = "oob")
cv_5 = trainControl(method = "cv", number = 5)

accuracy = function(actual, predicted){
  mean(actual == predicted)
}

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

get_best_result = function(caret_fit) {
  best_result = caret_fit$results[as.numeric(rownames(caret_fit$bestTune)), ]
  rownames(best_result) = NULL
  best_result
}

## ------------------------------------------------------------------------
set.seed(430)
student_por = read.csv("student-por.csv", sep=";",header=TRUE)
dim(student_por)

# Create Train and Test Data
id = createDataPartition(student_por$G3, p = 0.8, list = F)
por_trn = student_por[id, ]
por_tst = student_por[-id, ]

## ---- echo=FALSE---------------------------------------------------------
table(por_trn$school)

## ---- echo=FALSE---------------------------------------------------------
featurePlot(por_trn[, c( "absences", "G1", "G2", "G3")], por_trn$school, plot = "density",
            scales = list(x = list(relation = "free"),  y = list(relation = "free")), adjust= 1.5, pch = "|", 
            layout = c(4,1), auto.key = list(columns  = 2))

## ---- echo=FALSE---------------------------------------------------------
boxplot(age~school, data = por_trn, xlab = "School", ylab = "Age")

## ---- echo=FALSE---------------------------------------------------------
M = cor(data.frame(por_trn[, c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)], response = as.numeric(por_trn[, 1])))
corrplot(M, method = "circle")

## ---- echo=FALSE---------------------------------------------------------
ggplot(por_trn,aes(x= address, group = school,fill = school))+
  geom_bar(position="dodge",stat="count")+theme_bw() 
ggplot(por_trn,aes(x= reason, group = school,fill = school))+
  geom_bar(position="dodge",stat="count")+theme_bw()
ggplot(por_trn,aes(x= Medu, group = school,fill = school))+
  geom_histogram(position="dodge",binwidth=0.25)+theme_bw()
ggplot(por_trn,aes(x= Fedu, group = school,fill = school))+
  geom_histogram(position="dodge",binwidth=0.25)+theme_bw()
ggplot(por_trn,aes(x= Fjob, group = school,fill = school))+
  geom_bar(position="dodge",stat="count")+theme_bw()
ggplot(por_trn,aes(x= traveltime, group = school,fill = school))+
  geom_histogram(position="dodge",binwidth=0.25)+theme_bw()
ggplot(por_trn,aes(x= internet, group = school,fill = school))+
  geom_bar(position="dodge",stat="count")+theme_bw()

## ---- echo=FALSE---------------------------------------------------------
set.seed(430)
gbm_grid =  expand.grid(interaction.depth = 1:5,
                        n.trees = (1:6) * 500,
                        shrinkage = c(0.001, 0.01, 0.1),
                        n.minobsinnode = 10)
gbm_all_class = train(school ~ .- G1 -G2, data = por_trn,
                      method = "gbm",
                      trControl = cv_5,
                      verbose = FALSE,
                      tuneGrid = gbm_grid)

## ---- echo=FALSE---------------------------------------------------------
head(summary(gbm_all_class), 15) # obtain variable inportance plot

gbm_all_class_trn = accuracy(predict(gbm_all_class, por_trn), por_trn$school) # train acc
gbm_all_class_tst = accuracy(predict(gbm_all_class, por_tst), por_tst$school) # test acc

## ---- echo=FALSE---------------------------------------------------------
set.seed(430)
dt_all_class = rpart(school ~ .- G1 -G2, data = por_trn, method = "class")
#plotcp(dt_all_class)

dt_all_class_cp = dt_all_class$cptable[which.min(dt_all_class$cptable[,"xerror"]),"CP"]
dt_class_prune = prune(dt_all_class, cp =dt_all_class_cp)
prp(dt_class_prune)

head(dt_class_prune$variable.importance, 15)

dt_class_trn = accuracy(predict(dt_class_prune, por_trn, type = "class"), por_trn$school) # train acc
dt_class_tst = accuracy(predict(dt_class_prune, por_tst, type = "class"), por_tst$school) # test acc

## ---- echo=FALSE---------------------------------------------------------
X = model.matrix(school ~ ., por_trn)[, -1]
y = por_trn$school
lasso_class = cv.glmnet(X, y, family = "binomial", alpha = 1)
result_lasso = coef(lasso_class)
names(which(!(result_lasso == 0)[, 1]))[-1]

## ---- echo=FALSE---------------------------------------------------------
# Stepwise glm
glm_step = step(glm(school ~ ., data = por_trn, family = "binomial"), trace = FALSE)
# Update glm model 
(selected_formula = glm_step$formula)
glm_step_sel = train(selected_formula, data = por_trn, method = "glm", family = "binomial", trControl = cv_5, tuneLength=5)
glm_step_sel_trn = accuracy(actual = por_trn$school, predicted = predict(glm_step_sel, por_trn))
glm_step_sel_tst = accuracy(actual = por_tst$school, predicted = predict(glm_step_sel, por_tst))

## ------------------------------------------------------------------------
set.seed(430)
glm_add = train(school ~ . , data = por_trn, method = "glm", trControl = cv_5, tuneLength=5)

## ---- include=FALSE------------------------------------------------------
glm_add_trn = accuracy(actual = por_trn$school, predicted = predict(glm_add, por_trn))
glm_add_tst = accuracy(actual = por_tst$school, predicted = predict(glm_add, por_tst))

## ------------------------------------------------------------------------
set.seed(430)
glm_sel = train(school ~  G3 + age + absences + address + traveltime + internet +  Medu + Fedu + reason + studytime + Fjob + freetime + goout, data = por_trn, method = "glm", trControl = cv_5)

## ---- include=FALSE------------------------------------------------------
glm_sel_trn = accuracy(actual = por_trn$school, predicted = predict(glm_sel, por_trn))
glm_sel_tst = accuracy(actual = por_tst$school, predicted = predict(glm_sel, por_tst))

## ------------------------------------------------------------------------
set.seed(430)
enet = train( school ~ (G3 + age + absences + address + traveltime + internet +  Medu + reason + studytime + Fjob + goout + freetime)^2 , data = por_trn, method = "glmnet", trControl = cv_5, tuneLength = 10)

## ---- include=FALSE------------------------------------------------------
enet_trn = accuracy(predict(enet, por_trn), por_trn$school)
enet_tst = accuracy(predict(enet, por_tst), por_tst$school)

## ------------------------------------------------------------------------
set.seed(430)
rda_class = train(school ~  G3 + age + absences + address + internet +  Medu + traveltime + reason + studytime +Fjob + freetime +goout, data = por_trn, method = "rda", trControl = cv_5, tuneLength=10)

## ---- echo=FALSE---------------------------------------------------------
rda_trn = accuracy(predict(rda_class, por_trn), por_trn$school)
rda_tst = accuracy(predict(rda_class, por_tst), por_tst$school)

## ------------------------------------------------------------------------
set.seed(430)
gbm_sel_class = train(school ~  G3 + age + absences + address + traveltime + internet + Medu + Fedu + reason + studytime + Fjob + goout ,data=por_trn,method = "gbm", verbose = FALSE, trControl = cv_5, tuneGrid = gbm_grid)

## ---- include=FALSE------------------------------------------------------
gbm_sel_class_trn = accuracy(actual = por_trn$school, predicted = predict(gbm_sel_class, por_trn))
gbm_sel_class_tst = accuracy(actual = por_tst$school, predicted = predict(gbm_sel_class, por_tst))

## ------------------------------------------------------------------------
set.seed(430)
# Random forest with all variables
rf_grid_all=expand.grid(mtry=1:30)
rf_all_class=train(school~.-G1-G2,data=por_trn,method="rf",trControl=oob,verbose=FALSE,tuneGrid = rf_grid_all)
rf_all_class$bestTune

set.seed(430)
# Random forest with selected model
rf_grid_sel =  expand.grid(mtry = 1:13)
rf_sel_class=train(school~  G3 + age + absences + address + traveltime + internet +  Medu + Fedu+ reason + studytime + Fjob + goout + freetime , data = por_trn, method="rf", trControl=oob, verbose=FALSE, tuneGrid=rf_grid_sel)
rf_sel_class$bestTune

## ---- include=FALSE------------------------------------------------------
rf_all_class_trn = accuracy(predict(rf_all_class,por_trn),por_trn$school)
rf_all_class_tst = accuracy(predict(rf_all_class,por_tst),por_tst$school)

rf_sel_class_trn = accuracy(predict(rf_sel_class,por_trn),por_trn$school) 
rf_sel_class_tst = accuracy(predict(rf_sel_class,por_tst),por_tst$school)

## ------------------------------------------------------------------------
# KNN
knn_class = train( school ~ G3 + age + absences + address + internet + freetime + traveltime + studytime + reason + Fedu + Medu + Fjob + goout, data = por_trn, method = "knn", trControl = trainControl(method = "cv", number = 5), tuneGrid = expand.grid(k = seq(1, 20, by = 1)))
plot(knn_class)

## ---- include=FALSE------------------------------------------------------
knn_class_trn = accuracy(predict(knn_class,por_trn),por_trn$school)
knn_class_tst = accuracy(predict(knn_class,por_tst),por_tst$school)

## ---- include=FALSE------------------------------------------------------
method = c("Additive Logistic Model", 
           "Selected Logistic Model", 
           "Penalized Logistc", 
           "Selected RDA", 
           "Selected Boosting Model",
           "Additive Random Forest",
           "Selected Random Forest w Interactions", 
           "Selected KNN") 

train_acc = c(glm_add_trn, 
              glm_sel_trn, 
              enet_trn,
              rda_trn,
              gbm_sel_class_trn, 
              rf_all_class_trn, 
              rf_sel_class_trn,
              knn_class_trn)

test_acc = c(glm_add_tst, 
              glm_sel_tst, 
              enet_tst,
              rda_tst,
              gbm_sel_class_tst, 
              rf_all_class_tst, 
              rf_sel_class_tst,
              knn_class_tst)

results = data.frame(method, train_acc, test_acc)
colnames(results) = c("Methods", "Train Accuracy", "Test Accuracy")

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(results)

## ------------------------------------------------------------------------
id2 = createDataPartition(student_por$G3, p = 0.75, list = F)
por_trn_reg= student_por[id2, ]
por_tst_reg = student_por[-id2, ]

## ---- echo=FALSE---------------------------------------------------------
histogram(por_trn_reg$G1)

## ---- echo=FALSE---------------------------------------------------------
featurePlot(x = por_trn_reg[,c("absences", "age") ], 
            y = por_trn_reg$G3, 
            plot = "scatter",
            type = c("p", "smooth"),
            span = 1,
            layout = c(4, 1))

## ---- echo=FALSE---------------------------------------------------------
M = cor(data.frame(por_trn_reg[, c(3,7,8,13,14,15,24,25,26,27,28,29,30)], response = as.numeric(por_trn_reg[, 31])))
corrplot(M, method = "circle")

## ---- echo=FALSE---------------------------------------------------------
par(mfrow = c(1,4))
boxplot(G1~school, data = por_trn, xlab = "School")
boxplot(G1~guardian, data = por_trn, xlab = "Student's Guardian")
boxplot(G1~Walc, data = por_trn, xlab = "Weekend Alcohol Consumption")
boxplot(G1~Pstatus, data = por_trn, xlab = "Parent's Cohabitation Status")
boxplot(G1~Dalc, data = por_trn, xlab = "Workday Alchohol Consumption")
boxplot(G1~reason, data = por_trn, xlab = "Reason to Choose this school")
boxplot(G1~higher, data = por_trn, xlab = "Wants to Take Higher Education")
boxplot(G1~Mjob, data = por_trn, xlab = "Mother's Job")

## ---- include=FALSE------------------------------------------------------
gbm_grid =  expand.grid(interaction.depth = 1:5,
                        n.trees = (1:6) * 500,
                        shrinkage = c(0.001, 0.01, 0.1),
                        n.minobsinnode = 10)

gbm_all_reg = train(G1 ~ . - G2 - G3, data = por_trn_reg,
                      method = "gbm",
                      trControl = cv_5,
                      verbose = FALSE,
                      tuneGrid = gbm_grid)

## ---- echo=FALSE---------------------------------------------------------
head(summary(gbm_all_reg), 15) # obtain variable inportance plot

gbm_all_reg_trn = rmse(predict(gbm_all_reg , por_trn_reg), por_trn_reg$G1) # train rmse
gbm_all_reg_tst = rmse(predict(gbm_all_reg , por_tst_reg), por_tst_reg$G1) # test rmse

## ---- include=FALSE------------------------------------------------------
set.seed(42)
dt_all_reg = rpart(G1 ~ . - G2 - G3, data = por_trn_reg, method = "anova")
#plotcp(dt_all_reg )

dt_all_reg_cp = dt_all_reg $cptable[which.min(dt_all_reg $cptable[,"xerror"]),"CP"]
dt_reg_prune = prune(dt_all_reg, cp = dt_all_reg_cp)
prp(dt_reg_prune)

head(dt_reg_prune$variable.importance, 15)

dt_reg_prune_trn = rmse(predict(dt_reg_prune, por_trn_reg), por_trn_reg$G1) # train rmse
dt_reg_prune_tst = rmse(predict(dt_reg_prune, por_tst_reg), por_tst_reg$G1) # test rmse

## ---- echo=FALSE---------------------------------------------------------
# Stepwise lm
lm_step = step(lm(G1 ~ . -G2-G3, data = por_trn_reg), trace = FALSE)
# Update lm model 
(selected_formula = lm_step$call$formula)
lm_step_sel = train(selected_formula, data = por_trn_reg, method = "lm", trControl = cv_5)
lm_step_sel_trn = rmse(actual = por_trn_reg$G1, predicted = predict(lm_step_sel, por_trn_reg))
lm_step_sel_tst = rmse(actual = por_tst_reg$G1, predicted = predict(lm_step_sel, por_tst_reg))

## ---- echo=FALSE---------------------------------------------------------
X = model.matrix(G1 ~ .-G2-G3, data = por_trn_reg)[, -1]
y = por_trn_reg$G1
lasso_reg = cv.glmnet(X, y, alpha = 1)
result_lasso = coef(lasso_reg)
names(which(!(result_lasso == 0)[, 1]))[-1]

## ------------------------------------------------------------------------
set.seed(42)
lm_add = lm(G1 ~ . - G2 - G3, data = por_trn_reg)

## ---- echo=FALSE---------------------------------------------------------
lm_add_trn = rmse(predict(lm_add, por_trn_reg), por_trn_reg$G1) # train rmse
lm_add_tst = rmse(predict(lm_add, por_tst_reg), por_tst_reg$G1) # test rmse

## ------------------------------------------------------------------------
set.seed(42)
glmn_small = train(G1 ~ failures + higher + absences + studytime + Medu + Fedu + goout + schoolsup + Dalc + health, data = por_trn_reg, method = "glmnet", trControl = cv_5, tuneLength = 10)

## ---- echo=FALSE---------------------------------------------------------
glmn_small_trn = rmse(predict(glmn_small, por_trn_reg), por_trn_reg$G1) # train rmse
glmn_small_tst = rmse(predict(glmn_small, por_tst_reg), por_tst_reg$G1) # test rmse

## ---- message=FALSE, warning=FALSE---------------------------------------
set.seed(42)
gam_grid =  expand.grid(df = seq(1, 5, by = 0.5))

gam_small  = train(
G1 ~ failures + higher + absences + studytime + Medu * Fedu + goout + schoolsup + Dalc + health,
data = por_trn_reg,
method = "gamSpline",
verbose = FALSE,
trControl = cv_5,
tuneGrid = gam_grid
)

## ---- echo=FALSE---------------------------------------------------------
gam_small_trn = rmse(predict(gam_small, por_trn_reg), por_trn_reg$G1) # train rmse
gam_small_tst = rmse(predict(gam_small, por_tst_reg), por_tst_reg$G1)# test rmse

## ------------------------------------------------------------------------
set.seed(42)

gbm_small = train(G1 ~ failures + higher +absences + studytime + Medu * Fedu + goout + schoolsup + Dalc + health, method = "gbm", data = por_trn_reg, verbose = FALSE, trControl = cv_5, tuneGrid = gbm_grid)

## ---- echo=FALSE---------------------------------------------------------
gbm_small_trn = rmse(predict(gbm_small, por_trn_reg), por_trn_reg$G1) # train rmse
gbm_small_tst = rmse(predict(gbm_small, por_tst_reg), por_tst_reg$G1) # test rmse

## ---- message=FALSE, warning=FALSE---------------------------------------
set.seed(42)


## Random forest with selection 
rf_grid_small = expand.grid(mtry = 1: 11) 
rf_small_reg=train(G1~ failures+higher+absences+studytime+Medu*Fedu+goout+schoolsup+Dalc+health,data=por_trn_reg,method="rf",trControl=cv_5,verbose=FALSE,tuneGrid=rf_grid_small)

## ---- echo=FALSE---------------------------------------------------------
rf_small_reg_trn=rmse(predict(rf_small_reg,por_trn_reg),por_trn_reg$G1)
rf_small_reg_tst=rmse(predict(rf_small_reg,por_tst_reg),por_tst_reg$G1)

## ------------------------------------------------------------------------
set.seed(42)

# KNN with further selection 
knn_reg_small = train(
  G1 ~ failures+higher+absences+studytime+Medu*Fedu+goout+schoolsup+Dalc+health ,
  data = por_trn_reg, 
  method = "knn",
  trControl = trainControl(method = "cv", number = 5),
  preProcess = c("center", "scale"),
  tuneGrid = expand.grid(k = seq(1, 50, by = 1))
)

## ---- echo=FALSE---------------------------------------------------------
plot(knn_reg_small)

knn_small_reg_trn=rmse(predict(knn_reg_small,por_trn_reg),por_trn_reg$G1)
knn_small_reg_tst=rmse(predict(knn_reg_small,por_tst_reg),por_tst_reg$G1)

## ---- include=FALSE------------------------------------------------------
model = c("Simple Linear Regression",
          "Penalized Linear Regression",
          "Generalized Models",
          "Boosting Model", 
          "Random Forest",
          "KNN")

train_rmse = c(lm_add_trn,glmn_small_trn,gam_small_trn,gbm_small_trn,rf_small_reg_trn,knn_small_reg_trn)

test_rmse = c(lm_add_tst,glmn_small_tst,gam_small_tst,gbm_small_tst,rf_small_reg_tst,knn_small_reg_tst)
               

tables = data.frame(model, train_rmse, test_rmse)
colnames(tables) = c("Model", "Train RMSE", "Test RMSE")

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(tables)

## ----code = readLines(knitr::purl("path/to/file.Rmd", documentation = 1)), echo = T, eval = F----
## NA

