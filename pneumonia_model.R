library(DMwR)
library(glmnet) #its auc is faster for records lessthan 150K
library(h2o)
library(xgboost)
library(stringr)
library(tm)
library(dplyr)
library(randomForest)
#----- Data -------------
setwd("/Volumes/Capstone 2016/")
data <- readRDS("Merging/final_data_wo_labs.RDS")
#names(data[,1:200])
#Keys - 1, 2=5537 (patient id x,y)
#3-20 critical info -  (demographics, admission details etc)
#21- 64 Days from admission to all procedures
#65 - 116 Physician specialty
#117 - 185  Broad specialization (Vizient) -Type of surgery
#186 - 5531 - Diagnosis codes
#Target1 complications- 5532 - 5536, Target2 mortality - 5538, 5539
#Vital data - 5540-5549

# features indexing
demo <- 4:19 #Not considering count of procedures (index 20)
vital <- 5540:5549
days_to_surgery <- 21:64 
days_toz_proc1 <- 21
surgery_type <- 65:116
department <- 117:185
diagnosis_codes <- 186:5531


#Basic logistic
#Fixing target variable: 
#A- Pneumonia (P)
#B- PE/DVT
#C- Sepsis
#D- Cardiac
#E- Gastro / Ulcer
target <- data[,c(5532:5536)]
names(target) <- c("Pneumonia","DVT","Sepsis", "Cardiac", "Gastro")
target$Cardiac <- as.numeric(as.character(target$Cardiac))
#t <- table(target)
barplot(colSums(target)) #Visualizing distribution of complications

#----------------Pneumonia
y <- target$Pneumonia
#Considering only critical info, vitals
X <- data[,c(demo, vital)]
summary(X) 
#icu_days are in seconds - converting to mins
X$icu_days <- X$icu_days/60.00
summary(X$icu_days)

#Impute missing values in vitals
#apply(X,2, function(x){x[is.na(x)] <- median(x)})
X <- centralImputation(X)
set.seed(1002)
s <- sample(nrow(X), 0.7*nrow(X))
X_tr1 <- X[s,]
y_tr <- y[s]
X_ts1 <- X[-s,]
y_ts <- y[-s]

logistic.m <- glm(y_tr~., data = X_tr1, family = "binomial")
obj <- summary(logistic.m)
pval <- obj$coefficients[,4]
names(logistic.m$R[1,])[pval < 0.05] #Significant features
# [1] "Admission.SourceTransfer from a different hospital"        
# [2] "Emergency.Room.PatientYes"                                 
# [3] "RaceWhite"                                                 
# [4] "Vizient.Primary.PayerCommercial/Private Prisoners"         
# [5] "Vizient.Primary.PayerCommercial/Private Transplant Network"
# [6] "Admit.Severity.of.IllnessMajor"                            
# [7] "Admit.Severity.of.IllnessMinor"                            
# [8] "Month"                                                     
# [9] "Year"                                                      
# [10] "icu_trips"                                                 
# [11] "Count_Of_Procedures"                                       
# [12] "mews.score"    

#check performance
auc(y_tr, logistic.m$fitted.values)  #train auc: 0.9036954
thresh <- function(prob, threshold){
  return(ifelse(prob<threshold, 0, 1))
}
yhat <- thresh(logistic.m$fitted.values, 0.025)
table(y_tr, yhat)
# table(y_tr, yhat)
#       yhat
#     0     1
# 0 21223  1113
# 1    87   118

#validation on test
yhat_ts1 <- predict(logistic.m, X_ts1, type = "response")
auc(y_ts, yhat_ts1)  #test auc: 0.89
#Cross table
yhat_ts1_class <- thresh(yhat_ts1, 0.025)
table(y_ts, yhat_ts1_class)
# yhat_ts1_class
#     0    1
# 0 9122  455
# 1   35   49

# Add surgery type(surrogate: Physician spec) and days from admission features
X <- data[,c(demo,vital,days_toz_proc1,surgery_type)]
X <- centralImputation(X)
X_tr1 <- X[s,]
X_ts1 <- X[-s,]
#fit logistic
logistic.m1 <- glm(y_tr~., data = X_tr1, family = "binomial")
obj <- summary(logistic.m1)
pval <- obj$coefficients[,4]
names(logistic.m1$R[1,])[pval < 0.05] #Significant features
logistic.m1$coefficients[pval < 0.05] # and how they are effecting Pneumonia

#check performance
auc(y_tr, logistic.m1$fitted.values)  #train auc: 0.92
pROC::auc(y_tr, logistic.m1$fitted.values) #cross checking
yhat <- thresh(logistic.m1$fitted.values, 0.03)
table(y_tr, yhat)

# table(y_tr, yhat) #better than first model
#       yhat
#     0     1
# 0 21248  1088
# 1    61   144

#validation on test
yhat_ts1 <- predict(logistic.m1, X_ts1, type = "response")
auc(y_ts, yhat_ts1)  #test auc: 0.86 #reduced performance - overfitting!?
#k-fold crossvalidation to ensure

#Cross table
yhat_ts1_class <- thresh(yhat_ts1, 0.025)
table(y_ts, yhat_ts1_class)
# yhat_ts1_class
#     0    1
# 0 9110  467
# 1   37   47

#Fit randomforest
#let us fit randomForest using h20 
#intializing h2o process with 2 threads
local=h2o.init(nthreads=2,max_mem_size='2G',assertion = FALSE)
train_rf = as.h2o((cbind(as.factor(y_tr),X_tr1)))
features <- names(train_rf)[-1]
names(train_rf)[1] <- "target"
rf_pneu=h2o.randomForest(x=features,
                         y="target",
                        training_frame=train_rf,
                        ntrees=1000,
                        mtries=10,      
                        max_depth=6,
                        classification = T,
                        binomial_double_trees = T,
                        balance_classes = F,
                        verbose = T
                        )
t <- h2o.varimp(rf_pneu)
h2o.auc(rf_pneu) #0.89 on train
yhat_ts <- as.data.frame(h2o.predict(rf_pneu, as.h2o((X_ts1))))
auc(y_ts, yhat_ts$p1) #test auc 0.934
yhat_ts$predict <- thresh(yhat_ts$p1,0.03)
table(y_ts, yhat_ts$predict)
#     predicted
#     0    1
# 0 9121  456
# 1   29   55

#GBM model using h2o
gbm_pneu=h2o.gbm(x=features,
                         y="target",
                         training_frame=train_rf,
                         n.trees=2000,
                         shrinkage = 0.01,
                         holdout=6,
                         classification = T,
                         balance_classes = F,
                         verbose = T,
                         importance=T)

h2o.auc(gbm_pneu) #0.953 on train - overfitting?
t_gbm <- h2o.varimp(gbm_pneu)
yhat_ts_gbm <- as.data.frame(h2o.predict(gbm_pneu, as.h2o((X_ts1))))
auc(y_ts, yhat_ts_gbm$p1) #test auc 0.923
yhat_ts_gbm$predict <- thresh(yhat_ts_gbm$p1,0.03)
table(y_ts, yhat_ts$predict)

#Xgboost model
#time to work on data.matrix - to avoid multiple levels in features
features <- names(X)
for(f in features){
  if (class(X[[f]]) == "character"){
    levels <- unique(X[[f]])
    X[[f]] <- as.integer(factor(X[[f]], levels = levels))
  }
}
#split
X_tr1 <- X[s,]
X_ts1 <- X[-s,]

#------- fitting a xgb model
fit_xgb <-function(X_tr1, y_tr){
  #create validation index for xgb model
  val <- sample(nrow(X_tr1), 0.2*nrow(X_tr1))
  dtrain <- xgb.DMatrix(data.matrix(X_tr1[-val,]), label = y_tr[-val])
  dval <- xgb.DMatrix(data.matrix(X_tr1[val,]), label = y_tr[val])
  watchlist <- list(val = dval, train = dtrain)
  
  param <- list(  objective           = "binary:logistic", 
                  booster             = "gbtree",
                  eta                 = 0.0005, # 0.06, #0.01,
                  max_depth           = 6, #changed from default of 8
                  subsample           = 0.5, # 0.7
                  colsample_bytree    = 0.5, # 0.7
                  eval_metric         = "auc")
  # alpha = 0.0001
  # lambda = 1
  
  xgb.pneu <- xgb.train(params              = param, 
                        data                = dtrain, 
                        nrounds             = 2000, #300, #280, #125, #250
                        verbose             = 1,
                        early.stop.round    = 100,
                        watchlist           = watchlist,
                        maximize            = TRUE,
                        print.every.n       = 100 
                        #feval               = evalerror
  ) #99.16 auc - super overfitting
  
}

xgb.pneu <- fit_xgb(X_tr1, y_tr)
yhat_xgb_pneu <- predict(xgb.pneu, data.matrix(X_ts1))
auc(y_ts, yhat_xgb_pneu) #0.90 on test
yhat_xgb_pneu_class <- thresh(yhat_xgb_pneu, 0.115)
table(y_ts, yhat_xgb_pneu_class) #Better results so far
# yhat_xgb_pneu_class
#     0    1
# 0 9050  527
# 1   30   54

#try to use diagnosis codes
#idea: get the diagnosis codes of positives and get top 100 terms from train data
X_diagnosis <- data[,c(diagnosis_codes)]
Xtr_diagnosis <- X_diagnosis[s,]
Xts_diagnosis <- X_diagnosis[-s,]

#Get high freq disease terms in positive pneumonia
positive_pneu <- Xtr_diagnosis[which(y_tr == 1),]
get_dtm <- function(positive){
  corpus = character()
  for (i in (1:nrow(positive))){
    a <- t(positive[i,])
    a <- paste0(rownames(a)[a!=0], collapse = " ")
    corpus <- paste0(corpus, a, collapse = " ")
  }
  if(trimws(corpus) == ""){
    corpus <- str_replace_all(corpus, "[[:punct:]]", "")
    corpus <- str_replace_all(corpus, "[0123456789]", "")
    corpus <- as.data.frame(corpus)
    corpus <- VCorpus(DataframeSource(corpus))
    corpus = tm_map(corpus, stripWhitespace)
    #since it is not a prose
    corpus = tm_map(corpus, removeWords, stopwords("SMART"))
    #inspect(corpus)
    corpus = DocumentTermMatrix(corpus, control = list(weighting = weightTf))
  }
   
  return(corpus)
}
corpus.tf <- get_dtm(positive_pneu)
high_pneu <- findFreqTerms(corpus.tf, 10)
corpus_mat <- as.matrix(corpus.tf)

#for each word in the high freq list, get number of occurences in patient's document
positive_terms <- as.data.frame(matrix(nrow = 0,ncol = length(high_pneu)))
temp <- positive_terms
for (p in (1:nrow(X_diagnosis))){
  corpus <- t(X_diagnosis[p,])
  corpus <- paste0(rownames(corpus)[corpus!=0], collapse = " ")
  corpus <- str_replace_all(corpus, "[[:punct:]]", "")
  corpus <- trimws(str_replace_all(corpus, "[0123456789]", ""))
  corpus <- strsplit(corpus, " ")[[1]]
  corpus <- corpus[corpus != ""]
  freq <- list()
  for(w in high_pneu){
    count = 0
    if (w %in% corpus) count <- length(corpus[corpus == w])
    freq <- c(freq,count)
  }
  x <- as.data.frame(t(unlist(freq)))
  temp <- rbind_all(list(temp,x))
  if(p%%500 == 0){
    positive_terms <- rbind_all(list(positive_terms,temp))
    temp <- as.data.frame(matrix(nrow = 0,ncol = length(high_pneu)))
    print(p)
  }
}
positive_terms <- dplyr::rbind_all(list(positive_terms,temp))
colnames(positive_terms) <- high_pneu

#xgb on diagnosis alone
tr <- positive_terms[s,]
ts <- positive_terms[-s,]

#split train and validate for xgb - for features from diagnosis codes
xgb_dia <- fit_xgb(tr, y_tr)
dia_varimp <- xgb.importance(feature_names = names(tr), model = xgb_dia)
yhat_dia <- predict(xgb_dia, data.matrix(ts))

auc(y_ts, yhat_dia) #0.78 on test
yhat_dia_class <- thresh(yhat_dia, 0.035)
table(y_ts, yhat_dia_class) #not bad
#   yhat_dia_class
#     0    1
# 0 7779 1798
# 1   33   51

#run model with full set of features
X <- as.data.frame(cbind(X, positive_terms))
X_tr1 <- X[s,]
X_ts1 <- X[-s,]

#-----------Pneumonia - Using all the relevant features -------------
#directly fitting xgb - No point in trying different algorithms when xgb always beat them!!
#Removing days to surgery - technically speaking, we can't use'em
#Using only days to procedure1 instead
X <- data[,c(demo,vital,surgery_type, days_toz_proc1, department)]
X <- centralImputation(X)

#convert to numeric
features <- names(X)
for(f in features){
  if (class(X[[f]]) == "character"){
    levels <- unique(X[[f]])
    X[[f]] <- as.integer(factor(X[[f]], levels = levels))
  }
}
X <- as.data.frame(cbind(X, positive_terms))
X_tr1 <- X[s,]
X_ts1 <- X[-s,]

xgb.pneu.all <- fit_xgb(X_tr1, y_tr) #0.953 val auc, 0.97 train auc

yhat_all_pneu <- predict(xgb.pneu.all, data.matrix(X_ts1))
auc(y_ts, yhat_all_pneu) #0.94
auc(y_ts, rep(0,9661))
# arbitrary try to calculate auc
c <- expand.grid(pos = yhat_all_pneu[y_ts == 1], neg = yhat_all_pneu[y_ts==0])
mean(c$pos > c$neg)


xgb_varimp <- xgb.importance(names(X), model = xgb.pneu.all)
yhat_pneu_class <- thresh(yhat_all_pneu, 0.07)
table(y_ts, yhat_pneu_class)
#   yhat_pneu_class @ 0.055 threshold
#     0    1
# 0 9234  343
# 1   29   55

#   @ 0.07 thresh   
#    0    1
# 0 9383  194
# 1   39   45