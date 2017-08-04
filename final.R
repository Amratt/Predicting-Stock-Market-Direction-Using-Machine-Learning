library(quantmod)
library(randomForest)
library(TTR)
library(class)
library(tree)
library(SDMTools)
library(ROCR)
setwd("~/Desktop/Data Science (Google)/Data Analytics MS/Datasets")
################ DATA EXTRACTION #######################
getSymbols(c("A","AAL","AAP","ABBV","ABC","ABT",
             "ACN","ADBE","ADI","ADM","ADP","ADS","ADSK",
             "AEE","AEP","AES","AET","AFL","AGN","AIG",
             "AIV","AIZ","AJG","AKAM","ALB","ALGN","ALK",
             "ALL","ALLE","ALXN","AMAT","AMD","AME",
             "AMG","AMGN","AMP","AMT","AMZN","AN","ANSS",
             "ANTM","AON","APA","APC","P","APH",
             "ARE","ARNC","ATVI","AVB","AVGO","AVY",
             "AWK","AXP","AYI","AZO","BA","BAC","BAX",
             "BBBY","BBT","BBY","BCR","BDX","AAPL","BEN",
             "BIIB","BK","BLK","BLL","BMY","BSX","BWA",
             "BXP","C","CA","CAG","CAH","CAT","CB","CBG",
             "CBOE","CBS","CCI","CCL","CELG","CERN","CF",
             "CFG","CHD","CHK","CHRW","CHTR","CI","CINF",
             "CL","CLX","CMA","CMCSA","CME","CMG","CMI",
             "CMS","CNC","CNP","COF","COG","COH","COL",
             "COO","COP","COST","COTY","CPB", "CRM", "CSCO",
             "CSRA","CSX","CTAS", "CTL", "CTSH","CTXS","CVS",
             "CVX","CXO","D","DAL","DD","DE","DFS","DG",
             "DGX","DHI","DHR","DIS","DISCA","DISCK","DISH",
             "DLPH","DLR","DLTR","DOV","DOW","DPS","DRI",
             "DTE","DUK","DVA","DVN","DXC","EA","EBAY",
             "ECL","ED","EFX","EIX","EL","EMN","EMR","EOG",
             "EQIX","EQR","EQT","ES","ESRX","ESS","ETFC","ETN",
             "ETR","EVHC","EW","EXC","EXPD","EXPE","EXR","F",
             "FAST","FB","FBHS","FCX","FDX","FE","FFIV","FIS",
             "FISV","FITB","FL","FLIR","FLR","FLS","FMC","FOX",
             "FOXA","FRT","FTV","GD","GE","GGP","GILD","GIS",
             "GLW","GM","GOOG","GOOGL","GPC","GPN","GPS","GRMN",
             "GS","GT","GWW","HAL","HAS","HBAN","HBI","HCA","HCN",
             "HCP","HD","HES","HIG","HLT","HOG","HOLX","HON","HP",
             "HPE","HPQ","HRB","HRL","HRS","HSIC","HST","HSY","HUM",
             "IBM","ICE","IDXX","IFF","ILMN","INCY","INFO","INTC",
             "INTU","IP","IPG","IR","IRM","ISRG","IT","ITW","IVZ",
             "JBHT","JCI","JEC","JNJ","JNPR","JPM","JWN","K","KEY",
             "KHC","KIM","KLAC","KMB","KMI","KMX","KO","KORS","KR",
             "KSS","KSU","L","LB","LEG","LEN","LH","LKQ","LLL","LLY",
             "LMT","LNC","LNT","LOW","LRCX","LUK","LUV","LVLT","LYB",
             "M","MA","MAA","MAC","MAR","MAS","MAT","MCD","MCHP","MCK",
             "MCO","MDLZ","MDT","MET","MHK","MKC","MLM","MMC","MMM","MNK",
             "MNST","MO","MON","MOS","MPC","MRK","MRO","MS","MSFT","MSI",
             "MTB","MTD","MU","MUR","MYL","NAVI","NBL","NDAQ","NEE","NEM",
             "NFLX","NFX","NI","NKE","NLSN","NOC","NOV","NRG","NSC","NTAP",
             "NTRS","NUE","NVDA","NWL","NWS","NWSA","O","OKE","OMC","ORCL",
             "ORLY","OXY","PAYX","PBCT","PCAR","PCG","PCLN","PDCO",
             "PEG","PEP","PFE","PFG","PG","PGR","PH","PHM","PKI","PLD",
             "PM","PNC","PNR","PNW","PPG","PPL","PRGO","PRU","PSA","PSX",
             "PVH","PWR","PX","PXD","PYPL","QCOM","QRVO","RAI","RCL","RE",
             "REG","REGN","RF","RHI","RHT","RIG","RJF","RL","ROK",
             "ROP","ROST","RRC","RSG","RTN","SBUX","SCG","SCHW","SEE","SHW",
             "SIG","SJM","SLB","SLG","SNA","SNI","SNPS","SO","SPG","SPGI",
             "SPLS","SRCL","SRE","STI","STT","STX","STZ","SWK","SWKS","SYF",
             "SYK","T","TAP","TDG","TEL","TGT","TIF","TJX","TMK","TMO",
             "TRIP","TROW","TRV","TSCO","TSN","TSO","TSS","TWX","TXN","TXT",
             "UA","UAA","UAL","UDR","UHS","ULTA","UNH","UNM","UNP","UPS",
             "URI","USB","UTX","V","VAR","VFC","VIAB","VLO","VMC","VNO",
             "VRSK","VRSN","VRTX","VTR","VZ","WAT","WBA","WDC","WEC",
             "WFC","WFM","WHR","WLTW","WM","WMB","WMT","WRK","WU","WY","WYN",
             "WYNN","XEC","XEL","XL","XLNX","XOM","XRAY","XRX","XYL","YUM",
             "ZBH","ZION","ZTS"),from = "2016-07-20", to = "2017-07-20")


# Merge all of the stocks into 1 matrix
sp500.all<- as.matrix(merge(A,AAL,AAP,ABBV,ABC,ABT,
  ACN,ADBE,ADI,ADM,ADP,ADS,ADSK,
  AEE,AEP,AES,AET,AFL,AGN,AIG,
  AIV,AIZ,AJG,AKAM,ALB,ALGN,ALK,
  ALL,ALLE,ALXN,AMAT,AMD,AME,
  AMG,AMGN,AMP,AMT,AMZN,AN,ANSS,
  ANTM,AON,APA,APC,P,APH,
  ARE,ARNC,ATVI,AVB,AVGO,AVY,
  AWK,AXP,AYI,AZO,BA,BAC,BAX,
  BBBY,BBT,BBY,BCR,BDX,AAPL,BEN,
  BIIB,BK,BLK,BLL,BMY,BSX,BWA,
  BXP,C,CA,CAG,CAH,CAT,CB,CBG,
  CBOE,CBS,CCI,CCL,CELG,CERN,CF,
  CFG,CHD,CHK,CHRW,CHTR,CI,CINF,
  CL,CLX,CMA,CMCSA,CME,CMG,CMI,
  CMS,CNC,CNP,COF,COG,COH,COL,
  COO,COP,COST,COTY,CPB, CRM, CSCO,
  CSRA,CSX,CTAS, CTL, CTSH,CTXS,CVS,
  CVX,CXO,D,DAL,DD,DE,DFS,DG,
  DGX,DHI,DHR,DIS,DISCA,DISCK,DISH,
  DLPH,DLR,DLTR,DOV,DOW,DPS,DRI,
  DTE,DUK,DVA,DVN,DXC,EA,EBAY,
  ECL,ED,EFX,EIX,EL,EMN,EMR,EOG,
  EQIX,EQR,EQT,ES,ESRX,ESS,ETFC,ETN,
  ETR,EVHC,EW,EXC,EXPD,EXPE,EXR,F,
  FAST,FB,FBHS,FCX,FDX,FE,FFIV,FIS,
  FISV,FITB,FL,FLIR,FLR,FLS,FMC,FOX,
  FOXA,FRT,FTV,GD,GE,GGP,GILD,GIS,
  GLW,GM,GOOG,GOOGL,GPC,GPN,GPS,GRMN,
  GS,GT,GWW,HAL,HAS,HBAN,HBI,HCA,HCN,
  HCP,HD,HES,HIG,HLT,HOG,HOLX,HON,HP,
  HPE,HPQ,HRB,HRL,HRS,HSIC,HST,HSY,HUM,
  IBM,ICE,IDXX,IFF,ILMN,INCY,INFO,INTC,
  INTU,IP,IPG,IR,IRM,ISRG,IT,ITW,IVZ,
  JBHT,JCI,JEC,JNJ,JNPR,JPM,JWN,K,KEY,
  KHC,KIM,KLAC,KMB,KMI,KMX,KO,KORS,KR,
  KSS,KSU,L,LB,LEG,LEN,LH,LKQ,LLL,LLY,
  LMT,LNC,LNT,LOW,LRCX,LUK,LUV,LVLT,LYB,
  M,MA,MAA,MAC,MAR,MAS,MAT,MCD,MCHP,MCK,
  MCO,MDLZ,MDT,MET,MHK,MKC,MLM,MMC,MMM,MNK,
  MNST,MO,MON,MOS,MPC,MRK,MRO,MS,MSFT,MSI,
  MTB,MTD,MU,MUR,MYL,NAVI,NBL,NDAQ,NEE,NEM,
  NFLX,NFX,NI,NKE,NLSN,NOC,NOV,NRG,NSC,NTAP,
  NTRS,NUE,NVDA,NWL,NWS,NWSA,O,OKE,OMC,ORCL,
  ORLY,OXY,PAYX,PBCT,PCAR,PCG,PCLN,PDCO,
  PEG,PEP,PFE,PFG,PG,PGR,PH,PHM,PKI,PLD,
  PM,PNC,PNR,PNW,PPG,PPL,PRGO,PRU,PSA,PSX,
  PVH,PWR,PX,PXD,PYPL,QCOM,QRVO,RAI,RCL,RE,
  REG,REGN,RF,RHI,RHT,RIG,RJF,RL,ROK,
  ROP,ROST,RRC,RSG,RTN,SBUX,SCG,SCHW,SEE,SHW,
  SIG,SJM,SLB,SLG,SNA,SNI,SNPS,SO,SPG,SPGI,
  SPLS,SRCL,SRE,STI,STT,STX,STZ,SWK,SWKS,SYF,
  SYK,T,TAP,TDG,TEL,TGT,TIF,TJX,TMK,TMO,
  TRIP,TROW,TRV,TSCO,TSN,TSO,TSS,TWX,TXN,TXT,
  UA,UAA,UAL,UDR,UHS,ULTA,UNH,UNM,UNP,UPS,
  URI,USB,UTX,V,VAR,VFC,VIAB,VLO,VMC,VNO,
  VRSK,VRSN,VRTX,VTR,VZ,WAT,WBA,WDC,WEC,
  WFC,WFM,WHR,WLTW,WM,WMB,WMT,WRK,WU,WY,WYN,
  WYNN,XEC,XEL,XL,XLNX,XOM,XRAY,XRX,XYL,YUM,
  ZBH,ZION,ZTS))

############# DATA EXPLORATION & VISUALIZATION ##########################
# A look of the adjusted closing prices for some stocks

chartSeries(NFLX)
chartSeries(AMZN, theme = "white")

# Another look, but combining the charts
chart_Series(Ad(AMZN))
add_TA(Ad(NFLX), on = 1, col = "green")
add_TA(Ad(AAPL), on = 1, col = "blue")
title("Adjusted Closing Prices", cex.main = 1.5, font.main = 3, col.main = "black")

# The same chart but of showing the price rate of change of the adjusted closing

chart_Series(ROC(Ad(AMZN)))
add_TA(ROC(Ad(NFLX)), on = 1, col = "green")
add_TA(ROC(Ad(AAPL)), on = 1, col = "black")
title("Price Rate of Change", cex.main = 2, font.main = 4, col.main = "blue")


# Checking for correlation between the predictors

library(corrplot)
M = cor(Ad(sp500.all))
# it looks like many of the adjusting closing prices of the sp500 are correlated
corrplot(M[c(80:90),c(80:90)], method = "number")
corrplot(M[c(80:90),c(80:90)], method = "circle")

################ DATA PREPROCESSING #######################
# Removing Na Values
sp500.all<-na.omit(sp500.all)
# extracting only the adjusted closing values before transposing the matrix
sp500.adclose<- t(Ad(sp500.all)) # extract the adjusted closing prices, and transpose it
sp500.adclose.lastMnth<- sp500.adclose[,c(209:230)] # 22 days between june 19, 2017 - july 19, 2017
sp500.adclose<- sp500.adclose[,c(1:208)] # modfying the dimensions used for the model
dates<- colnames(sp500.adclose) # saving the names of the dates, incase it was erased

# this function find the difference in price between june 19, 2017 - july 19, 2017
Direction.lastMnth <- function(xdat){
        if(xdat[22]-xdat[1]>=0){
                1 # classfiy the direction as (+)
        }
        else{
                0 # # classfiy the direction as (-)
        }
}

# Y: The classfication column
sp500.Direction<-apply(sp500.adclose.lastMnth, 1, Direction.lastMnth)
# X1: Price Rate of Change Technical Indicator
sp500.ROC<- t(ROC(Ad(sp500.all), na.pad = FALSE))[,c(1:208)]
# this is the final look of the dataset
sp500.combined.ROC<- cbind(sp500.ROC,as.factor(sp500.Direction))

legal.names<- rep(NA,ncol(sp500.ROC))
for(i in 1:ncol(sp500.ROC)){
        legal.names[i]<- paste("Day",i, sep = "")
}

colnames(sp500.ROC)<- legal.names
sp500.legal<-cbind(sp500.ROC,sp500.Direction)

#sp500.legal is the final look for the dataset 
sp500.legal<- as.data.frame(sp500.legal)
sp500.legal$sp500.Direction<- as.factor(sp500.legal$sp500.Direction)
################ MODELING #######################
# Splitting the data
n <- nrow(sp500.legal)  #Number of rows of the data
t <- 0.7*n            #Set the size of the training set
set.seed(1)        #Setting a fix seed to make the results reproducible
trainIndex <- sample(1:n,t)

# Training: Price Rate Of Change
training<- as.data.frame(sp500.legal[trainIndex,])
# Validation: Price Rate Of Change
validation<- as.data.frame(sp500.legal[-trainIndex,])

##################### MODEL 1: KNN ###############################################
# 10 different models depending on the value of K
nearest1 <- knn(train=training[,c(1:208)],test=validation[,c(1:208)],cl=training[,c(209)],k=1)
nearest2 <- knn(train=training[,c(1:208)],test=validation[,c(1:208)],cl=training[,c(209)],k=2)
nearest3 <- knn(train=training[,c(1:208)],test=validation[,c(1:208)],cl=training[,c(209)],k=3)
nearest4 <- knn(train=training[,c(1:208)],test=validation[,c(1:208)],cl=training[,c(209)],k=4)
nearest5 <- knn(train=training[,c(1:208)],test=validation[,c(1:208)],cl=training[,c(209)],k=5)
nearest6 <- knn(train=training[,c(1:208)],test=validation[,c(1:208)],cl=training[,c(209)],k=6)
nearest7 <- knn(train=training[,c(1:208)],test=validation[,c(1:208)],cl=training[,c(209)],k=7)
nearest8 <- knn(train=training[,c(1:208)],test=validation[,c(1:208)],cl=training[,c(209)],k=8)
nearest9 <- knn(train=training[,c(1:208)],test=validation[,c(1:208)],cl=training[,c(209)],k=9)
nearest10 <- knn(train=training[,c(1:208)],test=validation[,c(1:208)],cl=training[,c(209)],k=10)

##################### MODEL 1 Evaluation: KNN ###############################################


#Calculate the proportion of correct classification on this training set
# Accuracy Values
pcorn1 <- 100*sum(validation[,c(209)]==nearest1)/(n-t)
pcorn2 <- 100*sum(validation[,c(209)]==nearest2)/(n-t)
pcorn3 <- 100*sum(validation[,c(209)]==nearest3)/(n-t)
pcorn4 <- 100*sum(validation[,c(209)]==nearest4)/(n-t)
pcorn5 <- 100*sum(validation[,c(209)]==nearest5)/(n-t)
pcorn6 <- 100*sum(validation[,c(209)]==nearest6)/(n-t)
pcorn7 <- 100*sum(validation[,c(209)]==nearest7)/(n-t)
pcorn8 <- 100*sum(validation[,c(209)]==nearest8)/(n-t)
pcorn9 <- 100*sum(validation[,c(209)]==nearest9)/(n-t)
pcorn10 <- 100*sum(validation[,c(209)]==nearest10)/(n-t)

# Identifying which k has the highest accuracy

# the reason I used data frame for knn.models instead of c() is because
# the variables in it are factors, when concating factors using c()
# the values changes from factors: 0  1 to integers: 1  2
knn.models<- data.frame(nearest1, nearest2, nearest3, nearest4, nearest5,nearest6,nearest7,nearest8,nearest9,nearest10)
# its okay to use c(), since the values of pcorn are already integers
acc.rates.knn<-c(pcorn1,pcorn2,pcorn3,pcorn4,pcorn5,pcorn6,pcorn7,pcorn8,pcorn9,pcorn10)
knn.best<- which(max(acc.rates.knn)==acc.rates.knn) # returns the index
paste("The Highest Accuracy in KNN is K=",knn.best,"With",round(acc.rates.knn[knn.best]),"% Accuracy Rate")

# Confusion Matrix Of The Highest K
confMat.knn<-table(knn.models[,knn.best],validation$sp500.Direction )
confMat.knn
acc.knn <-(confMat.knn[1,1]+confMat.knn[2,2])/(confMat.knn[1,2]+confMat.knn[2,1]+confMat.knn[1,1]+confMat.knn[2,2])
acc.knn # knn model overall accuracy 
error.knn<- 1-acc.knn
error.knn# knn model misclassification error rate


# Model Evaluation Of The Highest K, Using Data Visulization 

knn_scores <- prediction(as.numeric(knn.models[,knn.best]), as.numeric(validation$sp500.Direction))

#ROC Curve
knn_perf <- performance(knn_scores, "tpr", "fpr")

plot(knn_perf,
     main="KNN ROC Curves",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")

# Area Under The Curve
knn_auc <- performance(knn_scores, "auc")
as.numeric(knn_auc@y.values)  ##AUC Value

# Lift Curve
knn_lift <- performance(knn_scores, measure="lift", x.measure="rpp")
plot(knn_lift,
     main="KNN Lift Chart",
     xlab="% Stocks (Percentile)",
     ylab="Lift",
     col="darkblue", lwd = 3)
abline(1,0,col="red",  lwd = 3)
grid(col="aquamarine")
##################### MODEL 2: Logistic Regression ###############################################
# All Predictors Included
sp500.logit<-glm(sp500.Direction ~.,data=training, family=binomial)
# it looks like the p values are very large (since all are = 1)
#summary(sp500.logit)
# probabilities of the direction being up or down
sp500.probs <- predict(sp500.logit,validation,type = "response")

# since sp500 values are not formatted correctly, 
# meaning it classfyes values like this: 1.000000e+00, 1.000000e+00,2.220446e-16 
#                                       instead of: 1,1,0
# we need to tell R to convert "1.000000e+00" to "1", and "2.220446e-16" to "0"
# NOTE: using any threshold between 0.01-0.99 instead of 0.5 wil result
# in the same value, since the probabilities are very high (1.000000e+00) or low (2.220446e-16)
valrow <-nrow(validation)
glm.num.pred<-rep("0",valrow)
glm.num.pred[sp500.probs >0.5]="1"

################### MODEL 2 Evaluation: Logistic Regression ###############################################


# Confusion Matrix of Logistic Regression

confMat.logit<-table(glm.num.pred,validation$sp500.Direction )
confMat.logit
acc.logit <-(confMat.logit[1,1]+confMat.logit[2,2])/(confMat.logit[1,2]+confMat.logit[2,1]+confMat.logit[1,1]+confMat.logit[2,2])
acc.logit # logistic regression overall accuracy 
error.logit<- 1-acc.logit
error.logit# logistic regression misclassification error rate

# Another way of doing it:
#matrix = confusion.matrix(validation$sp500.Direction,sp500.probs ,threshold=0.2)    
#AccuMeasures = accuracy(validation$sp500.Direction,sp500.probs,threshold=0.2)
#matrix
#AccuMeasures

# Model Evaluation Using Data Visulization
glm_num_scores <- prediction(as.numeric(glm.num.pred), as.numeric(validation$sp500.Direction))

#ROC Curve
logit_perf <- performance(glm_num_scores, "tpr", "fpr")

plot(logit_perf,
     main="Logistic Regression ROC Curves",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")

# Area Under The Curve
logit_auc <- performance(glm_num_scores, "auc")
as.numeric(logit_auc@y.values)  ##AUC Value


# Lift Curve
glm_num_lift <- performance(glm_num_scores, measure="lift", x.measure="rpp")
plot(glm_num_lift,
     main="Logistic Regression Lift Chart",
     xlab="% Stocks (Percentile)",
     ylab="Lift",
     col="darkblue", lwd = 3)
abline(1,0,col="red",  lwd = 3)
grid(col="aquamarine")
##################### MODEL 3: Random Forest ###############################################

sp500.rf = randomForest(sp500.Direction~ ., data=training, importance=TRUE)
pred.rf = predict(sp500.rf , validation, type="class")

################### MODEL 3 Evaluation: Random Forest ###############################################

# Confusion MAtrix Of Random Forest

confMat.rf<-table(pred.rf,validation$sp500.Direction )
confMat.rf
acc.rf <-(confMat.rf[1,1]+confMat.rf[2,2])/(confMat.rf[1,2]+confMat.rf[2,1]+confMat.rf[1,1]+confMat.rf[2,2])
acc.rf # random forest overall accuracy
error.rf<- 1-acc.rf
error.rf# random forest model misclassification error rate

# Model Evaluation Using Data Visulization 

rf_scores <- prediction(as.numeric(pred.rf), as.numeric(validation$sp500.Direction))

#ROC Curve
rf_perf <- performance(rf_scores, "tpr", "fpr")

plot(rf_perf,
     main="Random Forest ROC Curves",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")

# Area Under The Curve
logit_auc <- performance(rf_scores, "auc")
as.numeric(logit_auc@y.values)  ##AUC Value


#Lift Curve
rf_lift <- performance(rf_scores, measure="lift", x.measure="rpp")
plot(rf_lift,
     main="Random Forest Lift Chart",
     xlab="% Stocks (Percentile)",
     ylab="Lift",
     col="darkblue", lwd = 3)
abline(1,0,col="red",  lwd = 3)
grid(col="aquamarine")
