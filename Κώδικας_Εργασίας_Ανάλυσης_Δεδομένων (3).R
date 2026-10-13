#Εργασία Ανάλυσης 4

library(sjmisc)
library(sjPlot)
library(car)
library(psych)
library(tidyverse)
library(likert)
library(lme4)
library(dplyr)
library(nortest)
library(cocor)
library(olsrr)
library(flexmix)

data<-read.csv("04_sleep.csv",sep="-")
summary(data)
str(data)

#Ερώτημα 1

#μετατροπή μεταβλητών
data$age<-as.numeric(data$age)
data$sbp<-as.numeric((data$sbp))
data$gender<-factor(data$gender)
data$alcohol<-factor(data$alcohol)
data$caffeine<-factor(data$caffeine)

str(data)
describe(data)
hist(data$REM) 

str(data$REM)  
summary(data)

#κατηγορικες μεταβλητές               
gender<-as.factor(ifelse(data$gender==1,'Woman'
                         ,ifelse(data$gender==0,'Man',0)))
summary(gender)

alcohol<-as.factor(ifelse(data$alcohol==1,'Yes'
                          ,ifelse(data$alcohol==0,'No',0)))
summary(alcohol)

caffeine<-as.factor(ifelse(data$caffeine==1,'Yes'
                           ,ifelse(data$caffeine==0,'No',0)))
summary(caffeine)

necksize<-as.factor(ifelse(data$necksize>=37.50,'1'
                           ,ifelse(data$necksize<37.50,'0',0)))
summary(necksize)
describe(data)

data%>% #hist για καταναλωση αλκοολ ανα φυλλο (δεν βγαζει λεξεις αλλα τους αριθμους)
  group_by(gender) %>%
  plot_frq(alcohol) %>%
  plot_grid()

data%>% #hist για καταναλωση καφεινης ανα φυλλο (δεν βγαζει λεξεις αλλα τους αριθμους)
  group_by(gender) %>%
  plot_frq(caffeine) %>%
  plot_grid()

#σχετικές συχνότητες
table(alcohol)/length(alcohol)
table(caffeine)/length(caffeine)

data2<-data[c(1,3,4,5,8,9)] #κραταμε μονο τις ποιοτικες μεταβλητες
describe(data2)

#τεστ κανονικοτητας
shapiro.test(data$age) #ναι
shapiro.test(data$BMI) #οχι
shapiro.test(data$necksize) #οχι
shapiro.test(data$sbp) #οχι
shapiro.test(data$REM) #ναι
shapiro.test(data$AI) #οχι

lillie.test(data$age) #ναι
lillie.test(data$BMI) #οχι
lillie.test(data$necksize) #οχι
lillie.test(data$sbp) #οχι
lillie.test(data$REM) #ναι
lillie.test(data$AI) #οχι

#κοιταω ιστογραμματα των κανονικων μεταβλητων
par(mfrow=c(1,2))
hist(data$age,xlab = "Κατανομή δεδομένων age",main = "Ιστόγραμμα age")
hist(data$REM,xlab = "Κατανομή δεδομένων REM",main = "Ιστόγραμμα REM")
#τα ιστογραμματα ειναι καλα

#Ερώτημα 2

#ελεγχοι απο τι επηρεαζεται ο REM υπνος
#διαγραμματα

plot(data$REM~data$sbp,xlab='Systolic blood pressure(Sbp)',ylab = 'REM')
abline(lm(data$REM~data$sbp)) #Plot για να βρουμε συσχέτση μεταξύ REM και sbp

plot(data$REM~data$AI,xlab='Awakening Index(AI)',ylab='REM')
abline(lm(data$REM~data$AI)) #Plot για να βρουμε συσχέτση μεταξύ REM και AI

cor(data[,c('REM','sbp')],method = "spearman") #αρνητικη αυτοσυσχετιση
cor(data[,c('REM','AI')],method = "spearman") #αρνητικη αυτοσυσχετιση
#Οι αρνητκες αυτοσυσχετισεις επιβεβαιώνονται και απο τα διαγραμματα

Scatter_Matrix <- pairs.panels(data2, main = "Scatter Plot Matrix των ποσοτικών μεταβλητών",method="spearman")

pairs(data2) #plots για ολες τις ποσοτικες μεταβλητες ανα ζευγαρια
cor(data$age,data$REM) #κανουμε pearson correlation επειδη οι δυο μεταβλητες ειναι κανονικες
cor(data2,method = "spearman") #προτιμότερο το spearman cor για μή κανονικές ματαβλητές
cor.test(data$age,data$REM) #σημαντικη συσχέτιση
#εφόσον μόνο οι μεταβλητές age και REM είναι κανονκές τότε μόνο στο correlation
#αυτών μπορούμε να κάνουμε cor.test

#χωριζουμε το data frame σε δυο group βαση του αλκοολ 
#και κοιταμε την κανονικοτητα του REM στα δυο group
split(data$REM,alcohol)
alcoholsplit1<-split(data$REM,alcohol)[1] #group 1 (>50 αρα S-W κα K-S)
alcoholsplit2<-split(data$REM,alcohol)[2] #group 2 (<50 αρα και S-W)

shapiro.test((alcoholsplit1$No)) #S-W του REM για οσους δεν πινουν αλκοολ #p-value>0.05 αρα κανονικο
lillie.test(alcoholsplit1$No) #κανουμε και lillie (kologorov-smirnov) test αφου το δειγμα >50 #p-value>0.05 αρα κανονικο
shapiro.test((alcoholsplit2$Yes)) #S-W του REM για οσους πινουν αλκοολ #p-value>0.05 αρα κανονικο

#και τα δυο δεγματα ειναι κανονικα

#ελεγχουμε διακυμανσεις στα δυο group
var.test(alcoholsplit1$No,alcoholsplit2$Yes) #οι διακυμανσεις βγαινουν ισες

#ελεγχος για μηδενικη διαφορα μεσων με ισες διακυμανσεις
t.test(alcoholsplit1$No,alcoholsplit2$Yes,var.equal = T)
#το p-value μεγαλυτερο απο το α=5% αρα δεν απορ Ηο αρα ισες μεσες τιμες
#αρα δεν υπαρχει στατιστικα σημαντικη διαφορα 
#στον μεσο του REM στα δυο groups του alcohol (α=5%)

#χωριζουμε το data frame σε δυο group βαση του caffeine 
#και κοιταμε την κανονικοτητα του REM στα δυο group
split(data$REM,caffeine)
csplit1<-split(data$REM,caffeine)[1] #group 1 <50
csplit2<-split(data$REM,caffeine)[2] #group 2>50

shapiro.test(csplit1$No) #κανοκικο
shapiro.test(csplit2$Yes) #κανονικο
lillie.test(csplit2$Yes) #κανονικο
#και τα δυο δειγματα εινα κανονικα

#ελεγχουμε διακυμανσεις και στα δυο groups
var.test(csplit1$No,csplit2$Yes) #ισες διακυμανσεις

#ελεγχος για μηδενικη δαφορα μεσων με ισες διακυμανσεις
t.test(csplit1$No,csplit2$Yes,var.equal = T) #ισες μεσες τιμες
#αρα δεν υπαρχει στατιστικα σημαντικη διαφορα στον μεσο του REM 
#στα δυο groups του caffeine (α=5%)

#Ερώτημα 3

# χωρίζουμε το δείγμα στα δύο βάση του necksize
necksplit <- split(data, data$necksize >= 37.5)
necksizesplit1 <- necksplit[["FALSE"]]  # necksize < 37.5
necksizesplit2 <- necksplit[["TRUE"]]   # necksize >= 37.5

# μετατροπή σε data frames
neck1DF <- as.data.frame(necksizesplit1)
neck2DF <- as.data.frame(necksizesplit2)

# χωρίζω κάθε group του necksize βάση του alcohol και caffeine
neck1REM <- as.vector(neck1DF$REM)
neck1Alcohol <- as.vector(neck1DF$alcohol) #παίρνουμε ως διάνυσμα τα δεδομενα για alcohol για μικρο necksize
neck1Caffeine <- as.vector(neck1DF$caffeine) #παίρνουμε ως διάνυσμα τα δεδομενα για caffeine για μικρο necksize
 
neck2REM <- as.vector(neck2DF$REM)
neck2Alcohol <- as.vector(neck2DF$alcohol) #παίρνουμε ως διάνυσμα τα δεδομενα για alcohol για μεγλαό necksize
neck2Caffeine <- as.vector(neck2DF$caffeine) #παίρνουμε ως διάνυσμα τα δεδομενα για caffeine για μεγάλο necksize

#Χωριζουμε καθε necksize group βαση της καταναλωσης αλκοολ
neck1alcoholsplit <- split(neck1REM, neck1Alcohol)
neck2alcoholsplit <- split(neck2REM, neck2Alcohol)

# Extracting τα ξεχωριστα group
neck1alcoholsplit1 <- neck1alcoholsplit[["0"]]  # Δεν πινουν αλκοολ με necksize <37.5
neck1alcoholsplit2 <- neck1alcoholsplit[["1"]] # πινουν αλκοολ με necksize <37.5

neck2alcoholsplit1 <- neck2alcoholsplit[["0"]]  # Δεν πινουν αλκοολ με necksize >37.5
neck2alcoholsplit2 <- neck2alcoholsplit[["1"]] # πινουν αλκοολ με necksize >37.5

#Χωριζουμε καθε necksize group βαση της καταναλωσης καφεινης
neck1coffeesplit <- split(neck1REM, neck1Caffeine)
neck2coffeesplit <- split(neck2REM, neck2Caffeine)

# Extracting τα ξεχωριστα group
neck1coffeesplit1 <- neck1coffeesplit[["0"]]  # δεν πινουν καφε με necksize <37.5
neck1coffeesplit2 <- neck1coffeesplit[["1"]] # πινουν καφε με necksize <37.5

neck2coffeesplit1 <- neck2coffeesplit[["0"]]  # δεν πινουν καφε με necksize >37.5
neck2coffeesplit2 <- neck2coffeesplit[["1"]] # πινουν καφε με necksize >37.5

# κανονικότητα
shapiro.test(neck1alcoholsplit1) # κανονικό
shapiro.test(neck1alcoholsplit2) # κανονικό

# διασπορές
var.test(neck1alcoholsplit1, neck1alcoholsplit2) # οι διασπορές βγαίνουν ίσες

# έλεγχος ισότητας μέσων τιμών
t.test(neck1alcoholsplit1, neck1alcoholsplit2, var.equal = T)
# οι μέσες τιμές είναι ίσες μεταξύ των δύο groups άρα δεν υπάρχει
# στατιστικά σημαντική διαφορά στον REM ύπνο στα δύο group με necksize <37.5

# necksize >37.5
# κανονικότητα
shapiro.test(neck2alcoholsplit1) # κανονικό
shapiro.test(neck2alcoholsplit2) # κανονικό

# διασπορές
var.test(neck2alcoholsplit1, neck2alcoholsplit2) # ίσες διασπορές

# έλεγχος ισότητας μέσων τιμών
t.test(neck2alcoholsplit1, neck2alcoholsplit2, var.equal = T)
# ίσες μέσες τιμές άρα όχι στατιστικά σημαντική διαφορά στον REM ύπνο
# στα δύο group με necksize >37.5

#ελεγχος για τα γκρουπ βάση καφείνης
# κανονικότητα
shapiro.test(neck1coffeesplit1) # κανονικό
shapiro.test(neck1coffeesplit2) # κανονικό

# διασπορές
var.test(neck1coffeesplit1, neck1coffeesplit2) # ίσες διασπορές

# έλεγχος ισότητας μέσων τιμών
t.test(neck1coffeesplit1, neck1coffeesplit2, var.equal = T)
# οι μέσες τιμές βγαίνουν ίσες άρα δεν υπάρχει στατιστικά σημαντική διαφορά
# στον REM ύπνο των δύο group με necksize <37.5

# για necksize >37.5
shapiro.test(neck2coffeesplit1) # κανονικό
shapiro.test(neck2coffeesplit2) # κανονικό

# διασπορές
var.test(neck2coffeesplit1, neck2coffeesplit2) # ίσες διασπορές

# έλεγχος ισότητας μέσων τιμών
t.test(neck2coffeesplit1, neck2coffeesplit2, var.equal = T)
# ίσες μέσες τιμές άρα όχι στατιστικά σημαντική διαφορά στον REM ύπνο
# στα δύο group με necksize >37.5

neck1sbp<-as.vector(neck1DF$sbp) #sbp για group με  necksize <37.5
neck2sbp<-as.vector(neck2DF$sbp) #sbp για group με necksize >37.5

neck1AI<-as.vector(neck1DF$AI) #AI για group με necksize <37.5
neck2AI<-as.vector(neck2DF$AI) #AI για group με necksize >37.5

cor(neck1REM,neck1sbp,method = "spearman") #οχι σημαντικη συχετιση
cor(neck2REM,neck2sbp,method = "spearman") #οχι σημαντικη συσχετιση

cor(neck1REM,neck1AI,method = "spearman") #όχι σημαντικη συσχετιση ωστόσο είναι κοντά
cor(neck2REM,neck2AI,method = "spearman") #σημαντικη συσχετιση (<-0.3)
#αρα ο αφυπνισεις ανα ωρα υπνου εχουν σημαντκη αυτοσυσχετιση με τον REM υπνο μόνο για μεγάλα necksizes


#γραμμικο μοντελο
#το μοντελο θα το κρινουμε βαση του BIC κα το R-squared

mfull<-lm(REM~.,data=data)
summary(mfull)
#για το πλήρες μοντέλο
step1<-step(mfull,direction = "both", k=log(106)) #step wise με BIC
step2<-step(mfull,direction = "both") #step wise με AIC
vif(mfull) #οχι πολυσυγγραμμικοτητα
summary(mfull)

#κανονικότητα
res2<-rstandard(mfull)
hist(res2)
lillie.test(res2) #p-value >0.05
qqnorm(res2,main="Q-Q plot πλήρες μοντέλου")
qqline(res2) #μέσος φαίνεται να είνα στο 0
#τηρείται η κανονικοτητα

#ομοσκεδαστικοτητα
fit2<-fitted(mfull)
par(mfrow=c(2,2)) #Plot καταλοίπων με προσαρμοσμένες τιμές
plot(fit2,res2)
abline(h=c(-2,2),col=2,lty=2)

plot(fit2,sqrt(abs(res2))) #Plot ρίζας καταλοίπων με προσαρμοσμένες τιμές
abline(h=sqrt(2),col=2,lty=2)

plot(fit2,res2^2) #Plot τετραγωνισμένων καταλοίπων με προσαρμοσμένες τιμές
abline(h=4,col=2,lty=2)

qfit2<-cut(fit2,breaks=quantile(fit2),include.lowest=T) #χωρίζουμε ανα τεταρτημόρια
library(car)
leveneTest(res2~qfit2) #p-value >0.05
boxplot(res2~qfit2)
ncvTest(mfull) #p-value >0.05
#υπαρχει ομοσκεδαστικότητα

#ανεξαρτησια
dwt(mfull) #δεν υπαρχουν στατιστικα σημαντικες αυτοσυσχετισεις αρα υπαρχει ανεξαρτησια
plot(res2,type="l") #Plot καταλοίπων
pacf(mfull$res) #AR plot για αναξερτησια καταλοίπων
#ανεξάρτητα

#γραμμικοτητα
par(mfrow=c(2,2))
plot(mfull)
#τηρειται η γραμμικότητα

#επιλογή μοντέλου
m_step1<-step(mfull,direction="both") #καταλήγει σε μοντέλο με μεταβλητές τις: age, necksize, alcohol, AI
summary(m_step1)
olspossible<-ols_step_all_possible(mfull,data=data) #απλή δοκιμή να δώ τι κάνει
#το μοντελο με το καλυτερο BIC (μικροτερο=καλυτερο) φαινεται να ειναι αυτο με μεταβλητες
#μόνο το AI. Όμως το R^2 είναι πολύ μικρο. Βάση AIC το καλύτερο μοντέλο ε΄ναι αυτό με τις μεταβλητές 
#age, alcohol, AI, necksize. Βάζοντας τις 4 αυτές μεταβλητές σε ένα DF κάνω ols_step_all_possible και 
#διερευνώ τα μοντέλα με το καλύτερο BIC προκειμένου να βρω την καλύτερη ισορροπία μεταξύ BIC και R^2

#τα μοντελα τα οποία επιλεχθηκαν
REM_model<-lm(REM~age+necksize+AI+alcohol,data=data)
data3<-data[c(1,4,6,9)] 
olspossible2<-ols_step_all_possible(REM_model,data=data3)
REM_model2<-lm(REM~AI,data=data) 
summary(REM_model2)  
REM_model3<-lm(REM~age+necksize+AI,data = data)
summary(REM_model3)
REM_model4<-lm(REM~necksize+AI+alcohol,data = data) 
summary(REM_model4)
REM_model5<-lm(REM~age+AI+alcohol,data=data) 
summary(REM_model5)
REM_model6<-lm(REM~age+AI,data=data)
summary(REM_model6)
BIC(REM_model)
BIC(REM_model2)
BIC(REM_model3)
BIC(REM_model4)
BIC(REM_model5)
BIC(REM_model6)
summary(REM_model)$r.squared
summary(REM_model2)$r.squared
summary(REM_model3)$r.squared
summary(REM_model4)$r.squared
summary(REM_model5)$r.squared
summary(REM_model6)$r.squared
#Tο R^2 του 2 είναι πολυ χαμηλο. Όμως έχει το καλύτερο BIC. το μοντέλο 1 έχει το καλύτερο R^2 παρόλο που ο 3 απο τις 4 μεταβλητές δεν είναι στατστικα σημαντκές
#επίσης το 1 έχει το χειρότερο BIC. γενικά σε κανένα μοντέλο, παρα μόνο στο μοντέλο μόνο με AI, δέν έχουμε όλες τις μεταβλητές στατστκά σημαντικές.
#Το μοντέλο το οποίο θα αναλύσουμε είναι με τις μεταβλητές: age, necksize, AI και alcohol

REM_model<-lm(REM~age+necksize+AI+alcohol,data = data) 
summary(REM_model)
anova(REM_model)
vif(REM_model) #δεν υπάρχει πολυσυγραμμικότητα

#κανονικοτητα
res<-rstandard(REM_model)
fit<-fitted(REM_model)
lillie.test(res) #p-value>0.05
qqnorm(res,main="Q-Q plot επιλεγμένου μοντέλου")
qqline(res)
resDF<-data.frame(res)
ggplot(resDF,aes(sample=data$REM))+stat_qq()+stat_qq_line()
hist(res) #μέσος φανεται να είναι στο 0
#τηρείται η κανονικοτητα

#ομοσκεδαστκοτητα
par(mfrow=c(2,2))
plot(fit,res) #Plot καταλοίπων με προσαρμοσμένες τιμές
abline(h=c(-2,2),col=2,lty=2)

plot(fit,sqrt(abs(res))) #Plot ρίζας καταλοίπων με προσαρμοσμένες τιμές
abline(h=sqrt(2),col=2,lty=2)

plot(fit,res^2) #Plot τετραγωνισμένων καταλοίπων με προσαρμοσμένες τιμές 
abline(h=4,col=2,lty=2)

qfit<-cut(fit,breaks=quantile(fit),include.lowest=T) #χωρίζουμε ανα τεταρτημόρια
library(car)
leveneTest(res~qfit)#p-value >0.05
boxplot(res~qfit)
ncvTest(REM_model) #p-value >0.05
#υπαρχει ομοσκεδαστικοτητα

#ανεξαρτησια
dwt(REM_model) #p-value >0.05
par(mfrow=c(2,1))
plot(res,type="l",ylab="Residuals") #Plot καταλοίπων
pacf(REM_model$res,main="Residual Correlation Plot") #AR plot για αναξερτησια καταλοίπων
#δεν υπαρχουν στατστκα σημαντικες αυτοσυσχετισεις αρα υπαρχει ανεξαρτησια

#γραμμικοτητα
par(mfrow=c(2,2))
plot(REM_model)
#ενδέχεται να υπαρχει προβλημα γραμμικοτητας στο μοντέλο

ols_plot_cooksd_bar(REM_model, print_plot = TRUE)
#απο το cooks dstance plot βλέπουμε ότι υπάρχουν outliers.

plot(data$REM,data$AI, xlab="REM",ylab="AI")
plot(data$REM,data$age)
plot(data$REM,data$necksize)
#αναλύοντας κάθε ανεξάρτητη μεταβλητή με την εξαρτημένη παρατηρούμε ότι υπάρχει
#μια τιμη που απέχει πολυ απο τις υπόλοιπες στο plot με REM και AI

   data3<-data[-c(70,29,14,79),]
#αφαιρούμε τεσσερις πιο ακραιες μεταβλητες απο τα δεδομένα και ξανατρέχουμε το μοντέλο.

   REM_model1.2<-lm(REM~age+necksize+AI+alcohol,data = data3) 
   summary(REM_model1.2) #καλυτερο adjR^2 ενώ η μεταβλητη necksize θεωρείται πλεον στατιστικα σημαντικη
   anova(REM_model1.2)
   vif(REM_model1.2) #δεν υπάρχει πολυσυγραμμικότητα
   BIC(REM_model1.2) #μικρη βελτώση στο BIC
   
   #κανονικοτητα
   res1.2<-rstandard(REM_model1.2)
   fit1.2<-fitted(REM_model1.2)
   lillie.test(res1.2) #p-value >0.05
   qqnorm(res1.2,main="Q-Q plot επιλεγμένου μοντέλου χωρίς ακραίες παρατηρήσεις")
   qqline(res1.2)
   resDF1.2<-data.frame(res1.2)
   ggplot(resDF,aes(sample=data3$REM))+stat_qq()+stat_qq_line()
   hist(res1.2) #μέσος φανεται να είναι στο 0
   #τηρείται η κανονικοτητα
   
   #ομοσκεδαστκοτητα
   par(mfrow=c(2,2))
   plot(fit1.2,res1.2) #Plot καταλοίπων με προσαρμοσμένες τιμές
   abline(h=c(-2,2),col=2,lty=2)
   
   plot(fit1.2,sqrt(abs(res1.2))) #Plot ρίζας καταλοίπων με προσαρμοσμένες τιμές
   abline(h=sqrt(2),col=2,lty=2)
   
   plot(fit1.2,res1.2^2) #Plot τετραγωνισμένων καταλοίπων με προσαρμοσμένες τιμές
   abline(h=4,col=2,lty=2)
   
   qfit1.2<-cut(fit1.2,breaks=quantile(fit1.2),include.lowest=T) #χωρίζουμε ανα τεταρτημόρια
   library(car)
   leveneTest(res1.2~qfit1.2) #p-value >0.05
   boxplot(res1.2~qfit1.2)
   ncvTest(REM_model1.2) #p-value >0.05
   #υπαρχει ομοσκεδαστικοτητα
   
   #ανεξαρτησια
   dwt(REM_model1.2) #p-value>0.05
   par(mfrow=c(2,1))
   plot(res1.2,type="l") #Plot καταλοίπων
   pacf(REM_model1.2$res,main="Residual Correlation Plot") #AR plot για αναξερτησια καταλοίπων
   #δεν υπαρχουν στατιστικα σημαντικες αυτοσυσχετισεις αρα υπαρχει ανεξαρτησια
   
   #γραμμικοτητα
   par(mfrow=c(2,2))
   plot(REM_model1.2)
   #η γραμμικότητα φαίνετα καλύτερη τωρα
   
   ols_plot_cooksd_bar(REM_model1.2, print_plot = TRUE)
   #παρατηρουμε πωςς υπάρχουν ακόμα outliers. Παρολα αυτα δεδομενου οτι το δειγμα ειναι μικρο, το να 
   #αφαιρεσουμε 11 μεταβλητες απο αυτο μειωνει το δειγμα κατα πολυ. Ακομη, τα ιατρικα δεδομενα 
   #εχουν μεγαλη μεταβλητοτητα η οποια ενδεχομένως να μπορουσε να περιγραφθει καλύτερα δεδομένου 
   #μεγαλυτερου δειγματος. Εφοσον ομως το δειγμα ειναι μικρο τοτε αφαιροντας τα τεσσερα μεγαλυτερα outliers
   #(τα οποια και αυτα ισως να μπορουσαν να περιγραφουν με περισσοτερα δεδομενα) βελτιωνουμε το μοντελο
   #αρκετα χωρις να πρεπει να αφαιρεσουμε παραπανω δεδομενα και να μικρυνουμε το δειγμα μας παιρετερω.