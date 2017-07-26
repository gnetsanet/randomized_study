setwd("/home/boston/BHD/")
rawdata = fread('/BHD/Clinical Data.csv', header = T, sep = ",")
#class(rawdata)
#head(rawdata)
#dim(rawdata)

baselineOnly = rawdata[rawdata$TimePoint==0]
setDF(baselineOnly)
total_no_obs = dim(baselineOnly)[1]


# state that Age and Sex appear to be balanced and that the study appears to be randomized so we may not need to worry about adjusting for confounders.
sum(baselineOnly$Gender=="Male")
sum(baselineOnly$Gender=="Female")
boxplot(baselineOnly$Age~baselineOnly$Group)
barplot(table(baselineOnly$Gender,baselineOnly$Group))


hist(baselineOnly[baselineOnly$Gender=="Male",baselineOnly$Age])
# hist(baselineOnly[baselineOnly$Gender=="Female",baselineOnly$Age])

outcomeContinuous = c("Age","Total-C","LDL-C","HDL-C","Triglycerides","Glucose","CRP")
# control_summ_stat<-vector()

# for(i in match(outcomeContinuous,colnames(baselineOnly))) {
#   med = round(median(subset.data.frame(baselineOnly,Group=="Control")[,i]),digits=2)
#   iqr = paste('(', round(quantile(subset.data.frame(baselineOnly,Group=="Control")[,i],0.75)[[1]],digits=2), "-", round(quantile(subset.data.frame(baselineOnly,Group=="Control")[,i],0.25)[[1]],digits=2), ')',sep="")
#   control_summ_stat<-c(control_summ_stat, paste(med, iqr))
# }


summary_tab=NULL
for(k in c("Control", "Treatment")) {
  summ_stat<-vector()
  for(i in match(outcomeContinuous,colnames(baselineOnly))) {
    med = round(median(subset.data.frame(baselineOnly,Group==k)[,i]),digits=2)
    iqr = paste('(', round(quantile(subset.data.frame(baselineOnly,Group==k)[,i],0.75)[[1]],digits=2), "-", round(quantile(subset.data.frame(baselineOnly,Group==k)[,i],0.25)[[1]],digits=2), ')',sep="")
    summ_stat<-c(summ_stat, paste(med, iqr))
  }
  summary_tab<-cbind(summary_tab,summ_stat)
}

pval_cont = vector()
for(i in match(outcomeContinuous,colnames(baselineOnly))) {
  pval_cont <- c(pval_cont, round(t.test(subset.data.frame(baselineOnly, Group=="Control")[,i], subset.data.frame(baselineOnly, Group=="Treatment")[,i])$p.value, digits=5))
}

summary_tab<-cbind(summary_tab,pval_cont)
# colnames(summary_tab)<-c("Characteristic", "Control", "Treatment")
# colnames(summary_tab)<-c("Control", "Treatment", "P-value")
# rownames(summary_tab)<-outcomeContinuous

###############
diabetic<-vector()
# diabetes <- c(diabetes,paste("Diabetic","\n", "Non-Diabetic"))
diabetic <- c(diabetic, paste(sum(baselineOnly$Diabetes==1 & baselineOnly$Group=="Control")," ", "(", (sum(baselineOnly$Diabetes==1 & baselineOnly$Group=="Control")/total_no_obs)*100,"%",")", sep=""))
diabetic <- c(diabetic, paste(sum(baselineOnly$Diabetes==1 & baselineOnly$Group=="Treatment")," ", "(", (sum(baselineOnly$Diabetes==1 & baselineOnly$Group=="Treatment")/total_no_obs)*100,"%",")", sep=""))
diabetic <- c(diabetic, "")
nondiabetic <- vector()
nondiabetic <- c(nondiabetic, paste(sum(baselineOnly$Diabetes==0 & baselineOnly$Group=="Control")," ", "(", (sum(baselineOnly$Diabetes==0 & baselineOnly$Group=="Control")/total_no_obs)*100,"%",")", sep=""))
nondiabetic <- c(nondiabetic, paste(sum(baselineOnly$Diabetes==0 & baselineOnly$Group=="Treatment")," ", "(", (sum(baselineOnly$Diabetes==0 & baselineOnly$Group=="Treatment")/total_no_obs)*100,"%",")", sep=""))
nondiabetic <- c(nondiabetic, "")

male<-vector()
# gender <- c(gender,paste("Male","\n", "Female"))
male <- c(male, paste(sum(baselineOnly$Gender=="Male" & baselineOnly$Group=="Control")," ", "(", (sum(baselineOnly$Gender=="Male" & baselineOnly$Group=="Control")/total_no_obs)*100,"%",")", sep=""))
male <- c(male, paste(sum(baselineOnly$Gender=="Male" & baselineOnly$Group=="Treatment")," ", "(", (sum(baselineOnly$Gender=="Male" & baselineOnly$Group=="Treatment")/total_no_obs)*100,"%",")", sep=""))
male <- c(male, "")

female <- vector()
female <- c(female, paste(sum(baselineOnly$Gender=="Female" & baselineOnly$Group=="Control")," ", "(", (sum(baselineOnly$Gender=="Female" & baselineOnly$Group=="Control")/total_no_obs)*100,"%",")", sep=""))
female <- c(female, paste(sum(baselineOnly$Gender=="Female" & baselineOnly$Group=="Treatment")," ", "(", (sum(baselineOnly$Gender=="Female" & baselineOnly$Group=="Treatment")/total_no_obs)*100,"%",")", sep=""))
female <- c(female,"")

nominal_vars <- rbind(rbind(diabetic,nondiabetic,rbind(male,female)))
all_combined<-rbind(summary_tab, nominal_vars)
rownames(all_combined)<-c(outcomeContinuous, "Diabetic", "Non-Diabetic", "Male", "Female")
colnames(all_combined)<-c("Control", "Treatment", "P-value")
###############
formattable(as.data.frame(all_combined))
# formattable(as.data.frame(summary_tab))

# table(baselineOnly$Group,baselineOnly$Gender)
# table(baselineOnly$Group,baselineOnly$Diabetes)

# Exploratory Plots
par( mfrow = c(2, 2))
boxplot(`LDL-C` ~ Gender, data = baselineOnly, ylab = "LDL-C")
boxplot(`HDL-C` ~ Gender, data = baselineOnly, ylab = "HDL-C")
boxplot(Triglycerides ~ Gender, data = baselineOnly, ylab = "Triglycerides")
boxplot(Glucose ~ Gender, data = baselineOnly, ylab = "Glucose")
title( "Boxplots", outer = TRUE )

par(mfrow=c(1,1)) # Reset

baselineOnly$Color = "black"
baselineOnly$Color[baselineOnly$Gender=="Female"]="green"
baselineOnly$Color[baselineOnly$Gender=="Male"]="purple"
plot(baselineOnly$`LDL-C`, baselineOnly$`Total-C`, xlab="LDL-C", ylab="Total-C", col=baselineOnly$Color)
fit <- lm(`Total-C`~ `LDL-C`, data=baselineOnly)
abline(fit, col="red")
legend("bottomright", pch=c(1,1), col=c("purple", "green"), c("Male", "Female"), bty="o", cex=.7)
mtext(paste("Coef:",round(coef(fit)[[2]],2)), 3, -1)

#################
allData <- setDF(rawdata)
treatmentData <- subset(allData, Group=="Treatment")
treat_diff_ldlc_followup_base <- aggregate(treatmentData$`LDL-C`,by=list(treatmentData$PatientId), FUN=function(x) { return(x[2] - x[1])})
colnames(treat_diff_ldlc_followup_base)<-c("Treatment","Change-LDL-C")
# print (show) these in the R-markdown
head(treat_diff_ldlc_followup_base[order(treat_diff_ldlc_followup_base$`Change-LDL-C`),],5)

treatment_baseline_followup_LDLC<-vector()
for (i in as.vector(head(treat_diff_ldlc_followup_base[order(treat_diff_ldlc_followup_base$`Change-LDL-C`),],5)[,1])) {
  treatment_baseline_followup_LDLC<-c(treatment_baseline_followup_LDLC, allData[allData$PatientId==i & allData$TimePoint==0,8])
  treatment_baseline_followup_LDLC<-c(treatment_baseline_followup_LDLC, allData[allData$PatientId==i & allData$TimePoint==1,8])
}

controlData <- subset(allData, Group=="Control")
control_diff_ldlc_followup_base <- aggregate(controlData$`LDL-C`,by=list(controlData$PatientId), FUN=function(x) { return(x[2] - x[1])})
colnames(control_diff_ldlc_followup_base)<-c("Treatment","Change-LDL-C")
# print (show) these in the R-markdown
head(control_diff_ldlc_followup_base[order(control_diff_ldlc_followup_base$`Change-LDL-C`),],5)

control_baseline_followup_LDLC<-vector()
for (i in as.vector(head(control_diff_ldlc_followup_base[order(control_diff_ldlc_followup_base$`Change-LDL-C`),],5)[,1])) {
  control_baseline_followup_LDLC<-c(control_baseline_followup_LDLC, allData[allData$PatientId==i & allData$TimePoint==0,8])
  control_baseline_followup_LDLC<-c(control_baseline_followup_LDLC, allData[allData$PatientId==i & allData$TimePoint==1,8])
}

# List of samples, each listed two times as factor. Example PID00842 PID00842 PID00679 PID00679
samples<-factor(c(rep(head(treat_diff_ldlc_followup_base[order(treat_diff_ldlc_followup_base$`Change-LDL-C`),],5)[,1],each=2),rep(head(control_diff_ldlc_followup_base[order(control_diff_ldlc_followup_base$`Change-LDL-C`),],5)[,1],each=2)))

# Build a data frame that I will be plotting shortly via ggplot2
topdiff.dat <- data.frame(Samples = samples, Group = factor( c(rep("Treatment", 10),rep("Control",10))), Timepoint = factor(rep(c("Baseline", "Followup"),10)), LDLC = c(treatment_baseline_followup_LDLC, control_baseline_followup_LDLC))
ggplot(data=topdiff.dat, aes(x=Timepoint, y=LDLC, group=samples, colour=Group)) + geom_line() + geom_point()

## Follow-up data

## End of discussion on the follow-up data

## Quantiles - Percentile table

percentile_tab<-NULL # Initialize would be matrix that will hold percentile data
for (i in 7:12) { 
  percentile_tab<-cbind(percentile_tab,as.matrix(quantile(allData[,i], c(0.05,0.10,0.25,0.5,0.75,0.9,0.95))))
}

colnames(percentile_tab)<-factor(colnames(allData)[7:12])
formattable(as.data.frame(percentile_tab))

## Linear Regression and Feature Selection

prediction_mod<-lm(`LDL-C` ~., data = allData)
## End of Regression and Feature Selection

# R Package
library("devtools")
library("roxygen2")

# Note to self, it is not like RStudo comes with 'devtools' and 'roxygen2' pre-installed, I had to do the following
#   sudo apt-get install  libssl-dev
#   sudo apt-get install libcurl
#   sudo apt-get install libcurl4-openssl-dev
#   sudo apt-get install libxml2-dev
