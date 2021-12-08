#Read data from unpacked files
clinicalpatient_data<- read.delim("data_clinical_patient.txt");
mutation_data <- read.delim("data_mutations.txt");

#Graph raw data to observe trends in clinical patient data

#histogram for diagnosis age
hist(as.numeric(clinicalpatient_data$Diagnosis.Age[5:length(clinicalpatient_data$Diagnosis.Age)]),main = "Histogram of Diagnosis Age", xlab = "Diagnosis Age",ylab = "Frequency");

#pie chart for sex distribution
sex_stat <- c(sum(clinicalpatient_data$Sex=='Male'),sum(clinicalpatient_data$Sex=='Female'));
sex_label<-c('Male','Female');
pie(sex_stat,labels =sex_label, main = "Sex",col = heat.colors(length(sex_stat)));

#pie chart for disease stages
disease_stage_stat<-c(sum(clinicalpatient_data$Neoplasm.Disease.Stage.American.Joint.Committee.on.Cancer.Code=='STAGE I'),sum(clinicalpatient_data$Neoplasm.Disease.Stage.American.Joint.Committee.on.Cancer.Code=='STAGE II'),sum(clinicalpatient_data$Neoplasm.Disease.Stage.American.Joint.Committee.on.Cancer.Code=='STAGE III'),sum(clinicalpatient_data$Neoplasm.Disease.Stage.American.Joint.Committee.on.Cancer.Code=='STAGE IV'));
disease_stage_label<- c('STAGE I','STAGE II','STAGE III','STAGE IV');
pie(disease_stage_stat,labels = disease_stage_label,main = 'Disease state',col=heat.colors(length(disease_stage_stat)));

#pie chart for ethnicity
ethnicity_stat<-c(sum(clinicalpatient_data$Ethnicity.Category=='Not Hispanic Or Latino'),sum(clinicalpatient_data$Ethnicity.Category=='Hispanic Or Latino'),sum(clinicalpatient_data$Ethnicity.Category==""));
ethnicity_label<-c('Not Hispanic or Latino','Hispanic or Latino','NA');
pie(ethnicity_stat,labels =ethnicity_label,main="Ethnicity",col = heat.colors(length(ethnicity_stat)));

#Pie chart for therapy administered prior to resection
therapy_stat<-c(sum(clinicalpatient_data$Neoadjuvant.Therapy.Type.Administered.Prior.To.Resection.Text=='No'),sum(clinicalpatient_data$Neoadjuvant.Therapy.Type.Administered.Prior.To.Resection.Text=='Yes'));
therapy_label<-c('No','Yes');
pie(therapy_stat,labels = therapy_label,main="Therapy administered prior to resection",col = heat.colors(length(therapy_stat)));

#pie chart for new tumor after therapy
new_tumor_stat<-c(sum(clinicalpatient_data$New.Neoplasm.Event.Post.Initial.Therapy.Indicator=='Yes'),sum(clinicalpatient_data$New.Neoplasm.Event.Post.Initial.Therapy.Indicator=='No'));
new_tumor_label<-c('Yes','No');
pie(new_tumor_stat,label=new_tumor_label,main="New tumor after treatment",col=heat.colors(length(new_tumor_stat)));

#pie chart for cancer metastasis stage
metastasis_stage_stat <- c(sum(clinicalpatient_data$American.Joint.Committee.on.Cancer.Metastasis.Stage.Code=='M0'),sum(clinicalpatient_data$American.Joint.Committee.on.Cancer.Metastasis.Stage.Code=='MX'),sum(clinicalpatient_data$American.Joint.Committee.on.Cancer.Metastasis.Stage.Code=='M1'),sum(clinicalpatient_data$American.Joint.Committee.on.Cancer.Metastasis.Stage.Code==""));
metastasis_stage_label<- c('M0','MX','M1','NA');
pie(metastasis_stage_stat,labels = metastasis_stage_label, main = "Cancer Metastasis Stage", col = heat.colors(length(metastasis_stage_stat)));

#pie chart for lymph node stage
N_stage_stat<-c(sum(clinicalpatient_data$Neoplasm.Disease.Lymph.Node.Stage.American.Joint.Committee.on.Cancer.Code=='N1'),sum(clinicalpatient_data$Neoplasm.Disease.Lymph.Node.Stage.American.Joint.Committee.on.Cancer.Code=='N2'),sum(clinicalpatient_data$Neoplasm.Disease.Lymph.Node.Stage.American.Joint.Committee.on.Cancer.Code=='N0'),sum(clinicalpatient_data$Neoplasm.Disease.Lymph.Node.Stage.American.Joint.Committee.on.Cancer.Code=='NX'),sum(clinicalpatient_data$Neoplasm.Disease.Lymph.Node.Stage.American.Joint.Committee.on.Cancer.Code=='N3'),sum(clinicalpatient_data$Neoplasm.Disease.Lymph.Node.Stage.American.Joint.Committee.on.Cancer.Code==""));
N_stage_label<-c('N1','N2','N0','NX','N3','NA');
pie(N_stage_stat,labels = N_stage_label, main = "Lymph Node Stage", col = heat.colors(length(N_stage_stat)));

#pie chart for tumor stage
tumor_stage_stat<-c(sum(clinicalpatient_data$American.Joint.Committee.on.Cancer.Tumor.Stage.Code=='T0'),sum(clinicalpatient_data$American.Joint.Committee.on.Cancer.Tumor.Stage.Code=='T1'),sum(clinicalpatient_data$American.Joint.Committee.on.Cancer.Tumor.Stage.Code=='T2'),sum(clinicalpatient_data$American.Joint.Committee.on.Cancer.Tumor.Stage.Code=='T2A'),sum(clinicalpatient_data$American.Joint.Committee.on.Cancer.Tumor.Stage.Code=='T2B'),sum(clinicalpatient_data$American.Joint.Committee.on.Cancer.Tumor.Stage.Code=='T3'),sum(clinicalpatient_data$American.Joint.Committee.on.Cancer.Tumor.Stage.Code=='T3A'),sum(clinicalpatient_data$American.Joint.Committee.on.Cancer.Tumor.Stage.Code=='T3B'),sum(clinicalpatient_data$American.Joint.Committee.on.Cancer.Tumor.Stage.Code=='T4'),sum(clinicalpatient_data$American.Joint.Committee.on.Cancer.Tumor.Stage.Code=='T4A'),sum(clinicalpatient_data$American.Joint.Committee.on.Cancer.Tumor.Stage.Code=='T4B'),sum(clinicalpatient_data$American.Joint.Committee.on.Cancer.Tumor.Stage.Code=='TX'),sum(clinicalpatient_data$American.Joint.Committee.on.Cancer.Tumor.Stage.Code==""));
tumor_stage_label<-c('T0','T1','T2','T2A','T2B','T3','T3A','T3B','T4','T4A','T4B','TX','NA');
pie(tumor_stage_stat,labels = tumor_stage_label,main="Tumor Stage",col = heat.colors(length(tumor_stage_stat)));

#pie chart for primary lymph node presentation assessment 
node_presentation_assessment_stat<-c(sum(clinicalpatient_data$Primary.Lymph.Node.Presentation.Assessment=='No'),sum(clinicalpatient_data$Primary.Lymph.Node.Presentation.Assessment=='Yes',sum(clinicalpatient_data$Primary.Lymph.Node.Presentation.Assessment=="")));
node_presentation_assessment_label<-c('No','Yes','NA');
pie(node_presentation_assessment_stat,labels = node_presentation_assessment_label, main = "Primary Lymph Node Presentation Assessment",col = heat.colors(length(node_presentation_assessment_stat)));

#pie chart for prior diagnosis
diagnosis_stat<-c(sum(clinicalpatient_data$Prior.Diagnosis=='Yes'),sum(clinicalpatient_data$Prior.Diagnosis=='No'));
diagnosis_label<-c('Yes','No');
pie(diagnosis_stat,labels = diagnosis_label, main = "Prior Diagnosis",col = heat.colors(length(diagnosis_stat)));


#pie chart for cancer status
cancer_stat<- c(sum(clinicalpatient_data$Person.Neoplasm.Cancer.Status=='With Tumor'),sum(clinicalpatient_data$Person.Neoplasm.Cancer.Status=='Tumor Free'));
cancer_label<-c("With Tumor","Tumor Free");
pie(cancer_stat,label=cancer_label,main="Cancer Status",col=heat.colors(length(cancer_stat)));

#pie chart for race
race_stat<-c(sum(clinicalpatient_data$Race.Category=='White'),sum(clinicalpatient_data$Race.Category=='Black or African American'),sum(clinicalpatient_data$Race.Category=='Asian'),
             sum(clinicalpatient_data$Race.Category==""));
race_label<-c('White','Black or African American','Asian','NA');
pie(race_stat,label=race_label,main="Race",col= heat.colors(length(race_stat)));

#histogram for patient weight
hist(as.numeric(clinicalpatient_data$Patient.Weight[5:length(clinicalpatient_data$Patient.Weight)]),main = "Histogram of Patient Weight", xlab = "Weight",ylab = "Frequency");

#pie chat for overall survival status
survival_stat<-c(sum(clinicalpatient_data$Overall.Survival.Status=='1:DECEASED'),sum(clinicalpatient_data$Overall.Survival.Status=='0:LIVING'));
survival_label<-c('Deceased','Living');
pie(survival_stat,labels = survival_label,main = "Survival Status", col= heat.colors(length(survival_stat)));

#histogram for overall survival months since diagnosis
hist(as.numeric(clinicalpatient_data$Overall.Survival..Months.[5:length(clinicalpatient_data$Overall.Survival..Months.)]),main = "Histogram of Survival Months after Initial Diagnosis", xlab = "Months",ylab = "Frequency");

#pie chart for disease specific survival status
disease_survival_stat<-c(sum(clinicalpatient_data$Disease.specific.Survival.status=='1:DEAD WITH TUMOR'),sum(clinicalpatient_data$Disease.specific.Survival.status=='0:ALIVE OR DEAD TUMOR FREE'));
disease_survival_label<-c('Dead with Tumor','Alive or Dead Tumor Free');
pie(disease_survival_stat,labels = disease_survival_label, main="Disease Specific Survival Status",col= heat.colors(length(disease_survival_stat)));

#histogram for disease specific survival months 
hist(as.numeric(clinicalpatient_data$Months.of.disease.specific.survival[5:length(clinicalpatient_data$Months.of.disease.specific.survival)]), main = "Histogram of Disease Specific Survival Months", xlab = "Months", ylab = "Frequency");

#pie chart for disease free status since initial treatment
disease_free_stat<-c(sum(clinicalpatient_data$Disease.Free.Status=='0:DiseaseFree'),sum(clinicalpatient_data$Disease.Free.Status=='1:Recurred/Progressed'),sum(clinicalpatient_data$Disease.Free.Status==""));
disease_free_label<-c('Disease Free','Recurred/Progressed','NA');
pie(disease_free_stat,labels = disease_free_label,main="Disease Free Status Since Initial Treatment",col = heat.colors(length(disease_free_stat)));

#histogram for disease free months since initial treatment
hist(as.numeric(clinicalpatient_data$Disease.Free..Months.[5:length(clinicalpatient_data$Disease.Free..Months.)]),main = "Histogram for Disease Free Months Since Initial Treatment",xlab = "Months",ylab="Frequency");

#pie chart for radiation therapy
radiation_stat<-c(sum(clinicalpatient_data$Radiation.Therapy=='No'),sum(clinicalpatient_data$Radiation.Therapy=='Yes'));
radiation_label<-c("No","Yes");
pie(radiation_stat,label=radiation_label,main="Radiation Therapy",col=heat.colors(length(radiation_stat)));

#pie chart for progression free status
progression_stat<-c(sum(clinicalpatient_data$Progression.Free.Status=='1:PROGRESSION'),sum(clinicalpatient_data$Progression.Free.Status=='0:CENSORED'));
progression_label<-c('Progression','Censored');
pie(progression_stat,labels = progression_label,main = "Progression Free Status",col=heat.colors(length(progression_stat)));

#histogram for progression free months
hist(as.numeric(clinicalpatient_data$Progress.Free.Survival..Months.[5:length(clinicalpatient_data$Progress.Free.Survival..Months.)]),main = "Histogram for Progress Free Survival Months",xlab="Months",ylab = "Frequency");


# Survival Analysis -------------------------------------------------------

#Load packages
BiocManager::install("survival")
BiocManager::install("survminer")
library("survival")
library("survminer")

number_of_rows<- length(clinicalpatient_data$X.Patient.Identifier);

Patient_identifier<- clinicalpatient_data$X.Patient.Identifier[5:number_of_rows];
Diagnosis_age<- as.numeric(clinicalpatient_data$Diagnosis.Age[5:number_of_rows]);
Survival_status<-clinicalpatient_data$Disease.specific.Survival.status[5:number_of_rows];
Disease_specific_survival<-as.numeric(clinicalpatient_data$Months.of.disease.specific.survival[5:number_of_rows]);
Tumor_stage<-clinicalpatient_data$American.Joint.Committee.on.Cancer.Tumor.Stage.Code[5:number_of_rows];
Months_to_last_follow_up<-as.numeric(clinicalpatient_data$Last.Communication.Contact.from.Initial.Pathologic.Diagnosis.Date[5:number_of_rows])/30;

clin_df <- data.frame(Patient_identifier,Diagnosis_age,Survival_status,Disease_specific_survival,Tumor_stage,Months_to_last_follow_up);


#Boolean variable that is TRUE for deceased patients with tumor
clin_df$deceased = clin_df$Survival_status == "1:DEAD WITH TUMOR";

#"Overall survival" variable that is equal to days to death for dead patients and days to last followup for other patients
clin_df$overall_survival = ifelse(clin_df$deceased, clin_df$Disease_specific_survival,clin_df$Months_to_last_follow_up);

Surv(clin_df$overall_survival,clin_df$deceased) ~ clin_df$Diagnosis_age;

#fit a survival model
fit_overall = survfit(Surv(overall_survival,deceased) ~ Diagnosis_age,data=clin_df);
print(fit_overall)
ggsurvplot(fit_overall, data=clin_df)

#fit a survival model for age range 30 to 40
clin_df_30 = subset(clin_df, clin_df$Diagnosis_age>=30 & clin_df$Diagnosis_age<40,);
fit_30 = survfit(Surv(overall_survival,deceased) ~ Diagnosis_age,data=clin_df_30);
print(fit_30)
ggsurvplot(fit_30,data=clin_df_30)

#fit a survival model for age range 40 to 50
clin_df_40 = subset(clin_df, clin_df$Diagnosis_age>=40 & clin_df$Diagnosis_age<50,);
fit_40 = survfit(Surv(overall_survival,deceased) ~ Diagnosis_age,data=clin_df_40);
print(fit_40)
ggsurvplot(fit_40,data=clin_df_40)

#fit a survival model for age range 50 to 60
clin_df_50 = subset(clin_df, clin_df$Diagnosis_age>=50 & clin_df$Diagnosis_age<60,);
fit_50 = survfit(Surv(overall_survival,deceased) ~ Diagnosis_age,data=clin_df_50);
print(fit_50)
ggsurvplot(fit_50,data=clin_df_50)


#fit a survival model for age range 60 to 70
clin_df_60 = subset(clin_df, clin_df$Diagnosis_age>=60 & clin_df$Diagnosis_age<70,);
fit_60 = survfit(Surv(overall_survival,deceased) ~ Diagnosis_age,data=clin_df_60);
print(fit_60)
ggsurvplot(fit_60,data=clin_df_60)


#fit a survival model for age range 70 to 80
clin_df_70 = subset(clin_df, clin_df$Diagnosis_age>=70 & clin_df$Diagnosis_age<80,);
fit_70 = survfit(Surv(overall_survival,deceased) ~ Diagnosis_age,data=clin_df_70);
print(fit_70)
ggsurvplot(fit_70,data=clin_df_70)

#fit a survival model for age range 80 to 91
clin_df_80 = subset(clin_df, clin_df$Diagnosis_age>=80 & clin_df$Diagnosis_age<=90,);
fit_80 = survfit(Surv(overall_survival,deceased) ~ Diagnosis_age,data=clin_df_80);
print(fit_80)
ggsurvplot(fit_80,data=clin_df_80)


# Attempted Linear Regression -------------------------------------------------------
install.packages("readxl")
library(readxl)

lm_DiagnosisAge = lm(Diagnosis_age~overall_survival,data = clin_df)
summary(lm_DiagnosisAge)

plot(clin_df$Diagnosis_age, clin_df$overall_survival, ylab='Overall Survival Days', xlab='Diagnosis Age', pch = 16, col = "blue")
abline(lm_DiagnosisAge)
  

