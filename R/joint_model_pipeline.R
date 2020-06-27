#' Title
#'
#' @param patient_table
#' @param joint_model
#'
#' @return
#' @export
#'
#' @examples
patient_prediction<-function(patient_table,joint_model){
  survPreds <- vector("list", nrow(patient_table))
  for (i in 1:nrow(patient_table)) {
    set.seed(123)
    #survPreds[[i]] <- survfitJM(jointFit_bayes_2, newdata = ND[1:i, ])
    #survPreds[[i]] <- survfitJM(jointFit_bayes_2_driver, newdata = ND[1:i, ])
    #survPreds[[i]] <- survfitJM(jointFit_cea, newdata = ND[1:i, ])
    survPreds[[i]] <- JMbayes::survfitJM(joint_model, newdata = patient_table[1:i, ])
  }

  time_prediction<-survPreds
  total_final_survival<-c()
  total_final_time<-c()
  for (i in 1:length(time_prediction)){
    tt<-time_prediction[[i]][[1]][[1]]
    final_survival<-as.numeric(tt[nrow(tt),"Mean"])
    final_time<-tt[nrow(tt),"times"]
    total_final_survival<-c(total_final_survival,final_survival)
    total_final_time<-c(total_final_time,final_time)
  }
  result<-data.frame(Patient_ID=patient_table$Patient_ID,Time_point=patient_table$Time_point,sample_gap=patient_table$sample_gap,survival= total_final_survival,times=total_final_time)
  return(result)
}

#' Title
#'
#' @param summary_table
#' @param input_joint_model
#' @param survival_table
#'
#' @return
#' @export
#'
#' @examples
survival_risk_AUC<-function(summary_table,input_joint_model,survival_table){
  patient_id<-unique(summary_table$Patient_ID)
  pred_result_table<-list()
  for(patient in patient_id){
    patient_t<-summary_table[summary_table$Patient_ID==patient,]
    if(nrow(patient_t)>=1){
      msg<-tryCatch({p_result<-patient_prediction(patient_table=patient_t,joint_model=input_joint_model)},error = function(e){""})
    }
    pred_result_table<-rlist::list.append(pred_result_table,p_result)
  }
  pred_result_table<-do.call(rbind,pred_result_table)

  pred_result_table<-unique(pred_result_table)
  pred_result_table_last_time<-lapply(split(pred_result_table,pred_result_table$Patient_ID),function(x) x[nrow(x),])
  pred_result_table_last_time<-do.call(rbind,pred_result_table_last_time)
  pred_result_table_last_time<-merge(pred_result_table_last_time,survival_table,by="Patient_ID")
  roc_result<-pROC::roc(pred_result_table_last_time$DFS_event, pred_result_table_last_time$survival,ci=TRUE,boot.n=100,ci.alpha=0.9)
  auc_value<-as.numeric(roc_result$auc)
  lower_ci<-as.character(roc_result$ci)[[1]]
  upper_ci<-as.character(roc_result$ci)[[3]]
  youden_index<-pROC::coords(roc_result, "best", ret="threshold", transpose = FALSE,
                       best.method="youden")
  index_youden<-which(roc_result$thresholds==youden_index)
  sense=roc_result$sensitivities[[index_youden]]
  spec=roc_result$specificities[[index_youden]]
  close_topleft<-pROC::coords(roc_result, "best", ret="threshold", transpose = FALSE,
                        best.method="closest.topleft")
  index_close<-which(roc_result$thresholds==close_topleft)
  sense_topleft<-roc_result$sensitivities[[index_close]]
  spec_topleft<-roc_result$specificities[[index_close]]
  result<-data.frame(auc=auc_value,lower_ci=lower_ci,upper_ci=upper_ci,youden=youden_index,sens_youden=sense,spec_youden=spec,topleft=close_topleft,sens_topleft=sense_topleft,spec_topleft=spec_topleft)
  final_result[["auc_result"]]<-result
  final_result[["roc_object"]]<-roc_result
  final_result[["roc_table"]]<-pred_result_table_last_time
  return(final_result)
}


#' Title
#'
#' @param original_table
#' @param i
#' @param df
#' @param input_survival_table
#' @param splits
#'
#' @return
#' @export
#'
#' @examples
CrossValJM_AUC <- function (original_table,i,df=3,input_survival_table,splits) {
  patientid<-unique(original_table$id)
  #splits <- split(seq_len(n), sample(rep(seq_len(i), length.out = n)))
  trainingData <- original_table[!original_table$id %in% patientid[splits[[i]]], ]
  trainingData.id <- trainingData[!duplicated(trainingData$id), ]
  testingData <- original_table[original_table$id %in% patientid[splits[[i]]], ]
  #library(nlme)
  #library(splines)
  ctrl<- nlme::lmeControl(opt='optim')
  lmeFit.ctdna_training <- nlme::lme(logAF_2 ~ ns(sample_gap, 3), data = trainingData,random = ~ ns(sample_gap, 3) | id,control=ctrl)
  coxFit.ctDNA.training <- survival::coxph(Surv(DFS_time, DFS_event) ~ PTEN+pTNMStage.2, data = trainingData.id, x = TRUE)

  jointFit_ctdna_training <-JMbayes::jointModelBayes(lmeFit.ctdna_training, coxFit.ctDNA.training, timeVar = "sample_gap")
  auc_testing <- JMbayes::dynCJM(jointFit_ctdna_training, newdata = testingData, Dt = 90)
  auc_training<-JMbayes::dynCJM(jointFit_ctdna_training, newdata = trainingData, Dt = 90)
  pe_testing <- JMbayes::prederrJM(jointFit_ctdna_training, newdata = testingData,Tstart = 365, Thoriz = 730)
  pe_training<-JMbayes::prederrJM(jointFit_ctdna_training, newdata = trainingData,Tstart = 365, Thoriz = 730)
  result<-list()
  result[["auc_testing"]]<-auc_testing
  result[["auc_training"]]<-auc_training
  result[["pe_testing"]]<-pe_testing
  result[["pe_training"]]<-pe_training
  risk_auc_training<-survival_risk_AUC(summary_table=trainingData,input_joint_model=jointFit_ctdna_training,survival_table=input_survival_table)
  risk_auc_testing<-survival_risk_AUC(summary_table=testingData,input_joint_model=jointFit_ctdna_training,survival_table=input_survival_table)
  result[["risk_auc_training"]]<-risk_auc_training$auc_result
  result[["risk_auc_testing"]]<-risk_auc_testing$auc_result

  return(result)
}

#' Title
#'
#' @param summary_table
#' @param repeat_time
#' @param i
#'
#' @return
#' @export
#'
#' @examples
CrossValJM_AUC_repeat<-function(summary_table,repeat_time=5,i=5){
  n<-length(unique(summary_table$id))
  patientid<-unique(summary_table$id)
  total_joint_training_auc<-c()
  total_joint_testing_auc<-c()
  total_joint_training_pe<-c()
  total_joint_testing_pe<-c()
  total_roc_training<-list()
  total_roc_testing<-list()

  for (j in 1:repeat_time){
    splits <- split(seq_len(n), sample(rep(seq_len(i), length.out = n)))
    joint_auc_test<-list()
    survival_risk_roc_training<-list()
    survival_risk_roc_testing<-list()
    for (i in 1:i){
      msg<-tryCatch({joint_test_result<-CrossValJM_AUC(original_table=summary_table, i=i,splits=splits)
      joint_auc_test<-rlist::list.append(joint_auc_test,joint_test_result)}, error = function(e){""})
      survival_risk_roc_training<-rlist::list.append(survival_risk_roc_training,joint_test_result$risk_auc_training)
      survival_risk_roc_testing<-rlist::list.append(survival_risk_roc_testing,joint_test_result$risk_auc_testing)

    }
    joint_training_auc<-sapply(joint_auc_test,function(x) x$auc_training$dynC)
    joint_testing_auc<-sapply(joint_auc_test,function(x) x$auc_testing$dynC)
    joint_training_pe<-sapply(joint_auc_test,function(x) x$pe_training$prederr)
    joint_testing_pe<-sapply(joint_auc_test,function(x) x$pe_testing$prederr)
    risk_roc_training_one_fold<-do.call(rbind,survival_risk_roc_training)
    risk_roc_testing_one_fold<-do.call(rbind,survival_risk_roc_testing)
    total_roc_training<-rlist::list.append(total_roc_training,risk_roc_training_one_fold)
    total_roc_testing<-rlist::list.append(total_roc_testing,risk_roc_testing_one_fold)
    total_joint_training_auc<-c(total_joint_training_auc,joint_training_auc)
    total_joint_testing_auc<-c(total_joint_testing_auc,joint_testing_auc)
    total_joint_training_pe<-c(total_joint_training_pe,joint_training_pe)
    total_joint_testing_pe<-c(total_joint_testing_pe,joint_testing_pe)
  }
  validation_auc<-data.frame(AUC=c(mean(total_joint_training_auc,na.rm=T),mean(total_joint_testing_auc,na.rm=T)),SD=c(stats::sd(total_joint_training_auc,na.rm=T),stats::sd(total_joint_testing_auc,na.rm=T)),Type=c("Training","Testing"))
  validation_pe<-data.frame(PE=c(mean(total_joint_training_pe,na.rm=T),mean(total_joint_testing_pe,na.rm=T)),SD=c(stats::sd(total_joint_training_pe,na.rm=T),stats::sd(total_joint_testing_pe,na.rm=T)),Type=c("Training","Testing"))
  risk_roc_training<-do.call(rbind,total_roc_training)
  risk_roc_testing<-do.call(rbind,total_roc_testing)
  final_result<-list()
  final_result[["validation_auc"]]<-validation_auc
  final_result[["validation_pe"]]<-validation_pe
  final_result[["risk_roc_training"]]<-risk_roc_training
  final_result[["risk_roc_testing"]]<-risk_roc_testing
  final_result[["auc_training_data"]]<-total_joint_training_auc
  final_result[["auc_testing_data"]]<-total_joint_testing_auc
  final_result[["pe_training_pe_ata"]]<-total_joint_training_pe
  final_result[["pe_testing_data"]]<-total_joint_testing_pe
  return(final_result)

}
