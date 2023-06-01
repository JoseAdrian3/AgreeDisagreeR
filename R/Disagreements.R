#' Disagreements
#'
#' Function to generate a disagreement matrix from the metrics of any three algorithms.
#'
#' The disagreements between the Accuracy, Kappa, F1, AUC, LiftAUC and BA metrics between the algorithms shall be represented
#' in the form of a triangular matrix where each position shall symbolise the number of disagreements for that pair of metrics.
#' triangular matrix where each position will symbolise the number of disagreements that exist for that pair of metrics for the
#' three algorithms.
#'
#' For example, the cell [Accuracy, Kappa] represents the number of times that both metrics did not give the same algorithm as
#' the best. algorithm the same algorithm.
#'
#' @param df1 First dataframe that will contain the metrics in lists of experiment lists where for each experiment 5
#' cross-validation experiments will have been done. The dataframe shall have the value of the metrics in the following order:
#' Accuracy, Kappa, F1, AUC, LiftAUC and BA.
#' @param df2 Second dataframe that will contain the metrics in lists of experiment lists where for each experiment 5
#' cross-validation experiments will have been done. The dataframe shall have the value of the metrics in the following order:
#' Accuracy, Kappa, F1, AUC, LiftAUC and BA.
#' @param df3 Third dataframe that will contain the metrics in lists of experiment lists where for each experiment 5
#' cross-validation experiments will have been done. The dataframe shall have the value of the metrics in the following order:
#' Accuracy, Kappa, F1, AUC, LiftAUC and BA.
#'
#' @export
#'
#' @examples
#'
#' AgreeDisagreeR::disagree("IPIP_unbalancing.rds", "SMOTE_unbalancing.rds", "ROSE_unbalancing.rds")
#' AgreeDisagreeR::disagree("IPIP_unbalancing_complete.rds", "SMOTE_unbalancing_complete.rds", "ROSE_unbalancing_complete.rds")
disagree <- function(df1, df2, df3){

  library(corrplot)

  metricsA1 = readRDS(system.file("data", df1, package = "AgreeDisagreeR"))
  metricsA2 = readRDS(system.file("data", df2, package = "AgreeDisagreeR"))
  metricsA3 = readRDS(system.file("data", df3, package = "AgreeDisagreeR"))

  metrics = names(metricsA1[[1]][[1]])

  disagreements <- matrix(0, nrow = length(metrics), ncol = length(metrics))
  agreementsA1 <- matrix(0, nrow = length(metrics), ncol = length(metrics))
  agreementsA2 <- matrix(0, nrow = length(metrics), ncol = length(metrics))
  agreementsA3 <- matrix(0, nrow = length(metrics), ncol = length(metrics))

  dfA1 <- data.frame()
  dfA2 <- data.frame()
  dfA3 <- data.frame()

  for(i in 1:length(metricsA1)) {
    dfA1 <- as.data.frame(t(as.data.frame(metricsA1[[i]])))
    dfA2 <- as.data.frame(t(as.data.frame(metricsA2[[i]])))
    dfA3 <- as.data.frame(t(as.data.frame(metricsA3[[i]])))

    for(j in 1:(length(metrics)-1)){
      for(k in (j+1):(length(metrics))){
        for(r in 1:5){
          maxJ <- max(dfA1[[j]][r], dfA2[[j]][r], dfA3[[j]][r])
          maxK <- max(dfA1[[k]][r], dfA2[[k]][r], dfA3[[k]][r])

          if(maxJ == dfA1[[j]][r]){
            agreementsA1[j,j] = agreementsA1[j,j] + 1
          } else if (maxJ == dfA2[[j]][r]){
            agreementsA2[j,j] = agreementsA2[j,j] + 1
          } else {
            agreementsA3[j,j] = agreementsA3[j,j] + 1
          }

          if(maxK == dfA1[[k]][r]){
            agreementsA1[k,k] = agreementsA1[k,k] + 1
          } else if (maxK == dfA2[[k]][r]){
            agreementsA2[k,k] = agreementsA2[k,k] + 1
          } else {
            agreementsA3[k,k] = agreementsA3[k,k] + 1
          }

          if(maxJ == dfA1[[j]][r] && maxK == dfA1[[k]][r]){
            agreementsA1[j,k] <- agreementsA1[j,k] + 1
            agreementsA1[k,j] <- agreementsA1[k,j] + 1
          } else if(maxJ == dfA2[[j]][r] && maxK == dfA2[[k]][r]){
            agreementsA2[j,k] <- agreementsA2[j,k] + 1
            agreementsA2[k,j] <- agreementsA2[k,j] + 1
          } else if(maxJ == dfA3[[j]][r] && maxK == dfA3[[k]][r]){
            agreementsA3[j,k] <- agreementsA3[j,k] + 1
            agreementsA3[k,j] <- agreementsA3[k,j] + 1
          } else {
            disagreements[j,k] <- disagreements[j,k] + 1
            disagreements[k,j] <- disagreements[k,j] + 1
          }



        }
      }
    }
  }


  disagreements <- disagreements / (length(metricsA1) * 5)
  colnames(disagreements) <- metrics
  rownames(disagreements) <- metrics
  corrplot(disagreements, method = "circle", tl.srt = 45, type = "upper", col.lim = c(0,1),  tl.cex = 0.7)
  mtext("Disagreement matrix", side=3, line=1, cex=0.9)

}
