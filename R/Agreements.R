#' Agreements
#'
#' Function to generate an agreement matrix from the metrics of any three algorithms.
#'
#' The agreements between the Accuracy, Kappa, F1, AUC, LiftAUC and BA measures for a particular algorithm shall be represented
#' in a triangular matrix form where:
#'
#' - The cells that are the diagonal will symbolise the number of times an algorithm has given as best algorithm the
#' particular algorithm. For example, the position [Accuracy, Accuracy] will represent the number of times that Accuracy has
#' given the algorithm as the best algorithm.
#'
#' - The cells that are not the diagonal will symbolise the number of agreements that exist for that pair of metrics for the
#' particular algorithm. For example, the position [Accuracy, Kappa] will represent the number of agreements that exist between
#' these characteristics for the algorithm.
#'
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
#' @param name Name of the algorithm for which your agreements will be shown
#' @param algorithm Algorithm from which agreements will be displayed. A 1 shall show the agreements of the first algorithm, a 2 of the
#' second algorithm and 3 of the third algorithm.
#'
#' @export
#'
#' @examples
#'
#' AgreeDisagreeR::agree("IPIP_unbalancing.rds", "SMOTE_unbalancing.rds", "ROSE_unbalancing.rds", "IPIP", 1)
#' AgreeDisagreeR::agree("IPIP_unbalancing.rds", "SMOTE_unbalancing.rds", "ROSE_unbalancing.rds", "SMOTE", 2)
#' AgreeDisagreeR::agree("IPIP_unbalancing.rds", "SMOTE_unbalancing.rds", "ROSE_unbalancing.rds", "ROSE", 3)
#' AgreeDisagreeR::agree("IPIP_unbalancing_complete.rds", "SMOTE_unbalancing_complete.rds", "ROSE_unbalancing_complete.rds", "IPIP", 1)
#' AgreeDisagreeR::agree("IPIP_unbalancing_complete.rds", "SMOTE_unbalancing_complete.rds", "ROSE_unbalancing_complete.rds", "SMOTE", 2)
#' AgreeDisagreeR::agree("IPIP_unbalancing_complete.rds", "SMOTE_unbalancing_complete.rds", "ROSE_unbalancing_complete.rds", "ROSE", 3)
agree <- function(df1, df2, df3, name, algorithm){

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

  if(algorithm == 1){

    par(mfrow=c(1,1), mar=c(0,0,1,1), oma=c(0,0,1,0))
    agreementsA1[row(agreementsA1) != col(agreementsA1)] <- agreementsA1[row(agreementsA1) != col(agreementsA1)] /  (length(metricsA1) * 5)
    diag(agreementsA1) <- diag(agreementsA1) / (length(metricsA1) * (length(metrics)-1) * 5)
    colnames(agreementsA1) <- metrics
    rownames(agreementsA1) <- metrics
    corrplot(agreementsA1, method = "circle", tl.srt = 45, type = "upper", col.lim = c(0,1), tl.cex = 0.7)
    mtext(paste0(name, " agreement matrix"), side=3, line=0, cex=0.9)

  } else if (algorithm == 2){

    par(mfrow=c(1,1), mar=c(0,0,1,1), oma=c(0,0,1,0))
    agreementsA2[row(agreementsA2) != col(agreementsA2)] <- agreementsA2[row(agreementsA2) != col(agreementsA2)] /  (length(metricsA1) * 5)
    diag(agreementsA2) <- diag(agreementsA2) / (length(metricsA1) * (length(metrics)-1) * 5)
    colnames(agreementsA2) <- metrics
    rownames(agreementsA2) <- metrics
    corrplot(agreementsA2, method = "circle", tl.srt = 45, type = "upper", col.lim = c(0,1), tl.cex = 0.7)
    mtext(paste0(name, " agreement matrix"), side=3, line=0, cex=0.9)

  } else if (algorithm == 3){

    par(mfrow=c(1,1), mar=c(0,0,1,1), oma=c(0,0,1,0))
    agreementsA3[row(agreementsA3) != col(agreementsA3)] <- agreementsA3[row(agreementsA3) != col(agreementsA3)] /  (length(metricsA1) * 5)
    diag(agreementsA3) <- diag(agreementsA3) / (length(metricsA1) * (length(metrics)-1) * 5)
    colnames(agreementsA3) <- metrics
    rownames(agreementsA3) <- metrics
    corrplot(agreementsA3, method = "circle", tl.srt = 45, type = "upper", col.lim = c(0,1), tl.cex = 0.7)
    mtext(paste0(name, " agreement matrix"), side=3, line=0, cex=0.9)

  }
}
