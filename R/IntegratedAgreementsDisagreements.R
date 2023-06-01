#' Integrated agreements and disagreements
#'
#' Function to generate a matrix of integrated agreements and disagreements from the metrics of any three algorithms.
#'
#' The integrated agreements and disagreements between the Accuracy, Kappa, F1, AUC, LiftAUC and BA measures between the
#' algorithms will be represented in the form of a triangular matrix where algorithms in the form of a triangular matrix where:
#'
#' - In the diagonal, the number of victories that each metric has had for each algorithm is symbolised by a piechart. This
#' piechart by its definition is always complete.  For example, the box [Accuracy, Accuracy] will represent the number of times
#' Accuracy has preferred each algorithm.
#'
#' - In the cells that are not the diagonal, the number of agreements that have existed between all the metric pairs shall be
#' symbolised with a piechart. the pairs of metrics. For example, the [Accuracy, Kappa] cell shall represent the number of times
#'  those metrics have agreed in each coloured algorithm and, in white, the number of disagreements of both metrics.
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
#' @param name1 Name of the first algorithm
#' @param name2 Name of the second algorithm
#' @param name3 Name of the third algorithm
#'
#' @export
#'
#' @examples
#'
#' AgreeDisagreeR::integrated_agree_disagree("IPIP_unbalancing.rds", "SMOTE_unbalancing.rds", "ROSE_unbalancing.rds", "IPIP", "SMOTE", "ROSE")
#' AgreeDisagreeR::integrated_agree_disagree("IPIP_unbalancing_complete.rds", "SMOTE_unbalancing_complete.rds", "ROSE_unbalancing_complete.rds", "IPIP", "SMOTE", "ROSE")
integrated_agree_disagree <- function(df1, df2, df3, name1, name2, name3){
  library(corrplot)
  library(tidyr)
  library(ggplot2)
  library(ggpubr)

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

  agreementsA1[row(agreementsA1) != col(agreementsA1)] <- agreementsA1[row(agreementsA1) != col(agreementsA1)] /  (length(metricsA1) * 5)
  diag(agreementsA1) <- diag(agreementsA1) / (length(metricsA1) * (length(metrics)-1) * 5)

  agreementsA2[row(agreementsA2) != col(agreementsA2)] <- agreementsA2[row(agreementsA2) != col(agreementsA2)] /  (length(metricsA1) * 5)
  diag(agreementsA2) <- diag(agreementsA2) / (length(metricsA1) * (length(metrics)-1) * 5)

  agreementsA3[row(agreementsA3) != col(agreementsA3)] <- agreementsA3[row(agreementsA3) != col(agreementsA3)] /  (length(metricsA1) * 5)
  diag(agreementsA3) <- diag(agreementsA3) / (length(metricsA1) * (length(metrics)-1) * 5)

  values = list()

  for (i in 1:(length(metrics))) {
    for (j in 1:(length(metrics))) {
      if(i <= j){
        values[[length(values)+1]] <- c(agreementsA1[i,j], agreementsA2[i,j], agreementsA3[i,j], 1-sum(agreementsA1[i,j],agreementsA2[i,j],agreementsA3[i,j]))
      } else {
        values[[length(values)+1]] <- c(0,0,0,0)
      }
    }
  }

  dfValues <- data.frame(matrix(unlist(values), ncol = 4, byrow = TRUE))
  colnames(dfValues) <- c(name1, name2, name3, "FILL")

  dfValues_long <- pivot_longer(dfValues, cols=1:4, names_to="algorithm", values_to="values")

  graphs <- list()

  k <- length(metrics)+3

  colours <- c("#FFFFFF", "#1F77B4", "#2CA02C", "#D62728")

  for(i in seq(1, nrow(dfValues_long), by=4)) {
    graph <- ggplot(dfValues_long[i:(i+3),], aes(x="", y=values, fill=algorithm)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      scale_fill_manual(values = colours) +
      theme_void() +
      guides(fill=guide_legend(title=NULL))

    # Agregamos el gráfico a la lista
    graphs[[k]] <- graph
    if(k %% (length(metrics)+1) == 0){
      k <- k + 2
    } else {
      k <- k + 1
    }
  }

  for(i in c(1:(length(metrics)))){
    graphs[[i+1]] <- ggplot(data = data.frame(metric = metrics[[i]]), aes(x = 0, y = 0, label = metric)) +
      geom_text(size = 4, vjust = 0.5, hjust = 0.5) +
      theme_void()

    graphs[[(length(metrics)+2)+(i-1)*(length(metrics)+2)]] <-  ggplot(data = data.frame(metric = metrics[[i]]), aes(x = 0, y = 0, label = metric)) +
      geom_text(size = 4, vjust = 0.5, hjust = 0.5) +
      theme_void()
  }


  ggarrange(plotlist = graphs, ncol = (length(metrics)+1), nrow =(length(metrics)+1), common.legend = TRUE, legend = "bottom")
}
