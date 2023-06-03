# AgreeDisagreeR
The aim of this R package is to be able to carry out an exhaustive study of different classical machine learning algorithms for working with non-equilibrium problems. 
Different experiments on an unbalanced anonymised set related to COVID-19 obtained from the SMS are given as examples of the execution of the different functions of 
the package. With this or any other dataset, it will be possible to visualise experiments in a real context of strong imbalance. It will then be possible to study the 
evolution of the data sets.

Four different functions are presented to visualise agreements, disagreements separately, to visualise them at the same time or together in an integrated plot.

The way it has been decided to visualise agreements and disagreements is by means of a tournament between the three algorithms and between all metrics. The idea is not
only to test which of the three algorithms is better, but to visualise the agreements and disagreements between metrics and algorithms. 

This methodology will be performed for each pair of existing statistics for each point of the tournament and for each round. Four resulting matrices will be obtained: 
Agreement matrix of the first algorithm, agreement matrix of the second algorithm, agreement matrix of the third algorithm and disagreement matrix. Showing in the 
diagonals of the matrix of each algorithm its victories for each metric, in the rest of the positions the agreements and in the positions of the disagreements matrix 
the disagreements between pairs of metrics.

To say that these matrices will be shown in a 2x2 mesh of four corrplots when we want to show them together in the agree_disagree function, for this we will have to 
divide each position of the matrices by the maximum value they can have to normalise them, it would be a very technical explanation for this section of the work, so I 
will leave it for the following sections.

To install the package use devtools::install_github("JoseAdrian3/AgreeDisagreeR")

## Requirements
R: >= 4.2.3

RStudio: >= 2023.03.0

devtools package

## Examples of use
Once installed you can access any help command where all example executions will be shown but even so several are shown here:

> AgreeDisagreeR::agree("IPIP_unbalancing.rds", "SMOTE_unbalancing.rds", "ROSE_unbalancing.rds", "IPIP", 1)

> AgreeDisagreeR::agree("IPIP_unbalancing_complete.rds", "SMOTE_unbalancing_complete.rds", "ROSE_unbalancing_complete.rds", "SMOTE", 2)

> AgreeDisagreeR::agree_disagree("IPIP_unbalancing.rds", "SMOTE_unbalancing.rds", "ROSE_unbalancing.rds", "IPIP", "SMOTE", "ROSE")

> AgreeDisagreeR::disagree("IPIP_unbalancing_complete.rds", "SMOTE_unbalancing_complete.rds", "ROSE_unbalancing_complete.rds")

> AgreeDisagreeR::integrated_agree_disagree("IPIP_unbalancing.rds", "SMOTE_unbalancing.rds", "ROSE_unbalancing.rds", "IPIP", "SMOTE", "ROSE")

