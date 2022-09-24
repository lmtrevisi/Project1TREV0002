
#' @title One way table function
#'
#' @param InputTable data for table to be analyze
#' @param alpha to calculate confidence interval
#' @param plottilte title for plot
#'
#' @return returns a list the point stimates, the confidence intervals, and the difference confidence intervals for each cell
#' @export
#'
#' @examples
onewaytable <- function(InputTable, alpha, plottilte)
{
  #store initial data table
  InputTableo <- InputTable
  # check the orientation of the one way table
  d <- dim(InputTable)
  # calculate the total number of samples
  n <- sum(InputTable[2])
  # find the probabilities of each category
  InputTable[2] <- InputTable[2] / n
  #define the output data frame
  Oneway = c(0,0,0)
  Onewayplot = c(0,0,0,0)
  # loop trough each category, calculate the probability boundaries and add them to the output data frame
  for (i in 1:d[1])
  {
    name = paste("P_",InputTable[i,1])
    PE = InputTable[i,2]
    LB = InputTable[i,2]+stats::qnorm((alpha)/2)*sqrt(InputTable[i,2]*(1-InputTable[i,2])/n)
    UB = InputTable[i,2]-stats::qnorm(alpha/2)*sqrt(InputTable[i,2]*(1-InputTable[i,2])/n)
    Oneway = rbind(Oneway,c(PE,LB,UB))
    rownames(Oneway)[i+1] = name

    nameplot = paste("P_",InputTable[i,1])
    PEplot = InputTable[i,2]
    LBplot = InputTable[i,2]+stats::qnorm((alpha)/2)*sqrt(InputTable[i,2]*(1-InputTable[i,2])/n)
    UBplot = InputTable[i,2]-stats::qnorm(alpha/2)*sqrt(InputTable[i,2]*(1-InputTable[i,2])/n)
    Onewayplot = rbind(Onewayplot,c(nameplot,PEplot,LBplot,UBplot))

  }
  # delete the first dummy row
  Oneway <- Oneway[-c(1),]
  Onewayplot <- Onewayplot[-c(1),]

  # modify the names of the data frame
  colnames(Oneway)<- c("Point Estimate","Lower Bound", "Upper Bound")
  colnames(Onewayplot)<- c("Variable","Point Estimate","Lower Bound", "Upper Bound")
  # plot results
  Onewayplot <- data.frame(Onewayplot)
  Onewayplot [,2] <- as.numeric(Onewayplot [,2] )
  Onewayplot [,3] <- as.numeric(Onewayplot [,3] )
  Onewayplot [,4] <- as.numeric(Onewayplot [,4] )
  g = ggplot2::ggplot(Onewayplot, ggplot2::aes(x = Onewayplot [,1], y = Onewayplot [,2], fill = Onewayplot [,1])) + ggplot2::geom_col() + ggplot2::geom_errorbar(data = Onewayplot, ggplot2::aes(x= Onewayplot [,1], ymin = Onewayplot [,3], ymax = Onewayplot [,4])) + ggplot2::labs(x = "Variables", y = "Probability", title = plottilte)
  print(g)
  # results for the chisq.test p0: no difference in probability distribution

  ch<-stats::chisq.test(InputTableo[,2])

  #define the output data frame
  ConfInterval = c(0,0)
  # loop trough each category, calculate the probability boundaries and add them to the output data frame
  # finding difference for cell proportions
  u = 1
  for( i in 1:d[1])
  {
    for (j in 1:d[1])
    {
      PIJ = paste("P",paste(InputTable[i,1],paste("-",paste("P",InputTable[j,1]))))
      LB = (InputTable[i,2]-InputTable[j,2])+stats::qnorm((alpha)/2)*sqrt((InputTable[i,2]*(1-InputTable[i,2])+InputTable[j,2]*(1-InputTable[j,2])+2*InputTable[i,2]*InputTable[j,2])/n)
      UB = (InputTable[i,2]-InputTable[j,2])-stats::qnorm((alpha)/2)*sqrt((InputTable[i,2]*(1-InputTable[i,2])+InputTable[j,2]*(1-InputTable[j,2])+2*InputTable[i,2]*InputTable[j,2])/n)
      ConfInterval = rbind(ConfInterval,c(LB,UB))
      rownames(ConfInterval)[u+1] <- PIJ
      u = u+1
    }
  }
  # remove the title row
  colnames(ConfInterval)<- c("Lower bound", "Upper bound")
  # output of the function
  l <- list(Oneway,ch,ConfInterval)
  names(l) <- c(paste("Probability Calculations with alpha:", alpha), "Chi square test", "Confidence Intervals")
  invisible(l)
  # return the results

}

