#' @title Two way table
#'
#' @param InputTable pre-processed table to be analyze. use xtab
#' @param alpha alpha for the confidence interval calculations
#'
#' @return a list with confidence intervals, chi tests, and many other uselful data.
#' @export
#'
#' @examples
twowaytable <- function (InputTable, alpha)
{
  # extracting the data
  x <-InputTable
  # adding margins to the data
  x <- stats::addmargins(x)
  # calculating the size of the data
  ubrow = dim(x)[1]-1
  ubcol = dim(x)[2]-1

  # calculating the probability table base on the data
  p.table <- matrix(nrow = ubrow, ncol = ubcol)
  for (i in 1:ubrow)
  {
    for (j in 1:ubcol)
    {
      p.table[i,j] <- x[i,j]/x[ubrow+1,ubcol+1]
    }
  }
  # calculating the expected count table for each cell
  exp.table <- matrix(nrow = ubrow, ncol = ubcol)
  for (i in 1:ubrow)
  {
    for (j in 1:ubcol)
    {
      exp.table[i,j] <- x[i,ubcol+1]*x[ubrow+1,j]/x[ubrow+1,ubcol+1]
    }
  }
  # checking if the data satisfy the CLT
  CLTtest <- sum(exp.table>=5)/(dim(exp.table)[1]*dim(exp.table)[2])*100
  if (CLTtest < 80)
    {
      print("The data set is too small, central limit theorem does not apply. Use another tecnique")
  }
  else
  {
      print("The data set is big enough to apply CLT")
  }
  # Performing and storing the chi test
  chi <- stats::chisq.test(x)

  # finding the row distributions

  r.table <- matrix(nrow = ubrow, ncol = ubcol)
  for (i in 1:ubrow)
  {
    for (j in 1:ubcol)
    {
      r.table[i,j] <- x[i,j]/x[i,ubcol+1]
    }
  }
  # printing the row distributions
  r.tabledf <- data.frame(r.table)
  for (i in 1: ubcol)
  {
    r.tabledf[,i] <- as.numeric(r.tabledf[,i])
  }
  name_columns <- colnames(InputTable)
  name_rows <- rownames(InputTable)
  g = ggplot2::ggplot(r.tabledf, ggplot2::aes(x = name_columns, y = t(r.tabledf [1,]), fill = name_columns))+ ggplot2::geom_col()  + ggplot2::labs(x = "Variables", y = "Probability", title = paste("row distribution for:", name_rows[1]) )
  print(g)
  for (i in 2:ubcol)
  {
    g = ggplot2::ggplot(r.tabledf, ggplot2::aes(x = name_columns, y = t(r.tabledf [i,]), fill = name_columns))+ ggplot2::geom_col()  + ggplot2::labs(x = "Variables", y = "Probability", title = paste("row distribution for:", name_rows[i]))
    print(g)
  }
  # calculating the distributions between
  l <- list()
  namesoutput <- c("expected count table","probability table", "chi square test results","row distribution table")
  for (i in 1:ubrow)
  {
    holdertable <- cbind(colnames(InputTable),InputTable[i,])
    holdertable <- data.frame(holdertable)
    holdertable[,2] <- as.numeric(holdertable[,2])
    plotname <- paste("comparation between level",rownames(InputTable)[i])
    g <- onewaytable1(holdertable,alpha,plottilte = plotname)
    l<-append(l,list(g))
    namesoutput <- c(namesoutput,plotname)
  }
  # calculating the distributions within
  for (i in 1:ubrow)
  {
    holdertable <- cbind(rownames(InputTable),InputTable[,i])
    holdertable <- data.frame(holdertable)
    holdertable[,2] <- as.numeric(holdertable[,2])
    plotname <- paste("comparation within catergory",colnames(InputTable)[i])
    g <- onewaytable1(holdertable,alpha,plottilte = plotname)
    l<-append(l,list(g))
    namesoutput <- c(namesoutput,plotname)
  }

  # returning the results
  outputlist <-list(exp.table,p.table,chi,r.table)
  outputlist <-append(outputlist,l)
  names(outputlist) <- namesoutput
  return (outputlist)
}

v <- Intro2R::myreadxl("C:/Users/lmtre/OneDrive - University of Oklahoma/OU/Applied Regression Analysis/R Package/Excel/")
v <- v$HYBRID
x <- xtabs(v$Number~v$Claim+v$Model)
k <- twowaytable(x,0.05)
