# Create additional summary and visualization functions for dataMaid reports
# following steps from: https://rdrr.io/cran/dataMaid/f/vignettes/extending_dataMaid.Rmd
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# summaryFunctions ----

# .summaryMean ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
meanSummary <- function(v, maxDecimals = 2) {
  UseMethod("meanSummary")
}

meanSummaryHelper <- function(v, maxDecimals) {
  #remove missing observations
  v <- na.omit(v)
  #compute mean and store "raw" output in `val`
  val <- mean(v)
  #store printable (rounded) output in `res`
  res <- round(val, maxDecimals)
  #output summaryResult
  summaryResult(list(feature = "Mean", result = res, value  = val))
}

meanSummary <- summaryFunction(meanSummary,  
                               description = "Compute arithmetic mean")


# visualFunctions ----

# .mosaicVisual ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mosaicVisual <- function(v, vnam, doEval) {
  #Define a (unevaluated) call to mosaicplot
  thisCall <- call("mosaicplot", table(v), main = vnam, xlab = "")
  
  #if doEval is TRUE, evaluate the call, thereby producing a plot
  #if doEval is FALSE, return the deparsed call
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}

mosaicVisual <- visualFunction(mosaicVisual,
                               description = "Mosaic plots using graphics",
                               classes = setdiff(allClasses(), 
                                                 c("numeric", 
                                                   "integer", 
                                                   "Date")))


# .prettierHist ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)

prettierHistHelper <- function(v, vnam) {
  #define a ggplot2 histogram 
  p <- ggplot(data.frame(v = v), aes(x = v)) + 
    geom_histogram(col = "white", bins = 20) +
    xlab(vnam)
  
  #return the plot
  p
}

#define visualFunction-style prettierHist()-function
prettierHist <- function(v, vnam, doEval = TRUE) {
  #define the call
  thisCall <- call("prettierHistHelper", v = v, vnam = vnam)
  
  #evaluate or deparse
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}

#Make it a proper visualFunction:
prettierHist <- visualFunction(prettierHist, 
                               description = "ggplot2 style histogram with contours",
                               classes = c("numeric", "integer", "logical", "Date"))

