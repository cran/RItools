\name{xtable.xbal}
\alias{xtable.xbal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ An xtable method for xbalance objects }
\description{
	This function uses the \code{\link[xtable]{xtable}} package framework to display the results of a call to \code{\link{xBalance}} in LaTeX format. At the moment, it ignores the omnibus chi-squared test information.
}
\usage{\method{xtable}{xbal}(x, caption = NULL, label = NULL, align = c("l", rep(".", ncol(xvardf))), digits = 2, display = NULL, col.labels = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object resulting from a call to \code{\link{xBalance}} }
  \item{caption}{ See \code{\link[xtable]{xtable}}. }
  \item{label}{ See \code{\link[xtable]{xtable}}. }
  \item{align}{ See \code{\link[xtable]{xtable}}. Our default is decimal aligned
  columns. This will require the use of the dcolumn package within LaTeX
and also a new column definition using \code{\\newcolumntype\{.\}\{D\{.\}\{.\}\{2.2\}}.}
  \item{digits}{ See \code{\link[xtable]{xtable}}. Default is 2. }
  \item{display}{ See \code{\link[xtable]{xtable}} }
  \item{col.labels}{ Labels for the columns (the test
    statistics). Default are come from the call to \code{\link{print.xbal}}. }
  \item{\dots}{ Other arguments to \code{\link{print.xbal}} }
}
%\details{
%}
\value{
 This function produces an xtable object which can then be printed with
 the appropriate print method (see \code{\link[xtable]{print.xtable}}).
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

data(nuclearplants)
require(xtable)

xb1<-xBalance(pr~ date + t1 + t2 + cap + ne + ct + bw + cum.n,
             strata=data.frame(unstrat=factor(character(32)),
               pt=factor(nuclearplants$pt)),
             data=nuclearplants,
             report=c('adj.means','adj.mean.diffs','std.diffs', 'z.scores', 'chisquare.test','p.values'))

xb1.xtab<-xtable(xb1)
print(xb1.xtab,add.to.row=attr(xb1.xtab,"latex.add.to.row"),hline.after=c(0,nrow(xb1.xtab)),sanitize.text.function=function(x){x},floating=TRUE,floating.environment="sidewaystable")


## The function is currently defined as
function (x, pvals = FALSE, caption = NULL, label = NULL, align = c("l", 
    rep(".", ncol(xvardf))), digits = 2, display = NULL, col.labels = NULL, 
    ...) 
{
    require(xtable)
    xprint <- print.xbal(x, digits = digits[1], printme = FALSE, 
        ...)
    numstrata <- dim(x$results)[3]
    latex.annotation <- attr(xprint, "latex.annotation")
    xvardf <- data.frame(xprint$vartable[, ])
    row.names(xvardf) <- attr(xprint$vartable, "row.vars")[["vars"]]
    names(xvardf) <- if (!is.null(col.labels)) {
        col.labels
    }
    else {
        paste("\\multicolumn{1}{c}{", rep(attr(xprint$vartable, 
            "col.vars")[["stat"]], numstrata), "}", sep = "")
    }
    vartab <- xtable(xvardf, digits = digits, align = align, 
        hline.after = c(0, nrow(xvardf)), ...)
    structure(vartab, latex.add.to.row = list(pos = list(-1), 
        command = latex.annotation))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
