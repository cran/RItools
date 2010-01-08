\name{xtable.xbal}
\alias{xtable.xbal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ An xtable method for xbalance objects }
\description{
	This function uses the \code{\link[xtable]{xtable}} package framework to display the results of a call to \code{\link{xBalance}} in LaTeX format. At the moment, it ignores the omnibus chi-squared test information.
}
\usage{\method{xtable}{xbal}(x, caption = NULL, label = NULL, align = c("l",rep("r",ncol(xvardf))), digits = 2, display = NULL, col.labels = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object resulting from a call to \code{\link{xBalance}} }
  \item{caption}{ See \code{\link[xtable]{xtable}}. }
  \item{label}{ See \code{\link[xtable]{xtable}}. }
  \item{align}{ See \code{\link[xtable]{xtable}}. Our default (as of
    version 0.1-7) is right-aligned columns; for decimal aligned
    columns, see details, below.}
  \item{digits}{ See \code{\link[xtable]{xtable}}. Default is 2. }
  \item{display}{ See \code{\link[xtable]{xtable}} }
  \item{col.labels}{ Labels for the columns (the test
    statistics). Default are come from the call to \code{\link{print.xbal}}. }
  \item{\dots}{ Other arguments to \code{\link{print.xbal}} }
}
\details{
  To get decimal aligned
  columns, specify \code{align=c("l", rep(".", <ncols>))}, where
  \code{<ncols>} is the number of columns to be printed, in your
  call to \code{xtable}.  Then use the dcolumn package and define
  \samp{'.'} within LaTeX: 
add the lines \code{\\usepackage\{dcolumn\}} and
\code{\\newcolumntype\{.\}\{D\{.\}\{.\}\{2.2\}\}} to your LaTeX
document's preamble.  
}
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

xb1.xtab<-xtable(xb1) ##right aligned
##xb1.xtab<-xtable(xb1,align = c("l",rep(".",ncol(xvardf)))) ##decimal aligned, see Details above. 
print(xb1.xtab,add.to.row=attr(xb1.xtab,"latex.add.to.row"),hline.after=c(0,nrow(xb1.xtab)),sanitize.text.function=function(x){x},floating=TRUE,floating.environment="sidewaystable")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
