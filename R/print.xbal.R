print.xbal <- function (x, which.strata=dimnames(x$results)[["strata"]],
                        which.stats=dimnames(x$results)[["stat"]],
                        which.vars=dimnames(x$results)[["vars"]],
                        print.overall=TRUE,
                        digits = NULL, printme=TRUE,
                        show.signif.stars=getOption("show.signif.stars"),
                        show.pvals=!show.signif.stars,
                        horizontal=TRUE,...)
{ ##Notes: right now we've decided that you can't print both signif stars and p-values. make a choice.

  # withOptions will allow us to safely reset the digits
  # even if an error is thrown the option should be the same after this function
  DIGITS = ifelse(is.null(digits), max(2, getOption("digits")-4), digits)
  withOptions(list(digits = DIGITS), function() {
    ##makeSigStarsStdNormal <- function(zs) {
    ##  if (length(zs)){##c('','.  ','*  ','** ','***'
    ##    factor(c('','.','*','**','***') [1+
    ##             apply(abs(zs)>=matrix(qnorm(c(.95, .975, .995,.9995)),
    ##                        length(zs),4, byrow=TRUE),1, sum)]
    ##           )} else {character(0)}
    ##                                     }

    theresults <- x$results
    if("overall" %in% names(x)) {
      theoverall <- x$overall
    } else {
      theoverall<-NULL
    }

    ##Summarize the output array as a flat contingency table
    if (length(theresults) == 0) {
      stop("There is a problem. Probably all of the variables (",
          all.vars(formula(x)),
          ") are constants within strata. Or else there is some other problem, try debug(RItools:::xBalance) to see what might be going on.")
    }
    
    signifier <- function(data) {
      symnum(data,
        corr = FALSE, 
        na = FALSE, 
        abbr.colnames=FALSE, ##from print.summary.lm
        cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
        symbols = c("***", "** ", "*  ", ".  ", "   "))
    }
    
    ftabler <- function(data) {
      ftable(data, col.vars=c("strata","stat"),row.vars=c("vars"))
    }
    
    if(show.signif.stars & !show.pvals) {
      
      Signif <- signifier(theresults[,"p",which.strata,drop=FALSE])

      ##Nicer alignment, but not as nice labels
      ##junk<-do.call(cbind,lapply(which.strata,function(x){cbind(as.data.frame(theresults[,,x])," "=format(Signif[,,x]))}))
      newresults <- array(dim=dim(theresults)+c(0,1,0),
                          dimnames=list(vars=dimnames(x$results)[["vars"]],
                          stat=c(dimnames(x$results)[["stat"]],"sig."),
                          strata=dimnames(x$results)[["strata"]]))
                          
      newresults[,-grep("sig.", dimnames(newresults)[[2]]),] <- format(theresults,DIGITS)
      newresults[dimnames(Signif)[["vars"]], "sig.",dimnames(Signif)[["strata"]]]<-format(Signif)
      
      
      if(horizontal){
        theftab <- ftabler(
          newresults[which.vars, c(which.stats[!(which.stats=="p")],"sig."), which.strata, drop=FALSE])
          
        attr(theftab,"col.vars")$stat[attr(theftab,"col.vars")$stat=="sig."] <- ""
        if("z" %in% which.stats) {
          attr(theftab,"col.vars")$stat[attr(theftab,"col.vars")$stat=="z"]<-"   z   "
        }
        
        thevartab<-theftab
      } else {
        thevartab <- sapply(which.strata,
          simplify=FALSE,
          function(x) { 
            cbind(
              as.data.frame(theresults[which.vars,c(which.stats[!(which.stats=="p")]),x]), 
              " " = format(Signif[which.vars,,x]))
          })
      }

    }
    if(show.pvals&("p"%in% dimnames(theresults)[["stat"]])) {
      if(horizontal){
        theftab <- ftabler(
          theresults[which.vars,which.stats,which.strata,drop=FALSE])
        thevartab <- theftab
      } else {
        thevartab <- sapply(
          which.strata,
          simplify=FALSE,
          function(x) { 
            as.data.frame(theresults[which.vars,which.stats,x])
          })
      }
    }
    
    if(show.pvals&!("p"%in% dimnames(theresults)[["stat"]])) { 
      stop("You need to request p-values when calling xBalance.")
    } ##irrelevant now.

   if (!is.null(theoverall)) {
     nc <- length(theresults)/2
     latex.annotation <- NULL
     ## paste("\\\\ \\hline Overall",
     ##       paste("\\multicolumn{",nc,"}{c}{",preSig,"}"),
     ##       paste("\\multicolumn{",nc,"}{c}{",postSig,"}"),
     ##       sep=" & ")
     if(show.signif.stars){
       ChiSignif <- signifier(theoverall[which.strata,"p.value"])
                      
       theoveralltab <- cbind(format(theoverall[which.strata,],digits=DIGITS),format(ChiSignif))
       names(theoveralltab)[4]<-" "
     }
     theoveralltab<-format(theoverall[which.strata,],digits=DIGITS)
   } else {
     theoveralltab<-NULL
   }
   
  if(printme) {
   ## RItools:::print.ftable(thevartab,justify.labels="center",justify.data="right") ##doesn't seem to help the alignment problem
    print(thevartab)
    if(!is.null(theoverall) && print.overall){
      cat("---Overall Test---\n")
      print(theoveralltab)
      if(show.signif.stars&!show.pvals){
        cat("---\nSignif. codes: ", attr(Signif, "legend"), "\n")
      }
    }
    ##cat(paste("n = ", n, ", k = ", k,
    ##          "\nresidual sd = ", fround(summ$sigma, digits), ", R-Squared = ", fround(summ$r.squared, 2),
    ##          "\n", sep = ""))
  }
  theprintresults <- list(vartable=thevartab,overalltable=theoveralltab)
  latex.annotation <- paste(paste(" & \\multicolumn{",length(which.stats),"}{c}{",which.strata[1],"}"),
                            paste("\\multicolumn{",length(which.stats),"}{c}{",which.strata[2],"} \\\\"),
                            sep=" & ")
  invisible(structure(theprintresults,latex.annotation=latex.annotation))
  })
}
