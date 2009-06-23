
xtable.xbal <- function(x,caption = NULL, label = NULL, align =c("l",rep("r",ncol(xvardf))),
                          digits = 2, display = NULL, col.labels=NULL, ...)
  {##By default use decimal alignment, which will require the dcolumn package in latex and an appropriate column definition like:
    ##\newcolumntype{.}{D{.}{.}{2.2}}
    ##Here is an example which works
    ##xb1<-xBalance(pr~ date + t1 + t2 + cap + ne + ct + bw + cum.n,
    ##         strata=data.frame(unstrat=factor(character(32)),
    ##           pt=factor(nuclearplants$pt)),
    ##         data=nuclearplants,
    ##         report=c("adj.means","adj.mean.diffs",'std.diffs', 'z.scores', 'chisquare.test','p.values'))
    ##
    ##junk<-xtable(xb1)
    ##print(junk,add.to.row=attr(junk,"latex.add.to.row"),hline.after=c(0,nrow(junk)),sanitize.text.function=function(x){x},floating=TRUE,floating.environment="sidewaystable")

    require(xtable)
    xprint <- print.xbal(x,digits=digits[1], printme=FALSE,...)
    numstrata<-dim(x$results)[3]
    latex.annotation <- attr(xprint, "latex.annotation")
    ##Setup the variable by varible table as a dataframe rather than ftable
    xvardf<-data.frame(xprint$vartable[,])
    row.names(xvardf)<-attr(xprint$vartable,"row.vars")[["vars"]]
    names(xvardf) < 
    
    if(!is.null(col.labels)) {
      names(xvardf) <- col.labels
    } else {
      ##paste(rep(attr(x$vartable,"col.vars")[["strata"]],each=length(attr(x$vartable,"col.vars")[["stat"]])),attr(x$vartable,"col.vars")[["stat"]],sep=".")
      
      # MF - commented out below as it was just pasing the \\multi... as plain text
      # the below works for me (e.g. myvar = 0, myvar = 1) better, if very simple
      # names(xvardf) <- paste(
      #   "\\multicolumn{1}{c}{",
      #   rep(attr(xprint$vartable,"col.vars")[["stat"]], numstrata),
      #   "}",sep="")

      names(xvardf) <- attr(xprint$vartable,"col.vars")[["stat"]]
    }

    ##call xtable on the resulting data.frame
    vartab <- xtable(xvardf,caption=caption, label=label, digits=digits,align=align,hline.after=c(0,nrow(xvardf)),...) ##NextMethod("xtable",xvardf)
    structure(vartab, latex.add.to.row=list(pos=list(-1),command=latex.annotation)) #pos=nrow(vartab),command=latex.annotation))

  }


