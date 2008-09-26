naImpute <- function(FMLA,DATA)
{
    fmla <- terms.formula(if (length(FMLA)>2) FMLA[-2] else FMLA,
                          dat=DATA, keep.order=TRUE)
    dat <- model.frame.default(fmla,DATA, na.action=na.pass)
    badfactor <- sapply(dat,function(x) nlevels(x)==1)
    dat[badfactor] <- lapply(dat[badfactor], as.integer) ##Is this right? shouldn't it be dat[,badfactor] ?
    factor.dat <- sapply(dat,is.factor)
    ordered.dat <- sapply(dat,is.ordered)
    dat.NA <- as.data.frame(lapply(dat, is.na))
    impute.dat <- sapply(dat.NA,any)
    dat.NA <- dat.NA[impute.dat& (!factor.dat | ordered.dat)]
  if (any(impute.dat & (!factor.dat | ordered.dat)))
      names(dat.NA) <- paste(names(dat.NA), 'NA',sep='.')

      if (any(impute.dat))  {
    dat <- lapply(dat, function(x){
    if (is.factor(x) & !is.ordered(x)){
      if (any(is.na(x))) levels(x) <- c(levels(x),'.NA')
      x[is.na(x)] <- '.NA'
    } else {
      if (is.ordered(x))
        {
          x[is.na(x)] <- levels(x)[1]
        } else {
       if (is.logical(x))
         {
           x[is.na(x)] <- mean(x,na.rm=TRUE)>.5
         } else
       {
         x[is.na(x)] <- mean(x,na.rm=TRUE)
       }
     }
    }
    x
  }
                  )
    dat <- as.data.frame(dat)
    dat <- data.frame(DATA[setdiff(names(DATA),
                                    c(names(dat),names(dat.NA)))],
                       dat, dat.NA)
    TFMLA <- if (length(dat.NA)) update.formula(FMLA, as.formula(paste(".~.+",paste(names(dat.NA), collapse=" + ")))) else FMLA
    TFMLA <- terms.formula(TFMLA,dat=dat, keep.order=TRUE) 
    return(structure(dat, TFMLA=TFMLA))
  } else return(structure(DATA,TFMLA=terms.formula(FMLA,dat=DATA, keep.order=TRUE)))
  }
