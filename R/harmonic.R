harmonic <- function(data)
{
tapply(data$Tx.grp,data$stratum.code,function(x){2*(length(x) -1)*var(x)})
}
