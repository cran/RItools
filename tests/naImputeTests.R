require("RItools")
data(nuclearplants)

nuclearplants$cum.n.fac <- factor(ifelse(c(T,T,T,F),nuclearplants$cum.n,NA))
nuclearplants$pt.log <- as.logical(nuclearplants$pt)
nuclearplants$pt.log[c(3,4)] <- NA
nuclearplants$pt.na <- nuclearplants$pt
nuclearplants$pt.na[3:4] <- NA
nuclearplants$date.mod <- nuclearplants$date
nuclearplants$date.mod[1:2] <- NA

RItools:::naImpute(cost~cap+date+cum.n+as.logical(pt),nuclearplants)

RItools:::naImpute(cost~cap+date.mod+cum.n+as.logical(pt),nuclearplants)

RItools:::naImpute(cost~cap+date+cum.n.fac+as.logical(pt),nuclearplants)

RItools:::naImpute(cost~cap+date+cum.n+pt.log,nuclearplants)
