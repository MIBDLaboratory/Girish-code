# load the library
library(mlbench)
library(caret)
# load the data
data(ksl)
indx <- sapply(ksl, is.factor)
ksl[indx] <- lapply(ksl[indx], function(x) as.numeric(as.character(x)))
ksl[1:3] <- lapply(ksl[1:3], as.numeric)
str(ksl)

kslf <- ksl[c(1:300), -c(6,7,9)]
head(kslf)
str(kslf)
kslf[,c(3,5,6)] <- lapply(kslf[,c(3,5,6)], as.factor)
kslf.nw <- network(kslf)
kslf.prior <- jointprior(kslf.nw)
kslf.nw <- learn(kslf.nw,kslf,kslf.prior)$nw
kslf.search <- autosearch(kslf.nw,kslf,kslf.prior,trace=TRUE)
kslf.heuristic <- heuristic(kslf.search$nw,kslf,
                            kslf.prior,
                              restart=2,degree=10,
                              trace=TRUE,trylist=kslf.search$trylist)
thebest6 <- kslf.heuristic$nw
res6 = bnlearn::model2network(deal::modelstring(thebest6))
graphviz.plot(res6)
res6 <- reverse.arc(res6, "logBMI", "Hyp", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)
res6 <- set.arc(res6, "Kol", "logBMI", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)
res6 <- drop.arc(res6, "Smok", "Hyp")
indx <- sapply(kslf, is.factor)
kslf[indx] <- lapply(kslf[indx], function(x) as.numeric(as.character(x)))
kslf[,c(1:6)] <- lapply(kslf[,c(1:6)], as.factor)
fittedbne6 <- bn.fit(res6, data = kslf, method = "mle")
fittedbne6$Hyp
*****************************************************************************************
# MB of Sex
kslfs <- ksl[c(1:300),c(1,2,4,5,6,7,8)]
head(kslfs)
kslfs[,c(1:7)] <- lapply(kslfs[,c(1:7)], as.factor)
indx <- sapply(kslfs, is.factor)
kslfs[indx] <- lapply(kslfs[indx], function(x) as.numeric(as.character(x)))
kslfs.nw <- network(kslfs)
kslfs.prior <- jointprior(kslfs.nw)
kslfs.nw <- learn(kslfs.nw,kslfs,kslfs.prior)$nw
kslfs.search <- autosearch(kslfs.nw,kslfs,kslfs.prior,trace=TRUE)
kslfs.heuristic <- heuristic(kslfs.search$nw,kslfs,
                            kslfs.prior,
                            restart=2,degree=10,
                            trace=TRUE,trylist=kslfs.search$trylist)
thebestf <- kslfs.heuristic$nw

s1 <- with(kslfs, table(Sex, Smok))
kslfs <- kslf[, -c(3)]
head(kslfs)
str(scorsex)
bgf <- hc(kslfs)
graphviz.plot(res7)
res7 <- set.arc(bgf, "Sex", "FEV", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)
res7 <- set.arc(res7, "Sex", "Kol", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)
res7 <- set.arc(res7, "Sex", "logBMI", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)
res7 <- set.arc(res7, "Kol", "logBMI", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)
res7 <- set.arc(res7, "Sex", "Alc", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)
res7 <- reverse.arc(res7, "Smok", "Sex", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)
fittedsex <- bn.fit(res7, data = kslfs, method = "mle")
fittedsex$Alc

# SB Sex-Smok
junctionsex = compile(as.grain(fittedsex))
JPsex = querygrain(junctionsex, nodes=c("Sex","Smok"), type = "joint")
gsex <- kslfs[,-c(1,2,3)]
scorsex <- mutate(gsex, Score = ifelse(kslfs["Smok"] == 1 & kslfs["Sex"] == 1, "0.08666667", 
                                     ifelse(kslfs["Smok"] == 1 & kslfs["Sex"] == 2, "0.23333333",
                                            ifelse(kslfs["Smok"] == 2 & kslfs["Sex"] == 1, "0.40", "0.28"))))
scorsex <- scorsex[,-c(1,2)]
scorsex <- as.data.frame(scorsex)
colnames(scorsex) <- "scorsex"
scorsex$scorsex <- as.numeric(as.character(scorsex$scorsex))
head(scorsex)

# SB Sex-Alc
sexalc <- ksl[c(1:300),c(6,8)]
scoral <- mutate(sexalc, Score = ifelse(sexalc["Alc"] == 1 & sexalc["Sex"] == 1, "0.1133333", 
                                       ifelse(sexalc["Alc"] == 1 & sexalc["Sex"] == 2, "0.2466667",
                                              ifelse(sexalc["Alc"] == 2 & sexalc["Sex"] == 1, "0.3733333", "0.2666667"))))
scoral <- scoral[,-c(1,2)]
scoral <- as.data.frame(scoral)
colnames(scoral) <- "scoralcsex"
scoral$scoralcsex <- as.numeric(as.character(scoral$scoralcsex))
head(scoral)

# SB Sex-Work
sexw <- ksl[c(1:300),c(7,8)]
scorw <- mutate(sexw, Score = ifelse(sexw["Work"] == 1 & sexw["Sex"] == 1, "0.06000000", 
                                      ifelse(sexw["Work"] == 1 & sexw["Sex"] == 2, "0.01666667",
                                             ifelse(sexw["Work"] == 2 & sexw["Sex"] == 1, "0.4266667", "0.4966667"))))
scorw <- scorw[,-c(1,2)]
scorw <- as.data.frame(scorw)
colnames(scorw) <- "scorew"
scorw$scorew <- as.numeric(as.character(scorw$scorew))
head(scorw)

# SB kol-logBMIf
kolBMI <- kslfs[,c(2,3), drop=FALSE]
str(kolBMI)
indx <- sapply(kolBMI, is.factor)
kolBMI[indx] <- lapply(kolBMI[indx], function(x) as.numeric(as.character(x)))
kolB <- abodOutlier::abod(kolBMI, method = "randomized", n_sample_size = 169)
kolB <- as.data.frame(kolB)
colnames(kolB) <- "ScorekolBMI"
head(kolB)
outkolB <- order(kolB, decreasing=F)[1:10]
print(outkolB)

# FEV normalized (FEV and Sex)
FEVSex <- kslfs[,c(1,5), drop=FALSE]
head(FEVSex)
str(FEVSex)
x2 <- FEVSex$FEV
normalized2 = (x2-min(x2))/(max(x2)-min(x2))
ru2 <- as.data.frame(normalized2)
ru2 <- cbind(ru2, train2)
head(ru2)
SexF <- abodOutlier::abod(ru2, method = "randomized", n_sample_size = 169)
SexF <- as.data.frame(SexF)

# Kol normalized (Kol and Sex)
KolSex <- kslfs[,c(2,5), drop=FALSE]
head(KolSex)
str(KolSex)
x3 <- KolSex$Kol
normalized3 = (x3-min(x3))/(max(x3)-min(x3))
ru3 <- as.data.frame(normalized3)
ru3 <- cbind(ru3, train2)
head(ru3)
SexK <- abodOutlier::abod(ru3, method = "randomized", n_sample_size = 169)
SexK <- as.data.frame(SexK)
outkol <- order(SexK, decreasing=F)[1:10]
print(outkol)

# logBMI normalized (logBMI and Sex)
BMISex <- kslfs[,c(3,5), drop=FALSE]
head(BMISex)
str(BMISex)
x4 <- BMISex$logBMI
normalized4 = (x4-min(x4))/(max(x4)-min(x4))
ru4 <- as.data.frame(normalized4)
ru4 <- cbind(ru4, train2)
head(ru4)
SexB <- abodOutlier::abod(ru4, method = "randomized", n_sample_size = 169)
SexB <- as.data.frame(SexB)
head(SexB)
otsxb <- order(SexB, decreasing=F)[1:10]
print(otsxb)

fs <- scorsex + scoral + scorw + kolBn + Fsex + Kolsex + lbsex   #(SFn + SexK + SexB)
head(fs)
outsex <- order(fs, decreasing=F)[1:10]
print(outsex)
print(ksl[outsex,])
'''
# SB FEV and Sex
SexFEV <- kslfs[,c(1), drop=FALSE]
head(SexFEV)
indx <- sapply(SexFEV, is.factor)
SexFEV[indx] <- lapply(SexFEV[indx], function(x) as.numeric(as.character(x)))
SexFEV["Mean"] <- mean(SexFEV$FEV)
SF <- abodOutlier::abod(SexFEV, method = "randomized", n_sample_size = 169)
SF <- as.data.frame(SF)
colnames(SF) <- "ScoreSexFEV"
head(SF)

# SB Kol
SexKol <- kslfs[,c(2), drop=FALSE]
head(SexKol)
indx <- sapply(SexKol, is.factor)
SexKol[indx] <- lapply(SexKol[indx], function(x) as.numeric(as.character(x)))
SexKol["Mean"] <- mean(SexKol$Kol)
SK <- abodOutlier::abod(SexKol, method = "randomized", n_sample_size = 169)
SK <- as.data.frame(SK)
colnames(SK) <- "ScoreSexkol"
head(SK)

# SB logBMI
SexBMI <- kslfs[,c(3), drop=FALSE]
head(SexBMI)
indx <- sapply(SexBMI, is.factor)
SexBMI[indx] <- lapply(SexBMI[indx], function(x) as.numeric(as.character(x)))
SexBMI["Mean"] <- mean(SexBMI$logBMI)
SL <- abodOutlier::abod(SexBMI, method = "randomized", n_sample_size = 169)
SL <- as.data.frame(SL)
colnames(SL) <- "ScoreSexBMI"
head(SL)
'''
**************************************************************************************************
# Markov Blanket of Smok

# BN OF SMOK
kslfsm <- ksl[c(1:300), c(1,5,6,8)]
str(galc)
kslfsm[,c(1)] <- lapply(kslfsm[,c(1)], as.factor)
indx <- sapply(kslfs, is.factor)
kslfs[indx] <- lapply(kslfs[indx], function(x) as.numeric(as.character(x)))
kslfsm.nw <- network(kslfsm)
kslfsm.prior <- jointprior(kslfsm.nw)
kslfsm.nw <- learn(kslfsm.nw,kslfsm,kslfsm.prior)$nw
kslfsm.search <- autosearch(kslfsm.nw,kslfsm,kslfsm.prior,trace=TRUE)
kslfsm.heuristic <- heuristic(kslfsm.search$nw,kslfsm,
                              kslfsm.prior,
                             restart=2,degree=10,
                             trace=TRUE,trylist=kslfsm.search$trylist)
thebestsm <- kslfsm.heuristic$nw
kslfsm[,'FEV']<-factor(kslfsm[,'FEV'])
bgsm <- hc(kslfsm)
graphviz.plot(res8)
res8 <- set.arc(bgsm, "Smok", "FEV", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)
res8 <- set.arc(res8, "Sex", "FEV", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)
res8 <- set.arc(res8, "Alc", "Smok", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)
res8 <- reverse.arc(res8, "Sex", "Smok", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)
fittedsmok <- bn.fit(res8, data = kslfsm, method = "mle")
fittedsmok$Alc

jpsmok = compile(as.grain(fittedsmok))
JPsm = querygrain(jpsmok, nodes=c("Smok","Alc"), type = "joint")

# SB sex-smok
junctionsmok = compile(as.grain(fittedsmok))
JPsmok = querygrain(junctionsmok, nodes=c("Sex","Smok"), type = "joint")
gsmok <- kslfsm[,-c(1)]
scorsmok <- mutate(gsmok, Score = ifelse(kslfsm["Smok"] == 1 & kslfsm["Sex"] == 1, "0.08666667", 
                                       ifelse(kslfsm["Smok"] == 1 & kslfsm["Sex"] == 2, "0.23333333",
                                              ifelse(kslfsm["Smok"] == 2 & kslfsm["Sex"] == 1, "0.40", "0.28"))))
scorsmok <- scorsmok[,-c(1,2)]
scorsmok <- as.data.frame(scorsmok)
colnames(scorsmok) <- "scorsmok"
scorsmok$scorsmok <- as.numeric(as.character(scorsmok$scorsmok))
head(scorsmok)
outp <- order(scorsmok, decreasing=F)[1:10]
print(outp)

# SB sex-alc
galc <- kslfsm[,c(3,4)]
scoral <- mutate(galc, Score = ifelse(kslfsm["Alc"] == 1 & kslfsm["Sex"] == 1, "0.1133333", 
                                       ifelse(kslfsm["Alc"] == 1 & kslfsm["Sex"] == 2, "0.2466667",
                                              ifelse(kslfsm["Alc"] == 2 & kslfsm["Sex"] == 1, "0.3733333", "0.2666667"))))
scoral <- scoral[,-c(1,2)]
scoral <- as.data.frame(scoral)
colnames(scoral) <- "scorea"
scoral$scorea <- as.numeric(as.character(scoral$scorea))
head(scoral)

# SB smok-alc
smalc <- kslfsm[,c(2,3)] head(smalc)
scorsmal <- mutate(smalc, Score = ifelse(kslfsm["Alc"] == 1 & kslfsm["Smok"] == 1, "0.1566667", 
                                      ifelse(kslfsm["Alc"] == 1 & kslfsm["Smok"] == 2, "0.2033333",
                                             ifelse(kslfsm["Alc"] == 2 & kslfsm["Smok"] == 1, "0.1633333", "0.4766667"))))
scorsmal <- scorsmal[,-c(1,2)]
scorsmal <- as.data.frame(scorsmal)
colnames(scorsmal) <- "smok_alc"
scorsmal$smok_alc <- as.numeric(as.character(scorsmal$smok_alc))
head(scorsmal)

# SB Smok-FEV
Smok_FEV <- kslf[,c(1,5), drop=FALSE]
head(Smok_FEV)
str(Smok_FEV)
x1 <- Smok_FEV$FEV
normalized1 = (x1-min(x1))/(max(x1)-min(x1))
ru1 <- as.data.frame(normalized)
ru1 <- cbind(ru1, train3)
head(ru1)
SmF <- abodOutlier::abod(ru1, method = "randomized", n_sample_size = 169)
SmF <- as.data.frame(SmF)

# FEV normalized (FEV and Sex)
FEVn <- kslfs[,c(1,5), drop=FALSE]
head(x)
str(FEVn)
x <- FEVn$FEV
normalized = (x-min(x))/(max(x)-min(x))
ru <- as.data.frame(normalized)
ru <- cbind(ru, train2)
head(SFn)
SFn <- abodOutlier::abod(ru, method = "randomized", n_sample_size = 169)
SFn <- as.data.frame(SFn)
colnames(SFn) <- "ScoreSexFEV"

fsm <- scorsmok + scoral + scorsmal + Fsex + fevsmok
head(fsm)
outsm <- order(fsm, decreasing=F)[1:10]
print(outsm)
print(ksl[outsm,])
'''
# SB FEV-sex
SexFEV <- kslfs[,c(1), drop=FALSE]
head(SexFEV)
indx <- sapply(SexFEV, is.factor)
SexFEV[indx] <- lapply(SexFEV[indx], function(x) as.numeric(as.character(x)))
SexFEV["Mean"] <- mean(SexFEV$FEV)
SF <- abodOutlier::abod(SexFEV, method = "randomized", n_sample_size = 169)
SF <- as.data.frame(SF)
colnames(SF) <- "ScoreSexFEV"
head(SF)
outs <- order(SF, decreasing=F)[1:10]
print(outs)
'''
*******************************************************************************************************
# Markov Blanket of Hyp

# BN OF HYP
BNhyp <- ksl[c(1:300),c(1,3,4), drop=FALSE]
kslfh <- ksl[c(1:300), c(1,3,4)]
str(BNhyp)
BNhyp[,c(1,2,3)] <- lapply(BNhyp[,c(1,2,3)], as.factor)
indx <- sapply(kslfh, is.factor)
kslfh[indx] <- lapply(kslfh[indx], function(x) as.numeric(as.character(x)))
kslfh.nw <- network(kslfh)
kslfh.prior <- jointprior(kslfh.nw)
kslfh.nw <- learn(kslfh.nw,kslfh,kslfh.prior)$nw
kslfh.search <- autosearch(kslfh.nw,kslfh,kslfh.prior,trace=TRUE)
kslfh.heuristic <- heuristic(kslfh.search$nw,kslfh,
                             kslfh.prior,
                              restart=2,degree=10,
                              trace=TRUE,trylist=kslfh.search$trylist)
thebesth <- kslfh.heuristic$nw

hypr = bnlearn::model2network(deal::modelstring(thebesth))

bghyp <- hc(BNhyp)
graphviz.plot(reshyp)
reshyp <- set.arc(reshyp, "FEV", "Hyp", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)
reshyp <- reverse.arc(hypr, "logBMI", "Hyp", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)
fittedhyp <- bn.fit(reshyp, data = BNhyp, method = "mle")
fittedhyp$Hyp

jphyp = compile(as.grain(fittedhyp))
JPhypr = querygrain(jphyp, nodes=c("Hyp","Smok"), type = "joint")

# SB Smok-Hyp
ghyp <- BNhyp[,c(1,3)]
scorhy <- mutate(ghyp, Score = ifelse(BNhyp["Hyp"] == 0 & BNhyp["Smok"] == 1, "0.1133333", 
                                      ifelse(BNhyp["Hyp"] == 0 & BNhyp["Smok"] == 2, "0.34",
                                             ifelse(BNhyp["Hyp"] == 1 & BNhyp["Smok"] == 1, "0.2066667", "0.34"))))
scorhy <- scorhy[,-c(1,2)]
scorhy <- as.data.frame(scorhy)
colnames(scorhy) <- "scorhysm"
scorhy$scorhysm <- as.numeric(as.character(scorhy$scorhysm))
head(scorhy)

# SB logBMI-Hyp
logBMIH <- ksl[c(1:300),c(3,4), drop=FALSE]
str(logBMIH)
xBH <- logBMIH$logBMI
nBH = (xBH-min(xBH))/(max(xBH)-min(xBH))
LH <- cbind(logBMIH, nBH)
str(LH)
LH <- LH[,-c(2)]
BMIhyp <- abodOutlier::abod(LH, method = "randomized", n_sample_size = 169)
BMIhyp <- as.data.frame(BMIhyp)
head(BMIhyp)
outkolBn <- order(kolBn, decreasing=F)[1:10]
print(outkolBn)

# FEV normalized (FEV and Hyp)
Fevhyp <- ksl[c(1:300),c(1,3), drop=FALSE]
head(Fevhyp)
str(Fevhyp)
fsl <- Fevhyp$FEV
nfsl = (fsl-min(fsl))/(max(fsl)-min(fsl))
nfsl <- as.data.frame(nfsl)
rfxsl <- cbind(nfsl, Fevhyp)
rfxsl <- rfxsl[,-c(2)]
head(rfxsl)
Fevhypc <- abodOutlier::abod(rfxsl, method = "randomized", n_sample_size = 169)
Fevhypc <- as.data.frame(Fevhypc)
head(Fevhypc)

# logBMI normalized (logBMI and Smok)
BMISm <- BNhyp[,c(2,3), drop=FALSE]
head(BMISm)
str(BMISm)
xsl <- BMISm$logBMI
nxsl = (xsl-min(xsl))/(max(xsl)-min(xsl))
rnxsl <- as.data.frame(nxsl)
rnxsl <- cbind(rnxsl, train3)
head(rnxsl)
SmokB <- abodOutlier::abod(rnxsl, method = "randomized", n_sample_size = 169)
SmokB <- as.data.frame(SmokB)
head(SmokB)

MBHyp <- logBhyp + Fhyp
outHYP <- order(MBHyp, decreasing=F)[1:10]
print(outHYP)
'''
# SB FEV normalized
SexFEVn <- kslfs[,c(1), drop=FALSE]
head(scorsmok)
ru <- scale(SexFEVn)
ru <- as.data.frame(ru)
ru["Mean"] <- mean(ru$FEV)
SFn <- abodOutlier::abod(ru, method = "randomized", n_sample_size = 169)
SF <- as.data.frame(SF)
colnames(SF) <- "ScoreSexFEV"
head(SF)
outsn <- order(SFn, decreasing=F)[1:10]
print(outsn)
fsmn <- scorsmok + SFn
outsmn <- order(fsmn, decreasing=F)[1:10]
print(outsmn)
write.csv(ksl,file="ksl.csv")
'''
*******************************************************************************************
# Markov Blanket of FEV

FevMB <- ksl[c(1:300),c(1,3,5,8)]
str(FevMB)
FevMB.nw <- network(FevMB)
FevMB.prior <- jointprior(FevMB.nw)
FevMB.nw <- learn(FevMB.nw,FevMB,FevMB.prior)$nw
FevMB.search <- autosearch(FevMB.nw,FevMB,FevMB.prior,trace=TRUE)
FevMB.heuristic <- heuristic(FevMB.search$nw,FevMB,
                             FevMB.prior,
                             restart=2,degree=10,
                             trace=TRUE,trylist=FevMB.search$trylist)
thebestFevMB <- FevMB.heuristic$nw

FevMB[,c(1)] <- lapply(FevMB[,c(1)], as.factor)
FevMB[1:2] <- lapply(FevMB[1:2], as.numeric)
bgFEV <- hc(FevMB)
graphviz.plot(fhy)
fhy <- drop.arc(fhy, "Smok", "Hyp")
fhy <- set.arc(fhy, "FEV", "Hyp", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)

# FEV normalized (FEV and Sex)
FEVn <- kslfs[,c(1,5), drop=FALSE]
head(FEVn)
str(FEVn)
x <- FEVn$FEV
normalized = (x-min(x))/(max(x)-min(x))
ru <- as.data.frame(normalized)
ru <- cbind(ru, train2)
head(ru)
SFn <- abodOutlier::abod(ru, method = "randomized", n_sample_size = 169)
SFn <- as.data.frame(SFn)
colnames(SFn) <- "ScoreSexFEV"
head(SFn)
outFEV <- order(SFn, decreasing=F)[1:10]
print(outFEV)

# SB Sex-Smok
junctionsex = compile(as.grain(fittedsex))
JPsex = querygrain(junctionsex, nodes=c("Sex","Smok"), type = "joint")
gsex <- kslfs[,-c(1,2,3)]
scorsex <- mutate(gsex, Score = ifelse(kslfs["Smok"] == 1 & kslfs["Sex"] == 1, "0.08666667", 
                                       ifelse(kslfs["Smok"] == 1 & kslfs["Sex"] == 2, "0.23333333",
                                              ifelse(kslfs["Smok"] == 2 & kslfs["Sex"] == 1, "0.40", "0.28"))))
scorsex <- scorsex[,-c(1,2)]
scorsex <- as.data.frame(scorsex)
colnames(scorsex) <- "scorsex"
scorsex$scorsex <- as.numeric(as.character(scorsex$scorsex))
head(scorsex)

MBFEV <- scorsex + Fsex + Fhyp + fevsmok
head(MBFEV)
outFEV <- order(MBFEV, decreasing=F)[1:10]
print(outFEV)
which(MBFEV$scorew == 1.2168752)
tu <- mean(MBFEV$scorew) - sd(MBFEV$scorew)
one <- MBFEV[MBFEV$scorew < 1.1, ]
one <- as.data.frame(one)
*******************************************************************************************
# Markov Blanket of kol

kolMB <- ksl[c(1:300),c(2,4,8), drop=FALSE]
str(kolMB)
kolMB.nw <- network(kolMB)
kolMB.prior <- jointprior(kolMB.nw)
kolMB.nw <- learn(kolMB.nw,kolMB,kolMB.prior)$nw
kolMB.search <- autosearch(kolMB.nw,kolMB,kolMB.prior,trace=TRUE)
kolMB.heuristic <- heuristic(kolMB.search$nw,kolMB,
                             kolMB.prior,
                               restart=2,degree=10,
                               trace=TRUE,trylist=kolMB.search$trylist)
thebestkolMB <- kolMB.heuristic$nw

kolMB <- ksl[c(1:300),c(2,4,8), drop=FALSE]
kolMB[1] <- lapply(kolMB[1], as.numeric)
bgKOL <- hc(kolMB)
graphviz.plot(kolarc)
kolarc <- drop.arc(fhy, "Smok", "Hyp")
kolarc <- set.arc(bgKOL, "Kol", "logBMI", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)

# Kol normalized (Kol and Sex)
KolSex <- kslfs[,c(2,5), drop=FALSE]
head(KolSex)
str(KolSex)
x3 <- KolSex$Kol
normalized3 = (x3-min(x3))/(max(x3)-min(x3))
ru3 <- as.data.frame(normalized3)
ru3 <- cbind(ru3, train2)
head(ru3)
SexKK <- abodOutlier::abod(ru3, method = "randomized", n_sample_size = 169)
SexKK <- as.data.frame(SexKK)
head(SexKK)
outkoll <- order(SexKK, decreasing=F)[1:10]
print(outkoll)

# logBMI normalized (logBMI and Sex)
BMISex <- kslfs[,c(3,5), drop=FALSE]
head(BMISex)
str(BMISex)
x4 <- BMISex$logBMI
normalized4 = (x4-min(x4))/(max(x4)-min(x4))
ru4 <- as.data.frame(normalized4)
ru4 <- cbind(ru4, train2)
head(ru4)
SexB <- abodOutlier::abod(ru4, method = "randomized", n_sample_size = 169)
SexB <- as.data.frame(SexB)
head(SexB)

MBkol <- Kolsex + lbsex + kolBn
head(MBkol)
outMBkol <- order(MBkol, decreasing=F)[1:10]
print(outMBkol)
*******************************************************************************************************
# Markov Blanket of logBMI

logBMIb <- ksl[c(1:300),c(2,3,4), drop=FALSE]
str(logBMIb)
logBMIb[,c(2)] <- lapply(logBMIb[,c(2)], as.factor)

indx <- sapply(logBMIb, is.factor)
logBMIb[indx] <- lapply(logBMIb[indx], function(x) as.numeric(as.character(x)))

logBMIb.nw <- network(logBMIb)
logBMIb.prior <- jointprior(logBMIb.nw)
logBMIb.nw <- learn(logBMIb.nw,logBMIb,logBMIb.prior)$nw
logBMIb.search <- autosearch(logBMIb.nw,logBMIb,logBMIb.prior,trace=TRUE)
logBMIb.heuristic <- heuristic(logBMIb.search$nw,logBMIb,
                               logBMIb.prior,
                             restart=2,degree=10,
                             trace=TRUE,trylist=logBMIb.search$trylist)
thebestBMI <- logBMIb.heuristic$nw
lBMI = bnlearn::model2network(deal::modelstring(thebestBMI))

logBMIb <- ksl[c(1:300),c(1,2,3,4,8)]
logBMIb[1:3] <- lapply(logBMIb[1:3], as.numeric)
bgBMI <- hc(logBMIb)
graphviz.plot(lbarc)
lbarc <- set.arc(bgBMI, "Kol", "logBMI", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)
lbarc <- set.arc(lbarc, "FEV", "Hyp", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)
fittedBMI <- bn.fit(res8, data = kslfsm, method = "mle")
fittedsex$FEV

# SB kol-logBMI
kolBMI <- kslfs[,c(2,3), drop=FALSE]
str(kolBMI)
indx <- sapply(kolBMI, is.factor)
kolBMI[indx] <- lapply(kolBMI[indx], function(x) as.numeric(as.character(x)))
xB <- kolBMI$logBMI
nB = (xB-min(xB))/(max(xB)-min(xB))
xk <- kolBMI$Kol
nk = (xk-min(xk))/(max(xk)-min(xk))
KBk <- as.data.frame(nB)
KBB <- as.data.frame(nk)
KB <- cbind(KBk, KBB)
head(KB)
kolBn <- abodOutlier::abod(KB, method = "randomized", n_sample_size = 169)
kolBn <- as.data.frame(kolBn)
colnames(kolBn) <- "ScorekolBMI"
head(kolBn)
outkolBn <- order(kolBn, decreasing=F)[1:10]
print(outkolBn)

# SB logBMI-Hyp
logBMIH <- ksl[c(1:300),c(3,4), drop=FALSE]
str(logBMIH)
xBH <- logBMIH$logBMI
nBH = (xBH-min(xBH))/(max(xBH)-min(xBH))
LH <- cbind(logBMIH, nBH)
str(LH)
LH <- LH[,-c(2)]
BMIhyp <- abodOutlier::abod(LH, method = "randomized", n_sample_size = 169)
BMIhyp <- as.data.frame(BMIhyp)
head(BMIhyp)
outlh <- order(BMIhyp, decreasing=F)[1:10]
print(outlh)

LHK <- Fsex + Kolsex + lbsex + kolBn + logBhyp + Fhyp
head(LHK)
outLHK <- order(LHK, decreasing=F)[1:10]
print(outLHK)
print(ksl[outsm,])
***********************************************************************************************
# Markov Blanket of Alc

MBalc <- ksl[c(1:300),c(5,6,7,8), drop=FALSE]
str(MBalc)
MBalc[,c(1:4)] <- lapply(MBalc[,c(1:4)], as.factor)
bgalc <- hc(MBalc)
graphviz.plot(resalc)
resalc <- set.arc(resalc, "Alc", "Work", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)
fittedalc <- bn.fit(resalc, data = MBalc, method = "mle")
fittedalc$Smok

jpalc = compile(as.grain(fittedalc))
JPalch = querygrain(jpalc, nodes=c("Alc","Smok"), type = "joint")

# SB Alc-Work
alcw <- ksl[c(1:300),c(6,7)]
head(scoralw)
scoralw <- mutate(alcw, Score = ifelse(alcw["Alc"] == 1 & alcw["Work"] == 1, "0.01333333", 
                                     ifelse(alcw["Alc"] == 1 & alcw["Work"] == 2, "0.3466667",
                                            ifelse(alcw["Alc"] == 2 & alcw["Work"] == 1, "0.06333333", "0.5766667"))))
scoralw <- scoralw[,-c(1,2)]
scoralw <- as.data.frame(scoralw)
colnames(scoralw) <- "scorealcw"
scoralw$scorealcw <- as.numeric(as.character(scoralw$scorealcw))
head(scoralw)

# SB Sex-Alc
sexalc <- ksl[c(1:300),c(6,8)]
scoral <- mutate(sexalc, Score = ifelse(sexalc["Alc"] == 1 & sexalc["Sex"] == 1, "0.1133333", 
                                        ifelse(sexalc["Alc"] == 1 & sexalc["Sex"] == 2, "0.2466667",
                                               ifelse(sexalc["Alc"] == 2 & sexalc["Sex"] == 1, "0.3733333", "0.2666667"))))
scoral <- scoral[,-c(1,2)]
scoral <- as.data.frame(scoral)
colnames(scoral) <- "scoralcsex"
scoral$scoralcsex <- as.numeric(as.character(scoral$scoralcsex))
head(scoral)

# SB Sex-Work
sexw <- ksl[c(1:300),c(7,8)]
scorw <- mutate(sexw, Score = ifelse(sexw["Work"] == 1 & sexw["Sex"] == 1, "0.06000000", 
                                     ifelse(sexw["Work"] == 1 & sexw["Sex"] == 2, "0.01666667",
                                            ifelse(sexw["Work"] == 2 & sexw["Sex"] == 1, "0.4266667", "0.4966667"))))
scorw <- scorw[,-c(1,2)]
scorw <- as.data.frame(scorw)
colnames(scorw) <- "scorew"
scorw$scorew <- as.numeric(as.character(scorw$scorew))
head(scorw)

# SB sex-smok
junctionsmok = compile(as.grain(fittedsmok))
JPsmok = querygrain(junctionsmok, nodes=c("Sex","Smok"), type = "joint")
gsmok <- kslfsm[,-c(1)]
scorsmok <- mutate(gsmok, Score = ifelse(kslfsm["Smok"] == 1 & kslfsm["Sex"] == 1, "0.08666667", 
                                         ifelse(kslfsm["Smok"] == 1 & kslfsm["Sex"] == 2, "0.23333333",
                                                ifelse(kslfsm["Smok"] == 2 & kslfsm["Sex"] == 1, "0.40", "0.28"))))
scorsmok <- scorsmok[,-c(1,2)]
scorsmok <- as.data.frame(scorsmok)
colnames(scorsmok) <- "scorsmok"
scorsmok$scorsmok <- as.numeric(as.character(scorsmok$scorsmok))
head(scorsmok)

# SB Alc-Smok
alcsm <- ksl[c(1:300),c(5,6)]
scoralcsm <- mutate(alcsm, Score = ifelse(alcsm["Smok"] == 1 & alcsm["Alc"] == 1, "0.1566667", 
                                     ifelse(alcsm["Smok"] == 1 & alcsm["Alc"] == 2, "0.1633333",
                                            ifelse(alcsm["Smok"] == 2 & alcsm["Alc"] == 1, "0.2033333", "0.4766667"))))
scoralcsm <- scoralcsm[,-c(1,2)]
scoralcsm <- as.data.frame(scoralcsm)
colnames(scoralcsm) <- "scoralcsm"
scoralcsm$scoralcsm <- as.numeric(as.character(scoralcsm$scoralcsm))
head(scoralcsm)

# Same as Smok
MBALCH <- scoralw + scoral + scorw + scorsmok + scoralcsm
head(MBALCH)
mean(MBALCH$scorealcw) sd(MBALCH$scorealcw)
outalc <- order(MBALCH, decreasing=F)[1:10]
print(outalc)
***************************************************************************************************
# Markov Blanket of Work

W <- ksl[c(1:300),c(1,7,8), drop=FALSE]
str(W)
W[,c(2,3)] <- lapply(W[,c(2,3)], as.factor)

W.nw <- network(W)
W.prior <- jointprior(W.nw)
W.nw <- learn(W.nw,W,W.prior)$nw
W.search <- autosearch(W.nw,W,W.prior,trace=TRUE)
W.heuristic <- heuristic(W.search$nw,W,
                         W.prior,
                               restart=2,degree=10,
                               trace=TRUE,trylist=W.search$trylist)
thebestWo <- W.heuristic$nw
W[,'FEV']<-factor(W[,'FEV'])
lW = bnlearn::model2network(deal::modelstring(thebestWo))

W <- ksl[c(1:300),c(6,7,8)]
bgWORK <- hc(W)
graphviz.plot(warc)
warc <- set.arc(bgWORK, "Alc", "Work", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)
warc <- reverse.arc(warc, "Alc", "Sex", check.cycles = TRUE, check.illegal = TRUE, debug = FALSE)
fittedwork <- bn.fit(warc, data = W, method = "mle")
fittedsex$FEV

jpwork = compile(as.grain(fittedwork))
JPwo = querygrain(jpwork, nodes=c("Sex","Work"), type = "joint")

# SB Sex-Work
sexw <- ksl[c(1:300),c(7,8)]
head(sexw)
scorw <- mutate(sexw, Score = ifelse(sexw["Work"] == 1 & sexw["Sex"] == 1, "0.06000000", 
                                     ifelse(sexw["Work"] == 1 & sexw["Sex"] == 2, "0.01666667",
                                            ifelse(sexw["Work"] == 2 & sexw["Sex"] == 1, "0.4266667", "0.4966667"))))
scorw <- scorw[,-c(1,2)]
scorw <- as.data.frame(scorw)
colnames(scorw) <- "scorew"
scorw$scorew <- as.numeric(as.character(scorw$scorew))
head(scorw)

# FEV normalized (FEV and Sex)
FEVn <- kslfs[,c(1,5), drop=FALSE]
head(FEVn)
str(sexalc)
x <- FEVn$FEV
normalized = (x-min(x))/(max(x)-min(x))
ru <- as.data.frame(normalized)
ru <- cbind(ru, train2)
head(ru)
SFn <- abodOutlier::abod(ru, method = "randomized", n_sample_size = 169)
SFn <- as.data.frame(SFn)
colnames(SFn) <- "ScoreSexFEV"
head(SFn)

MBwork <- scorw + scoral + scoralw
min(MBwork)
which(MBwork==min(MBwork))
outwork <- order(MBwork, decreasing=F)[1:10]
print(outwork)