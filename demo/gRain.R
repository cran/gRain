### chunk number 1: 
library(gRain)
options("prompt"=" ","width"=100)


### chunk number 2: 
yn <- c("yes","no")
a    <- cpt(~asia, values=c(1,99),levels=yn)
t.a  <- cpt(~tub+asia, values=c(5,95,1,99),levels=yn)
s    <- cpt(~smoke, values=c(5,5), levels=yn)
l.s  <- cpt(~lung+smoke, values=c(1,9,1,99), levels=yn)
b.s  <- cpt(~bronc+smoke, values=c(6,4,3,7), levels=yn)
e.lt <- cpt(~either+lung+tub,values=c(1,0,1,0,1,0,0,1),levels=yn)
x.e  <- cpt(~xray+either, values=c(98,2,5,95), levels=yn)
d.be <- cpt(~dysp+bronc+either, values=c(9,1,7,3,8,2,1,9), levels=yn)


### chunk number 3: 
plist <- cptspec(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
pn <- newgmInstance(plist)
pn


### chunk number 4: 
querygm(pn,nodes=c("lung","bronc"))


### chunk number 5: 
pn2  <- enterEvidence(pn,nodes=c("asia","dysp"),states=c("yes","yes"))


### chunk number 6: 
querygm(pn2,nodes=c("lung","bronc"))


### chunk number 7: 
querygm(pn2,nodes=c("lung","bronc"), type="joint")


### chunk number 8: 
pnc <- compilegm(pn)


### chunk number 9: 
pnc <- propagate(pnc)


### chunk number 10: 
querygm(pnc,nodes=c("lung","bronc"))
querygm(pnc,nodes=c("lung","bronc"),type="joint")
querygm(pnc,nodes=c("lung","bronc"),type="conditional")


### chunk number 11: 
pnc2  <- enterEvidence(pnc,nodes=c("asia","dysp"),states=c("yes","yes"))
pnc2  <- enterEvidence(pnc,evlist=list(c("asia","yes"),c("dysp", "yes")))


### chunk number 12: 
evidence(pnc2)


### chunk number 13: 
pevidence(pnc2)


### chunk number 14: 
querygm(pnc2,nodes=c("lung","bronc"))
querygm(pnc2,nodes=c("lung","bronc"), type="joint")
querygm(pnc2,nodes=c("lung","bronc"), type="conditional")


### chunk number 15: 
pnc2  <- enterEvidence(pnc,nodes=c("asia"),states=c("yes"),propagate=FALSE)
##evidence(pnc2)
pnc2  <- enterEvidence(pnc2,nodes=c("dysp"),states=c("yes"),propagate=FALSE)
##evidence(pnc2)
pnc2 <- propagate(pnc2)
evidence(pnc2)


### chunk number 16: 
pnc3 <- retractEvidence(pnc2, nodes="asia")
evidence(pnc3)


### chunk number 17: 
summary(pn)
summary(pnc)


### chunk number 18: 
plot(pn)
plot(pnc)


### chunk number 19: 
nodeNames(pn)
nodeStates(pn)


### chunk number 20: 
pnc2 <- compilegm(pn, root=c("lung", "bronc", "tub"), propagate=TRUE)


### chunk number 21: 
system.time({for (i in 1:10) querygm(pnc,nodes=c("lung","bronc", "tub"),type="joint")})
system.time({for (i in 1:10) querygm(pnc2,nodes=c("lung","bronc", "tub"),type="joint")})


### chunk number 22: 
simulate(pnc, nsim=20)
simulate(pnc2, nsim=20)


### chunk number 23: 
nd <- structure(list(bronc = structure(c(1L, 1L, 1L, 1L), .Label = c("yes", 
"no"), class = "factor"), dysp = structure(c(1L, 1L, 1L, 1L), .Label = c("yes", 
"no"), class = "factor"), either = structure(c(1L, 1L, 1L, 2L
), .Label = c("yes", "no"), class = "factor"), lung = structure(c(1L, 
1L, 2L, 2L), .Label = c("yes", "no"), class = "factor"), tub = structure(c(2L, 
2L, 1L, 2L), .Label = c("yes", "no"), class = "factor"), asia = structure(c(2L, 
2L, 2L, 1L), .Label = c("yes", "no"), class = "factor"), xray = structure(c(1L, 
1L, 1L, 1L), .Label = c("yes", "no"), class = "factor"), smoke = structure(c(1L, 
2L, 1L, 2L), .Label = c("yes", "no"), class = "factor")), .Names = c("bronc", 
"dysp", "either", "lung", "tub", "asia", "xray", "smoke"), row.names = c(NA, 
4L), class = "data.frame")


### chunk number 24: 
nd
predict(pnc, response=c("lung","bronc"), newdata=nd, 
 predictors=c("smoke", "asia", "tub" , "dysp", "xray"), type="class")


### chunk number 25: 
predict(pnc, response=c("lung","bronc"), newdata=nd,
 predictors=c("smoke", "asia", "tub" , "dysp", "xray"), type="dist")


### chunk number 26: 
chestNames <- c("asia", "smoke", "tub", "lung", "bronc", "either", "xray", "dysp")
gmd <- newgmData(chestNames,valueLabels=c("yes","no"))
gmd


### chunk number 27: 
a    <- cpt(~asia, values=c(1,99),gmData=gmd)
t.a  <- cpt(~tub+asia, values=c(5,95,1,99),gmData=gmd)
s    <- cpt(~smoke, values=c(5,5), gmData=gmd)
l.s  <- cpt(~lung+smoke, values=c(1,9,1,99), gmData=gmd)
b.s  <- cpt(~bronc+smoke, values=c(6,4,3,7), gmData=gmd)
e.lt <- cpt(~either+lung+tub,values=c(1,0,1,0,1,0,0,1),gmData=gmd)
x.e  <- cpt(~xray+either, values=c(98,2,5,95), gmData=gmd)
d.be <- cpt(~dysp+bronc+either, values=c(9,1,7,3,8,2,1,9), gmData=gmd)


### chunk number 28: 
t.a


### chunk number 29: 
plist <- cptspec(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))


### chunk number 30: 
pn <- newgmInstance(plist, gmData=gmd)


### chunk number 31: 
chestSim <- simulate(pnc, nsim=1000)
chestSsim <- as.cumcounts(chestSim, Freq="Freq")
chestSim[1:10,]


### chunk number 32: 
g<-list(~asia, ~tub + asia, ~smoke, ~lung + smoke, ~bronc + smoke, 
    ~either + lung + tub, ~xray + either, ~dysp + bronc + either)
dag<-newdagsh(g)
dag


### chunk number 33: 
pnx <- newgmInstance(dag, gmData=as.gmData(chestSim))
pnx <- compilegm(pnx, propagate=TRUE)


### chunk number 34: 
g<-list(~asia + tub, ~either + lung + tub, ~either + lung + smoke, 
    ~bronc + either + smoke, ~bronc + dysp + either, ~either + 
        xray)
ug <- newugsh(g)
ug


### chunk number 35: 
pny <- newgmInstance(ug, as.gmData(chestSim))
pny <- compilegm(pny, propagate=TRUE)


