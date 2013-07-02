### R code from vignette source 'gRain-intro.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: gRain-intro.Rnw:31-35
###################################################
require( gRain )
prettyVersion <- packageDescription("gRain")$Version
prettyDate <- format(Sys.Date())
dir.create( "figures" )


###################################################
### code chunk number 2: gRain-intro.Rnw:111-124
###################################################
yn <- c("yes","no")
a    <- cptable(~asia, values=c(1,99),levels=yn)
t.a  <- cptable(~tub|asia, values=c(5,95,1,99),levels=yn)
s    <- cptable(~smoke, values=c(5,5), levels=yn)
l.s  <- cptable(~lung|smoke, values=c(1,9,1,99), levels=yn)
b.s  <- cptable(~bronc|smoke, values=c(6,4,3,7), levels=yn)
e.lt <- cptable(~either|lung:tub,values=c(1,0,1,0,1,0,0,1),levels=yn)
x.e  <- cptable(~xray|either, values=c(98,2,5,95), levels=yn)
d.be <- cptable(~dysp|bronc:either, values=c(9,1,7,3,8,2,1,9), levels=yn)
plist <- compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
plist
net1 <- grain(plist)
net1


###################################################
### code chunk number 3: LS
###################################################
plot(net1)


###################################################
### code chunk number 4: gRain-intro.Rnw:203-205
###################################################
library(gRain)
options("prompt"="> ","width"=85)


###################################################
### code chunk number 5: gRain-intro.Rnw:242-251
###################################################
yn <- c("yes","no")
a    <- cptable(~asia, values=c(1,99),levels=yn)
t.a  <- cptable(~tub|asia, values=c(5,95,1,99),levels=yn)
s    <- cptable(~smoke, values=c(5,5), levels=yn)
l.s  <- cptable(~lung|smoke, values=c(1,9,1,99), levels=yn)
b.s  <- cptable(~bronc|smoke, values=c(6,4,3,7), levels=yn)
e.lt <- cptable(~either|lung:tub,values=c(1,0,1,0,1,0,0,1),levels=yn)
x.e  <- cptable(~xray|either, values=c(98,2,5,95), levels=yn)
d.be <- cptable(~dysp|bronc:either, values=c(9,1,7,3,8,2,1,9), levels=yn)


###################################################
### code chunk number 6: gRain-intro.Rnw:257-261
###################################################
plist <- compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
plist
net1 <- grain(plist)
net1


###################################################
### code chunk number 7: gRain-intro.Rnw:273-274
###################################################
querygrain(net1,nodes=c("lung","bronc"), type="marginal")


###################################################
### code chunk number 8: gRain-intro.Rnw:280-281
###################################################
querygrain(net1,nodes=c("lung","bronc"), type="joint")


###################################################
### code chunk number 9: gRain-intro.Rnw:287-288
###################################################
net12  <- setFinding(net1,nodes=c("asia","dysp"),states=c("yes","yes"))


###################################################
### code chunk number 10: gRain-intro.Rnw:294-296
###################################################
querygrain(net12,nodes=c("lung","bronc"))
querygrain(net12,nodes=c("lung","bronc"), type="joint")


###################################################
### code chunk number 11: gRain-intro.Rnw:304-305
###################################################
net13  <- setFinding(net1,nodes=c("either","tub"),states=c("no","yes"))


###################################################
### code chunk number 12: gRain-intro.Rnw:310-311
###################################################
pFinding(net13)


###################################################
### code chunk number 13: gRain-intro.Rnw:317-318
###################################################
querygrain(net13,nodes=c("lung","bronc"), type="joint")


