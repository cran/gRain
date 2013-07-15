### R code from vignette source 'gRain-intro.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: gRain-intro.Rnw:25-29
###################################################
require( gRain )
prettyVersion <- packageDescription("gRain")$Version
prettyDate <- format(Sys.Date())
dir.create( "figures" )


###################################################
### code chunk number 2: gRain-intro.Rnw:105-118
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
### code chunk number 4: gRain-intro.Rnw:197-199
###################################################
library(gRain)
options("prompt"="> ","width"=85)


###################################################
### code chunk number 5: gRain-intro.Rnw:236-245
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
### code chunk number 6: gRain-intro.Rnw:251-257
###################################################
plist <- compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
plist
plist$tub    
plist$either ## Notice: a logical node
net1 <- grain(plist)
net1


###################################################
### code chunk number 7: gRain-intro.Rnw:269-270
###################################################
querygrain(net1, nodes=c("lung","bronc"), type="marginal")


###################################################
### code chunk number 8: gRain-intro.Rnw:276-277
###################################################
querygrain(net1,nodes=c("lung","bronc"), type="joint")


###################################################
### code chunk number 9: gRain-intro.Rnw:282-285
###################################################
net12  <- setEvidence(net1, 
                      nodes=c("asia", "dysp"), states=c("yes", "yes"))
net12  <- setEvidence(net1, nslist=list(asia="yes", dysp="yes"))


###################################################
### code chunk number 10: gRain-intro.Rnw:291-293
###################################################
querygrain( net12, nodes=c("lung","bronc") )
querygrain( net12, nodes=c("lung","bronc"), type="joint" )


###################################################
### code chunk number 11: gRain-intro.Rnw:301-303
###################################################
net13 <- setEvidence(net1,nodes=c("either", "tub"), 
                     states=c("no","yes"))


###################################################
### code chunk number 12: gRain-intro.Rnw:308-309
###################################################
pEvidence( net13 )


###################################################
### code chunk number 13: gRain-intro.Rnw:315-316
###################################################
querygrain( net13, nodes=c("lung","bronc"), type="joint" )


