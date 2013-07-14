### R code from vignette source 'gRain-evidence.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: gRain-evidence.Rnw:25-29
###################################################
require( gRain )
prettyVersion <- packageDescription("gRain")$Version
prettyDate <- format(Sys.Date())
dir.create( "figures" )


###################################################
### code chunk number 2: gRain-evidence.Rnw:112-121
###################################################
yn <- c("yes","no")
a    <- cptable(~asia, values=c(1,99),levels=yn)
t.a  <- cptable(~tub|asia, values=c(5,95,1,99),levels=yn)

( plist1 <- compileCPT( list( a, t.a ) ) )
plist1[[1]]
plist1[[2]]
( chest1 <- grain(plist1) )
querygrain( chest1 )


###################################################
### code chunk number 3: gRain-evidence.Rnw:135-138
###################################################
setFinding(  chest1, nodes="asia", states="yes")
setEvidence( chest1, nodes="asia", states="yes")
setEvidence( chest1, nslist=list(asia="yes"))


###################################################
### code chunk number 4: gRain-evidence.Rnw:142-143
###################################################
querygrain( setEvidence( chest1, nslist=list(asia="yes")) )


###################################################
### code chunk number 5: gRain-evidence.Rnw:165-167
###################################################
g.a <- parray(c("guess.asia", "asia"), levels=list(yn, yn), 
              values=c(.8,.2, .1,.9)) 


###################################################
### code chunk number 6: gRain-evidence.Rnw:176-179
###################################################
( plist2 <- compileCPT( list( a, t.a, g.a ) ) )
( chest2 <- grain(plist2) )
querygrain( chest2 )


###################################################
### code chunk number 7: gRain-evidence.Rnw:187-188
###################################################
querygrain( setEvidence( chest2, nslist=list(guess.asia="yes")) )


###################################################
### code chunk number 8: gRain-evidence.Rnw:198-199
###################################################
querygrain( setEvidence( chest1, nslist=list(asia=c(.8, .1))) )


###################################################
### code chunk number 9: gRain-evidence.Rnw:205-206
###################################################
querygrain( setEvidence( chest1, nslist=list(asia=c(1, 0))) )


