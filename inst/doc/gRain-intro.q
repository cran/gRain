### chunk number 1: 
library(gRain)
options("prompt"="> ","width"=85)


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
in1 <- newgmInstance(plist)
in1


### chunk number 4: 
querygm(in1,nodes=c("lung","bronc"), type="marginal")


### chunk number 5: 
querygm(in1,nodes=c("lung","bronc"), type="joint")


### chunk number 6: 
srcit()
in12  <- enterEvidence(in1,nodes=c("asia","dysp"),states=c("yes","yes"))


### chunk number 7: 
querygm(in12,nodes=c("lung","bronc"))
querygm(in12,nodes=c("lung","bronc"), type="joint")


