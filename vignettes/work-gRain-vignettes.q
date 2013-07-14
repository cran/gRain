library(gRain)

yn <- c("yes","no")
a    <- cptable(~asia, values=c(1,99),levels=yn)
t.a  <- cptable(~tub|asia, values=c(5,95,1,99),levels=yn)

plist1 <- compileCPT( list( a, t.a ) )
plist1
chest1 <- grain(plist1)
chest1

querygrain( chest1 )



ch1a <- setEvidence(chest1, c("asia"), c("yes"))
ch1b <- setFinding(chest1, c("asia"), c("yes"))



summary(ch1a)
summary(ch1b)


g.a <- cptable(~guess.asia|asia, values=c(.8,.2, .1,.9), levels=yn)
plist2 <- compileCPT( list( a, t.a, g.a ) )
plist2
chest2 <- grain(plist2)
chest2

ch2a <- setEvidence(chest2, "guess.asia", "yes")
querygrain( ch2a, )

ch1a <- setEvidence(chest1, "asia", list(c(.8,.1)))
querygrain( ch1a, nodes="tub" )


g.a <- parray(c("guess.asia", "asia"), levels=list(yn, yn), values=c(.8,.2, .1,.9))
