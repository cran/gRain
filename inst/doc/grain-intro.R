## ----include=FALSE,echo=FALSE,warning=FALSE----------------------------------------
library(knitr)
dir.create("figures")
opts_chunk$set(fig.height=2.5,
               fig.path='figures/grain-',
               warning=FALSE, message=FALSE
)
options("prompt"="> ","width"=85)

## ----echo=FALSE--------------------------------------------------------------------
require(gRain)
prettyVersion <- packageDescription("gRain")$Version
prettyDate <- format(Sys.Date())

## ----------------------------------------------------------------------------------
citation("gRain")

## ----echo=F, results='hide'--------------------------------------------------------
yn <- c("yes","no")
a    <- cpt(~asia, values=c(1,99),levels=yn)
t.a  <- cpt(~tub|asia, values=c(5,95,1,99),levels=yn)
s    <- cpt(~smoke, values=c(5,5), levels=yn)
l.s  <- cpt(~lung|smoke, values=c(1,9,1,99), levels=yn)
b.s  <- cpt(~bronc|smoke, values=c(6,4,3,7), levels=yn)
e.lt <- cpt(~either|lung:tub,values=c(1,0,1,0,1,0,0,1),levels=yn)
x.e  <- cpt(~xray|either, values=c(98,2,5,95), levels=yn)
d.be <- cpt(~dysp|bronc:either, values=c(9,1,7,3,8,2,1,9), levels=yn)
plist <- compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
plist
chest_bn <- grain(plist, compile=FALSE)
chest_bn

## ----chest-LS, echo=F, fig.cap="Chest clinic example from Lauritzen and Spiegelhalter (1988)."----
plot(chest_bn)

## ----------------------------------------------------------------------------------
yn <- c("yes","no")
a    <- cpt(~asia, values=c(1, 99),levels=yn)
t.a  <- cpt(~tub|asia, values=c(5, 95, 1, 99),levels=yn)
s    <- cpt(~smoke, values=c(5, 5), levels=yn)
l.s  <- cpt(~lung|smoke, values=c(1, 9, 1, 99), levels=yn)
b.s  <- cpt(~bronc|smoke, values=c(6, 4, 3, 7), levels=yn)
e.lt <- cpt(~either|lung:tub,values=c(1, 0, 1, 0, 1, 0, 0, 1),levels=yn)
x.e  <- cpt(~xray|either, values=c(98, 2, 5, 95), levels=yn)
d.be <- cpt(~dysp|bronc:either, values=c(9, 1, 7, 3, 8, 2, 1, 9), levels=yn)

## ----------------------------------------------------------------------------------
chest_cpt <- compileCPT(a, t.a, s, l.s, b.s, e.lt, x.e, d.be)
chest_cpt

## ----------------------------------------------------------------------------------
chest_cpt$tub
chest_cpt$tub  |> as.data.frame.table()

## ----------------------------------------------------------------------------------
chest_cpt$either  |> as.data.frame.table()

## ----------------------------------------------------------------------------------
chest_bn <- grain(chest_cpt)
chest_bn

## ----------------------------------------------------------------------------------
chest_bn <- compile(chest_bn)

## ----------------------------------------------------------------------------------
querygrain(chest_bn, nodes=c("lung", "bronc"), type="marginal")

## ----------------------------------------------------------------------------------
querygrain(chest_bn, nodes=c("lung", "bronc"), type="joint")

## ----------------------------------------------------------------------------------
chest_ev  <- setEvidence(chest_bn,
                          evidence=list(asia="yes", dysp="yes"))
chest_ev  <- setEvidence(chest_bn,
                          nodes=c("asia", "dysp"), states=c("yes", "yes"))

## Also: modify object with
## evidence(chest_bn) <- list(asia="yes", dysp="yes")

## ----------------------------------------------------------------------------------
getEvidence(chest_ev) |> as.data.frame()

## ----------------------------------------------------------------------------------
querygrain(chest_ev, nodes=c("lung", "bronc"))

## ----------------------------------------------------------------------------------
pEvidence(chest_ev)

## ----------------------------------------------------------------------------------
pEvidence(chest_bn, evidence=list(asia="yes", dysp="yes"))

## ----------------------------------------------------------------------------------
yn <- c("yes","no")
flist <- c(
    ~asia, ~tub|asia, ~smoke, ~lung|smoke, ~bronc|smoke, ~either|lung:tub,
    ~xray|either, ~dysp|bronc:either
)
## or
flist <- list("asia", c("tub", "asia"), "smoke", c("lung", "smoke"),
              c("bronc", "smoke"), c("either", "tub", "lung"),
              c("xray", "either"), c("dysp", "bronc", "either"))

chest_cpt2 <- lapply(flist, function(f){
    cpt(f, levels=yn)
})

bn2 <- compileCPT(chest_cpt2) |> grain()

lst2 <- list(asia=c(1, 99),
            tub=c(5, 95, 1, 99),
            smoke=c(5, 5),
            lung=c(1, 9, 1, 99),            
            bronc=c(6, 4, 3, 7),
            either=c(1, 0, 1, 0, 1, 0, 0, 1),
            xray=c(98, 2, 5, 95),
            dysp=c(9, 1, 7, 3, 8, 2, 1, 9))

bn2 <- replaceCPT(bn2, lst2)

## ----------------------------------------------------------------------------------
querygrain(chest_bn, nodes=c("lung", "bronc"), simplify = TRUE)

## ----------------------------------------------------------------------------------
querygrain(chest_bn, nodes=c("lung", "bronc"), result="data.frame")

## ----------------------------------------------------------------------------------
querygrain(chest_bn,
           evidence=list(asia="yes", dysp="yes"),
           nodes=c("lung", "bronc"), simplify = TRUE)

## ----------------------------------------------------------------------------------
querygrain(chest_bn,
           evidence=list(asia=c(1,0), dysp=c(1,0)),
           nodes=c("lung", "bronc"), simplify = TRUE)

## ----------------------------------------------------------------------------------
querygrain(chest_bn,
           evidence=list(asia=c(1, 0), dysp=c(1, 0)),
           nodes=c("lung", "bronc", "asia", "dysp"),
           exclude=FALSE, simplify = TRUE)

## ----------------------------------------------------------------------------------
querygrain(chest_bn,
           evidence=list(asia="yes", dysp="yes"),
           simplify = TRUE)

## ----------------------------------------------------------------------------------
querygrain(chest_bn,
           evidence=list(asia="yes", dysp="yes"),
           exclude = FALSE, simplify = TRUE)

## ----------------------------------------------------------------------------------
chest_bn3 <- setEvidence(chest_bn, evidence=list(either="no", tub="yes"))

## ----------------------------------------------------------------------------------
pEvidence(chest_bn3)
querygrain(chest_bn3, nodes=c("lung", "bronc"), type="joint")

## ----------------------------------------------------------------------------------
yn <- c("yes", "no")
a    <- cpt(~asia, values=c(1, 99), levels=yn)
t.a  <- cpt(~tub|asia, values=c(5, 95, 1, 99), levels=yn)

plist1 <- compileCPT(list(a, t.a))
chest1 <- grain(plist1)
querygrain(chest1, simplify = TRUE) 

## ----------------------------------------------------------------------------------
setEvidence(chest1, evidence=list(asia="yes"))

## ----------------------------------------------------------------------------------
g.a <- cpt(~guess_asia+asia, levels=yn,
              values=c(.8, .2, .1, .9))

## ----------------------------------------------------------------------------------
plist2 <- compileCPT(list(a, t.a, g.a ))
chest2 <- grain(plist2)
querygrain(chest2, simplify = TRUE)

## ----------------------------------------------------------------------------------
querygrain(chest2, evidence=list(guess_asia="yes"),
           simplify=TRUE, exclude = FALSE)

## ----------------------------------------------------------------------------------
chest1_ve <- chest1 |> setEvidence(evidence=list(asia=c(.8, .1)))
chest1_ve |> querygrain(simplify = TRUE)
getEvidence(chest1_ve, short=FALSE)

## ----------------------------------------------------------------------------------
querygrain(chest1, evidence=list(asia=c(1, 0)), simplify=T)

## ----------------------------------------------------------------------------------
plot(dag(~y1:x1 + x2:x1 + y2:x2))

## ----------------------------------------------------------------------------------
u <- list(x1=yn, x2=yn)
x1 <- cpt(~x1, values=c(1, 3), levels=yn)
x2 <- cpt(~x2|x1, values=c(1, 3, 3, 1), levels=yn)
bn <- grain(compileCPT(x1, x2))
querygrain(bn, simplify=TRUE)

## ----------------------------------------------------------------------------------
v <- 2
mu <- c(mu1=2, mu2=5)
lambda <- c(lambda1=0, lambda2=7)

## ----------------------------------------------------------------------------------
y1 <- 1 # Observed value for y1
lik1 <- dnorm(y1, mean=mu, sd=sqrt(v))
querygrain(bn, exclude = FALSE,
           evidence=list(x1=lik1), simplify = TRUE) 

## ----------------------------------------------------------------------------------
x1_upd <- getgrain(bn, "cptlist")$x1 * lik1
bn2 <- replaceCPT(bn, list(x1=x1_upd))
querygrain(bn2) 

## ----------------------------------------------------------------------------------
set.seed(2022)
nsim <- 1000
xsim1 <- simulate(bn, nsim)
head(xsim1)
xsim2 <- simulate(bn2, nsim)
head(xsim2)

par(mfrow=c(1,2))
y2sim <- rpois(n=nsim, lambda=lambda[xsim1$x2])
y22sim <- rpois(n=nsim, lambda=lambda[xsim2$x2])
y2sim |> hist(prob=T, ylim=c(0, .4), breaks=10)
y22sim |> hist(prob=T, ylim=c(0, .4), breaks=10)

## ----------------------------------------------------------------------------------
dG  <- dag(~A:B + B:C)
uG  <- ug(~A:B + B:C)
par(mfrow=c(1,2)); plot( dG ); plot( uG )

## ----------------------------------------------------------------------------------
dat <- tabNew(c("A", "B", "C"), levels=c("lev1", "lev2"), #levels=c(2,2,2),
              values=c(0, 0, 2, 3, 1, 2, 1, 4))
class(dat)

## ----------------------------------------------------------------------------------
gr.dG <- compile(grain( dG, data=dat ))
gr.uG <- compile(grain( uG, data=dat ))

## ----------------------------------------------------------------------------------
extractCPT(dat, dG) |> c() ## FIXME: Printing problem
extractPOT(dat, uG) |> c() ## FIXME: Printing problem

## ----------------------------------------------------------------------------------
p.A_B <- tabDiv(dat, tabMarg(dat, "B")) ## p(A|B)
p.B   <- tabMarg(dat, "B") / sum(dat)   ## p(B)
p.AB2 <- tabMult(p.A_B, p.B)            ## P(AB)

## ----------------------------------------------------------------------------------
e <- 1e-2
(dat.e <- dat + e)

## ----------------------------------------------------------------------------------
pe.A_B <- tabDiv(dat.e, tabMarg(dat.e, "B"))
pe.B   <- tabMarg(dat.e, "B") / sum(dat.e)
pe.AB  <- tabMult(pe.A_B, pe.B)


## ----------------------------------------------------------------------------------
dat2.e <- dat.e / sum(dat.e)

## ----------------------------------------------------------------------------------
(dat2.e - pe.AB)  |> ftable()

## ----------------------------------------------------------------------------------

gr.dG <- grain(dG, data=dat, smooth=e)

## ----------------------------------------------------------------------------------
extractCPT(dat, dG, smooth=e)  |> c()

## ----------------------------------------------------------------------------------
querygrain(gr.dG, exclude=FALSE, simplify=TRUE) 
querygrain(gr.uG, exclude=FALSE, simplify=TRUE) 

## ----------------------------------------------------------------------------------
querygrain(gr.dG, evidence=list(B="lev1"), exclude=FALSE, simplify=TRUE)
querygrain(gr.uG, evidence=list(B="lev1"), exclude=FALSE, simplify=TRUE)

## ----------------------------------------------------------------------------------
gr.uG <- grain(uG, data=dat, smooth=e)

## ----------------------------------------------------------------------------------
extractPOT(dat, uG, smooth=e) |> c()

## ----------------------------------------------------------------------------------
querygrain(gr.dG, exclude=FALSE, simplify=TRUE) 
querygrain(gr.uG, exclude=FALSE, simplify=TRUE) 

## ----------------------------------------------------------------------------------
querygrain(gr.dG, evidence=list(B="lev1"), exclude=FALSE, simplify=TRUE) 
querygrain(gr.uG, evidence=list(B="lev1"), exclude=FALSE, simplify=TRUE) 

## ----------------------------------------------------------------------------------
joint <- tabListMult(chest_cpt)
dim(joint)
joint  |> as.data.frame.table() |> head()

## ----------------------------------------------------------------------------------
tabMarg(joint, "lung")
tabMarg(joint, "bronc")

## ----------------------------------------------------------------------------------
ev <- list(asia="yes", dysp="yes")
cond1 <- tabSlice(joint, slice=ev)
cond1 <- cond1 / sum(cond1)
dim(cond1)
tabMarg(cond1, "lung")
tabMarg(cond1, "bronc")

## ----------------------------------------------------------------------------------
cond2 <- tabSliceMult(joint, slice=ev)
cond2 <- cond2 / sum(cond2)
dim(cond2)
tabMarg(cond2, "lung")
tabMarg(cond2, "bronc")

