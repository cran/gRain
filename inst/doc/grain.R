## ----include=FALSE,echo=FALSE,warning=FALSE----------------------------------------
dir.create("figures")
knitr::opts_chunk$set(fig.height=3, fig.width=5,
               fig.path='figures/grain-',
               warning=FALSE, message=FALSE
)
options("prompt"="> ","width"=85, "digits"=4)
library(gRain)
library(igraph)

## ----echo=FALSE--------------------------------------------------------------------
require(gRain)
prettyVersion <- packageDescription("gRain")$Version
prettyDate <- format(Sys.Date())

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
cpt_list <- list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be)
plist <- compile_cpt(cpt_list)
plist
chest_bn <- grain(plist, compile=FALSE)
chest_bn

## ----------------------------------------------------------------------------------
chest_dag <- dag(list("asia", c("tub", "asia"), "smoke", c("lung", "smoke"), c("bronc", 
"smoke"), c("either", "tub", "lung"), c("xray", "either"), c("dysp", 
"bronc", "either")))

## ----chest-LS, echo=F, fig.height=3, fig.cap="Chest clinic example from Lauritzen and Spiegelhalter (1988)."----
par(mar=c(0,0,0,0))
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
chest_cpt <- compile_cpt(a, t.a, s, l.s, b.s, e.lt, x.e, d.be)
chest_cpt

## ----------------------------------------------------------------------------------
chest_cpt$tub
chest_cpt$tub  |> as.data.frame.table()

## ----------------------------------------------------------------------------------
chest_bn <- grain(chest_cpt)
chest_bn

## ----------------------------------------------------------------------------------
chest_bn <- compile(chest_bn)

## ----------------------------------------------------------------------------------
disease <- c("tub", "lung", "bronc")
asia_dysp <- list(asia="yes", dysp="yes")

## ----------------------------------------------------------------------------------
chest_bn |> querygrain(nodes=disease, type="marginal")
chest_bn |> querygrain(nodes=disease, type="marginal", simplify = TRUE)

chest_bn |> querygrain(nodes=disease, type="joint") 
chest_bn |> querygrain(nodes=disease, type="joint", simplify = TRUE)

chest_bn |> querygrain(nodes=disease, type="conditional")
chest_bn |> querygrain(nodes=disease, type="conditional", simplify = TRUE)

## ----------------------------------------------------------------------------------
asia_dysp <- list(asia="yes", dysp="yes")

chest_ev <- chest_bn |>
    evidence_add(evidence=asia_dysp)

## ----------------------------------------------------------------------------------
chest_ev |> evidence_get() 

## ----------------------------------------------------------------------------------
chest_ev |> querygrain(nodes=disease, simplify = TRUE)
chest_ev |> evidence_prob()

## ----------------------------------------------------------------------------------
chest_ev |> querygrain(nodes=disease, simplify=TRUE)
chest_ev |> evidence_prob()

## ----------------------------------------------------------------------------------
chest_bn |> evidence_prob(evidence=list(asia="yes", dysp="yes"))

## ----------------------------------------------------------------------------------
yn <- c("yes", "no")

node_parents_list <-
    list("asia", c("tub", "asia"), "smoke", c("lung", "smoke"),
         c("bronc", "smoke"), c("either", "tub", "lung"),
         c("xray", "either"), c("dysp", "bronc", "either"))

chest_dummy_cpt2 <- lapply(node_parents_list, function(f){
    cpt(f, levels=yn)
})
bn_temp <- compile_cpt(chest_dummy_cpt2) |> grain()

## ----------------------------------------------------------------------------------
cpt_values <- list(asia=c(1, 99),
                   tub=c(5, 95, 1, 99),
                   smoke=c(5, 5),
                   lung=c(1, 9, 1, 99),            
                   bronc=c(6, 4, 3, 7),
                   either=c(1, 0, 1, 0, 1, 0, 0, 1),
                   xray=c(98, 2, 5, 95),
                   dysp=c(9, 1, 7, 3, 8, 2, 1, 9))
bn_real <- replace_cpt(bn_temp, cpt_values)

## ----------------------------------------------------------------------------------
bn_temp |> querygrain(evi=asia_dysp, nodes=disease, simplify = TRUE)
bn_real |> querygrain(evi=asia_dysp, nodes=disease, simplify = TRUE)

## ----------------------------------------------------------------------------------
querygrain(chest_bn, nodes=disease, simplify = TRUE)

## ----------------------------------------------------------------------------------
querygrain(chest_bn, nodes=disease, result="data.frame")

## ----------------------------------------------------------------------------------
chest_bn |> querygrain(evidence=asia_dysp,
                       nodes=disease, simplify = TRUE)

## ----------------------------------------------------------------------------------
chest_bn |> querygrain(evidence=list(asia=c(1, 0), dysp=c(1, 0)),
                       nodes=disease, simplify = TRUE)

## ----------------------------------------------------------------------------------
querygrain(chest_bn,
           evidence=list(asia=c(1, 0), dysp=c(1, 0)),
           nodes=c("lung", "bronc", "asia", "dysp"),
           exclude=FALSE, simplify = TRUE)

## ----------------------------------------------------------------------------------
querygrain(chest_bn,
           evidence=asia_dysp,
           simplify = TRUE)

## ----------------------------------------------------------------------------------
querygrain(chest_bn,
           evidence=asia_dysp,
           exclude = FALSE, simplify = TRUE)

## ----------------------------------------------------------------------------------
chest_bn3 <- evidence_add(chest_bn, evidence=list(either="no", tub="yes"))

## ----------------------------------------------------------------------------------
evidence_prob(chest_bn3)
querygrain(chest_bn3, nodes=disease, type="joint")

## ----------------------------------------------------------------------------------
chest_ev1 <- chest_bn |> evidence_add(list(smoke="yes"))
chest_ev2 <- chest_bn |> evidence_add(list(smoke=c(1, 0)))

## ----------------------------------------------------------------------------------
chest_bn |> querygrain(nodes=disease, simplify = TRUE)
chest_ev1 |> querygrain(nodes=disease, simplify = TRUE)

## ----------------------------------------------------------------------------------
g.s <- cpt(~ smoke_guess|smoke, levels=yn,
              values=c(.8, .2, .1, .9))
g.s

## ----------------------------------------------------------------------------------
chest_ext <- c(cpt_list, list(g.s)) |> compile_cpt() |> grain()
chest_ext |> querygrain(nodes=disease, simplify = TRUE)
chest_ext |> querygrain(nodes=disease, evidence=list(smoke="yes"), simplify = TRUE)
chest_ext |> querygrain(nodes=disease, evidence=list(smoke_guess="yes"), simplify = TRUE)

## ----------------------------------------------------------------------------------
chest_ve <- chest_bn |>
    evidence_add(evidence = list(smoke = c(.8, .1)))
chest_ve |> querygrain(nodes=disease, simplify = TRUE)
evidence_get(chest_ve)

## ----------------------------------------------------------------------------------
chest_bn |> setEvidence(evidence=list(smoke=c(1, 0)))

## ----------------------------------------------------------------------------------
node_parents_list <- list("asia", c("tub", "asia"), "smoke", c("lung", "smoke"),
              c("bronc", "smoke"), c("either", "tub", "lung"),
              c("xray", "either"), c("dysp", "bronc", "either"))

g1 <- dag(node_parents_list)
par(mar=c(0,0,0,0))
plot(g1)

## ----------------------------------------------------------------------------------
cliq <- list(c("xray", "either"), c("asia", "tub"), c("smoke", "lung", 
"bronc"), c("lung", "either", "tub"), c("lung", "either", "bronc"
), c("bronc", "either", "dysp"))

g2 <- ug(cliq)
par(mar=c(0,0,0,0))
plot(g2)

## ----------------------------------------------------------------------------------
bn1 <- grain(g1, data=gRbase::chestSim100000)
bn2 <- grain(g2, data=gRbase::chestSim100000)

## ----------------------------------------------------------------------------------
j1 <- querygrain(bn1, type="joint")
j2 <- querygrain(bn2, type="joint")
d <- j1 %a-% j2
max(abs(d))

## ----------------------------------------------------------------------------------
g1  <- dag(~A:C + B:C, result="igraph")
g2  <- ug(~A:C + B:C, result="igraph")
par(mfrow=c(1,2), mar=c(0,0,0,0))
plot(g1); plot(g2)

## ----------------------------------------------------------------------------------
tab <- tabNew(~A:B:C, levels=c("+", "-"), 
              values=c(1, 2, 3, 5, 1, 2, 1, 4))

## ----------------------------------------------------------------------------------
bn1 <- grain(g1, data=tab)
bn2 <- grain(g2, data=tab)

## ----------------------------------------------------------------------------------
j1 <- querygrain(bn1, type="joint")
j2 <- querygrain(bn2, type="joint")
d <- j1 %a-% j2
max(abs(d))

## ----------------------------------------------------------------------------------
p <- extract_cpt(tab, g1) |> c()
p

## ----------------------------------------------------------------------------------
q <- extract_pot(tab, g2) |> c()
q

## ----------------------------------------------------------------------------------
tabProd(p) |> ftable()
tabProd(q) |> ftable()

## ----------------------------------------------------------------------------------
tab0 <- tab
tab0[1:4] <- 0
tab0

## ----------------------------------------------------------------------------------
n_BC <- tab0 |> tabMarg(~B:C)
n_AC <- tab0 |> tabMarg(~A:C)
n_C  <- tab0 |> tabMarg(~C)

p.B_C <- n_BC %a/% n_C
p.A_C <- n_AC %a/% n_C
p.C <- n_C / sum(n_C)
p.C
p.B_C
p.A_C

## ----------------------------------------------------------------------------------
bn01 <- grain(g1, data=tab0)
bn02 <- grain(g2, data=tab0)

## ----------------------------------------------------------------------------------
p <- extract_cpt(tab0, g1) |> c()
q <- extract_pot(tab0, g2) |> c()
p
q
tabProd(p) |> ftable()
tabProd(q) |> ftable()

## ----------------------------------------------------------------------------------
eps <- 0.01
bn01e <- grain(g1, data=tab0, smooth=eps)
bn02e <- grain(g2, data=tab0, smooth=eps)

## ----------------------------------------------------------------------------------
j1 <- querygrain(bn01e, type="joint")
j2 <- querygrain(bn02e, type="joint")
j1
j2
d <- j1 %a-% j2
max(abs(d))

## ----eval=T------------------------------------------------------------------------
dat <- gRbase::chestSim10000

## Using gRim and stepwise selection
sat_model <-  gRim::dmod(~.^., data=dat)
mm1 <- stepwise(sat_model, criterion="aic", type="decomposable")
##bn1 <- grain(mm1$modelinfo$ug, data=dat)
bn1 <- grain(mm1$modelinfo$ug, data=dat, smooth=0.01)

## Using bnlearn and hill climbing
sat_graph <- bnlearn::random.graph(names(dat), prob = 1) # complete graph
mm2 <- bnlearn::hc(dat, start=sat_graph)
bn2 <- bnlearn::as.grain(bnlearn::bn.fit(mm2, dat))
bn2$dag

## ----bn1, echo=F, fig.cap="XXXX."--------------------------------------------------
par(mfrow=c(1, 1), mar=c(0,0,0,0))
coords <- layout_(bn1$ug, nicely())
plot(bn1$ug, layout=coords)

## ----bn2, echo=F, fig.cap="XXXX."--------------------------------------------------
par(mfrow=c(1,2), mar=c(0,0,0,0))
new_coords <- function(coords, src, dst) {
  nms1 <- nodes(src)
  nms2 <- nodes(dst)
  coords[match(nms2, nms1),]
}
plot(bn2$dag, layout=new_coords(coords, bn1$ug, bn2$dag))
plot(bn2$ug, layout=new_coords(coords, bn1$ug, bn2$ug))

## ----------------------------------------------------------------------------------
par(mar=c(0,0,0,0))
plot(dag(~y1:x1 + x2:x1 + y2:x2))

## ----------------------------------------------------------------------------------
u <- list(x1=yn, x2=yn)
x1 <- cpt(~x1, values=c(1, 3), levels=u)
x2 <- cpt(~x2|x1, values=c(3, 1, 1, 7), levels=u)
bn <- grain(compile_cpt(x1, x2))
querygrain(bn, simplify=TRUE)

## ----------------------------------------------------------------------------------
s <- 2
mu <- c(mu1=2, mu2=5)
lambda <- c(lambda1=1, lambda2=5)

## ----------------------------------------------------------------------------------
y1_obs <- 14 # Observed value for y1_obs
lik1 <- dnorm(y1_obs, mean=mu, sd=s)
lik1

## ----------------------------------------------------------------------------------
querygrain(bn, simplify=TRUE, exclude=FALSE)
bn1 <- setEvidence(bn, evidence=list(x1=lik1))
querygrain(bn1, simplify=TRUE, exclude=FALSE)

x1_upd <- getgrain(bn, "cptlist")$x1 * lik1
bn2 <- replace_cpt(bn, list(x1=x1_upd))
querygrain(bn2, simplify=TRUE) 

## ----------------------------------------------------------------------------------
nsim <- 10000
xsim_marg <- simulate(bn, nsim, seed=2022)
xsim_cond <- simulate(bn1, nsim, seed=2022)
y2marg  <- rpois(n=nsim, lambda=lambda[xsim_marg$x2])
y2cond  <- rpois(n=nsim, lambda=lambda[xsim_cond$x2])
summary(y2marg)
summary(y2cond)
par(mfrow=c(1,2))
y2marg |> hist(prob=T, ylim=c(0, .4), breaks=20, main="marginal p(y2)")
y2cond |> hist(prob=T, ylim=c(0, .4), breaks=20, main="conditional p(y2|y1*)")

## ----------------------------------------------------------------------------------
joint <- tabProd(chest_cpt)
dim(joint)
joint  |> as.data.frame.table() |> head()

## ----------------------------------------------------------------------------------
tabMarg(joint, "lung")
tabMarg(joint, "bronc")

## ----------------------------------------------------------------------------------
asia_dysp
cond1 <- tabSlice(joint, slice=asia_dysp)
cond1 <- cond1 / sum(cond1)
dim(cond1)
tabMarg(cond1, "lung")
tabMarg(cond1, "bronc")

## ----------------------------------------------------------------------------------
cond2 <- tabSliceMult(joint, slice=asia_dysp)
cond2 <- cond2 / sum(cond2)
dim(cond2)
tabMarg(cond2, "lung")
tabMarg(cond2, "bronc")

