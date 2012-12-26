pkgname <- "gRain"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('gRain')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("andtable")
### * andtable

flush(stderr()); flush(stdout())

### Name: andtable
### Title: Conditional probability tables based on logical dependcies
### Aliases: andtable ortable
### Keywords: utilities

### ** Examples

ortable(c("v","A","B"), levels=c("yes","no"))



cleanEx()
nameEx("cptable")
### * cptable

flush(stderr()); flush(stdout())

### Name: cptable
### Title: Create conditional probability tables (CPTs)
### Aliases: cptable
### Keywords: models

### ** Examples


yn <- c("yes","no")
ynm <- c("yes","no","maybe")
a    <- cptable(~asia, values=c(1,99),levels=yn)
t.a  <- cptable(~tub+asia, values=c(5,95,1,99,1,999),levels=ynm)
d.a  <- cptable(~dia+asia, values=c(5,5,1,99,100,999),levels=ynm)
compileCPT(list(a,t.a,d.a))




cleanEx()
nameEx("extractCPT")
### * extractCPT

flush(stderr()); flush(stdout())

### Name: extractCPT,extractPOT
### Title: Extract conditional probabilities and clique potentials from
###   data
### Aliases: extractCPT extractCPT.table extractCPT.data.frame extractPOT
###   extractPOT.table extractPOT.data.frame
### Keywords: utilities

### ** Examples


## Asia (chest clinique) example:

## Version 1) Specify conditional probability tables.
yn <- c("yes","no")
a    <- cptable(~asia, values=c(1,99),levels=yn)
t.a  <- cptable(~tub+asia, values=c(5,95,1,99),levels=yn)
s    <- cptable(~smoke, values=c(5,5), levels=yn)
l.s  <- cptable(~lung+smoke, values=c(1,9,1,99), levels=yn)
b.s  <- cptable(~bronc+smoke, values=c(6,4,3,7), levels=yn)
e.lt <- cptable(~either+lung+tub,values=c(1,0,1,0,1,0,0,1),levels=yn)
x.e  <- cptable(~xray+either, values=c(98,2,5,95), levels=yn)
d.be <- cptable(~dysp+bronc+either, values=c(9,1,7,3,8,2,1,9), levels=yn)
plist <- compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
pn1 <- grain(plist)
q1 <- querygrain(pn1)

## Version 2) Specify DAG and data
data(chestSim100000, package="gRbase")
dgf   <- ~asia + tub * asia + smoke + lung * smoke + bronc * smoke + either * tub * lung + xray * either + dysp * bronc * either
dg    <- dag(dgf)
pp    <- extractCPT(chestSim100000, dg)
cpp2  <- compileCPT(pp)
pn2   <- grain(cpp2)
q2    <- querygrain(pn2)

## Version 2) Specify triangulated undirected graph and data
ugf <- list(c("either", "lung", "tub"), c("either", "lung", "bronc"), 
    c("either", "xray"), c("either", "dysp", "bronc"), c("smoke", 
    "lung", "bronc"), c("asia", "tub"))
gg    <- ugList(ugf)
pp    <- extractPOT(chestSim100000, gg)
cpp3  <- compilePOT(pp)
pn3   <- grain(cpp3)
q3    <- querygrain(pn3)

## Compare results:
str(q1)
str(q2[names(q1)])
str(q3[names(q1)])



cleanEx()
nameEx("grain")
### * grain

flush(stderr()); flush(stdout())

### Name: grain
### Title: Graphical Independence Network
### Aliases: grain grain.CPTspec grain.POTspec grain.graphNEL nodeNames
###   nodeStates nodeNames.grain nodeStates.grain plot.grain iplot.grain
### Keywords: models

### ** Examples


## Asia (chest clinique) example:
yn <- c("yes","no")
a    <- cptable(~asia, values=c(1,99),levels=yn)
t.a  <- cptable(~tub+asia, values=c(5,95,1,99),levels=yn)
s    <- cptable(~smoke, values=c(5,5), levels=yn)
l.s  <- cptable(~lung+smoke, values=c(1,9,1,99), levels=yn)
b.s  <- cptable(~bronc+smoke, values=c(6,4,3,7), levels=yn)
e.lt <- cptable(~either+lung+tub,values=c(1,0,1,0,1,0,0,1),levels=yn)
x.e  <- cptable(~xray+either, values=c(98,2,5,95), levels=yn)
d.be <- cptable(~dysp+bronc+either, values=c(9,1,7,3,8,2,1,9), levels=yn)
plist <- compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
pn <- grain(plist)
pn
summary(pn)
plot(pn)
pnc <- compile(pn, propagate=TRUE)

## If we want to query the joint distribution of the disease nodes,
## computations can be speeded up by forcing these nodes to be in
## the same clique of the junction tree:

pnc2 <- compile(pn, root=c("lung", "bronc", "tub"), propagate=TRUE)

system.time({
  for (i in 1:200) 
    querygrain(pnc, nodes=c("lung","bronc", "tub"), type="joint")})
system.time({
  for (i in 1:200) 
    querygrain(pnc2, nodes=c("lung","bronc", "tub"), type="joint")})


## Create network from gmData (with data) and graph specification.
## There are different ways:
data(HairEyeColor)
d   <- HairEyeColor
daG <- dagList(list(~Hair, ~Eye:Hair, ~Sex:Hair))
class(daG)
uG <- ugList(list(~Eye:Hair, ~Sex:Hair))
class(uG)

## Create directly from dag:
b1  <- grain(daG,d)
class(b1)

## Build model from undirected (decomposable) graph
b3  <- grain(uG,d)
class(b3)

## Simple example - one clique only in triangulated graph:
plist <- compileCPT(list(a, t.a))
pn <- grain(plist)
querygrain(pn)

## Simple example - disconnected network:
plist <- compileCPT(list(a, t.a, s))
pn <- grain(plist)
querygrain(pn)




cleanEx()
nameEx("loadSaveHuginNet")
### * loadSaveHuginNet

flush(stderr()); flush(stdout())

### Name: loadHuginNet
### Title: Load and save Hugin net files
### Aliases: loadHuginNet saveHuginNet
### Keywords: utilities

### ** Examples


tf <- system.file("huginex", "chest_clinic.net", package = "gRain")
chest <- loadHuginNet(tf, details=1)
chest 

td <- tempdir()
saveHuginNet(chest, paste(td,"/chest.net",sep=''))

chest2 <- loadHuginNet(paste(td,"/chest.net",sep=''))

tf <- system.file("huginex", "golf.net", package = "gRain")
golf <- loadHuginNet(tf, details=1)

saveHuginNet(golf, paste(td,"/golf.net",sep=''))
golf2 <- loadHuginNet(paste(td,"/golf.net",sep=''))





cleanEx()
nameEx("querygrain")
### * querygrain

flush(stderr()); flush(stdout())

### Name: querygrain
### Title: Query an independence network
### Aliases: querygrain querygrain.grain setFinding retractFinding
###   getFinding pFinding
### Keywords: models utilities

### ** Examples

 testfile <- system.file("huginex", "chest_clinic.net", package = "gRain")
 chest <- loadHuginNet(testfile, details=0)


 qb <- querygrain(chest)
 qb

 lapply(qb, as.numeric) 
 sapply(qb, as.numeric) 




cleanEx()
nameEx("repeatPattern")
### * repeatPattern

flush(stderr()); flush(stdout())

### Name: repeatPattern
### Title: Create repeated patterns in Bayesian networks
### Aliases: repeatPattern
### Keywords: utils

### ** Examples

## Specify hidden markov models. The x[i]'s are unobserved, the
## y[i]'s can be observed.

yn <- c("yes","no")

## Specify p(x0)
x.0 <- cptable(~x0, values=c(1,1), levels=yn)

## Specify transition density
x.x <- cptable(~x[i]|x[i-1], values=c(1,99,2,98),levels=yn)

## Specify emissiob density
y.x <- cptable(~y[i]|x[i],   values=c(1,99,2,98),levels=yn)

## The pattern to be repeated
pp <- list(x.x, y.x)

## Repeat pattern and create network
ppp <- repeatPattern(pp, instances=1:10)
qqq <- compileCPT(c(list(x.0),ppp))
rrr <- grain(qqq)





cleanEx()
nameEx("simulate")
### * simulate

flush(stderr()); flush(stdout())

### Name: simulate.grain
### Title: Simulate from an independence network
### Aliases: simulate.grain
### Keywords: models

### ** Examples


## Not run: 
##D 
##D tf <- system.file("huginex", "chest_clinic.net", package = "gRain")
##D chest <- loadHuginNet(tf, details=1)
##D 
##D simulate(chest,n=10)
##D 
##D chest2 <- setFinding(chest, c("VisitToAsia", "Dyspnoea"),
##D c("yes","yes"))
##D 
##D simulate(chest2,n=10)
## End(Not run)




### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
