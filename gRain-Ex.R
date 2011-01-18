pkgname <- "gRain"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
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
### Aliases: cptable print.cptable
### Keywords: models

### ** Examples


yn <- c("yes","no")
ynm <- c("yes","no","maybe")
a    <- cptable(~asia, values=c(1,99),levels=yn)
t.a  <- cptable(~tub+asia, values=c(5,95,1,99,1,999),levels=ynm)
d.a  <- cptable(~dia+asia, values=c(5,5,1,99,100,999),levels=ynm)
compileCPT(list(a,t.a,d.a))




cleanEx()
nameEx("grain")
### * grain

flush(stderr()); flush(stdout())

### Name: grain
### Title: Graphical Independence Network
### Aliases: grain print.grain grain.CPTspec grain.POTspec grain.graphNEL
###   nodeNames nodeStates nodeNames.grain nodeStates.grain plot.grain
###   iplot.grain
### Keywords: models

### ** Examples


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


data(HairEyeColor)
d   <- HairEyeColor
dag <- dagList(list(~Hair, ~Eye+Hair, ~Sex+Hair))
class(dag)
ug <- ugList(list(~Eye+Hair, ~Sex+Hair))
class(ug)

b1  <- grain(dag,d)
class(b1)

b3  <- grain(ug,d)
class(b3)

plist <- compileCPT(list(a, t.a))
pn <- grain(plist)
querygrain(pn)

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
###   getFinding pFinding print.grainFinding
### Keywords: models utilities

### ** Examples

 testfile <- system.file("huginex", "chest_clinic.net", package = "gRain")
 chest <- loadHuginNet(testfile, details=0)


 qb <- querygrain(chest)
 qb

 lapply(qb, as.numeric) 
 sapply(qb, as.numeric) 




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
