setwd("C:/Users/galla/Documents/Masters/Semester_2/Research_Project/Fruit_Color_Fleshy_Project")

#get data#
one.spec.dat3<-read.csv("Data/one.spec.dat3.csv")

####Character evolution test using phylogeny####
#load packages#
library(ape)
library(V.PhyloMaker2)
library(phytools)

#plant phylogenetic tree from V.PhyloMaker2 package#
phy<-GBOTB.extended.LCVP

####add species from data to tree####
#data frame with taxonomic info#
species<-data.frame(species=one.spec.dat3$Genus_Species_Accepted, 
                    genus=one.spec.dat3$Genus_Accepted, family=one.spec.dat3$Family,
                    order=one.spec.dat3$Order)

#adds species from our data to the tree using taxonomic data#
PhyloMaker<-phylo.maker(species, tree=phy, scenarios="S1")

#save#
saveRDS(PhyloMaker, "output/phylomaker_function")
saveRDS(PhyloMaker$scenario.1, "output/phylomaker_tree")

#make two character vectors#
#fleshy data#
x<-one.spec.dat3[,8]
#add species names#
names(x)<-one.spec.dat3$Genus_Species_Accepted
#color data#
y<-one.spec.dat3[,11]
#add species names
names(y)<-one.spec.dat3$Genus_Species_Accepted

#grab tree from 
tree<-PhyloMaker$scenario.1

#Pagel's test for correlated evolution of binary traits# 
#default tests for evolving independently or evolving co-dependently# 
fit.c.f<-fitPagel(tree,x1,y1)

##one way dependencies##
#testing if fleshy/dry is the dependent variable 
fit.fleshy.dep<-fitPagel(tree, x, y, dep.var="x")


#testing if color is the dependent variable
fit.color.dep<-fitPagel(tree, x, y, dep.var="y")

#Save results 
saveRDS(fit.c.f, "output/Pagel_result_twoWayDependencies")
saveRDS(fit.fleshy.dep, "output/Pagel_result_fleshyDependent")
saveRDS(fit.color.dep, "output/Pagel_result_ColorDependent")

#read in results# 
#fit.c.f<-readRDS("output/Pagel_result_twoWayDependencies")
#fit.fleshy.dep<-readRDS("output/Pagel_result_fleshyDependent")
#fit.color.dep<-readRDS("output/Pagel_result_ColorDependent")


#compare results#
aic<-setNames(c(fit.c.f$independent.AIC,
                fit.color.dep$dependent.AIC,
                fit.fleshy.dep$dependent.AIC,
                fit.c.f$dependent.AIC),
              c("independent","dependent color",
                "dependent fleshy","dependent c&f"))
aic


#save distance plots in pdf
pdf(file="output/Distance_Plots.pdf")
par(mfrow = c(2,2))
plot(fit.c.f)
plot(fit.color.dep)
plot(fit.fleshy.dep)
dev.off()



