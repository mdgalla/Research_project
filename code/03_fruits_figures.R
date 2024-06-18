setwd("C:/Users/galla/Documents/Masters/Semester_2/Research_Project/Fruit_Color_Fleshy_Project")
#load packages#
library(phytools)

#get dataset#
one.spec.dat3<-read.csv("Data/one.spec.dat3.csv")

#get Pagel fit results#
two.way<-readRDS("output/Pagel_result_twoWayDependencies")
fleshy.dep<-readRDS("output/Pagel_result_fleshyDependent")
color.dep<-readRDS("output/Pagel_result_ColorDependent")

#get the tree 
tree<-readRDS("output/phylomaker_tree")

#character matrixes#
#fleshy data#
x<-one.spec.dat3[,8]
#add species names#
names(x)<-one.spec.dat3$Genus_Species_Accepted
#color data#
y<-one.spec.dat3[,11]
#add species names
names(y)<-one.spec.dat3$Genus_Species_Accepted


#distance plots#
pdf(file="output/two_way_correlated_evolution.pdf")
plot(two.way)
dev.off()
plot(color.dep)
plot(fleshy.dep)


#make phylogeny with trait colors# 
pdf(file="output/tree_with_traits.pdf")
par(mfrow=c(1,2))
plot(tree,show.tip.label=FALSE,no.margin=TRUE)
par(fg="transparent")
tiplabels(pie=to.matrix(x[tree$tip.label],c("dry","fleshy")),piecol=c("blue","red"),
          cex=0.3)
par(fg="black")
legend(fill=c("blue","red", "darkgreen", "yellow"),legend=c("dry","fleshy", "conspicuous ", "non-conspicuous"),
                  x=35,y=1000)
par(fg="transparent")
plot(tree,show.tip.label=FALSE,no.margin=TRUE,direction="leftwards")
tiplabels(pie=to.matrix(y[tree$tip.label],c("conspicuous ","non-conspicuous")),piecol=c("darkgreen","yellow"),
          cex=0.3)
dev.off()

#location and frugivore info#

#Add location and frugivore data#
dat3<-read.csv("Data/Database.Region.info.csv")
#clean up NAs#
dat3<-dat3[which(!is.na(dat3$Genus_Species_Accepted)),]

#add geographic and frugivore data#
add<-dat3$Genus_Species_Accepted
for(i in 1:length(spec)){
  for (j in 1:length(add)){
    for(k in 6:length(colnames(dat3))){
    focal.spec<-spec[i]
    focal.loc<-add[j]
    if(focal.spec==focal.loc){
      one.spec.dat3[[colnames(dat3)[k]]][i]<-dat3[[colnames(dat3)[k]]][j]
    }
  }
}}

###save##
write.csv(one.spec.dat3, "Data/all_plant_dat")
#read in#
one.spec.dat3<-read.csv("Data/all_plant_dat")
#frequency of locations in data#
loc<-one.spec.dat3$Continent
tab<-table(loc, useNA="ifany")
cont.percents<-tab/length(loc)

write.csv(cont.percents, "output/cont_percents.csv", row.names = F)

###robustness of the data####
#how many fleshy/color groups in data#
one.spec.dat3$color_fleshy<-paste(one.spec.dat3$Binary_Color,
                                  one.spec.dat3$NEW_Fruit_fleshy_or_dry_rev_checked_Anna,
                                  sep="_")

groups<-table(one.spec.dat3$color_fleshy)
fleshyordry<-table(one.spec.dat3$NEW_Fruit_fleshy_or_dry_rev_checked_Anna)

#how many of each color#
color<-table(one.spec.dat3$Binary_Color)

#how many in each order#
order<-table(one.spec.dat3$Order)

#frugivore info#
table(one.spec.dat3$Mammal_combi)
table(one.spec.dat3$Bird_combi)

