##set working directory##
setwd("C:/Users/galla/Documents/Masters/Semester_2/Research_Project/Fruit_Color_Fleshy_Project")

##load in data##
cleandat<-read.csv("Data/Plant_Data_With_BinaryColor.csv")

##check if colors/fruit match for each species
species<-unique(cleandat$Genus_Species_Accepted)
Conflicting.entry.species<- rep(NA, length(species))

#function 
#'@param function to check for any conflicting entries for multiple 
# specemins of the same species 
#'@param function to check for conflicting entries for same species
#'@param dataset name is cleandat
#'@param col is character string for name of species column
#'@param col1 character string name for column being checked 

Check_for_conflict<-function(col, col1){
  x<-unique(cleandat[[col]])
  Conflicting.entry.species<- rep(NA, length(x))
  for (i in (1:length(x))){
    focal<- x[i]
    current<-cleandat[which(cleandat[[col]]==focal),]
    {for(j in 1:length(current[[col]])){
      for(l in 1:length(current[[col]])){
        if(current[[col1]][l]!=current[[col1]][j]){
          Conflicting.entry.species[i]<- focal
        }
      }}}}
  return(unique(Conflicting.entry.species))
}

Conflicting.entry.species<-Check_for_conflict("Genus_Species_Accepted", "Binary_Color")
#conflicts found#

#Check_for_conflict("Genus_Species_Accepted", "NEW_Fruit_fleshy_or_dry_rev_checked_Anna")
##no conflicts found#


#filter conflicting fruit color specimens
#make a new data frame
no.conflicting.color<-cleandat


conflicting<-unique(Conflicting.entry.species)

#filter out conflicting colors
for(i in 2:length(unique(conflicting))){
  focal<-conflicting[i]
  no.conflicting.color<-no.conflicting.color[which(no.conflicting.color$Genus_Species_Accepted!=focal),]
}


##prune down to one specimen for each species##
#make a new data.frame
one.spec.dat<-no.conflicting.color
#species in data set 
species1<-unique(one.spec.dat$Genus_Species_Accepted)
#
for(i in 1:length(species1)){
  focal<- species[i]
  current<-one.spec.dat[which(one.spec.dat$Genus_Species_Accepted==focal),]
  if (length(current$Genus_Species_Accepted)>1){
    #define which rows should be removed
    remove<-which(one.spec.dat$Genus_Species_Accepted==focal)[2:length(which(one.spec.dat==focal))]
    #remove them
    one.spec.dat<- one.spec.dat[-remove,]
  }
}

##check##
tab<-table(one.spec.dat$Genus_Species_Accepted)
##4 still have multiples##

mult<-tab[which(tab>1)]
one.spec.dat1<-one.spec.dat

#remove what is left##
for(i in 1:length(mult)){
  focal<-names(mult[i])
  current<-one.spec.dat1[which(one.spec.dat1$Genus_Species_Accepted==focal),]
  remove<-which(one.spec.dat1$Genus_Species_Accepted==focal)[2:length(which(one.spec.dat1==focal))]
  one.spec.dat1<- one.spec.dat1[-remove,]
}

#clean up uncertain
one.spec.dat2<-one.spec.dat1[which(one.spec.dat1$NEW_Fruit_fleshy_or_dry_rev_checked_Anna!="dry/fleshy"),]
one.spec.dat3<-one.spec.dat2[which(one.spec.dat2$Binary_Color!="uncertain "),]

one.spec.dat3<-one.spec.dat3[which(one.spec.dat3$NEW_Fruit_fleshy_or_dry_rev_checked_Anna!="dry/fleshy"),]

#save to a file
write.csv(one.spec.dat3, "Data/one.spec.dat3.csv")
