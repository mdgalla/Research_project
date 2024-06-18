setwd("C:/Users/galla/Documents/Masters/Semester_2/Research_Project/Fruit_Color_Fleshy_Project")

dat<-read.csv("Plant_Data.csv")
#remove data with NA for fleshy/dry and fruit color
dat1<-dat[which(!is.na(dat$NEW_Fruit_fleshy_or_dry_rev_checked_Anna)),]
cleandat<- dat1[which(!is.na(dat1$Fruit_color)),]


#colors to binary data 
colors<- read.csv("Color_to_Binary.csv")

#vector with fruit colors
col<-colors$Color

#add column for binary color
for(j in 1:length(cleandat$Fruit_color)){
  for(i in 1:length(col)){
    if(cleandat$Fruit_color[j]==col[i]){
      cleandat$Binary_Color[j]<-colors$Binary[i]
    }
  }}

#save to file 
write.csv(cleandat, "Plant_Data_With_BinaryColor.csv")