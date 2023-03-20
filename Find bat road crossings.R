# Cleans everything
rm(list = ls())

# Working directory
setwd("")

# Opening the table containing the results of the script Pairing microphones
library(data.table)
library(Hmisc)
TrajTot=read.csv2("TrajTot.csv")

#####
# Warning: have a choice
#####
# If you have synchronized acoustic recorders:
# Create a column in "ListPairs" file named 'decalageFinal' 
# Fill with '0'
ListPaires=read.csv2("ListPairs.csv")

# If you have non-synchronized acoustic recorders:
# Opening the table containing the results of 'automatic_calculation_of_the_time_shift.R'
# Delete line with NA in 'decalageFinal' if it is the case (you have an acoustic recorder recording in mono!)
ListPaires=read.csv2("ListPairs_Dec_Man.csv")
     
######

TravFinal=vector()

for (h in 1:nrow(ListPaires)){
  TrajTot1=subset(TrajTot, participation %in% ListPaires$participation_1[h])
  TrajTot1=cbind(TrajTot1,Cote=1, Point=ListPaires$point_1[h])
  TrajTot2=subset(TrajTot, participation %in% ListPaires$participation_2[h])
  TrajTot2=cbind(TrajTot2,Cote=2, Point=ListPaires$point_2[h])
  Trav=rbind(TrajTot1,TrajTot2)
  Trav$DecDeb=as.numeric(as.character(Trav$DecDeb))
  Trav$DecFin=as.numeric(as.character(Trav$DecFin))
  Trav$Prob=as.numeric(as.character(Trav$probabilite))
  Date=vector(length=nrow(Trav))
  Heure=vector(length=nrow(Trav))
  Minute=vector(length=nrow(Trav))
  Seconde=vector(length=nrow(Trav))
  MiliSec=vector(length=nrow(Trav))
  Fich=as.character(Trav$File0)
  for (i in 1:length(Fich)){
    Date[i]=substr(Fich[i],nchar(Fich[i])-18,nchar(Fich[i])-11)
    Heure[i]=substr(Fich[i],nchar(Fich[i])-9,nchar(Fich[i])-8)
    Minute[i]=substr(Fich[i],nchar(Fich[i])-7,nchar(Fich[i])-6)
    Seconde[i]=substr(Fich[i],nchar(Fich[i])-5,nchar(Fich[i])-4)
    MiliSec[i]=substr(Fich[i],nchar(Fich[i])-2,nchar(Fich[i]))
  }
  Datenum=as.numeric(Date)
  Heure=as.numeric(Heure)
  Minute=as.numeric(Minute)
  Seconde=as.numeric(Seconde)
  MiliSec=as.numeric(MiliSec)
  Temps=Heure*3600+Minute*60+Seconde+MiliSec/1000
  Trav=cbind(Trav,Heure,Minute,Seconde,MiliSec,Temps,Date,Datenum)
  Trav$espece=as.numeric(as.factor(Trav$espece))
  Ent1=subset(Trav, DecDeb>0 & DecFin<0 & Cote==1) 
  Sort1=subset(Trav, DecDeb<0 & DecFin>0 & Cote==1) 
  Ent2=subset(Trav, DecDeb>0 & DecFin<0 & Cote==2)
  Sort2=subset(Trav, DecDeb<0 & DecFin>0 & Cote==2)
  if((((nrow(Ent1)==0)|nrow(Sort2)==0))&((nrow(Ent2)==0)|(nrow(Sort1)==0)))
  {
    TravTot12=NA
  }else{
    decalage=ListPaires$decalageFinal[h]
    decalage=as.numeric(as.character(decalage))
    Ent1$Temps=Ent1$Temps+decalage
    Sort1$Temps=Sort1$Temps+decalage
    library(Hmisc)
    if((((nrow(Ent1)>0)&(nrow(Sort2)>0))))
    {
      TC12=find.matches(cbind(Ent1$Datenum,Ent1$espece,Ent1$Temps)
                        ,cbind(Sort2$Datenum,Sort2$espece,Sort2$Temps)
                        ,tol=c(0,0,(ListPaires$inter_distance[h]/5+(ListPaires$inter_distance[h]/5*0.5)+5))
                        ,maxmatch=1)
      DecMesure12=vector()
      for (i in 1:length(TC12$matches))
      {
        if (TC12$matches[i]!=0)
        {
          DecMesure12=c(DecMesure12,Ent1$Temps[i]-Sort2$Temps[TC12$matches[i]])
        }
      }
    }
    if((((nrow(Ent2)>0)&(nrow(Sort1)>0))))
    {
      TC21=find.matches(cbind(Ent2$Datenum,Ent2$espece,Ent2$Temps)
                        ,cbind(Sort1$Datenum,Sort1$espece,Sort1$Temps)
                        ,tol=c(0,0,(ListPaires$inter_distance[h]/5+(ListPaires$inter_distance[h]/5*0.5)+5))
                        ,maxmatch=1)
      DecMesure21=vector()
      for (i in 1:length(TC21$matches))
      {
        if (TC21$matches[i]!=0)
        {
          DecMesure21=c(DecMesure21,Sort1$Temps[TC21$matches[i]]-Ent2$Temps[i])
        }
      }
    }
    rm(Trav12,Trav21)
    rm(TravTot12)
    Trav12=subset(Ent1,TC12$matches!=0)
    Trav21=subset(Ent2,TC21$matches!=0)
    Trav12=cbind(Trav12,DecMesure12)
    names(Trav12)[27]<-"DecMesure"
    if(nrow(Trav12)>0){Trav12=cbind(Trav12,Sens=12)}
    Trav21=cbind(Trav21,DecMesure21)
    names(Trav21)[27]<-"DecMesure"
    if(nrow(Trav21)>0){Trav21=cbind(Trav21,Sens=21)}
    TravTot12=rbind(Trav12,Trav21)
    print(paste(h, "/", nrow(ListPaires)))
    if(nrow(TravTot12)>0)
    {TravTot12=cbind(TravTot12,Paire=ListPaires$num_paire[h],Site=ListPaires$site[h],Passage=ListPaires$passage[h])
    }
    TravFinal=rbind(TravFinal,TravTot12)
  }
}

#File containing bat crossing
write.csv2(TravFinal,"TravFinal.csv")
