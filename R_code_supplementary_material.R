###############################################################################
#													#
#	<insert citation of paper here>							#
#													#
###############################################################################

### database compiled by Islands Conservation (Nick Holmes)
### prioritisation code written by Steffen Oppel on 23 December 2014 (steffen.oppel@rspb.org.uk)
### adapted from Dawson, J., Oppel, S., Cuthbert, R.J., Holmes, N., Bird, J.P., Butchart, S.H.M., Spatz, D.R., Tershy, B., 2015.
###              Prioritizing islands for the eradication of invasive vertebrates in the United Kingdom overseas territories. Conservation Biology 29, 143-153.

setwd("A:\\MANUSCRIPTS\\in_prep\\IC_eradication_evaluation")  ##Steffen's location
setwd("C:\\STEFFEN\\MANUSCRIPTS\\in_prep\\IC_eradication_evaluation")  ###Steffen's location
setwd("C:\\Users\\nholmes\\Dropbox\\Islands2")
load("GLOBAL_ISLAND_INPUT.RData")


### DATA CONTAIN THE FOLLOWING ITEMS:
# aliens - data.frame listing all invasive mammal species for each island
# EF - data.frame listing the eradication feasibility threshold values (area, human pop size) for each invasive mammal species/group
# interactions - data.frame that lists the impact of each invasive mammal on each native species on every island
# island_codes - vector of all island codes (a numeric version of individual island names)
# islands - data.frame of all islands including location, name, area, and human population size
# islpops - data.frame that converts human population size ranges into a single numeric value for calculation
# natives - data.frame listing all globally threatened native vertebrate species for each island



#####################################################################################################################################################
#
# STUFF TO BE REMOVED FOR SUPPLEMENTARY MATERIAL (but retained to maintain functionality)
# 
#####################################################################################################################################################
library(RODBC)
setwd("A:\\MANUSCRIPTS\\in_prep\\IC_eradication_evaluation")  ##Steffen's location
setwd("C:\\STEFFEN\\MANUSCRIPTS\\in_prep\\IC_eradication_evaluation")  ###Steffen's location
setwd("C:\\Users\\nholmes\\Dropbox\\Islands2")
IR <- odbcConnectAccess2007('GP_June2016.accdb')
islands <- sqlQuery(IR, "SELECT * FROM export_islands")
natives <- sqlQuery(IR, "SELECT * FROM export_natives")
aliens <- sqlQuery(IR, "SELECT * FROM export_aliens")
interactions <- sqlQuery(IR, "SELECT * FROM Interactions")
EF <- sqlQuery(IR, "SELECT * FROM EradFeasibility")
islpops<- sqlQuery(IR, "SELECT * FROM Nom_hum_hab_conversion") #defines population size max for each category
odbcClose(IR)
head(islands)
names(islands)[c(1,6,9)]<-c("Island_Code","Area","Population_size")
head(aliens)
names(aliens)[1]<-"Island_Code"
head(natives)
names(natives)[1:6]<-c("Island_Code","Animal_type","Scientific_name","IUCN","ExtinctRisk","Irreplace")
head(interactions)
names(interactions)[1:5]<-c("Island_Code",'Target_Species',"IUCN",'Invasive_Species','impact_score')
head(islpops)
names(islpops)[2:3]<-c("Human_Habitation_Category","Population_size")
island_codes<-unique(islands$Island_Code)						## extracts the unique Island_Codes into a separate vector
save.image("GLOBAL_ISLAND_INPUT_2.RData")






#####################################################################################################################################################
#
# ERADICATION FEASIBILITY DEPENDING ON SPECIES, ISLAND SIZE, HUMAN HABITATION
# 
#####################################################################################################################################################

## POPULATING MISSING HUMAN POP FIELDS FROM CATEGORY VIA MEAN POP SIZE
aliens$Human_Habitation_Number[is.na(aliens$Human_Habitation_Number)]<-ifelse(aliens$Human_Habitation_Category[is.na(aliens$Human_Habitation_Number)]=='unknown',NA,islpops$Population_size[match(aliens$Human_Habitation_Category[is.na(aliens$Human_Habitation_Number)], islpops$Human_Habitation_Category)])	##assigns max pop size by category to islands in aliens table with blank pop size field
aliens$EF<-1

## Eradication considered not feasible (EF=0) if island is inhabited by more people than group-specific THRESHOLD OF HUMAN POPULATION (except where habitation is 'research station')
aliens$EF<-ifelse(aliens$Human_Habitation_Number>EF$human_pop_cutoff[match(aliens$IC_Nominate_Type,EF$Animal_category)],ifelse(aliens$Human_Habitation_Type=="Research Station",1,0),1)	

## Eradication considered not feasible (EF=0) if island is larger than species-specific THRESHOLD OF HUMAN POPULATION SIZE
aliens$EF<-ifelse(aliens$Corrected_Area_KM2>EF$largest_feasible_area[match(aliens$IC_Nominate_Type,EF$Animal_category)],0,aliens$EF)

## Eradication considered not feasible (EF=0) if humen habitation is unknown
aliens$EF[is.na(aliens$Human_Habitation_Number)]<-0





#####################################################################################################################################################
#
# CALCULATE CONSERVATION VALUE FOR ALL SPECIES ON ALL ISLANDS (PCV AND RCV)
# 
#####################################################################################################################################################

## create blank table in which output is stored
FINAL_SCORES<-data.frame(Island_Code=island_codes, n_natives=0, n_aliens=0, CV_now=0, CV_erad=0,BENEFIT_SPECIFIC=0, BENEFIT_GENERAL=0,
					invasives=NA, invasives_erad=NA, threatened=NA, threatened_benefit=NA,
					sum_irreplace=0, sum_extrisk=0, sum_severity=0,sum_severity_erad=0)

## Start loop over every island (n=2415)
for (i in 1:length(island_codes)){							


##################################
# TAKE SUBSAMPLE OF SPECIES FOR ISLAND
##################################
ias<-subset(aliens, Island_Code==island_codes[i])					## takes subset of aliens for the island
targ<-subset(natives, Island_Code==island_codes[i])					## takes subset of natives for the island

isl_interactions<-interactions[interactions$Target_Species %in% targ$Scientific_name,]		## takes subset of interactions for the native species on an island
isl_interactions<-isl_interactions[isl_interactions$Invasive_Species %in% ias$Scientific_Name,]	## takes subset of interactions for the invasives on an island
isl_interactions<-isl_interactions[isl_interactions$Island_Code==island_codes[i],]		## takes subset of interactions for the island

FINAL_SCORES$n_natives[FINAL_SCORES$Island_Code==island_codes[i]]<-dim(targ)[1]		## writes the number of native species on an island into output table
FINAL_SCORES$n_aliens[FINAL_SCORES$Island_Code==island_codes[i]]<-dim(ias)[1]			## writes the number of alien invasive species on an island into output table

FINAL_SCORES$invasives[FINAL_SCORES$Island_Code==island_codes[i]]<-paste(ias$Scientific_Name, collapse=", ")		## writes the names of all invasive species on an island into output table
FINAL_SCORES$invasives_erad[FINAL_SCORES$Island_Code==island_codes[i]]<-paste(ias$Scientific_Name[ias$EF==1], collapse=", ")		## writes the names of all invasive species that can be eradicated from an island into output table
FINAL_SCORES$threatened[FINAL_SCORES$Island_Code==island_codes[i]]<-paste(targ$Scientific_name, collapse=", ")		## writes the names of all native threatened species on an island into output table



##################################
# CALCULATE BENEFIT SCORE FOR EACH SPECIES BEFORE ERADICATION
##################################
if (dim(targ)[1]>0){										## prevents error if no natives or invasives are present on respective island

## create a table to hold output for each species on an island
species_benefits<-data.frame(Island=island_codes[i], Species=targ$Scientific_name, irep= targ$Irreplace, threat=targ$ExtinctRisk, pot=0, imp_now=0, B_now=0, imp_erad=0, B_erad=0)

## list all species as 'potential' that do not currently occur as breeding or non-breeding species but may colonize in future
species_benefits$pot<-ifelse(targ$Present_Breeding_Status %in% c('Potential','Data Deficient', 'Absent'),1,0)	


for (s in 1:dim(targ)[1]){									## starts loop over every species on the island
spec<-as.character(targ[s,3])									## picks the name of the selected species
species_benefits$imp_now[species_benefits$Species==spec]<-ifelse(dim(isl_interactions)[1]==0,0,max(isl_interactions$impact_score[isl_interactions$Target_Species==spec]))		## writes the maximum impact of any of the island's aliens on the selected target species into the assessment table
}												## ends loop over each species on the island

species_benefits$B_now<-species_benefits$threat*species_benefits$irep*species_benefits$imp_now			## CALCULATE BENEFIT SCORE
species_benefits$B_now<-ifelse(species_benefits$pot==0,species_benefits$B_now,(species_benefits$B_now/2))	## adjust BENEFIT SCORE FOR POTENTIAL FUTURE SPECIES by multiplying with 0.5


##################################
# SIMULATE ERADICATION OF ALIENS
##################################
## remove all invasive species that have an eradication feasibility EF = 1
remaining_ias<-subset(ias, EF==0)								
isl_interactions<-isl_interactions[isl_interactions$Invasive_Species %in% remaining_ias$Scientific_Name,]	## takes subset of interactions for the remaining invasives on an island after all feasible eradications



##################################
# CALCULATE BENEFIT SCORE FOR EACH SPECIES AFTER ERADICATION
##################################

for (s in 1:dim(targ)[1]){									## starts loop over every species on the island
spec<-as.character(targ[s,3])									## picks the name of the selected species
species_benefits$imp_erad[species_benefits$Species==spec]<-ifelse(dim(isl_interactions)[1]==0,0,max(isl_interactions$impact_score[isl_interactions$Target_Species==spec]))		## writes the maximum impact of any of the REMAINING island aliens on the selected target species into the assessment table							
}												## ends loop over each species on the island

species_benefits$B_erad<-species_benefits$threat*species_benefits$irep*species_benefits$imp_erad		## CALCULATE BENEFIT SCORE AFTER ERADICATION
species_benefits$B_erad<-ifelse(species_benefits$pot==0,species_benefits$B_erad,(species_benefits$B_erad/2))	## POTENTIAL COLONISING SPECIES RECEIVE ONLY HALF THE BENEFIT SCORE




##################################
# SUMMARISE SCORES FOR EACH ISLAND
##################################

FINAL_SCORES$CV_now[FINAL_SCORES$Island_Code==island_codes[i]]<-sum(species_benefits$B_now)				## CONSERVATION VALUE BEFORE ERADICATION = the sum of benefit values over all native species on an island
FINAL_SCORES$CV_erad[FINAL_SCORES$Island_Code==island_codes[i]]<-sum(species_benefits$B_erad)				## CONSERVATION VALUE AFTER ERADICATION = the sum of benefit values over all native species on an island

FINAL_SCORES$threatened_benefit[FINAL_SCORES$Island_Code==island_codes[i]]<-paste(species_benefits$Species[species_benefits$B_erad<species_benefits$B_now], collapse=", ")		## writes the names of all native species that benefit from eradication into output table

FINAL_SCORES$sum_irreplace[FINAL_SCORES$Island_Code==island_codes[i]]<-sum(species_benefits$irep)		## SUM of all irreplaceability scores of all threatened species on an island
FINAL_SCORES$sum_extrisk[FINAL_SCORES$Island_Code==island_codes[i]]<-sum(species_benefits$threat)		## SUM of all extinction risk probabilities of all threatened species on an island
FINAL_SCORES$sum_severity[FINAL_SCORES$Island_Code==island_codes[i]]<-sum(species_benefits$imp_now)		## SUM of all severity of impact scores of all threatened species on an island
FINAL_SCORES$sum_severity_erad[FINAL_SCORES$Island_Code==island_codes[i]]<-sum(species_benefits$imp_erad)	## SUM of all severity of impact scores of all threatened species on an island AFTER successful eradication


}else{										## alternatively, if no native species are present
FINAL_SCORES$CV_now[FINAL_SCORES$Island_Code==island_codes[i]]<-0		## CONSERVATION VALUE BEFORE ERADICATION = 0
FINAL_SCORES$CV_erad[FINAL_SCORES$Island_Code==island_codes[i]]<-0		## CONSERVATION VALUE AFTER ERADICATION = 0

}										## close the alternative loop for islands without native species


##################################
# CALCULATE ERADICATION BENEFIT BASED ON CONSERVATION VALUE
##################################

### subtract conservation value AFTER eradication from conservation value BEFORE eradication
FINAL_SCORES$BENEFIT_SPECIFIC[FINAL_SCORES$Island_Code==island_codes[i]]<-(FINAL_SCORES$CV_now[FINAL_SCORES$Island_Code==island_codes[i]]-FINAL_SCORES$CV_erad[FINAL_SCORES$Island_Code==island_codes[i]])	## removed in February: *islands$NRR[islands$Island_Code==island_codes[i]]

}													## close loop over all islands




#####################################################################################################################################################
# SORT FINAL SCORE TABLE BY BENEFIT AND MERGE WITH ISLAND NAMES
#####################################################################################################################################################

OUTPUT<-merge(FINAL_SCORES, islands, by='Island_Code', all.x=T)			## create output table with island and UKOT names
OUTPUT$General_Rank<-rank(-OUTPUT$CV_now, ties.method = "min")			## ranking based on benefit from eradication ALL invasive vertebrates					
OUTPUT$Specific_Rank<-rank(-OUTPUT$BENEFIT_SPECIFIC, ties.method = "min")	## ranking based on benefit from eradication of feasible invasive ve
OUTPUT<-OUTPUT[order(OUTPUT$Specific_Rank),]
OUTPUT<-OUTPUT[!(OUTPUT$BENEFIT_SPECIFIC==0 & OUTPUT$BENEFIT_SPECIFIC==0),]	## reduce output to just those islands where eradication actually makes a difference
OUTPUT[1:20,c(26,16,17,2,3,6,7)]								## quick screen view
write.table(OUTPUT, "Global_species_prioritisation_final_ranking_no_area_restriction_June2016.csv", sep=",", row.names=F)		## export table