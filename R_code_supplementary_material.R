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
FINAL_SCORES<-data.frame(Island_Code=island_codes, n_natives=0, n_aliens=0, CV_now=0, CV_erad=0, BENEFIT_SPECIFIC=0, BENEFIT_GENERAL=0)

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

FINAL_SCORES$n_natives[FINAL_SCORES$Island_Code==island_codes[i]]<-dim(targ)[1]			## writes the number of native species on an island into output table
FINAL_SCORES$n_aliens[FINAL_SCORES$Island_Code==island_codes[i]]<-dim(ias)[1]			## writes the number of alien invasive species on an island into output table



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
OUTPUT[1:20,c(18,8,9,2,3,6,7,12,15)]
