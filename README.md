# IslandPriority
Prioritising the world's islands for the eradication of invasive vertebrates to safeguard globally threatened vertebrates

###############################################################################
# R code to accompany paper by Nick Holmes et al.
###############################################################################

### database compiled by Islands Conservation (Nick Holmes)
### prioritisation code written by Steffen Oppel on 23 December 2014 (steffen.oppel@rspb.org.uk)
### adapted from Dawson, J., Oppel, S., Cuthbert, R.J., Holmes, N., Bird, J.P., Butchart, S.H.M., Spatz, D.R., Tershy, B., 2015.
###              Prioritizing islands for the eradication of invasive vertebrates in the United Kingdom overseas territories. Conservation Biology 29, 143-153.
### DATA CONTAIN THE FOLLOWING ITEMS:
# aliens - data.frame listing all invasive mammal species for each island
# EF - data.frame listing the eradication feasibility threshold values (area, human pop size) for each invasive mammal species/group
# interactions - data.frame that lists the impact of each invasive mammal on each native species on every island
# island_codes - vector of all island codes (a numeric version of individual island names)
# islands - data.frame of all islands including location, name, area, and human population size
# islpops - data.frame that converts human population size ranges into a single numeric value for calculation
# natives - data.frame listing all globally threatened native vertebrate species for each island
