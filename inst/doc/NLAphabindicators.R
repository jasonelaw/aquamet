## ----setup,results='hide', warning=FALSE, message=FALSE------------------
library(aquamet)
library(reshape2)
library(plyr)
library(dplyr)

## ----dd------------------------------------------------------------------
bfMets <- nlaBankFeatures(angle=subset(nlaPhabEx,PARAMETER=='ANGLE',select=-PARAMETER),
                                  drawdown=subset(nlaPhabEx,PARAMETER=='DRAWDOWN',
                                                  select=-PARAMETER),
                                  horizontalDistance=subset(nlaPhabEx,PARAMETER=='HORIZ_DIST',
                                                            select=-PARAMETER),
                                  horizontalDistanceDrawdown=subset(nlaPhabEx,PARAMETER='HORIZ_DIST_DD',
                                                                    select=-PARAMETER),
                                  verticalHeight=subset(nlaPhabEx,PARAMETER=='VERT_DIST',
                                                        select=-PARAMETER),
                                  verticalHeightDrawdown=subset(nlaPhabEx,PARAMETER=='VERT_DIST_DD',
                                                                select=-PARAMETER))


## ----dd.1----------------------------------------------------------------

sitedata <- data.frame(SITE=c(6400, 6469, 6768, 6865, 6869, 7623, 8184, 8251, 8657, 1000048),
                       ECO9=c('SAP', 'SAP', 'TPL', 'NPL', 'XER', 'WMT', 'WMT', 'WMT', 'WMT','CPL'),
                       ORIGIN=c('MAN_MADE', 'MAN_MADE', 'MAN_MADE', 'NATURAL', 'MAN_MADE', 'NATURAL', 
                                'MAN_MADE', 'MAN_MADE', 'NATURAL', 'MAN_MADE'), stringsAsFactors=F)

bfMets.wide <- reshape2::dcast(bfMets, SITE~METRIC, value.var='VALUE')

ddIn <- merge(sitedata, subset(bfMets.wide, select=c('SITE', 'BFXHORIZDIST_DD', 'BFXVERTHEIGHT_DD')), by='SITE')

ddOut <- nlaDrawdownIndicator(ddIn, sampID='SITE', bfxVertDD='BFXVERTHEIGHT_DD', bfxHorizDD='BFXHORIZDIST_DD',
                              ecoreg='ECO9', lake_origin='ORIGIN')

ddOut


## ----ripdist-------------------------------------------------------------

buildings <- subset(nlaPhabEx,PARAMETER=='HI_BUILDINGS',select=-PARAMETER)
buildings_dd <- subset(nlaPhabEx,PARAMETER=='HI_BUILDINGS_DD',select=-PARAMETER)
commercial <- subset(nlaPhabEx,PARAMETER=='HI_COMMERCIAL',select=-PARAMETER)
commercial_dd <- subset(nlaPhabEx,PARAMETER=='HI_COMMERCIAL_DD',select=-PARAMETER)
crops <- subset(nlaPhabEx,PARAMETER=='HI_CROPS',select=-PARAMETER)
crops_dd <- subset(nlaPhabEx,PARAMETER=='HI_CROPS_DD',select=-PARAMETER)
docks <- subset(nlaPhabEx,PARAMETER=='HI_DOCKS',select=-PARAMETER)
docks_dd <- subset(nlaPhabEx,PARAMETER=='HI_DOCKS_DD',select=-PARAMETER)
landfill <- subset(nlaPhabEx,PARAMETER=='HI_LANDFILL',select=-PARAMETER)
landfill_dd <- subset(nlaPhabEx,PARAMETER=='HI_LANDFILL_DD',select=-PARAMETER)   
lawn <- subset(nlaPhabEx,PARAMETER=='HI_LAWN',select=-PARAMETER)
lawn_dd <- subset(nlaPhabEx,PARAMETER=='HI_LAWN_DD',select=-PARAMETER)
orchard <- subset(nlaPhabEx,PARAMETER=='HI_ORCHARD',select=-PARAMETER)
orchard_dd <- subset(nlaPhabEx,PARAMETER=='HI_ORCHARD_DD',select=-PARAMETER)
other <- subset(nlaPhabEx,PARAMETER=='HI_OTHER',select=-PARAMETER)
other_dd <- subset(nlaPhabEx,PARAMETER=='HI_OTHER_DD',select=-PARAMETER)
park <- subset(nlaPhabEx,PARAMETER=='HI_PARK',select=-PARAMETER)
park_dd <- subset(nlaPhabEx,PARAMETER=='HI_PARK_DD',select=-PARAMETER)
pasture <- subset(nlaPhabEx,PARAMETER=='HI_PASTURE',select=-PARAMETER)
pasture_dd <- subset(nlaPhabEx,PARAMETER=='HI_PASTURE_DD',select=-PARAMETER)
powerlines <- subset(nlaPhabEx,PARAMETER=='HI_POWERLINES',select=-PARAMETER)
powerlines_dd <- subset(nlaPhabEx,PARAMETER=='HI_POWERLINES_DD',select=-PARAMETER)
roads <- subset(nlaPhabEx,PARAMETER=='HI_ROADS',select=-PARAMETER)
roads_dd <- subset(nlaPhabEx,PARAMETER=='HI_ROADS_DD',select=-PARAMETER)
walls <- subset(nlaPhabEx,PARAMETER=='HI_WALLS',select=-PARAMETER)
walls_dd <- subset(nlaPhabEx,PARAMETER=='HI_WALLS_DD',select=-PARAMETER)
drawdown <- subset(nlaPhabEx,PARAMETER=='DRAWDOWN',select=-PARAMETER)
horizontalDistance_dd <- subset(nlaPhabEx,PARAMETER=='HORIZ_DIST_DD',select=-PARAMETER)
   
# Use defaults for data2007, fillinDrawdown, and proximityWeights
# arguments
hi <- nlaHumanImpact(buildings, buildings_dd, commercial, commercial_dd,
  crops, crops_dd,docks, docks_dd, landfill, landfill_dd, lawn, lawn_dd, orchard,
  orchard_dd, other, other_dd, park, park_dd, pasture, pasture_dd, powerlines,
  powerlines_dd, roads, roads_dd, walls, walls_dd, drawdown, horizontalDistance_dd)

hi.wide <- reshape2::dcast(hi, SITE~METRIC, value.var='VALUE') 


## ----ripdist.1-----------------------------------------------------------

ripdistOut <- nlaRipDistIndicator(hi.wide, 'SITE', 'HIIAG_SYN', 'HIINONAG_SYN', 'HIFPANYCIRCA_SYN')

ripdistOut


## ----rv------------------------------------------------------------------
bigTrees <- subset(nlaPhabEx,PARAMETER=='C_BIGTREES',select=-PARAMETER)
bigTrees_dd <- subset(nlaPhabEx,PARAMETER=='C_BIGTREES_DD',select=-PARAMETER)
smallTrees <- subset(nlaPhabEx,PARAMETER=='C_SMALLTREES',select=-PARAMETER)
smallTrees_dd <- subset(nlaPhabEx,PARAMETER=='C_SMALLTREES_DD',select=-PARAMETER)
canopyType <- subset(nlaPhabEx,PARAMETER=='CANOPY',select=-PARAMETER)
canopyType_dd <- subset(nlaPhabEx,PARAMETER=='CANOPY_DD',select=-PARAMETER)
grdcvrBare <- subset(nlaPhabEx,PARAMETER=='GC_BARE',select=-PARAMETER)
grdcvrBare_dd <- subset(nlaPhabEx,PARAMETER=='GC_BARE_DD',select=-PARAMETER)
grdcvrInund <- subset(nlaPhabEx,PARAMETER=='GC_INUNDATED',select=-PARAMETER)
grdcvrInund_dd <- subset(nlaPhabEx,PARAMETER=='GC_INUNDATED_DD',select=-PARAMETER)
grdcvrNw <- subset(nlaPhabEx,PARAMETER=='GC_NONWOODY',select=-PARAMETER)
grdcvrNw_dd <- subset(nlaPhabEx,PARAMETER=='GC_NONWOODY_DD',select=-PARAMETER)
grdcvrW <- subset(nlaPhabEx,PARAMETER=='GC_WOODY',select=-PARAMETER)
grdcvrW_dd <- subset(nlaPhabEx,PARAMETER=='GC_WOODY_DD',select=-PARAMETER)
undNonW <- subset(nlaPhabEx,PARAMETER=='U_NONWOODY',select=-PARAMETER)
undNonW_dd <- subset(nlaPhabEx,PARAMETER=='U_NONWOODY_DD',select=-PARAMETER)
undW <- subset(nlaPhabEx,PARAMETER=='U_WOODY',select=-PARAMETER)
undW_dd <- subset(nlaPhabEx,PARAMETER=='U_WOODY_DD',select=-PARAMETER)
undType <- subset(nlaPhabEx,PARAMETER=='UNDERSTORY',select=-PARAMETER)
undType_dd <- subset(nlaPhabEx,PARAMETER=='UNDERSTORY_DD',select=-PARAMETER)
drawdown <- subset(nlaPhabEx,PARAMETER=='DRAWDOWN',select=-PARAMETER)
horizontalDistance_dd <- subset(nlaPhabEx,PARAMETER=='HORIZ_DIST_DD',select=-PARAMETER)

# Use defaults for fillinDrawdown and createSyntheticCovers
rv <- nlaRiparianVegetation(bigTrees, bigTrees_dd, smallTrees, smallTrees_dd,
    canopyType, canopyType_dd, grdcvrBare, grdcvrBare_dd, grdcvrInund, grdcvrInund_dd,
    grdcvrNw, grdcvrNw_dd, grdcvrW, grdcvrW_dd, undNonW, undNonW_dd, undW, undW_dd,
    undType, undType_dd, drawdown, horizontalDistance_dd)

rv.wide <- dcast(rv, SITE~METRIC, value.var='VALUE')


## ----ss------------------------------------------------------------------
bedrock <- subset(nlaPhabEx,PARAMETER=='SS_BEDROCK',select=-PARAMETER)
boulder <- subset(nlaPhabEx,PARAMETER=='SS_BOULDERS',select=-PARAMETER)
cobble <- subset(nlaPhabEx,PARAMETER=='SS_COBBLE',select=-PARAMETER)
gravel <- subset(nlaPhabEx,PARAMETER=='SS_GRAVEL',select=-PARAMETER)
organic <- subset(nlaPhabEx,PARAMETER=='SS_ORGANIC',select=-PARAMETER)
other <- subset(nlaPhabEx,PARAMETER=='SS_OTHER',select=-PARAMETER)
sand <- subset(nlaPhabEx,PARAMETER=='SS_SAND',select=-PARAMETER)
silt <- subset(nlaPhabEx,PARAMETER=='SS_SILT',select=-PARAMETER)
wood <- subset(nlaPhabEx,PARAMETER=='SS_WOOD',select=-PARAMETER)

ss <- nlaShorelineSubstrate(bedrock,boulder,
cobble,gravel,organic,other,sand,silt,wood)

ss.wide <- dcast(ss, SITE~METRIC, value.var='VALUE') %>% 
  select(SITE, SSFCBEDROCK,SSFCBOULDERS) %>%
  mutate(SSFCBEDROCK=as.numeric(SSFCBEDROCK), SSFCBOULDERS=as.numeric(SSFCBOULDERS))

## ----hi------------------------------------------------------------------
buildings <- subset(nlaPhabEx,PARAMETER=='HI_BUILDINGS',select=-PARAMETER)
buildings_dd <- subset(nlaPhabEx,PARAMETER=='HI_BUILDINGS_DD',select=-PARAMETER)
commercial <- subset(nlaPhabEx,PARAMETER=='HI_COMMERCIAL',select=-PARAMETER)
commercial_dd <- subset(nlaPhabEx,PARAMETER=='HI_COMMERCIAL_DD',select=-PARAMETER)
crops <- subset(nlaPhabEx,PARAMETER=='HI_CROPS',select=-PARAMETER)
crops_dd <- subset(nlaPhabEx,PARAMETER=='HI_CROPS_DD',select=-PARAMETER)
docks <- subset(nlaPhabEx,PARAMETER=='HI_DOCKS',select=-PARAMETER)
docks_dd <- subset(nlaPhabEx,PARAMETER=='HI_DOCKS_DD',select=-PARAMETER)
landfill <- subset(nlaPhabEx,PARAMETER=='HI_LANDFILL',select=-PARAMETER)
landfill_dd <- subset(nlaPhabEx,PARAMETER=='HI_LANDFILL_DD',select=-PARAMETER)
lawn <- subset(nlaPhabEx,PARAMETER=='HI_LAWN',select=-PARAMETER)
lawn_dd <- subset(nlaPhabEx,PARAMETER=='HI_LAWN_DD',select=-PARAMETER)
orchard <- subset(nlaPhabEx,PARAMETER=='HI_ORCHARD',select=-PARAMETER)
orchard_dd <- subset(nlaPhabEx,PARAMETER=='HI_ORCHARD_DD',select=-PARAMETER)
other <- subset(nlaPhabEx,PARAMETER=='HI_OTHER',select=-PARAMETER)
other_dd <- subset(nlaPhabEx,PARAMETER=='HI_OTHER_DD',select=-PARAMETER)
park <- subset(nlaPhabEx,PARAMETER=='HI_PARK',select=-PARAMETER)
park_dd <- subset(nlaPhabEx,PARAMETER=='HI_PARK_DD',select=-PARAMETER)
pasture <- subset(nlaPhabEx,PARAMETER=='HI_PASTURE',select=-PARAMETER)
pasture_dd <- subset(nlaPhabEx,PARAMETER=='HI_PASTURE_DD',select=-PARAMETER)
powerlines <- subset(nlaPhabEx,PARAMETER=='HI_POWERLINES',select=-PARAMETER)
powerlines_dd <- subset(nlaPhabEx,PARAMETER=='HI_POWERLINES_DD',select=-PARAMETER)
roads <- subset(nlaPhabEx,PARAMETER=='HI_ROADS',select=-PARAMETER)
roads_dd <- subset(nlaPhabEx,PARAMETER=='HI_ROADS_DD',select=-PARAMETER)
walls <- subset(nlaPhabEx,PARAMETER=='HI_WALLS',select=-PARAMETER)
walls_dd <- subset(nlaPhabEx,PARAMETER=='HI_WALLS_DD',select=-PARAMETER)
drawdown <- subset(nlaPhabEx,PARAMETER=='DRAWDOWN',select=-PARAMETER)
horizontalDistance_dd <- subset(nlaPhabEx,PARAMETER=='HORIZ_DIST_DD',select=-PARAMETER)

# Use defaults for data2007, fillinDrawdown, and proximityWeights
# arguments
hi <- nlaHumanImpact(buildings, buildings_dd, commercial, commercial_dd,
    crops, crops_dd,docks, docks_dd, landfill, landfill_dd, lawn, lawn_dd, orchard,
    orchard_dd, other, other_dd, park, park_dd, pasture, pasture_dd, powerlines,
    powerlines_dd, roads, roads_dd, walls, walls_dd, drawdown, horizontalDistance_dd)

hi.wide <- dcast(hi, SITE~METRIC, value.var='VALUE')


## ----mergeDF-------------------------------------------------------------

metIn <- merge(rv.wide, ss.wide, by='SITE') %>% 
  merge(hi.wide, by='SITE')


sitedata.1 <- data.frame(sitedata, LAT_DD=c(35.72678,36.12929,41.46944,48.00697,38.84754,45.86918,
                                            40.05559,34.03126,48.568425306,33.067121087),
                     LON_DD=c(-82.08422,-79.83690,-93.92054,-101.53242,-111.96139,-113.54893,
                              -105.74708,-109.44311,-123.0735207,-95.73801377),
                     ELEV=c(427.50,251.63,278.75,620.39,1589.51,2412.81,3029.04,2519.09,47.75,148.99),
                     AREA=c(0.69732608,0.02485657,0.93282221,0.74152161,0.93174515,0.07763512,0.51027966,
                               0.17282305,0.26395493,0.14714400))

sitedata.1 

ripvegIn <- merge(sitedata.1,metIn,by='SITE')


## ----ripveg--------------------------------------------------------------

ripvegOut <- nlaRipVegCompIndicator(ripvegIn,sampID='SITE',lat='LAT_DD',lon='LON_DD'
                               ,lake_origin='ORIGIN',area='AREA',elev='ELEV'
                               ,ecoreg='ECO9',rviWoody='RVIWOODY_SYN'
                               ,rvfcGndInundated='RVFCGNDINUNDATED_SYN',rvfcUndWoody='RVFCUNDWOODY_SYN'
                               ,rvfcGndWoody='RVFCGNDWOODY_SYN',rvfpCanBig='RVFPCANBIG_SYN'
                               ,ssfcBedrock='SSFCBEDROCK',ssfcBoulders='SSFCBOULDERS'
                               ,hipwWalls='HIPWWALLS_SYN')

ripvegOut

## ----fc------------------------------------------------------------------
aquatic <- subset(nlaPhabEx,PARAMETER=='FC_AQUATIC',select=-PARAMETER)
aquatic_dd <- subset(nlaPhabEx,PARAMETER=='FC_AQUATIC_DD',select=-PARAMETER)
boulders <- subset(nlaPhabEx,PARAMETER=='FC_BOULDERS',select=-PARAMETER)
boulders_dd <- subset(nlaPhabEx,PARAMETER=='FC_BOULDERS_DD',select=-PARAMETER)
brush <- subset(nlaPhabEx,PARAMETER=='FC_BRUSH',select=-PARAMETER)
brush_dd <- subset(nlaPhabEx,PARAMETER=='FC_BRUSH_DD',select=-PARAMETER)
ledges <- subset(nlaPhabEx,PARAMETER=='FC_LEDGES',select=-PARAMETER)
ledges_dd <- subset(nlaPhabEx,PARAMETER=='FC_LEDGES_DD',select=-PARAMETER)
livetrees <- subset(nlaPhabEx,PARAMETER=='FC_LIVETREES',select=-PARAMETER)
livetrees_dd <- subset(nlaPhabEx,PARAMETER=='FC_LIVETREES_DD',select=-PARAMETER)
overhang <- subset(nlaPhabEx,PARAMETER=='FC_OVERHANG',select=-PARAMETER)
overhang_dd <- subset(nlaPhabEx,PARAMETER=='FC_OVERHANG_DD',select=-PARAMETER)
snags <- subset(nlaPhabEx,PARAMETER=='FC_SNAGS',select=-PARAMETER)
snags_dd <- subset(nlaPhabEx,PARAMETER=='FC_SNAGS_DD',select=-PARAMETER)
structures <- subset(nlaPhabEx,PARAMETER=='FC_STRUCTURES',select=-PARAMETER)
structures_dd <- subset(nlaPhabEx,PARAMETER=='FC_STRUCTURES_DD',select=-PARAMETER)
drawdown <- subset(nlaPhabEx,PARAMETER=='DRAWDOWN',select=-PARAMETER)
horizontalDistance_dd <- subset(nlaPhabEx,PARAMETER=='HORIZ_DIST_DD',select=-PARAMETER)

# Use default values for fillinDrawdown (TRUE), createSyntheticCovers (TRUE), 
# and coverClassInfo:
# coverClassInfo = data.frame(field = c(NA,'0','1','2','3','4')
#						 	                        ,characteristicCover = c(NA,0,0.05,0.25,0.575,0.875)
#						 	                        ,presence = c(NA,0,1,1,1,1)
#						 	                        ,stringsAsFactors=FALSE
#						 	                        )

fc <- nlaFishCover(aquatic,aquatic_dd,boulders,boulders_dd,brush,
  brush_dd,ledges,ledges_dd,livetrees,livetrees_dd,overhang,overhang_dd,
  snags,snags_dd,structures,structures_dd,drawdown,horizontalDistance_dd)

fc.wide <- dcast(fc, SITE~METRIC, value.var='VALUE') %>%
  select(SITE, FCFCSNAGS_LIT, FCINATURAL_LIT, FCFCBOULDERS_LIT, FCFCBRUSH_LIT,
         FCFCLEDGES_LIT, FCFCLIVETREES_LIT, FCFCOVERHANG_LIT)

fc.wide


## ----am------------------------------------------------------------------
emerg <- subset(nlaPhabEx,PARAMETER=='AM_EMERGENT',select=-PARAMETER)
float <- subset(nlaPhabEx,PARAMETER=='AM_FLOATING',select=-PARAMETER)
submerg <- subset(nlaPhabEx,PARAMETER=='AM_SUBMERGENT',select=-PARAMETER)
totcvr <- subset(nlaPhabEx,PARAMETER=='AM_TOTALCOVER',select=-PARAMETER)

am <- nlaAquaticMacrophytes(emergent=emerg,
  floating=float,submergent=submerg,totalCover=totcvr)

am.wide <- dcast(am, SITE~METRIC, value.var='VALUE') %>%
  select(SITE, AMFCFLOATING, AMFCEMERGENT) %>%
  mutate(AMFCFLOATING = as.numeric(AMFCFLOATING),
         AMFCEMERGENT = as.numeric(AMFCEMERGENT))

am.wide


## ----mergeDF.1-----------------------------------------------------------
litvegIn <- merge(sitedata.1, fc.wide, by='SITE', all.x=T) %>%
  merge(am.wide, by='SITE', all.x=T) 

litvegIn


## ----litveg--------------------------------------------------------------
litvegOut <- nlaLitVegCompIndicator(litvegIn, sampID='SITE', lat='LAT_DD', 
                                    lon='LON_DD', lake_origin='ORIGIN', 
                                    area='AREA', elev='ELEV', ecoreg='ECO9', 
                                    fciNatural='FCINATURAL_LIT', 
                                    fcfcSnag='FCFCSNAGS_LIT', 
                                    amfcFloating='AMFCFLOATING', 
                                    amfcEmergent='AMFCEMERGENT', 
                                    fcfcBoulders='FCFCBOULDERS_LIT', 
                                    fcfcBrush='FCFCBRUSH_LIT', 
                                    fcfcLedges='FCFCLEDGES_LIT', 
                                    fcfcLiveTrees='FCFCLIVETREES_LIT', 
                                    fcfcOverhang='FCFCOVERHANG_LIT')

litvegOut


## ----assemble------------------------------------------------------------
lrIn <- merge(sitedata.1, litvegOut, by='SITE') %>%
  merge(ripvegOut, by='SITE')


## ----lrveg---------------------------------------------------------------

litripOut <- nlaLitRipVegCompIndicator(lrIn,sampID='SITE',lat='LAT_DD',lon='LON_DD'
                                    ,lake_origin='ORIGIN',area='AREA',elev='ELEV'
                                    ,ecoreg='ECO9',rvegq='RVegQ',litcvrq='LitCvrQ')

litripOut

