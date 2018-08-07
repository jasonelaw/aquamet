## ----Setup, results='hide', message=FALSE, warning=FALSE-----------------
library(aquamet)
library(reshape2)
library(plyr)
library(dplyr)

## ----prepsite------------------------------------------------------------

nrsaPhabIndicEx


## ----rbs-----------------------------------------------------------------
rbsOut <- nrsaRelBedStabilityIndicator(nrsaPhabIndicEx,sampID='SITE', 
                                       ecoreg='AGGR_ECO9_2015', 
                                       protocol='REALM', lrbs='LRBS_G08', 
                                       lat='LAT_DD83', lon='LON_DD83', 
                                       area='WSAREASQKM', elev='ELEV_PT', 
                                       slope='XSLOPE', xwidth='XWIDTH')

rbsOut


## ----rd------------------------------------------------------------------
ripdistOut <- nrsaRipDistIndicator(nrsaPhabIndicEx,sampID='SITE',
                                   w1_hall='W1_HALL')

ripdistOut


## ----fc------------------------------------------------------------------
algae <- subset(fishcoverEx,PARAMETER=='ALGAE')
boulder <- subset(fishcoverEx,PARAMETER=='BOULDR')
brush <- subset(fishcoverEx,PARAMETER=='BRUSH')
liveTree <- subset(fishcoverEx,PARAMETER=='LVTREE')
macrophytes <- subset(fishcoverEx,PARAMETER=='MACPHY')
overhang <- subset(fishcoverEx,PARAMETER=='OVRHNG')
structures <- subset(fishcoverEx,PARAMETER=='STRUCT')
undercut <- subset(fishcoverEx,PARAMETER=='UNDCUT')
woodyDebris <- subset(fishcoverEx,PARAMETER=='WOODY')

fishCvrOut <- nrsaFishCover(algae, boulder,
                            brush, liveTree, macrophytes,
                            overhang, structures,
                            undercut, woodyDebris)

fishCvrOut.wide <- filter(fishCvrOut, METRIC=='xfc_nat') %>%
  dcast(SITE~METRIC, value.var='VALUE')

fishCvrOut.wide


## ----xfc-----------------------------------------------------------------
xfcIn <- subset(nrsaPhabIndicEx, select=-XFC_NAT) %>%
  merge(fishCvrOut.wide, by='SITE')

xfcIn

## ----instrmcvr-----------------------------------------------------------
instrmcvrOut <- nrsaInstrmCoverIndicator(xfcIn,sampID='SITE', ecoreg='AGGR_ECO9_2015', 
                                         protocol='REALM', xfc_nat='xfc_nat', 
                                         lat='LAT_DD83', lon='LON_DD83', 
                                         area='WSAREASQKM', elev='ELEV_PT', 
                                         slope='XSLOPE', xwidth='XWIDTH')

instrmcvrOut

## ----rv------------------------------------------------------------------
canCovLD <- subset(visripEx,PARAMETER=='CANBTRE', select=c('SITE','TRANSECT','BANK','VALUE'))
canCovSD <- subset(visripEx,PARAMETER=='CANSTRE',select=c('SITE','TRANSECT','BANK','VALUE'))
canVegT <- subset(visripEx,PARAMETER=='CANVEG',select=c('SITE','TRANSECT','BANK','VALUE'))
gCovB <- subset(visripEx,PARAMETER=='BARE',select=c('SITE','TRANSECT','BANK','VALUE'))
gCovNW <- subset(visripEx,PARAMETER=='GCNWDY',select=c('SITE','TRANSECT','BANK','VALUE'))
gCovW <- subset(visripEx,PARAMETER=='GCWDY',select=c('SITE','TRANSECT','BANK','VALUE'))
undCNW <- subset(visripEx,PARAMETER=='UNDNWDY',select=c('SITE','TRANSECT','BANK','VALUE'))
undCW <- subset(visripEx,PARAMETER=='UNDWDY',select=c('SITE','TRANSECT','BANK','VALUE'))
undVT <- subset(visripEx,PARAMETER=='UNDERVEG',select=c('SITE','TRANSECT','BANK','VALUE'))

# Use the default values for the coverCalculationValues argument
rvOut <- nrsaRiparianVegetation(canCovLD, canCovSD,
                                canVegT, gCovB,
                                gCovNW, gCovW,
                                undCNW, undCW,
                                undVT)

rvOut.wide <- filter(rvOut, METRIC=='xcmgw') %>%
  dcast(SITE~METRIC, value.var='VALUE')

rvOut.wide

## ----ripvegIn------------------------------------------------------------
ripvegIn <- subset(nrsaPhabIndicEx, select=-XCMGW) %>%
  merge(rvOut.wide, by='SITE')


## ----ripvegOut-----------------------------------------------------------
ripvegOut <- nrsaRiparianVegIndicator(ripvegIn,sampID='SITE', 
                                      ecoreg='AGGR_ECO9_2015', 
                                      protocol='REALM', 
                                      xcmgw='xcmgw', lat='LAT_DD83',
                                      lon='LON_DD83', area='WSAREASQKM', 
                                      elev='ELEV_PT', slope='XSLOPE', 
                                      xwidth='XWIDTH')

ripvegOut

