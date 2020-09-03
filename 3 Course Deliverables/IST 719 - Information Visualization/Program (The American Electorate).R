# Lauren Lawless
# IST 719: Information Visualization
# Poster Project DRAFT

library(naniar)
library(alluvial)
library(tidyr)
library(lattice)
library(maps)
library(dummies)
library(sqldf)

default_par <- par()

df <- read.csv('/cloud/project/Project/VOTER_Survey_Jan.csv')

#####
# Alluvial
df_ideo <- drop_na(df[c('ideo5_2016','ideo5_2019')])
df_ideo <- df_ideo[(df_ideo$ideo5_2016 <= 5) & (df_ideo$ideo5_2019 <= 5),]
df_ideo_alluv <- aggregate(df_ideo$ideo5_2016
                           , list(ideo_2016=df_ideo$ideo5_2016
                                  , ideo_2019=df_ideo$ideo5_2019)
                           , length)
df_ideo_alluv <- df_ideo_alluv[order(df_ideo_alluv$ideo_2016),]

colors_16 <- c()
colors_19 <- c()
for (i in 1:nrow(df_ideo_alluv)){
  if (df_ideo_alluv$ideo_2016[i] == 1){
    colors_16 <- c(colors_16,'royalblue')
  }
  else if (df_ideo_alluv$ideo_2016[i] == 2){
    colors_16 <- c(colors_16,'steelblue1')
  }
  else if (df_ideo_alluv$ideo_2016[i] == 3){
    colors_16 <- c(colors_16,'grey80')
  }
  else if (df_ideo_alluv$ideo_2016[i] == 4){
    colors_16 <- c(colors_16,'indianred1')
  }
  else if (df_ideo_alluv$ideo_2016[i] == 5){
    colors_16 <- c(colors_16,'firebrick')
  }
  
  if (df_ideo_alluv$ideo_2019[i] == 1){
    colors_19 <- c(colors_19,'royalblue')
  }
  else if (df_ideo_alluv$ideo_2019[i] == 2){
    colors_19 <- c(colors_19,'steelblue1')
  }
  else if (df_ideo_alluv$ideo_2019[i] == 3){
    colors_19 <- c(colors_19,'grey80')
  }
  else if (df_ideo_alluv$ideo_2019[i] == 4){
    colors_19 <- c(colors_19,'indianred1')
  }
  else if (df_ideo_alluv$ideo_2019[i] == 5){
    colors_19 <- c(colors_19,'firebrick')
  }
}

royalblue_a <- rgb(70, 107, 216, max=255, alpha=255*0.7)
steelblue1_a <- rgb(117, 182, 248, max=255, alpha=255*0.7)
grey80_a <- rgb(203, 203, 203, max=255, alpha=255*0.7)
indianred1_a <- rgb(237, 115, 111, max=255, alpha=255*0.7)
firebrick_a <- rgb(164, 48, 42, max=255, alpha=255*0.7)

colors_16_a <- c()
for (i in 1:nrow(df_ideo_alluv)){
  if (df_ideo_alluv$ideo_2016[i] == 1){
    colors_16_a <- c(colors_16_a,royalblue_a)
  }
  else if (df_ideo_alluv$ideo_2016[i] == 2){
    colors_16_a <- c(colors_16_a,steelblue1_a)
  }
  else if (df_ideo_alluv$ideo_2016[i] == 3){
    colors_16_a <- c(colors_16_a,grey80_a)
  }
  else if (df_ideo_alluv$ideo_2016[i] == 4){
    colors_16_a <- c(colors_16_a,indianred1_a)
  }
  else if (df_ideo_alluv$ideo_2016[i] == 5){
    colors_16_a <- c(colors_16_a,firebrick_a)
  }
}

df_ideo_alluv$ideo_2016[df_ideo_alluv$ideo_2016 == 1] <- '1 Very Liberal'
df_ideo_alluv$ideo_2016[df_ideo_alluv$ideo_2016 == 2] <- '2 Liberal'
df_ideo_alluv$ideo_2016[df_ideo_alluv$ideo_2016 == 3] <- '3 Moderate'
df_ideo_alluv$ideo_2016[df_ideo_alluv$ideo_2016 == 4] <- '4 Conservative'
df_ideo_alluv$ideo_2016[df_ideo_alluv$ideo_2016 == 5] <- '5 Very Conservative'

df_ideo_alluv$ideo_2019[df_ideo_alluv$ideo_2019 == 1] <- '1 Very Liberal'
df_ideo_alluv$ideo_2019[df_ideo_alluv$ideo_2019 == 2] <- '2 Liberal'
df_ideo_alluv$ideo_2019[df_ideo_alluv$ideo_2019 == 3] <- '3 Moderate'
df_ideo_alluv$ideo_2019[df_ideo_alluv$ideo_2019 == 4] <- '4 Conservative'
df_ideo_alluv$ideo_2019[df_ideo_alluv$ideo_2019 == 5] <- '5 Very Conservative'

alluvial(df_ideo_alluv[,1:2]
         , freq=df_ideo_alluv$x
         , col=colors_16
         , alpha=0.7
         , border=colors_16_a
         , cex=0.7
)


#####
# State bars
df_ideo_state <- drop_na(df[c('ideo5_2019','inputstate2_2019', 'weight_2019')])
df_ideo_state <- df_ideo_state[df_ideo_state$ideo5_2019 <= 5,]
state_encoding <- read.csv('/cloud/project/Project/State Encoding.csv')
df_ideo_state <- merge(state_encoding, df_ideo_state, by.x='Code', by.y='inputstate2_2019')

df_ideo_state$ideo5_2019 <- as.factor(df_ideo_state$ideo5_2019)
ideo <- dummy.data.frame(df_ideo_state, names=c('ideo5_2019'), sep='_')
colnames(ideo)[4:10] <- c('Region1','Region2','VL','L','M','C','VC')
ideo$VL <- ideo$VL*ideo$weight_2019
ideo$L <- ideo$L*ideo$weight_2019
ideo$M <- ideo$M*ideo$weight_2019
ideo$C <- ideo$C*ideo$weight_2019
ideo$VC <- ideo$VC*ideo$weight_2019

ideo_tab <- sqldf(paste(
  'SELECT Abbr',
  ', Region1',
  ', Region2',
  ', sum(VL) AS VL',
  ', sum(L) AS L',
  ', sum(M) AS M',
  ', sum(C) AS C',
  ', sum(VC) AS VC',
  'FROM ideo',
  'GROUP BY Abbr',
  ', Region1',
  ', Region2'
)
)

for (i in 1:nrow(ideo_tab)){
  rowsum <- sum(ideo_tab[i,4:8])
  for (j in 4:8){
    ideo_tab[i,j] <- ideo_tab[i,j]/rowsum
  }
}
ideo_tab$totalL <- ideo_tab$VL + ideo_tab$L
ideo_tab <- ideo_tab[order(ideo_tab$Region1,ideo_tab$totalL),]


rownames(ideo_tab) <- ideo_tab$Abbr
ideo_tab <- ideo_tab[c('VL','L','M','C','VC')]
ideo_tab <- as.matrix(t(ideo_tab))
par(mar=c(2,2,0,1))

bp <- barplot(ideo_tab
              , col=c('royalblue','steelblue1','grey80','indianred1','firebrick')
              , horiz=TRUE
              , las=1
              , cex.names=0.7
              , cex.axis=0.7
)


#####
# Electoral vs. Popular votes
par(default_par)
votes <- read.csv('/cloud/project/Project/Results2016.csv')

# Fix Congressional district-allocated EC votes
votes[which(votes$State=='Maine'),]$electoralDem <- 3
votes[which(votes$State=='Maine'),]$electoralRep <- 1
votes[which(votes$State=='Nebraska'),]$electoralRep <- 5

votes <- merge(state_encoding, votes, by='State')
votes <- votes[order(-votes$Pop),]
top_5_pop <- votes[order(-votes$Pop),]$Pop[5] # 12820900
bottom_5_pop <- votes[order(votes$Pop),]$Pop[5]
votes_10 <- votes[(votes$Pop >= top_5_pop) | (votes$Pop <= bottom_5_pop),]
rownames(votes_10) <- votes_10$Abbr
pop_10 <- votes_10[,c('votesDem', 'votesRep')]
ec_10 <- votes_10[,c('electoralDem', 'electoralRep')]

votes_scatter <- votes[c('Abbr','State','electoralDem','electoralRep','Pop')]
votes_scatter$popM <- votes_scatter$Pop/1000000
votes_scatter$ecTotal <- votes_scatter$electoralDem + votes_scatter$electoralRep

ec_pc <- sum(votes_scatter$ecTotal)/sum(votes_scatter$popM)
votes_scatter$ec_pc <- votes_scatter$ecTotal/votes_scatter$popM
votes_scatter$ec_pc_dist <- abs(votes_scatter$ec_pc - ec_pc)
votes_scatter[which(votes_scatter$ec_pc_dist==min(votes_scatter$ec_pc_dist)),]

par(default_par)
plot(x=votes_scatter$popM
     , y=votes_scatter$ecTotal
     , pch=16
     # , cex=0.5
     , xlab='Population (in millions)'
     , ylab = 'Electoral Votes'
     , log='xy'
     , col='grey80'
     # , ylim=c(0,60)
     # , sub='*  state has additional Electoral College votes (Maine 2, Nebraska 3) allocated by congressional district'
)
abline(lm(votes_scatter$ecTotal ~ votes_scatter$popM), untf=TRUE)
abline(a=0,b=ec_pc, untf=TRUE)
text(ecTotal ~ popM
     , labels=ifelse(votes_scatter$Abbr %in% c('ME','NE'),paste0(votes_scatter$Abbr,'*'),votes_scatter$Abbr)
     , data=votes_scatter
     , cex=0.6#*votes_scatter$popM
     , col=ifelse(votes_scatter$electoralDem > votes_scatter$electoralRep,'royalblue','firebrick')
)
text(2.2, 3
     , '* state has two Electoral College votes allocated by state popular vote\n  and additional votes (Maine 2, Nebraska 3) allocated by congressional\n  district'
     , cex=0.7
     , col='grey80'
     , pos=4
)
text(1.5, 20
     , '— ideal Electoral College vote allocation if\n    distributed equally across all Americans'
     , cex=0.7
     , col='grey80'
     , pos=4
)

#####
# Map
cpop <- read.csv('https://www2.census.gov/geo/docs/reference/cenpop2010/CenPop2010_Mean_ST.txt?#')
votes_map <- merge(votes_scatter, cpop[c('STNAME','LATITUDE','LONGITUDE')], by.x='State', by.y='STNAME')
votes_map_copy <- votes_map

# all states, no points
par(mar=rep(0,4))
map('world', region='usa'
    , wrap=TRUE
    , xlim=c(-200,-6)
    , ylim=c(20,75)
    , col='grey80'
    , projection='polyconic'
    , orientation=c(60, -120, 0)
)
map('state', add=TRUE
    , col='grey80'
    , projection='polyconic'
    , orientation=c(60,-120,0)
)

# all states plus points, not projected
par(mar=rep(0,4))
map('world', region='usa'
    , wrap=TRUE
    , xlim=c(-180,-65)
    , ylim=c(20,75)
    , col='grey80'
    # , projection='polyconic'
    # , orientation=c(60, -120, 0)
)
map('state', add=TRUE
    , col='grey80'
    # , projection='polyconic'
    # , orientation=c(60,-120,0)
)
votes_map_tmp <- votes_map[9,]
points(votes_map$LONGITUDE, votes_map$LATITUDE
       , pch=16
       , col=ifelse(votes_map$electoralDem > votes_map$electoralRep,royalblue_a,firebrick_a)
       , cex=5*votes_map$ecTotal/max(votes_map$ecTotal)
)


#####
# Race to 270
race_2020 <- data.frame(t(ideo_tab))
race_2020$state <- rownames(race_2020)
race_2020 <- merge(race_2020, votes_map[c('Abbr','ecTotal')], by.x='state', by.y='Abbr')
rownames(race_2020) <- race_2020$state
race_2020 <- race_2020[-1]
race_2020$totalD <- race_2020$VL + race_2020$L
race_2020$totalR <- race_2020$VC + race_2020$C
race_2020$voteD <- ifelse((race_2020$totalD > race_2020$totalR),1,0)
race_2020$voteR <- ifelse((race_2020$totalR > race_2020$totalD),1,0)
race_2020[c('CO','DE','HI','MN','NH','NV','VA','WA'),]$voteD <- 1
race_2020[c('CO','DE','HI','MN','NH','NV','VA','WA'),]$voteR <- 0
race_2020[c('AK','NE','OK'),]$voteR <- 1
race_2020[c('AK','NE','OK'),]$voteD <- 0

# # race_2020$voteD <- ifelse((race_2020$totalD - race_2020$totalR > 0.1)|(race_2020$VL - race_2020$VC > 0.04),1,0)
# # race_2020$voteR <- ifelse((race_2020$totalR - race_2020$totalD > 0.1)|(race_2020$VC - race_2020$VL > 0.04),1,0)
# race_2020[which((race_2020$voteD+race_2020$voteR==2)|(race_2020$voteD+race_2020$voteR==0)),][9] <- c(1,1,1,?,?,?,?)
# race_2020[which((race_2020$voteD+race_2020$voteR==2)|(race_2020$voteD+race_2020$voteR==0)),][9] <- c(0,0,0,?,?,?,?)
# race_2020[which((race_2020$voteD==0)&(race_2020$voteR==1)),]
race_2020$ecD <- race_2020$voteD*race_2020$ecTotal
race_2020$ecR <- race_2020$voteR*race_2020$ecTotal

# # segmented bar
# pred_2020_seg <- data.frame(race_2020[c('ecD','ecR')])
# ecD_seg <- race_2020$ecD[race_2020$ecD > 0]
# ecR_seg <- rev(race_2020$ecR[race_2020$ecR > 0])
# ec_seg <- c(ecD_seg, ecR_seg)
# col_seg <- c(rep('royalblue',length(ecD_seg)), rep('firebrick',length(ecR_seg)))
# plot(x=c(0,sum(ec_seg)), y=c(0,1)
#      , type='n'
#      , xaxt='n', yaxt='n'
#      , bty='n'
#      , xlab=NA, ylab=NA
# )
# xleft <- 0
# for(i in 1:length(ec_seg)){
#   rect(xleft,0,xleft+ec_seg[i],1, col=col_seg[i])
#   xleft <- xleft + ec_seg[i]
# }
# abline(v=269, lty=3)
# 
# text(y=bp270, x=c(pred_2020_agg[1]/2, pred_2020_agg[2]/2+pred_2020_agg[1]), labels=pred_2020_agg[,1], col='white')
# 
# 

# agg bar
pred_2020_agg <- data.frame(ecD=sum(race_2020$ecD), ecR=sum(race_2020$ecR))
pred_2020_agg <- as.matrix(t(pred_2020_agg))

par(default_par)
bp270_agg <- barplot(pred_2020_agg
                     , horiz=TRUE
                     , col=c('royalblue','firebrick')
                     , xaxt='n'
)
text(y=bp270_agg, x=c(pred_2020_agg[1]/2, pred_2020_agg[2]/2+pred_2020_agg[1]), labels=pred_2020_agg[,1], col='white')

abline(v=269
       , lty=3
)


#####
# Single Dim Plots
single_dim <- merge(data.frame(votes[c('Abbr','State','votesDem','votesRep','Pop')]), votes_scatter[,c('Abbr','ecTotal','ec_pc')], by='Abbr')
single_dim <- single_dim[order(-single_dim$ecTotal, single_dim$ec_pc),]

par(default_par)
plot(x=0, y=0, type='n'
     , xlab=NA
     , ylab=NA
     , main='Votes by Candidate (Millions)'
     , names.arg=votes$Abbr
     , xlim=c(0,51)
     , ylim=c(0,max(c(single_dim$votesDem, single_dim$votesRep))/1000000)
     , xaxt='n'
)
axis(side=1, at=1:51, labels=single_dim$Abbr, cex.axis=0.35)
segments(x0=1:51
         , y0=single_dim$votesDem/1000000, y1=single_dim$votesRep/1000000
)
points(x=rep(1:51,2)
       , y=c(single_dim$votesRep/1000000, single_dim$votesDem/1000000)
       , col=c(rep('firebrick', 51), rep('royalblue',51))
       , pch=1
)

plot(single_dim$ecTotal
     , xlab=NA
     , ylab=NA
     , main='Electoral College Votes'
     , col='gray80'
     , xaxt='n'
)
axis(side=1, at=1:51, labels=single_dim$Abbr, cex.axis=0.35)

plot(single_dim$ec_pc
     , xlab=NA
     , ylab=NA
     , main='Electoral College Votes per Million People'
     , col='gray80'
     , xaxt='n'
)
axis(side=1, at=1:51, labels=single_dim$Abbr, cex.axis=0.35)

