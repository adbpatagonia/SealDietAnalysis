## ADB July 25, 2017.
## The intent of this script is to calculate the lengths and weights
## of all prey species of all marine mammals based on HPA
## This process takes more than a minute, and thus the output is saved
## It is later loaded in the scripts written to do the actual analyses

## Read data and source functions needed to estimate length and weight ----
source('R/functions/lengthregs.R');  source('R/functions/weightregs.R')
diet <- read.csv('data/diet_data.csv',header = T)
#diet <- diet[order(diet$pMVmm, diet$length),]

## create indices for easier handling ----
diet$rowindex <- 1:nrow(diet)
idindex <- data.frame(idsex = unique(diet$idsex), idindex = 1:length(unique(diet$idsex)))
diet <- merge(diet,idindex)

## Estimate prey length and weight based on regressions from hard parts ----
# Create place holders
diet$predlength <- diet$predweight <- NA
# estimate length and weight
s1 <- Sys.time()
for (i in 1:nrow(diet)) {
  diet$predlength[i] <- ifelse(is.na(diet$pMVmm[i]), NA, lengthregs(diet$preycode[i], as.character(diet$group[i]), diet$pMVmm[i]))
  diet$predweight[i] <- weightregs(preycode = diet$preycode[i], group = as.character(diet$group[i]), pMVmm = diet$pMVmm[i], preylength = diet$predlength[i])
}
s2 <- Sys.time()
s2 - s1
# ubuntu: Time difference of 51.37254 secs
# win7: Time difference of 1.978967 mins = 118.73802 secs
#118.73802/51.37254

## Define the weight used in the diet description --> use totalpreyweight ----
diet$preyweight <- diet$predweight
# for inverts, use measured weight
diet$preyweight <- ifelse(diet$preycode > 999,diet$measuredweight,diet$preyweight)
# for fish, use the weights that were measured
diet$preyweight <- ifelse(diet$preycode < 1000 & diet$codemeasuredestimated > 4,diet$weight,diet$preyweight)

## For invertebrates, there can be a conflict between prey item weight and total prey sp weight ----
# There are weights per row within mammal in column weight
# and also a "Prey species weight" in column measuredweight (which is repeated in all rows for the same idsex)
# for inverts, there are no regressions yet
# Check if the sum of weights per prey species per idsex is not equal to mesuredweight
# If these are different, assign the maximum of [sum(weight) and unique(measuredweight)] as the value to be used
# and assign NA to the rest of rows that correspond to that prey sp within the digestive tract section within the mammal


# step 1. Define the subset of inverts
inverts <- subset(diet, preycode > 999 & preycode < 9900)
# step 2. compare columns measured weight and sum of weight
diffs <- merge(aggregate(inverts$weight, by = list(inverts$idsex, inverts$preycode), FUN = sum), unique(data.frame(Group.1 = inverts$idsex, Group.2 = inverts$preycode, inverts$measuredweight)))
# step 3. detect where these differ
ids <- diffs[which(abs(diffs[,3] - diffs[,4]) != 0), 'Group.1']
#step 4. subset of prey species per idsex detected in step 3
nonconsolinverts <- diet[diet$idsex %in% ids ,c('idsex', 'preycode', 'prey', 'codemeasuredestimated', 'measuredestimated', 'length', 'predlength', 'predweight', 'weight', 'preyweight', 'measuredweight', 'preyunits', 'rowindex', 'idindex')]
nonconsolinverts <- subset(nonconsolinverts, preycode > 999 & preycode < 9900)
#step 5. assign the maximum of [sum(weight) and unique(measuredweight)] as the value to be used
# and assign NA to the rest of rows that correspond to that prey sp within the digestive tract section within the mammal
newweigths <- data.frame(rowindex = as.integer(rep(NA,nrow(nonconsolinverts))),idindex = as.integer(rep(NA,nrow(nonconsolinverts))),preyweight = as.numeric(rep(NA,nrow(nonconsolinverts))))
k <- 1
for (i in unique(nonconsolinverts$idsex)) {
  dat <- diet[which(diet$idsex %in% i & diet$preycode > 999 & diet$preycode < 9900),]
  for (dts in unique(dat$codedigestivetractsection)) {
    dat2 <- dat[dat$codedigestivetractsection %in% dts,]
    for (j in unique(dat2$preycode)) {
      dat3 <- dat2[dat2$preycode %in% j,]
      ww <- na.omit(dat3[, c('weight', 'idsex', 'preycode')])
      if (nrow(ww) == 0) {                              ## nested ifs to avoid the issue prey sp for which there are only NAs for weights
        if  (length(is.na(unique(dat3$preyweight))) == 1){
          if  (is.na(unique(dat3$preyweight))) {
            w <- NA
          }
        } else {
          w <- as.numeric(na.omit(unique(dat3$preyweight)))
        }
      } else {
        w <-
          max(as.numeric(na.omit(unique(dat3$preyweight))), as.numeric(aggregate(
            ww$weight,
            by = list(ww$idsex, ww$preycode),
            FUN = sum
          )['x']))
      }
      newweigths$preyweight[k:(k + nrow(dat3) - 1)] <- c(w, rep(NA, nrow(dat3) - 1))
      newweigths$rowindex[k:(k + nrow(dat3) - 1)]   <- dat3$rowindex
      newweigths$idindex[k:(k + nrow(dat3) - 1)]    <- dat3$idindex
      k <- k + nrow(dat3)
    }
  }
}
rm(list=setdiff(ls(), c('diet', 'newweigths')))

# assign the calculated values in the diet data frame
diet$preyweight[newweigths$rowindex] <- newweigths$preyweight

## multiply individual prey weights times number of prey units ----
diet$totalpreyweight <-  ifelse(!is.na(diet$preyunits),diet$preyunits * diet$preyweight,diet$preyweight)

## drop unused levels
diet <- droplevels(diet)

## save output ----
save(diet, file = 'interimsteps/diet.rdata')
