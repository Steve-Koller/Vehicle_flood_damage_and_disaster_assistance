# Regression runs corresponding to "Vehicle flood damage and disaster assistance in the United States" 
# Citation:  Koller, S. F. (2025). Vehicle flood damage and household disaster assistance in the United States. Risk Management & Insurance Review, 1–40. https://doi.org/10.1111/rmir.70002

# Note: file to be used for replication titled "VFD_all.csv"

library(pacman)
p_load(pryr, AER, data.table, tidyverse, ggplot2, grattantheme, scales, tigris, 
       mapview, ggspatial, viridis, fixest, FedData, AER, censReg,texreg, fixest)

#read in files
VFD_all = fread("VFD_all.csv", fill=TRUE)

# create "applied year" variable
VFD_all$appliedyear = substr_right(VFD_all$`Applied Date`, 2)
VFD_all$appliedyear = paste0("20", VFD_all$appliedyear)

# create numeric "approved_transportation_amount" variable
VFD_all$approved_transportation_amount = as.numeric(VFD_all$`Approved Transportation Amount`)

# create numeric "Water Level_num" variable
VFD_all$`Water Level_num` = as.numeric(VFD_all$`Water Level`)

# create subset dataframe only pertaining to Transportation Assistance applications that received an award. 
ReceivedTA_all = subset(VFD_all, `Approved Transportation Amount`>0)


# create binary outcome variable for TA award >0 (1) and =0 (0).
VFD_all = VFD_all %>% 
  mutate(ReceivedTA_binary = case_when(`Approved Transportation Amount`>0 ~ 1,
                                       `Approved Transportation Amount`==0 ~ 0))

## Main text regressions----
### Table 3---- 
probit1 = feglm(ReceivedTA_binary ~ i(`Gross Income`, ref="$30,001-$60,000")+ `Water Level_num`, data = VFD_all, family = "probit", vcov="hetero")

probit2 = feglm(ReceivedTA_binary ~ i(`Gross Income`, ref="$30,001-$60,000") + `Water Level_num` + i(`Household Composition`, ref="3"), data = VFD_all, family = "probit",vcov="hetero")

probit3 = feglm(ReceivedTA_binary ~ i(`Gross Income`, ref="$30,001-$60,000") + `Water Level_num` + i(`Flood Insurance Y/N`, ref="N") +  i(`Own/Rent`, ref="O") + i(`Household Composition`, ref="3"), data = VFD_all, family = "probit", vcov="hetero")

probit4 = feglm(ReceivedTA_binary ~ i(`Gross Income`, ref="$30,001-$60,000") + `Water Level_num`+ i(`Flood Insurance Y/N`, ref="N") + i(`Own/Rent`, ref="O")+ i(`Household Composition`, ref="3") | `Disaster Number`, data = VFD_all, family = "probit", vcov="hetero")

probit5 = feglm(ReceivedTA_binary ~ i(`Gross Income`, ref="$30,001-$60,000") + `Water Level_num`+ i(`Flood Insurance Y/N`, ref="N") +  i(`Own/Rent`, ref="O") + i(`Household Composition`, ref="3")| FIPS^appliedyear, data = VFD_all, family = "probit", cluster="FIPS")

probit6 = feglm(ReceivedTA_binary ~ i(`Gross Income`, ref="$30,001-$60,000") + `Water Level_num` + i(`Flood Insurance Y/N`, ref="N") +  i(`Own/Rent`, ref="O")+ i(`Household Composition`, ref="3")| FIPS^appliedyear + `Disaster Number`, data = VFD_all, family = "probit", cluster="FIPS")

probit7 = feglm(ReceivedTA_binary ~ i(`Gross Income`, ref="$30,001-$60,000") + `Water Level_num` + i(`SBA Approved`, ref="N") + i(`Flood Insurance Y/N`, ref="N") +  i(`Own/Rent`, ref="O")+ i(`Household Composition`, ref="3")| FIPS^appliedyear + `Disaster Number`, data = VFD_all, family = "probit", cluster="FIPS")

etable(probit1, probit2, probit3, probit4, probit5, probit6, probit7, tex=TRUE)

etable(probit7, tex=TRUE)

### Table 4----

ReceivedTA_all$approved_transportation_amount = as.numeric(ReceivedTA_all$approved_transportation_amount )

lm1 = feols(log(approved_transportation_amount) ~  i(`Gross Income`, ref="$30,001-$60,000") +`Water Level_num`  , ReceivedTA_all , vcov="hetero")
summary(lm1)

lm2 = feols(log(approved_transportation_amount) ~  i(`Gross Income`, ref="$30,001-$60,000") +`Water Level_num` + i(`Household Composition`, ref="3") , ReceivedTA_all,vcov="hetero")
summary(lm2)

lm3 = feols(log(approved_transportation_amount) ~  i(`Gross Income`, ref="$30,001-$60,000") +`Water Level_num` + i(`Household Composition`, ref="3")+`Flood Insurance Y/N`+i(`Own/Rent`, ref="O") , ReceivedTA_all, vcov="hetero")
summary(lm3)

lm4 = feols(log(approved_transportation_amount) ~  i(`Gross Income`, ref="$30,001-$60,000") +`Water Level_num` + i(`Household Composition`, ref="3")+`Flood Insurance Y/N`+i(`Own/Rent`, ref="O") | `Disaster Number` , ReceivedTA_all, vcov="hetero")
summary(lm4)

lm5 = feols(log(approved_transportation_amount) ~  i(`Gross Income`, ref="$30,001-$60,000") +`Water Level_num` + i(`Household Composition`, ref="3")+`Flood Insurance Y/N`+i(`Own/Rent`, ref="O") | FIPS^appliedyear , ReceivedTA_all, cluster="FIPS")
summary(lm5)

lm6 = feols(log(approved_transportation_amount) ~  i(`Gross Income`, ref="$30,001-$60,000") +`Water Level_num` + i(`Household Composition`, ref="3")+`Flood Insurance Y/N`+i(`Own/Rent`, ref="O") | FIPS^appliedyear+`Disaster Number` , ReceivedTA_all, cluster="FIPS")
summary(lm6)

lm7 = feols(log(approved_transportation_amount) ~  i(`Gross Income`, ref="$30,001-$60,000") +`Water Level_num` +i(`SBA Approved`, ref="N") + i(`Household Composition`, ref="3")+`Flood Insurance Y/N`+i(`Own/Rent`, ref="O") | FIPS^appliedyear+`Disaster Number` , ReceivedTA_all, cluster="FIPS")
summary(lm7)

etable(lm1, lm2, lm3, lm4, lm5, lm6,lm7, tex=TRUE)




## Appendix regressions----

### Table A1----
feols1_ext = feols(ReceivedTA_binary ~ i(`Gross Income`, ref="$30,001-$60,000")+ `Water Level_num`, data = VFD_all,  vcov="hetero")

feols2_ext = feglm(ReceivedTA_binary ~ i(`Gross Income`, ref="$30,001-$60,000") + `Water Level_num` + i(`Household Composition`, ref="3"), data = VFD_all, ,vcov="hetero")

feols3_ext = feglm(ReceivedTA_binary ~ i(`Gross Income`, ref="$30,001-$60,000") + `Water Level_num` + i(`Flood Insurance Y/N`, ref="N") +  i(`Own/Rent`, ref="O") + i(`Household Composition`, ref="3"), data = VFD_all,  vcov="hetero")

feols4_ext = feglm(ReceivedTA_binary ~ i(`Gross Income`, ref="$30,001-$60,000") + `Water Level_num`+ i(`Flood Insurance Y/N`, ref="N") + i(`Own/Rent`, ref="O")+ i(`Household Composition`, ref="3") | `Disaster Number`, data = VFD_all,  vcov="hetero")

feols5_ext = feglm(ReceivedTA_binary ~ i(`Gross Income`, ref="$30,001-$60,000") + `Water Level_num`+ i(`Flood Insurance Y/N`, ref="N") +  i(`Own/Rent`, ref="O") + i(`Household Composition`, ref="3")| FIPS^appliedyear, data = VFD_all, cluster="FIPS")

feols6_ext = feglm(ReceivedTA_binary ~ i(`Gross Income`, ref="$30,001-$60,000") + `Water Level_num` + i(`Flood Insurance Y/N`, ref="N") +  i(`Own/Rent`, ref="O")+ i(`Household Composition`, ref="3")| FIPS^appliedyear + `Disaster Number`, data = VFD_all,  cluster="FIPS")

feols7_ext = feglm(ReceivedTA_binary ~ i(`Gross Income`, ref="$30,001-$60,000") + `Water Level_num` + i(`SBA Approved`, ref="N") + i(`Flood Insurance Y/N`, ref="N") +  i(`Own/Rent`, ref="O")+ i(`Household Composition`, ref="3")| FIPS^appliedyear + `Disaster Number`, data = VFD_all, cluster="FIPS")

etable(feols1_ext, feols2_ext, feols3_ext, feols4_ext, feols5_ext, feols6_ext, feols7_ext, tex=TRUE)


### Table A2----
pois1 = fepois(approved_transportation_amount ~ i(`Gross Income`, ref="$30,001-$60,000") +`Water Level_num`, data = VFD_all, vcov="hetero")

pois2 = fepois(approved_transportation_amount ~ i(`Gross Income`, ref="$30,001-$60,000") +`Water Level_num` + i(`Household Composition`, ref="3"), data = VFD_all, vcov="hetero")

pois3 = fepois(approved_transportation_amount ~ i(`Gross Income`, ref="$30,001-$60,000") + +`Water Level_num` + i(`Household Composition`, ref="3")+ i(`Flood Insurance Y/N`, ref="N")+i(`Own/Rent`, ref="O"), data = VFD_all,  vcov="hetero")

pois4 = fepois(approved_transportation_amount ~ i(`Gross Income`, ref="$30,001-$60,000") + +`Water Level_num` + i(`Household Composition`, ref="3")+ i(`Flood Insurance Y/N`, ref="N") + i(`Own/Rent`, ref="O") | `Disaster Number`, data = VFD_all, vcov="hetero")

pois5 = fepois(approved_transportation_amount ~ i(`Gross Income`, ref="$30,001-$60,000") + +`Water Level_num` + i(`Household Composition`, ref="3")+ i(`Flood Insurance Y/N`, ref="N") + i(`Own/Rent`, ref="O") | FIPS^appliedyear, data = VFD_all, cluster="FIPS")

pois6 = fepois(approved_transportation_amount ~ i(`Gross Income`, ref="$30,001-$60,000") + +`Water Level_num` + i(`Household Composition`, ref="3")+ i(`Flood Insurance Y/N`, ref="N") + i(`Own/Rent`, ref="O") | FIPS^appliedyear + `Disaster Number`, data = VFD_all, cluster="FIPS")

pois7 = fepois(approved_transportation_amount ~ i(`Gross Income`, ref="$30,001-$60,000") + +`Water Level_num` + i(`SBA Approved`, ref="N") + i(`Household Composition`, ref="3")+ i(`Flood Insurance Y/N`, ref="N") + i(`Own/Rent`, ref="O") | FIPS^appliedyear + `Disaster Number`, data = VFD_all, cluster="FIPS")

etable(pois1, pois2, pois3, pois4, pois5, pois6, tex=TRUE)

etable(pois7, tex=TRUE)


### Table A3----


lm1R = feols(approved_transportation_amount ~  i(`Gross Income`, ref="$30,001-$60,000") +`Water Level_num`  , ReceivedTA_all , vcov="hetero")
summary(lm1R)

lm2R = feols(approved_transportation_amount ~  i(`Gross Income`, ref="$30,001-$60,000") +`Water Level_num` + i(`Household Composition`, ref="3") , ReceivedTA_all,vcov="hetero")
summary(lm2R)

lm3R = feols(approved_transportation_amount ~  i(`Gross Income`, ref="$30,001-$60,000") +`Water Level_num` + i(`Household Composition`, ref="3")+`Flood Insurance Y/N`+i(`Own/Rent`, ref="O") , ReceivedTA_all, vcov="hetero")
summary(lm3R)

lm4R = feols(approved_transportation_amount ~  i(`Gross Income`, ref="$30,001-$60,000") +`Water Level_num` + i(`Household Composition`, ref="3")+`Flood Insurance Y/N`+i(`Own/Rent`, ref="O") | `Disaster Number` , ReceivedTA_all, vcov="hetero")
summary(lm4R)

lm5R = feols(approved_transportation_amount ~  i(`Gross Income`, ref="$30,001-$60,000") +`Water Level_num` + i(`Household Composition`, ref="3")+`Flood Insurance Y/N`+i(`Own/Rent`, ref="O") | FIPS^appliedyear , ReceivedTA_all, cluster="FIPS")
summary(lm5R)

lm6R = feols(approved_transportation_amount ~  i(`Gross Income`, ref="$30,001-$60,000") +`Water Level_num` + i(`Household Composition`, ref="3")+`Flood Insurance Y/N`+i(`Own/Rent`, ref="O") | FIPS^appliedyear+`Disaster Number` , ReceivedTA_all, cluster="FIPS")
summary(lm6R)

lm7R = feols(approved_transportation_amount ~  i(`Gross Income`, ref="$30,001-$60,000") +`Water Level_num` +i(`SBA Approved`, ref="N") + i(`Household Composition`, ref="3")+`Flood Insurance Y/N`+i(`Own/Rent`, ref="O") | FIPS^appliedyear+`Disaster Number` , ReceivedTA_all, cluster="FIPS")
summary(lm7R)

etable(lm1R, lm2R, lm3R, lm4R, lm5R, lm6R, lm7R, tex=TRUE)



