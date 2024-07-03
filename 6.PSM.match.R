#PSM数据准备，PSM不允许存在NA
#提取进行轨迹分析的ID号
ids <- gly.flw84ms.glygroup.unique %>% pull(ID)
info <- info %>% filter(ID %in% ids)
#####治疗方案#####

info <- subset(info, !is.na(InitialRegimen))
info$InitialRegimen <- gsub("双汰芝","AZT+3TC", info$InitialRegimen)
info$InitialRegimen <- gsub("克力芝","LPV/r", info$InitialRegimen)
info$InitialRegimen <- gsub("/", "+", info$InitialRegimen)

NRTIs <- c("3TC","D4T", "TDF", "TAF", "FTC", "AZT", "DDI")
NNRTIs <- c("EFV", "RPV", "DOR","NVP")
PIs <- c("LPV", "r", "DRV", "c", "NFV", "ANV")
INSTIs <- c("DTG", "RAL", "BIC", "EVG")

f.Regimenbased <- function(regimen) {
  drugs <- unlist(str_split(regimen, "\\+"))
  if (any(drugs %in% NNRTIs)) {
    return("NNRTIs-based")
  } else if (any(drugs %in% PIs)) {
    return("PIs-based")
  } else if (any(drugs %in% INSTIs)) {
    return("INSTIs-based")
  } else {
    return("Other/combinations")
  }
}

info.1 <- info %>% mutate(Regimenbased = sapply(InitialRegimen, f.Regimenbased))
info.1 <- info.1 %>%
  mutate(Regimenbased.a = case_when(
    Regimenbased == "NNRTIs-based" ~ 1,
    Regimenbased == "PIs-based" ~ 2,
    Regimenbased == "INSTIs-based" ~ 3,
    Regimenbased == "Other/combinations" ~ 4,
    TRUE ~ NA_real_  ))

#####开始随访时年龄#####
ids.StflwDate.VLgroup <- gly.flw84ms.glygroup.unique %>% select(1,6,7,17)
info.2 <- info.1 %>% left_join(ids.StflwDate.VLgroup, by = "ID") 
info.2 <- info.2 %>% ungroup %>% mutate_at(vars(ends_with("Date")), as.Date)
str(info.2)

info.2 <- info.2 %>% 
  mutate(StflwAge = floor(as.numeric(difftime(StartflwDate, BirthDate, units = "days"))/365.25))
summary(info.2$StflwAge)
hist(info.2$StflwAge)
info.2 <- info.2 %>% filter(StflwAge>18)
info.2 <- info.2 %>%
  mutate(StflwAge.a = case_when(
    StflwAge >= 18 & StflwAge <= 34 ~ 1,
    StflwAge >= 35 & StflwAge <= 49 ~ 2,
    StflwAge >= 50 ~ 3))
table(info.2$StflwAge.a)
table(info.2$Regimenbased.a)
table(info.2$Gender)

#####VL分组改为01#####
info.3 <- info.2 %>%
  mutate(VLgroup.a = case_when(
    VLgroup == "LLV" ~ 1,
    VLgroup == "VS" ~ 0))
table(info.3$VLgroup.a)
info.3 <- info.3 %>% select(1:5,23,everything())
info.3 <- info.3 %>% select(1:6,8:21, everything())
info.3 <- info.3 %>% select(1:21,23,25,everything())
info.3 <- info.3 %>% select(1:6,8:21,everything())
write.xlsx(info.1, "6.0.info.xlsx")
write.xlsx(info.2, "6.1.info.xlsx")
write.xlsx(info.3, "6.2.info.xlsx")


####4.PSM匹配####
library(MatchIt)

#####4.1Nearest Neighbor Matching#####
matchlist <- matchit(VLgroup.a ~ Regimenbased.a + Gender,
                     data = info.3,
                     method = "nearest",
                     distance = "glm",
                     caliper = 0.01,
                     ratio = 4,
                     replace = F)
summary(matchlist)
#####4.2提取匹配后的数据#####
matchdata <- match.data(matchlist,
                        group = "all",
                        distance = "distance",
                        weights = "weights",
                        subclass = "subclass",
                        data = NULL,
                        include.s.weights = TRUE,
                        drop.unmatched = TRUE)
#检验VLgroup与Regimen之间是否还有统计学差异（无）
table <- table(matchdata$VLgroup.a, matchdata$Regimenbased.a)
table
chisq.test(table)

#Warning message:
#In chisq.test(table) : Chi-squared近似算法有可能不准,改用Fisher 精确检验
fisher.test(table)

#检验VLgroup与Gender之间是否还有统计学差异（无）
table <- table(matchdata$VLgroup.a, matchdata$Gender)
chisq.test(table)

#检验VLgroup与Age之间是否还有统计学差异（无）
# table <- table(matchdata$VLgroup.a, matchdata$StflwAge.a)
# chisq.test(table)



#检验VLgroup与Glygroup之间是否有统计学差异（有）
table <- table(matchdata$VLgroup.a, matchdata$Glygroup)
chisq.test(table)

write.xlsx(matchdata,"7.1.matchdata.xlsx")
