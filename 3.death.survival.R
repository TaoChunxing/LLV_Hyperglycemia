####4.查找开始随访日期早于死亡日期的ID并剔除####
#计算SurvTime.m和SurvivalStatus
ids.death <- death %>% select(1:2)
survival <- gly.flw84ms.glygroup %>% left_join(ids.death, by="ID") %>% 
  group_by(ID) %>% arrange(ID, TestDate) %>% 
  mutate(SurvTime.m = ifelse(
    is.na(DiedDate),
    round(interval(start=StartflwDate, end=max(TestDate)) / months(1), digits = 2),
    round(interval(start=StartflwDate, end=DiedDate) / months(1), digits = 2)))
survival.2 <- survival %>% 
  mutate(SurvivalStatus=ifelse(is.na(DiedDate), 0, 1)) %>% 
  mutate(SurvivalStatus=ifelse(SurvivalStatus==1&SurvTime.m>=84, 0, SurvivalStatus))%>% 
  mutate(SurvTime.m = ifelse(SurvTime.m>=84,84,SurvTime.m))

#保留一条ID记录并简化数据框
survival.2.unique <- survival.2 %>% group_by(ID) %>% distinct(ID, .keep_all = TRUE)
summary(survival.2.unique$SurvTime.m)
ids.testlaterdeath <- survival.2.unique %>% filter(SurvTime.m<=0) %>% pull(ID)
survival.2 <- survival.2 %>% filter(!ID %in% ids.testlaterdeath)
write.xlsx(survival.2, "3.0.survival.xlsx")
#剔除gly.flw84ms.glygroup中错误的id(检测时间晚于死亡时间的ID)
gly.flw84ms.glygroup <- gly.flw84ms.glygroup %>% filter(!ID %in% ids.testlaterdeath)
gly.flw84ms.glygroup.unique <- gly.flw84ms.glygroup %>% group_by(ID) %>% distinct(ID, .keep_all = TRUE)
write.xlsx(gly.flw84ms.glygroup, "2.3.gly.flw84ms.glygroup.xlsx")
table(gly.flw84ms.glygroup.unique$VLgroup)
table(gly.flw84ms.glygroup.unique$Glygroup)
