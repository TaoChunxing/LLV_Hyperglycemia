####5.lcmm识别不同轨迹组####
library(lcmm)
#####5.1简化数据框，生成Subject#####
hlme <- gly.flw84ms.glygroup.new %>% select( 1, 7, 13, 16, 17) %>% 
  group_by(ID) %>% arrange(ID, Flwtime.m)%>% 
  mutate(Subject = cur_group_id())

write.xlsx(hlme, "4.0.hlme.xlsx")

#####5.2计算模型#####
model1 <- hlme(fixed = Gly ~ 1 + Flwtime.m + I(Flwtime.m^2),
                random = ~ 1 + Flwtime.m,
                ng = 1, 
                data = hlme, 
                subject = "Subject")
summary(model1)
model2 <- hlme(fixed = Gly ~ 1 + Flwtime.m + I(Flwtime.m^2),
                mixture = ~ 1 + Flwtime.m + I(Flwtime.m^2),
                random = ~ 1 + Flwtime.m,
                ng = 2,
                nwg = TRUE, 
                B = model1,
                idiag = FALSE, 
                maxiter = 500,
                data = hlme, 
                subject = "Subject")
summary(model2)
postprob(model2)
#####5.3绘图#####
windows(width=10, height=8)
datnew  <- data.frame(Flwtime.m = seq(0,84, length = 1000))
plotpred <- predictY(model2, datnew, var.time ="Flwtime.m")
plot(plotpred, lty = 1, lwd = 5,marg=FALSE, shades=T,
     xlab="Follow Time (Months)", ylab="Blood Glucose Level (mmol/L)", 
     legend.loc = "topleft", cex=0.75)

#####5.4提取不同轨迹组的对象#####
class <- model2$pprob[,1:2]
#####5.5合并hlme与class#####
hlme.class <- hlme %>% left_join(class, by = "Subject") %>% rename(Class=class)
hlme.class.unique <- hlme.class %>% distinct(ID, .keep_all = TRUE)

#####5.6制作VLgroup与class的交叉表#####
table <- table(hlme.class.unique$VLgroup, hlme.class.unique$Class)
percent <- prop.table(table, margin = 1) * 100
count.percent <- table
for (i in 1:nrow(table)) {
  for (j in 1:ncol(table)) {
    count.percent[i, j] <- paste0(table[i, j], " (", round(percent[i, j], 2), "%)")
  }
}
count.percent <- as.data.frame.matrix(count.percent)
rowtotals <- rowSums(table)
coltotals <- colSums(table)
count.percent$Total <- rowtotals
count.percent <- rbind(count.percent, Total = c(coltotals, sum(coltotals)))
#count.percent <- count.percent %>% select(`1`, `0`,`Total`)
count.percent
chisq.test(table)
fisher.test(table)

