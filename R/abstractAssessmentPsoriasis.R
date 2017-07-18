# --- 
# title: "Abstract analysis method facilitates filtering low-methodological quality and high-bias risk systematic reviews on psoriasis interventions" 
# author: "Juan Ruano" 
# date: "12 May 2017" 
# institutions: Department of Dermatology, IMIBIC/Reina Sofia University Hospital/University of Cordoba, Cordoba, Spain
# analysis: abstract-reporting completeness and full-text methodological quality and bias risk
# --- 
# 
# R version 3.3.1 (2016-06-21)
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# Running under: OS X 10.9.5 (Mavericks)


######## 0: packages ----------------

library("ggplot2")
library("tidyverse")
library("ggpubr")
library("ggsignif")
require("likert")
require("grid")
require("lattice")
require("latticeExtra")
library("tree")
detach("package:HH", unload=TRUE)
detach("package:ggfortify", unload=TRUE)


########  1: reading .csv files ----------------

DB1 <- read_csv("prismaForAbstractsDataset.csv")
names(DB1)

DB1$AMSTAR_levels        <- factor(DB1$AMSTAR_levels, levels=c("high_quality", "moderate_quality", "low_quality"))
DB1$abstract_format      <- factor(DB1$abstract_format, levels=c("8-headings", "IMRAD", "free"))
DB1$conflict_of_interest <- as.numeric(DB1$conflict_of_interest)

DB1<-as.data.frame(DB1)
DB1$total_score          <- as.numeric(DB1$total_score)
DB1$funding_academic     <- as.factor(DB1$funding_academic)
DB1$funding_academic     <- as.factor(DB1$funding_academic)
DB1$PEA1                 <- as.factor(DB1$PEA1)
DB1$PEA5                 <- as.factor(DB1$PEA5)
DB1$AMSTAR_levels_new    <- DB1$AMSTAR_levels

DB1$AMSTAR_levels_10 <- car::recode(DB1$AMSTAR_levels, "'low_quality'='low'; c('high_quality', 'moderate_quality')='high_moderate'")

########  2: Plots  ------------------
######## Fig. 2 ----------------------

DB4_all<- na.omit(DB1[,c(18:29, 4, 6, 50)])
desired.order <- c("No", "Yes")
DB4_all$PEA1 <- as.factor(DB4_all$PEA1)
DB4_all$PEA1 <- revalue(DB4_all$PEA1, c("0"="No", "1"="Yes"))
DB4_all$PEA1 <- factor(DB4_all$PEA1, levels=desired.order, ordered=TRUE)

DB4_all$PEA2 <- as.factor(DB4_all$PEA2)
DB4_all$PEA2 <- revalue(DB4_all$PEA2, c("0"="No", "1"="Yes"))
DB4_all$PEA2 <- factor(DB4_all$PEA2, levels=desired.order, ordered=TRUE)

DB4_all$PEA3 <- as.factor(DB4_all$PEA3)
DB4_all$PEA3 <- revalue(DB4_all$PEA3, c("0"="No", "1"="Yes"))
DB4_all$PEA3 <- factor(DB4_all$PEA3, levels=desired.order, ordered=TRUE)

DB4_all$PEA4 <- as.factor(DB4_all$PEA4)
DB4_all$PEA4 <- revalue(DB4_all$PEA4, c("0"="No", "1"="Yes"))
DB4_all$PEA4 <- factor(DB4_all$PEA4, levels=desired.order, ordered=TRUE)

DB4_all$PEA5 <- as.factor(DB4_all$PEA5)
DB4_all$PEA5 <- revalue(DB4_all$PEA5, c("0"="No", "1"="Yes"))
DB4_all$PEA5 <- factor(DB4_all$PEA5, levels=desired.order, ordered=TRUE)

DB4_all$PEA6 <- as.factor(DB4_all$PEA6)
DB4_all$PEA6 <- revalue(DB4_all$PEA6, c("0"="No", "1"="Yes"))
DB4_all$PEA6 <- factor(DB4_all$PEA6, levels=desired.order, ordered=TRUE)

DB4_all$PEA7 <- as.factor(DB4_all$PEA7)
DB4_all$PEA7 <- revalue(DB4_all$PEA7, c("0"="No", "1"="Yes"))
DB4_all$PEA7 <- factor(DB4_all$PEA7, levels=desired.order, ordered=TRUE)

DB4_all$PEA8 <- as.factor(DB4_all$PEA8)
DB4_all$PEA8 <- revalue(DB4_all$PEA8, c("0"="No", "1"="Yes"))
DB4_all$PEA8 <- factor(DB4_all$PEA8, levels=desired.order, ordered=TRUE)

DB4_all$PEA9 <- as.factor(DB4_all$PEA9)
DB4_all$PEA9 <- revalue(DB4_all$PEA9, c("0"="No", "1"="Yes"))
DB4_all$PEA9 <- factor(DB4_all$PEA9, levels=desired.order, ordered=TRUE)

DB4_all$PEA10 <- as.factor(DB4_all$PEA10)
DB4_all$PEA10 <- revalue(DB4_all$PEA10, c("0"="No", "1"="Yes"))
DB4_all$PEA10 <- factor(DB4_all$PEA10, levels=desired.order, ordered=TRUE)

DB4_all$PEA11 <- as.factor(DB4_all$PEA11)
DB4_all$PEA11 <- revalue(DB4_all$PEA11, c("0"="No", "1"="Yes"))
DB4_all$PEA11 <- factor(DB4_all$PEA11, levels=desired.order, ordered=TRUE)

DB4_all$PEA12 <- as.factor(DB4_all$PEA12)
DB4_all$PEA12 <- revalue(DB4_all$PEA12, c("0"="No", "1"="Yes"))
DB4_all$PEA12 <- factor(DB4_all$PEA12, levels=desired.order, ordered=TRUE)

attach(DB4_all)

######## Fig. 2 and Fig. 3 ----------------------

##### likert scale ROBIS subgroups

items <- as.data.frame(DB4_all[1:12])
items <- as.data.frame(rename(items, c(PEA12 = "Registration", PEA11 = "Funding", PEA10 = "Interpretation", PEA9 = "Strenghts and Limitations of evidence", PEA8 = "Description of the effect", PEA7 = "Synthesis of results", PEA6 = "Included studies", PEA5 = "Risk of bias", PEA4 = "Information sources", PEA3 = "Eligibility criteria", PEA2 = "Objectives", PEA1 = "Title")))
plot(likert::likert(rev(items),grouping=DB4_all$RoB_ROBIS),
     main="All SRs (n=139) ~ RoB subgroups", 
     ylab="PRISMA for abstracts items")
likert <- likert(items)
likert:Item
likert::likert.bar.plot(likert::likert(items), 
                centered = FALSE, 
                main="All SRs (n=139)", 
                ylab="PRISMA for abstracts items", 
                legend="", 
                legend.position = "right", 
                ordered=TRUE, 
                low.color="lightsalmon1", 
                high.color="skyblue3", 
                neutral.color="seagreen3")


##### likert scale AMSTAR subgroups

items <- as.data.frame(DB4_all[1:12])
items <- as.data.frame(rename(items, c(PEA1 = "Title", PEA2 = "Objectives", PEA3 = "Eligibility criteria", PEA4 = "Information sources", PEA5 = "Risk of bias", PEA6 = "Included studies", PEA7 = "Synthesis of results", PEA8 = "Description of the effect", PEA9 = "Strenghts and Limitations of evidence", PEA10 = "Interpretation", PEA11 = "Funding", PEA12 = "Registration")))

plot(likert::likert(rev(items)),grouping=DB4_all$AMSTAR_levels),
     main="All SRs (n=139) ~ AMSTAR levels", 
     ylab="PRISMA for abstracts items")

##### likert scale AMSTAR subgrupos

items <- as.data.frame(DB4_all[1:12])
items <- as.data.frame(rename(items, c(PEA1 = "Title", PEA2 = "Objectives", PEA3 = "Eligibility criteria", PEA4 = "Information sources", PEA5 = "Risk of bias", PEA6 = "Included studies", PEA7 = "Synthesis of results", PEA8 = "Description of the effect", PEA9 = "Strenghts and Limitations of evidence", PEA10 = "Interpretation", PEA11 = "Funding", PEA12 = "Registration")))

##### likert scale abstract format

plot(likert::likert(rev(items),grouping=DB4_all$abstract_format),
     main="All SRs (n=139) ~ abstract format", 
     ylab="PRISMA for abstracts items")


######## Fig. 4 and Fig. 5 ----------------------
##########  Classification trees 1/2

data      <- na.omit(DB1[,c("AMSTAR_levels_10","RoB_ROBIS","amstar_robis","PEA1", "PEA2", "PEA3","PEA4","PEA5","PEA6","PEA7","PEA8","PEA9","PEA10","PEA11","PEA12","total_score" )])
set.seed(101)
alpha     <- 0.7 # percentage of training set
inTrain   <- sample(1:nrow(data), alpha * nrow(data))
train.set <- data[inTrain,]
test.set  <- data[-inTrain,]

#### AMSTAR_levels_10
tree.model <- tree(AMSTAR_levels_10 ~PEA1+PEA2+PEA3+PEA4+PEA5+PEA6+PEA7+PEA8+PEA9+PEA10+PEA11+PEA12+total_score, data=train.set)
tree.model
summary(tree.model)
my.prediction <- predict(tree.model, test.set)
maxidx <- function(arr) {
    return(which(arr == max(arr)))
}
idx <- apply(my.prediction, c(1), maxidx)
prediction <- c('high_moderate', 'low')[idx]
table(prediction, test.set$AMSTAR_levels_10)
plot(tree.model)
text(tree.model)
pruned.tree <- prune.tree(tree.model, best=4)
plot(pruned.tree)
text(pruned.tree)

##########  Classification trees 2/2

tree <- rpart(AMSTAR_levels_10 ~ PEA1+PEA2+PEA3+PEA4+PEA5+PEA6+PEA7+PEA8+PEA9+PEA10+PEA11+PEA12+total_score,data=DB1,method="class")
rsq.rpart(tree)
rpart.plot(tree, extra=104, box.palette=list( "Blues", "Reds"),branch.lty=3, shadow.col="gray", nn=TRUE)
prp(tree, branch.type=5, yesno=FALSE, faclen=0)
plotmo(tree, type="prob", nresponse="low")
plotcp(tree)
text(tree, pretty=0)

demo1.cv <- cv.tree(tree)
plot(demo3.cv)

tree <- rpart(RoB_ROBIS ~ PEA1+PEA2+PEA3+PEA4+PEA5+PEA6+PEA7+PEA8+PEA9+PEA10+PEA11+PEA12+total_score,data=DB1,method="class")
rsq.rpart(tree)
rpart.plot(tree, extra=104, box.palette=list("Browns", "Greens"),branch.lty=3, shadow.col="gray", nn=TRUE)
prp(tree, branch.type=5, yesno=FALSE, faclen=0)
plotmo(tree, type="prob", nresponse="low")
plotcp(tree)
text(tree, pretty=0)

DB1$amstar_robis<-factor(DB1$amstar_robis)
tree <- rpart(amstar_robis ~ PEA1+PEA2+PEA3+PEA4+PEA5+PEA6+PEA7+PEA8+PEA9+PEA10+PEA11+PEA12+total_score,data=DB1,control = rpart.control(minsplit = 1, minbucket = 0))
rsq.rpart(tree)
pfit<- prune(tree, cp=0.01)
plot(pfit)
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
rpart.plot(tree, extra=104, box.palette=list("Reds", "Green"),branch.lty=3, shadow.col="gray", nn=TRUE)
prp(tree, branch.type=5, yesno=FALSE, faclen=0)
plotmo(tree, type="prob", nresponse="low")
plotcp(tree)
text(tree, pretty=0)

######## Fig. 5 ----------------------
######## Supl. Fig. 1 and Supl. Fig. 2 ----------------------

DB1$abstract_format <- factor(DB1$abstract_format,
                              levels=c("8-headings", "IMRAD", "free"))
my_comparisons      <- list(c("moderate_quality", "low_quality"), c("high_quality", "moderate_quality"), c("high_quality", "low_quality"))
my_comparisons_2    <- list(c("high", "low"))

compare_means(total_score ~ AMSTAR_levels,  
              data = na.omit(DB1[,c("total_score","RoB_ROBIS", "AMSTAR_levels")]), 
              method = "anova", group.by="RoB_ROBIS")
p <- ggboxplot(na.omit(DB1[,c("total_score","AMSTAR_levels")]), 
               x = "AMSTAR_levels", 
               y = "total_score",
               color = "AMSTAR_levels", 
               fill="AMSTAR_levels", 
               palette = "jco",
               add = "jitter")
p + stat_compare_means(comparisons=my_comparisons_2)
p + stat_compare_means(label = "p.format", method = "t.test", ref.group = ".all.")


compare_means(total_score ~ RoB_ROBIS,  
              data = na.omit(DB1[,c("total_score","RoB_ROBIS")]), 
              method = "anova")
p <- ggboxplot(na.omit(DB1[,c("total_score","RoB_ROBIS")]), 
               x = "RoB_ROBIS", y = "total_score",
               color = "RoB_ROBIS", 
               fill = "RoB_ROBIS", 
               alpha=0.3, 
               palette = "pancake",
               add = "jitter")
p + stat_compare_means(comparisons=my_comparisons_2)
p + stat_compare_means(label.y=13) 

#########  decision trees
library("rpart.plot")
library("plotmo")
library("car")


####### Regression analysis --------------------------------

####### univariate with total PRISMA
m1<-glm(data=DB1, total_score~journal_impact_factor_3)
summary(m1)

####### multivariate regression with total PRISMA

DB1$cochrane_affiliation <- as.factor(DB1$cochrane_affiliation)
DB1$total_score <- as.factor(DB1$total_score)
DB1$abstract_format_8 <- DB1$abstract_format=="8-headings"
DB1$abstract_word_count_300 <- DB1$abstract_word_count > 300
DB1$journal_impact_factor_3<-DB1$journal_impact_factor >3

m2<-polr(total_score ~ num_authors + funding_academic + journal_PRISMA_endorsement , data = DB1, Hess=TRUE, na.action=na.omit)
summary(m2)
(ctable <- coef(summary(m2)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ci <- confint(m2))
confint.default(m2)
exp(coef(m2))
exp(cbind(OR = coef(m2), ci))










