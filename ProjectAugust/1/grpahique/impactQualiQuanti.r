setwd("/Users/julienhubar/Documents/#Master1/HDDA/High-dimensional-data-analysis/ProjectAugust")
data <- read.table("hcc-data.txt", header = FALSE, na.strings = "?", sep = ",")
attach(data)
#Imparct of V1
pdf("boxplotImpactV1.pdf")
par(mfrow=c(2,4))
boxplot(V24~V1,varwidth = TRUE)
boxplot(V30~V1,varwidth = TRUE)
boxplot(V31~V1,varwidth = TRUE)
boxplot(V32~V1,varwidth = TRUE)
boxplot(V33~V1,varwidth = TRUE)
boxplot(V34~V1,varwidth = TRUE)
boxplot(V35~V1,varwidth = TRUE)
boxplot(V36~V1,varwidth = TRUE)
boxplot(V37~V1,varwidth = TRUE)
boxplot(V38~V1,varwidth = TRUE)
boxplot(V39~V1,varwidth = TRUE)
boxplot(V40~V1,varwidth = TRUE)
boxplot(V41~V1,varwidth = TRUE)
boxplot(V42~V1,varwidth = TRUE)
boxplot(V43~V1,varwidth = TRUE)
boxplot(V44~V1,varwidth = TRUE)
boxplot(V45~V1,varwidth = TRUE)
dev.off()
#Imparct of V2
pdf("boxplotImpactV2.pdf")
par(mfrow=c(2,4))
boxplot(V24~V2,varwidth = TRUE)
boxplot(V24~V2,varwidth = TRUE)
boxplot(V30~V2,varwidth = TRUE)
boxplot(V31~V2,varwidth = TRUE)
boxplot(V32~V2,varwidth = TRUE)
boxplot(V33~V2,varwidth = TRUE)
boxplot(V34~V2,varwidth = TRUE)
boxplot(V35~V2,varwidth = TRUE)
boxplot(V36~V2,varwidth = TRUE)
boxplot(V37~V2,varwidth = TRUE)
boxplot(V38~V2,varwidth = TRUE)
boxplot(V39~V2,varwidth = TRUE)
boxplot(V40~V2,varwidth = TRUE)
boxplot(V41~V2,varwidth = TRUE)
boxplot(V42~V2,varwidth = TRUE)
boxplot(V43~V2,varwidth = TRUE)
boxplot(V44~V2,varwidth = TRUE)
boxplot(V45~V2,varwidth = TRUE)
dev.off()
#Imparct of V3
pdf("boxplotImpactV3.pdf")
par(mfrow=c(2,4))
boxplot(V24~V3,varwidth = TRUE)
boxplot(V30~V3,varwidth = TRUE)
boxplot(V31~V3,varwidth = TRUE)
boxplot(V32~V3,varwidth = TRUE)
boxplot(V33~V3,varwidth = TRUE)
boxplot(V34~V3,varwidth = TRUE)
boxplot(V35~V3,varwidth = TRUE)
boxplot(V36~V3,varwidth = TRUE)
boxplot(V37~V3,varwidth = TRUE)
boxplot(V38~V3,varwidth = TRUE)
boxplot(V39~V3,varwidth = TRUE)
boxplot(V40~V3,varwidth = TRUE)
boxplot(V41~V3,varwidth = TRUE)
boxplot(V42~V3,varwidth = TRUE)
boxplot(V43~V3,varwidth = TRUE)
boxplot(V44~V3,varwidth = TRUE)
boxplot(V45~V3,varwidth = TRUE)
dev.off()
#Imparct of V4
pdf("boxplotImpactV4.pdf")
par(mfrow=c(2,4))
boxplot(V24~V4,varwidth = TRUE)
boxplot(V30~V4,varwidth = TRUE)
boxplot(V31~V4,varwidth = TRUE)
boxplot(V32~V4,varwidth = TRUE)
boxplot(V33~V4,varwidth = TRUE)
boxplot(V34~V4,varwidth = TRUE)
boxplot(V35~V4,varwidth = TRUE)
boxplot(V36~V4,varwidth = TRUE)
boxplot(V37~V4,varwidth = TRUE)
boxplot(V38~V4,varwidth = TRUE)
boxplot(V39~V4,varwidth = TRUE)
boxplot(V40~V4,varwidth = TRUE)
boxplot(V41~V4,varwidth = TRUE)
boxplot(V42~V4,varwidth = TRUE)
boxplot(V43~V4,varwidth = TRUE)
boxplot(V44~V4,varwidth = TRUE)
boxplot(V45~V4,varwidth = TRUE)
dev.off()
#Imparct of V6
pdf("boxplotImpactV6.pdf")
par(mfrow=c(2,4))
boxplot(V24~V6,varwidth = TRUE)
boxplot(V30~V6,varwidth = TRUE)
boxplot(V31~V6,varwidth = TRUE)
boxplot(V32~V6,varwidth = TRUE)
boxplot(V33~V6,varwidth = TRUE)
boxplot(V34~V6,varwidth = TRUE)
boxplot(V35~V6,varwidth = TRUE)
boxplot(V36~V6,varwidth = TRUE)
boxplot(V37~V6,varwidth = TRUE)
boxplot(V38~V6,varwidth = TRUE)
boxplot(V39~V6,varwidth = TRUE)
boxplot(V40~V6,varwidth = TRUE)
boxplot(V41~V6,varwidth = TRUE)
boxplot(V42~V6,varwidth = TRUE)
boxplot(V43~V6,varwidth = TRUE)
boxplot(V44~V6,varwidth = TRUE)
boxplot(V45~V6,varwidth = TRUE)
dev.off()
#Imparct of V7
pdf("boxplotImpactV7.pdf")
par(mfrow=c(2,4))
boxplot(V24~V7,varwidth = TRUE)
boxplot(V30~V7,varwidth = TRUE)
boxplot(V31~V7,varwidth = TRUE)
boxplot(V32~V7,varwidth = TRUE)
boxplot(V33~V7,varwidth = TRUE)
boxplot(V34~V7,varwidth = TRUE)
boxplot(V35~V7,varwidth = TRUE)
boxplot(V36~V7,varwidth = TRUE)
boxplot(V37~V7,varwidth = TRUE)
boxplot(V38~V7,varwidth = TRUE)
boxplot(V39~V7,varwidth = TRUE)
boxplot(V40~V7,varwidth = TRUE)
boxplot(V41~V7,varwidth = TRUE)
boxplot(V42~V7,varwidth = TRUE)
boxplot(V43~V7,varwidth = TRUE)
boxplot(V44~V7,varwidth = TRUE)
boxplot(V45~V7,varwidth = TRUE)
dev.off()
#Imparct of V8
pdf("boxplotImpactV8.pdf")
par(mfrow=c(2,4))
boxplot(V24~V8,varwidth = TRUE)
boxplot(V30~V8,varwidth = TRUE)
boxplot(V31~V8,varwidth = TRUE)
boxplot(V32~V8,varwidth = TRUE)
boxplot(V33~V8,varwidth = TRUE)
boxplot(V34~V8,varwidth = TRUE)
boxplot(V35~V8,varwidth = TRUE)
boxplot(V36~V8,varwidth = TRUE)
boxplot(V37~V8,varwidth = TRUE)
boxplot(V38~V8,varwidth = TRUE)
boxplot(V39~V8,varwidth = TRUE)
boxplot(V40~V8,varwidth = TRUE)
boxplot(V41~V8,varwidth = TRUE)
boxplot(V42~V8,varwidth = TRUE)
boxplot(V43~V8,varwidth = TRUE)
boxplot(V44~V8,varwidth = TRUE)
boxplot(V45~V8,varwidth = TRUE)
dev.off()
#Imparct of V11
pdf("boxplotImpactV11.pdf")
par(mfrow=c(2,4))
boxplot(V24~V11,varwidth = TRUE)
boxplot(V30~V11,varwidth = TRUE)
boxplot(V31~V11,varwidth = TRUE)
boxplot(V32~V11,varwidth = TRUE)
boxplot(V33~V11,varwidth = TRUE)
boxplot(V34~V11,varwidth = TRUE)
boxplot(V35~V11,varwidth = TRUE)
boxplot(V36~V11,varwidth = TRUE)
boxplot(V37~V11,varwidth = TRUE)
boxplot(V38~V11,varwidth = TRUE)
boxplot(V39~V11,varwidth = TRUE)
boxplot(V40~V11,varwidth = TRUE)
boxplot(V41~V11,varwidth = TRUE)
boxplot(V42~V11,varwidth = TRUE)
boxplot(V43~V11,varwidth = TRUE)
boxplot(V44~V11,varwidth = TRUE)
boxplot(V45~V11,varwidth = TRUE)
dev.off()
#Imparct of V12
pdf("boxplotImpactV12.pdf")
par(mfrow=c(2,4))
boxplot(V24~V12,varwidth = TRUE)
boxplot(V30~V12,varwidth = TRUE)
boxplot(V31~V12,varwidth = TRUE)
boxplot(V32~V12,varwidth = TRUE)
boxplot(V33~V12,varwidth = TRUE)
boxplot(V34~V12,varwidth = TRUE)
boxplot(V35~V12,varwidth = TRUE)
boxplot(V36~V12,varwidth = TRUE)
boxplot(V37~V12,varwidth = TRUE)
boxplot(V38~V12,varwidth = TRUE)
boxplot(V39~V12,varwidth = TRUE)
boxplot(V40~V12,varwidth = TRUE)
boxplot(V41~V12,varwidth = TRUE)
boxplot(V42~V12,varwidth = TRUE)
boxplot(V43~V12,varwidth = TRUE)
boxplot(V44~V12,varwidth = TRUE)
boxplot(V45~V12,varwidth = TRUE)
dev.off()
#Imparct of V13
pdf("boxplotImpactV13.pdf")
par(mfrow=c(2,4))
boxplot(V24~V13,varwidth = TRUE)
boxplot(V30~V13,varwidth = TRUE)
boxplot(V31~V13,varwidth = TRUE)
boxplot(V32~V13,varwidth = TRUE)
boxplot(V33~V13,varwidth = TRUE)
boxplot(V34~V13,varwidth = TRUE)
boxplot(V35~V13,varwidth = TRUE)
boxplot(V36~V13,varwidth = TRUE)
boxplot(V37~V13,varwidth = TRUE)
boxplot(V38~V13,varwidth = TRUE)
boxplot(V39~V13,varwidth = TRUE)
boxplot(V40~V13,varwidth = TRUE)
boxplot(V41~V13,varwidth = TRUE)
boxplot(V42~V13,varwidth = TRUE)
boxplot(V43~V13,varwidth = TRUE)
boxplot(V44~V13,varwidth = TRUE)
boxplot(V45~V13,varwidth = TRUE)
dev.off()
#Imparct of V14
pdf("boxplotImpactV14.pdf")
par(mfrow=c(2,4))
boxplot(V24~V14,varwidth = TRUE)
boxplot(V30~V14,varwidth = TRUE)
boxplot(V31~V14,varwidth = TRUE)
boxplot(V32~V14,varwidth = TRUE)
boxplot(V33~V14,varwidth = TRUE)
boxplot(V34~V14,varwidth = TRUE)
boxplot(V35~V14,varwidth = TRUE)
boxplot(V36~V14,varwidth = TRUE)
boxplot(V37~V14,varwidth = TRUE)
boxplot(V38~V14,varwidth = TRUE)
boxplot(V39~V14,varwidth = TRUE)
boxplot(V40~V14,varwidth = TRUE)
boxplot(V41~V14,varwidth = TRUE)
boxplot(V42~V14,varwidth = TRUE)
boxplot(V43~V14,varwidth = TRUE)
boxplot(V44~V14,varwidth = TRUE)
boxplot(V45~V14,varwidth = TRUE)
dev.off()
#Imparct of V15
pdf("boxplotImpactV15.pdf")
par(mfrow=c(2,4))
boxplot(V24~V15,varwidth = TRUE)
boxplot(V30~V15,varwidth = TRUE)
boxplot(V31~V15,varwidth = TRUE)
boxplot(V32~V15,varwidth = TRUE)
boxplot(V33~V15,varwidth = TRUE)
boxplot(V34~V15,varwidth = TRUE)
boxplot(V35~V15,varwidth = TRUE)
boxplot(V36~V15,varwidth = TRUE)
boxplot(V37~V15,varwidth = TRUE)
boxplot(V38~V15,varwidth = TRUE)
boxplot(V39~V15,varwidth = TRUE)
boxplot(V40~V15,varwidth = TRUE)
boxplot(V41~V15,varwidth = TRUE)
boxplot(V42~V15,varwidth = TRUE)
boxplot(V43~V15,varwidth = TRUE)
boxplot(V44~V15,varwidth = TRUE)
boxplot(V45~V15,varwidth = TRUE)
dev.off()
#Imparct of V16
pdf("boxplotImpactV16.pdf")
par(mfrow=c(2,4))
boxplot(V24~V16,varwidth = TRUE)
boxplot(V30~V16,varwidth = TRUE)
boxplot(V31~V16,varwidth = TRUE)
boxplot(V32~V16,varwidth = TRUE)
boxplot(V33~V16,varwidth = TRUE)
boxplot(V34~V16,varwidth = TRUE)
boxplot(V35~V16,varwidth = TRUE)
boxplot(V36~V16,varwidth = TRUE)
boxplot(V37~V16,varwidth = TRUE)
boxplot(V38~V16,varwidth = TRUE)
boxplot(V39~V16,varwidth = TRUE)
boxplot(V40~V16,varwidth = TRUE)
boxplot(V41~V16,varwidth = TRUE)
boxplot(V42~V16,varwidth = TRUE)
boxplot(V43~V16,varwidth = TRUE)
boxplot(V44~V16,varwidth = TRUE)
boxplot(V45~V16,varwidth = TRUE)
dev.off()
#Imparct of V17
pdf("boxplotImpactV17.pdf")
par(mfrow=c(2,4))
boxplot(V24~V17,varwidth = TRUE)
boxplot(V30~V17,varwidth = TRUE)
boxplot(V31~V17,varwidth = TRUE)
boxplot(V32~V17,varwidth = TRUE)
boxplot(V33~V17,varwidth = TRUE)
boxplot(V34~V17,varwidth = TRUE)
boxplot(V35~V17,varwidth = TRUE)
boxplot(V36~V17,varwidth = TRUE)
boxplot(V37~V17,varwidth = TRUE)
boxplot(V38~V17,varwidth = TRUE)
boxplot(V39~V17,varwidth = TRUE)
boxplot(V40~V17,varwidth = TRUE)
boxplot(V41~V17,varwidth = TRUE)
boxplot(V42~V17,varwidth = TRUE)
boxplot(V43~V17,varwidth = TRUE)
boxplot(V44~V17,varwidth = TRUE)
boxplot(V45~V17,varwidth = TRUE)
dev.off()#Imparct of V19
pdf("boxplotImpactV19.pdf")
par(mfrow=c(2,4))
boxplot(V24~V19,varwidth = TRUE)
boxplot(V30~V19,varwidth = TRUE)
boxplot(V31~V19,varwidth = TRUE)
boxplot(V32~V19,varwidth = TRUE)
boxplot(V33~V19,varwidth = TRUE)
boxplot(V34~V19,varwidth = TRUE)
boxplot(V35~V19,varwidth = TRUE)
boxplot(V36~V19,varwidth = TRUE)
boxplot(V37~V19,varwidth = TRUE)
boxplot(V38~V19,varwidth = TRUE)
boxplot(V39~V19,varwidth = TRUE)
boxplot(V40~V19,varwidth = TRUE)
boxplot(V41~V19,varwidth = TRUE)
boxplot(V42~V19,varwidth = TRUE)
boxplot(V43~V19,varwidth = TRUE)
boxplot(V44~V19,varwidth = TRUE)
boxplot(V45~V19,varwidth = TRUE)
dev.off()
#Imparct of V20
pdf("boxplotImpactV20.pdf")
par(mfrow=c(2,4))
boxplot(V24~V20,varwidth = TRUE)
boxplot(V30~V20,varwidth = TRUE)
boxplot(V31~V20,varwidth = TRUE)
boxplot(V32~V20,varwidth = TRUE)
boxplot(V33~V20,varwidth = TRUE)
boxplot(V34~V20,varwidth = TRUE)
boxplot(V35~V20,varwidth = TRUE)
boxplot(V36~V20,varwidth = TRUE)
boxplot(V37~V20,varwidth = TRUE)
boxplot(V38~V20,varwidth = TRUE)
boxplot(V39~V20,varwidth = TRUE)
boxplot(V40~V20,varwidth = TRUE)
boxplot(V41~V20,varwidth = TRUE)
boxplot(V42~V20,varwidth = TRUE)
boxplot(V43~V20,varwidth = TRUE)
boxplot(V44~V20,varwidth = TRUE)
boxplot(V45~V20,varwidth = TRUE)
dev.off()
#Imparct of V21
pdf("boxplotImpactV21.pdf")
par(mfrow=c(2,4))
boxplot(V24~V21,varwidth = TRUE)
boxplot(V30~V21,varwidth = TRUE)
boxplot(V31~V21,varwidth = TRUE)
boxplot(V32~V21,varwidth = TRUE)
boxplot(V33~V21,varwidth = TRUE)
boxplot(V34~V21,varwidth = TRUE)
boxplot(V35~V21,varwidth = TRUE)
boxplot(V36~V21,varwidth = TRUE)
boxplot(V37~V21,varwidth = TRUE)
boxplot(V38~V21,varwidth = TRUE)
boxplot(V39~V21,varwidth = TRUE)
boxplot(V40~V21,varwidth = TRUE)
boxplot(V41~V21,varwidth = TRUE)
boxplot(V42~V21,varwidth = TRUE)
boxplot(V43~V21,varwidth = TRUE)
boxplot(V44~V21,varwidth = TRUE)
boxplot(V45~V21,varwidth = TRUE)
dev.off()
#Imparct of V22
pdf("boxplotImpactV22.pdf")
par(mfrow=c(2,4))
boxplot(V24~V22,varwidth = TRUE)
boxplot(V30~V22,varwidth = TRUE)
boxplot(V31~V22,varwidth = TRUE)
boxplot(V32~V22,varwidth = TRUE)
boxplot(V33~V22,varwidth = TRUE)
boxplot(V34~V22,varwidth = TRUE)
boxplot(V35~V22,varwidth = TRUE)
boxplot(V36~V22,varwidth = TRUE)
boxplot(V37~V22,varwidth = TRUE)
boxplot(V38~V22,varwidth = TRUE)
boxplot(V39~V22,varwidth = TRUE)
boxplot(V40~V22,varwidth = TRUE)
boxplot(V41~V22,varwidth = TRUE)
boxplot(V42~V22,varwidth = TRUE)
boxplot(V43~V22,varwidth = TRUE)
boxplot(V44~V22,varwidth = TRUE)
boxplot(V45~V22,varwidth = TRUE)
dev.off()
#Imparct of V23
pdf("boxplotImpactV23.pdf")
par(mfrow=c(2,4))
boxplot(V24~V23,varwidth = TRUE)
boxplot(V30~V23,varwidth = TRUE)
boxplot(V31~V23,varwidth = TRUE)
boxplot(V32~V23,varwidth = TRUE)
boxplot(V33~V23,varwidth = TRUE)
boxplot(V34~V23,varwidth = TRUE)
boxplot(V35~V23,varwidth = TRUE)
boxplot(V36~V23,varwidth = TRUE)
boxplot(V37~V23,varwidth = TRUE)
boxplot(V38~V23,varwidth = TRUE)
boxplot(V39~V23,varwidth = TRUE)
boxplot(V40~V23,varwidth = TRUE)
boxplot(V41~V23,varwidth = TRUE)
boxplot(V42~V23,varwidth = TRUE)
boxplot(V43~V23,varwidth = TRUE)
boxplot(V44~V23,varwidth = TRUE)
boxplot(V45~V23,varwidth = TRUE)
dev.off()
#Imparct of V27
pdf("boxplotImpactV27.pdf")
par(mfrow=c(2,4))
boxplot(V24~V27,varwidth = TRUE)
boxplot(V30~V27,varwidth = TRUE)
boxplot(V31~V27,varwidth = TRUE)
boxplot(V32~V27,varwidth = TRUE)
boxplot(V33~V27,varwidth = TRUE)
boxplot(V34~V27,varwidth = TRUE)
boxplot(V35~V27,varwidth = TRUE)
boxplot(V36~V27,varwidth = TRUE)
boxplot(V37~V27,varwidth = TRUE)
boxplot(V38~V27,varwidth = TRUE)
boxplot(V39~V27,varwidth = TRUE)
boxplot(V40~V27,varwidth = TRUE)
boxplot(V41~V27,varwidth = TRUE)
boxplot(V42~V27,varwidth = TRUE)
boxplot(V43~V27,varwidth = TRUE)
boxplot(V44~V27,varwidth = TRUE)
boxplot(V45~V27,varwidth = TRUE)
dev.off()
#Imparct of V28
pdf("boxplotImpactV28.pdf")
par(mfrow=c(2,4))
boxplot(V24~V28,varwidth = TRUE)
boxplot(V30~V28,varwidth = TRUE)
boxplot(V31~V28,varwidth = TRUE)
boxplot(V32~V28,varwidth = TRUE)
boxplot(V33~V28,varwidth = TRUE)
boxplot(V34~V28,varwidth = TRUE)
boxplot(V35~V28,varwidth = TRUE)
boxplot(V36~V28,varwidth = TRUE)
boxplot(V37~V28,varwidth = TRUE)
boxplot(V38~V28,varwidth = TRUE)
boxplot(V39~V28,varwidth = TRUE)
boxplot(V40~V28,varwidth = TRUE)
boxplot(V41~V28,varwidth = TRUE)
boxplot(V42~V28,varwidth = TRUE)
boxplot(V43~V28,varwidth = TRUE)
boxplot(V44~V28,varwidth = TRUE)
boxplot(V45~V28,varwidth = TRUE)
dev.off()
#Imparct of V29
pdf("boxplotImpactV29.pdf")
par(mfrow=c(2,4))
boxplot(V24~V29,varwidth = TRUE)
boxplot(V30~V29,varwidth = TRUE)
boxplot(V31~V29,varwidth = TRUE)
boxplot(V32~V29,varwidth = TRUE)
boxplot(V33~V29,varwidth = TRUE)
boxplot(V34~V29,varwidth = TRUE)
boxplot(V35~V29,varwidth = TRUE)
boxplot(V36~V29,varwidth = TRUE)
boxplot(V37~V29,varwidth = TRUE)
boxplot(V38~V29,varwidth = TRUE)
boxplot(V39~V29,varwidth = TRUE)
boxplot(V40~V29,varwidth = TRUE)
boxplot(V41~V29,varwidth = TRUE)
boxplot(V42~V29,varwidth = TRUE)
boxplot(V43~V29,varwidth = TRUE)
boxplot(V44~V29,varwidth = TRUE)
boxplot(V45~V29,varwidth = TRUE)
dev.off()#Imparct of V50
pdf("boxplotImpactV50.pdf")
par(mfrow=c(2,4))
boxplot(V24~V50,varwidth = TRUE)
boxplot(V30~V50,varwidth = TRUE)
boxplot(V31~V50,varwidth = TRUE)
boxplot(V32~V50,varwidth = TRUE)
boxplot(V33~V50,varwidth = TRUE)
boxplot(V34~V50,varwidth = TRUE)
boxplot(V35~V50,varwidth = TRUE)
boxplot(V36~V50,varwidth = TRUE)
boxplot(V37~V50,varwidth = TRUE)
boxplot(V38~V50,varwidth = TRUE)
boxplot(V39~V50,varwidth = TRUE)
boxplot(V40~V50,varwidth = TRUE)
boxplot(V41~V50,varwidth = TRUE)
boxplot(V42~V50,varwidth = TRUE)
boxplot(V43~V50,varwidth = TRUE)
boxplot(V44~V50,varwidth = TRUE)
boxplot(V45~V50,varwidth = TRUE)
dev.off()