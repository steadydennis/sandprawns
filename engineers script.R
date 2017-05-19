# Life in the Sea - Engineers
# Dean Erasmus' Script

# MDS, ANOSIM, SIMPER, Size Freq Distr

# PRAWNS ----

ep <- read.csv("engineers.prawn.csv")
attach(ep)
require(visreg)

# Subsetting data by treatment
dis <- ep[Treatment == "Disturbed",]
undis <- ep[Treatment == "Undisturbed",]

# Sand Prawns

boxplot(dis$SP.size, undis$SP.size, names = c("Disturbed", "Undisturbed"), 
        ylab = "Sandprawn Size", col = c("pink", "light blue"), 
        main = "Sandprawn Size per Site", xlab = "Sites")

# Sandprawn Histogram

hist(dis$SP.size, ylab = "Frequency", col = rgb(1,0,0,0.5), 
     main = "Sandprawn Size Class Frequencies per Site", xlab = "Sandprawn Size Classes", 
     breaks = 17, xlim = c(0, 1.7), ylim = c(0, 15), 
     border = "white", xaxt = "n", yaxt = "n")
# ADDING UNDIS
hist(undis$SP.size, col = rgb(0,0,1,0.5), breaks = 17, 
     xlim = c(0, 1.7), ylim = c(0, 15), add = T, border = "white")
# OTHER EDITS
legend("right", c("Disturbed", "Undisturbed", "Overlapping"), 
       title = "Sites", col = c("red", "blue"), 
       fill = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5), "purple"), border = "white")
axis(side = 1, at = seq(0, 1.7, 0.1))
axis(side = 2, at = seq(0, 15, 1))

?hist
?axis
?legend

sd(dis$SP.size) / sd(undis$SP.size) 
# equal variance assumption is reasonable

t.test(dis$SP.size, undis$SP.size, var.equal = T)
#  t = -3.2209, df = 144, p-value = 0.00158
# strong evidence against H0

mean(dis$SP.size)
mean(undis$SP.size)

ep.lm <- lm(SP.size ~ Treatment, data = ep)
summary(ep.lm)
# Sand prawn size can be predicted by Treatment, but only accounts for 0.06 of variation
visreg(ep.lm, main = "Sand Prawn Size per Treatment", ylab = "Sand Prawn Size")
# The location affects abundance, but not size?

# Mud Prawns

boxplot(dis$MP.size, undis$MP.size)
# Disturbed only has one observation - Not worth investigating

detach(ep)

# SPP ----

es <- read.csv("engineers.spp.csv") # What is the reading under each sp?
attach(es)
require(vegan)

dis1 <- es[Treatment == "Disturbed",]
undis1 <- es[Treatment == "Undisturbed",]

boxplot(dis1[,3:23], undis1[,3:23])
t.test(dis1[,3:23], undis1[,3:23])
# t = -2.1709, df = 285.15, p-value = 0.03076

sum(dis1[,3:23]) # Disturbed total
sum(undis1[,3:23]) # Undisturbed total

# Dendrogram - relevant?

es.spp <- es[, c(1, 3:24)]
es.spp
es.spp.deco <- decostand(es.spp[, c(2:23)], method = "total")
es.spp.dist <- vegdist(es.spp.deco, method = "bray")
clust <- hclust(es.spp.dist, "average")

plot(clust, labels = es$Site, ylab = "Average Number of Species") 
# Not entirely sure what to read from this - what is "Height"?

# Nonmetric MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name

library(MASS)
d <- dist(es[,3:24]) # euclidean distances between the rows
fit <- isoMDS(d, k=2) # k is the number of dim
fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Nonmetric MDS", type="n")
text(x, y, labels = row.names(es[,3:24]), cex=.7)

detach(es)
