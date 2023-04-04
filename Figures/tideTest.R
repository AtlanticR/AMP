#create vectors to hold plant heights from each sample
group1 <- c(8, 8, 14)
group2 <- c(11, 12, 13)

# Perform two sample t-test
# If var.equal = T, then the Welch modification to the degrees of freedom is used
t.test(group1, group2, var.equal=T, alternative = "two.sided")


install.packages("lsr")
library("lsr")

cohensD(group1, group2)

install.packages("effsize")
library("effsize")

cohen.d(group1, group2)

