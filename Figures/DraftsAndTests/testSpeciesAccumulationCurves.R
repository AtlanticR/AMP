
source("Figures/permanovaRegions.R")

stPspp = as.matrix(stPeters[,which(colnames(stPeters)== "Acartia spp. (civ-vi)"): ncol(stPeters)])



stP.rich = data.frame(t(data.frame(replicate(999, specaccum(x, "random")$richness))))

stP.sd = data.frame(t(data.frame(replicate(999, specaccum(x, "random")$sd))))


stP.meanRich = colMeans(stP.rich)
stP.meanSd = colMeans(stP.sd)

stPdata = data.frame(t(rbind(stP.meanRich, stP.meanSd)))
stPdata$ind = c(1:26)


stP.chao1 = specpool(stPspp)$chao


ggplot()+
  geom_ribbon(data = stPdata, aes(x = ind, ymin = (stP.meanRich-2*stP.meanSd), ymax = (stP.meanRich+2*stP.meanSd)), alpha = 0.6, fill = "lightblue")+
  geom_line(data = stPdata, aes(x = ind, y = stP.meanRich))+
  geom_hline(yintercept = stP.chao1, linetype = "dashed", col = "gray40")+
  theme_bw()+
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13)
  )






# See here for some good examples
# https://royalsocietypublishing.org/doi/10.1098/rspb.2020.0248
# Has extrapolations and asymptotes

# https://www.neonscience.org/resources/learning-hub/tutorials/aquatic-diversity-algae





