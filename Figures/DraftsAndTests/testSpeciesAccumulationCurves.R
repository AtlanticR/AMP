

x = as.matrix(stPeters[,which(colnames(stPeters)== "Acartia spp. (civ-vi)"): ncol(stPeters)])



# See here for some good examples
# https://royalsocietypublishing.org/doi/10.1098/rspb.2020.0248
# Has extrapolations and asymptotes

# https://www.neonscience.org/resources/learning-hub/tutorials/aquatic-diversity-algae


# Calculate and plot species accumulcation curve for the 11 sampling events
# The CIs are based on random permutations of observed samples
alg_spec_accum_result <- x %>% vegan::specaccum(., "random", permutations = 999, weights = rand.num, gamma = "jack1")
plot(alg_spec_accum_result)

sp1 <- x %>% vegan::specaccum(., "random", gamma = "jack1")

library(vegan)
data(BCI)
specaccum(comm = BCI, method = "random", gamma = "chao") 

specpool(BCI)

mod1 <- fitspecaccum(sp1, "lomolino")

plot(sp1)
plot(mod1, col = 2, lwd = 2)
lines(sp1, col = 1)

alg_spec_accum_result$perm

specpool(x)

data(dune)

specaccum(dune, "random", gamma = "jack1")

install.packages("BiodiversityR")
library(BiodiversityR)

test = reshape::melt(hi) 
test$ind = c(1:26)

ggplot()+
  geom_line(data = test, aes(x = ind, y = value, col = variable))


sp1 <- x %>% vegan::specaccum(., "random", gamma = "jack1")

hi = data.frame(replicate(100, specaccum(x, "random")$richness))

matplot(hi, type = "l")

matplot(test, type = "l")

# Extract the resampling data used in the above algorithm
spec_resamp_data <- data.frame(
  data_set = "observed", 
  sampling_effort = rep(1:nrow(alg_spec_accum_result$perm), 
                        each = ncol(alg_spec_accum_result$perm)),
  richness = c(t(alg_spec_accum_result$perm)))


# Fit species accumulation model
spec_accum_mod_1 <- x %>% vegan::fitspecaccum(model = "logis")


# create a "predicted" data set from the model to extrapolate out 
# beyond the number of samples collected
sim_spec_data <- data.frame()
for(i in 1:50){
  d_tmp <- data.frame(
    data_set = "predicted",
    sampling_effort = i,
    richness = predict(spec_accum_mod_1, newdata = i))
  
  sim_spec_data <- sim_spec_data %>%
    bind_rows(d_tmp)
}


# plot the "observed" and "simulated" curves with 95% CIs
data_plot <- spec_resamp_data %>% bind_rows(sim_spec_data) 

data_plot %>%
  ggplot(aes(sampling_effort, richness, 
             color = as.factor(data_set),
             fill = as.factor(data_set),
             linetype = as.factor(data_set))) +
  stat_summary(fun.data = median_hilow, fun.args = list(conf.int = .95), 
               geom = "ribbon", alpha = 0.25) +
  stat_summary(fun.data = median_hilow, geom = "line", 
               size = 1) 




mod <- c("arrhenius", "gleason", "gitay", "lomolino", "asymp", "gompertz", 
         "michaelis-menten", "logis", "weibull")
extraps <- matrix(NA, 100, length(mod))
colnames(extraps) <- mod
for(i in 1:nrow(extraps)) {
  ## use the same accumulation for all nls models 
  m <- specaccum(BCI[sample(50,25),], "exact") 
  for(p in mod) { 
    ## need try because some nls models can fail
    tmp <-  try(predict(fitspecaccum(m, p), newdata=50))
    if(!inherits(tmp, "try-error")) extraps[i,p] <- tmp
  } 
}


x = predict(fitspecaccum(BCI, "mich", "random"), newdata=c(1:100))





