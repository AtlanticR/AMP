

x = as.matrix(stPeters[,which(colnames(argyle)== "Acartia spp."): ncol(stPeters)])



# See here for some good examples
# https://royalsocietypublishing.org/doi/10.1098/rspb.2020.0248
# Has extrapolations and asymptotes

# https://www.neonscience.org/resources/learning-hub/tutorials/aquatic-diversity-algae


# Calculate and plot species accumulcation curve for the 11 sampling events
# The CIs are based on random permutations of observed samples
alg_spec_accum_result <- round(x) %>% vegan::specaccum(., "random", permutations = 999, weights = rand.num)
plot(alg_spec_accum_result)

rand.num = sample(1:1000, size = 25)


# Extract the resampling data used in the above algorithm
spec_resamp_data <- data.frame(
  data_set = "observed", 
  sampling_effort = rep(1:nrow(alg_spec_accum_result$perm), 
                        each = ncol(alg_spec_accum_result$perm)),
  richness = c(t(alg_spec_accum_result$perm)))


# Fit species accumulation model
spec_accum_mod_1 <- x %>% vegan::fitspecaccum(model = "arrhenius")


# create a "predicted" data set from the model to extrapolate out 
# beyond the number of samples collected
sim_spec_data <- data.frame()
for(i in 1:30){
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