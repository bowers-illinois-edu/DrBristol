plot_p <- function(obs_support,total_obs){
  ## Test to make sure that obs_support is less than or equal to total_obs
  stopifnot("The number of observations in favor of the working hypothesis must be less than or equal to the total number of observations"=obs_support<=total_obs)
  obs_oppose <- obs_support+1
  stopifnot("Observations are already compatible with the null. The number of observations in favor of the working hypothesis must be greater than or equal to half of the total number of observations"=obs_support >= (total_obs/2))
  ## We assume odds=1 here
 thep <- dFNCHypergeo(x=obs_support, m1 = obs_support, m2 = obs_oppose,
                      n = total_obs, odds = 1)



k_values <- 0:total_obs  # Possible number of observations favoring working theory

# Probability of getting each k value in a sample of observations from null
probabilities <- dhyper(k_values, m = obs_support, n = obs_oppose, k = total_obs)


null <- data.frame(k_values, probabilities)

plot_null<-ggplot(null, aes(x = factor(k_values), y = probabilities, 
               fill = factor(k_values == obs_support))) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("skyblue", "red"), guide = FALSE)+
  labs(title = "Null distribution",
       x = "Observations favoring working theory",
       y = "Frequency of occurrence")+ 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks =c(0.05,0.1))+
  theme_classic()+
    geom_hline(yintercept=0.05,linetype="dashed")+
  geom_hline(yintercept=0.1,linetype="dotted")

  return(plot_null)

}
  
