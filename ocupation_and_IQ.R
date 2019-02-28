library(ggplot2)

#data estimated from: https://www.gwern.net/docs/iq/2002-hauser.pdf

### COMPARE IQ SCORES OF MACHINE OPERATORS AND PROFESSORS ###

#descriptives for the two distributions
mean_coal = 93
sd_coal = 15

mean_prof = 114
sd_prof = 15

sd = 15

#find the score that gets you into the top 10% of coal miners (z = 1.65)
#z = 1.2816 #90th percentile
z = 0.8416 #80th percentile
pnorm(z)

coal_miner_IQ_of_interest = z * sd_coal + mean_coal #a score of 118 would put a coal miner in the top 10% of coal miners

#give the z-score for an IQ score of x among professors
z_coal_miner_in_profs = (coal_miner_IQ_of_interest - mean_prof) / sd_prof #a z-score of 0.25 has a p-value of 
pnorm(z_coal_miner_in_profs)

(mean_prof - mean_coal) / sd

### GRAPH THE DIFFERENCES ###

#creates two normal distributions
coal_dat = as.data.frame(rnorm(250, mean_coal, sd_coal))
prof_dat = as.data.frame(rnorm(250, mean_prof, sd_prof))

#rename columns
colnames(coal_dat)[1] = "IQ"
colnames(prof_dat)[1] = "IQ"

#remove professors with extremely low IQs (there's likely to be a floor effect)
prof_dat = subset(prof_dat, IQ > 75)

#add occupation variable
coal_dat$Occupation = "Machine operator"
prof_dat$Occupation = "Professor"

#combine the datasets into one
total.data = rbind(coal_dat, prof_dat)

#make the graph
ggplot(total.data, aes(x = IQ, fill = Occupation)) + 
  geom_histogram(binwidth = 5, alpha=0.5, position="identity", colour = "black") + 
  #xlim(-1000, 1000) +
  xlab("IQ score") +
  ylab("Frequency in population") + 
  scale_fill_discrete(labels=c("Coal miners", "Professors")) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
       panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2),
       legend.key = element_blank(), text = element_text(size = 20, family = "sans"), axis.text.y = element_blank()) +
  scale_color_manual(values=c("#999999", "ivory", "snow4"))+
  scale_fill_manual(values=c("#999999", "ivory", "snow4")) + 
  geom_vline(aes(xintercept= coal_miner_IQ_of_interest), color="black", size=1, show.legend = FALSE) +
  annotate("text", x = 133, y = 38, label = " ← IQ of 106 (top 20% of machine operators)") +
  scale_x_continuous(breaks = c(40,50,60,70,80,90,100,110,120,130,140,150,160))
  
