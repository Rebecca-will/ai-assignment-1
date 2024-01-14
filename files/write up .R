library(ggplot2)
library(survival)
library(coxme)
library(lme4)
library(car)
library(MASS)
library(metafor)
library(meta)
library(ggplot2)
library(readr)
library(doBy)
library(hrbrthemes)
library("ggpubr")
library(tidyr)
library(rcompanion)

attach(Book1)
summaryBy(pk1 ~ group,data = Book1,
          FUN = c(mean,sd, median), na.rm = TRUE)

summaryBy(pk5 ~ group,data = Book1,
          FUN = c(mean,sd, median), na.rm = TRUE)

hist(Book1$pk1)
hist(Book1$pk5)
hist(Book1$painmedspk1)
hist(Book1$painmedspk5)

qqp(control_grp$pk5, "norm")
qqp(treatment_grp$pk5, "norm")

qqp(control_grp$pk1, "norm")
qqp(treatment_grp$pk1, "norm")

qqp(control_grp$pk5, "lnorm")
qqp(treatment_grp$pk5, "lnorm")

qqp(control_grp$pk1, "lnorm")
qqp(treatment_grp$pk1, "lnorm")



summary(Book1)
hist(Book1$age)
colSums(is.na(Book1))
#scatter plot of the combined groups
viz <- ggplot(data=Book1, aes(x=id, y=pk5)) +
  geom_point(aes(color=pk5), alpha=0.5) + 
  labs(title="Severity Score 5", y="Severity score", x="ID", color = "Severity score")
viz


#histogram of the data
hist(Book1$pk1)
hist(Book1$pk5)

#scatter plot comparing the median of groups after intervention
ggline(Book1, x = "group", y = "pk5", 
       add = c("median","jitter"),
       order = c("0", "1"),
       ylab = "pk5", xlab = "group") 


control_grp <- subset(Book1, group == 0)
treatment_grp <- subset(Book1, group == 1)

control_plot <- ggplot(control_grp, aes(x = pk5)) + 
  geom_density(fill = "blue", alpha = 0.2) +
  ggtitle("pk5 in control")

control_plot

treatment_plot <- ggplot(treatment_grp, aes(x = pk5)) + 
  geom_density(fill = "green", alpha = 0.2) +
  ggtitle("pk 5 treatment")

treatment_plot


combined_plot <- ggplot() +
  geom_density(data = control_grp, aes(x = pk5), fill = "blue", alpha = 0.2) +
  geom_density(data = treatment_grp, aes(x = pk5), fill = "green", alpha = 0.2) +
  ggtitle("pk5 in control vs treatment")

combined_plot

#treatment group 

treatment_plot_2 <- ggplot() + 
  geom_density(data=treatment_grp, aes(x=pk1), fill = 'blue', alpha =0.2) + 
  geom_density(data=treatment_grp, aes(x=pk5), fill = 'green', alpha =0.2) +
  ggtitle('pk before and after')

treatment_plot_2

control_plot_2 <-ggplot() + 
  geom_density(data=control_grp, aes(x=pk1), fill = 'Control', alpha =0.2) + 
  geom_density(data=control_grp, aes(x=pk5), fill = 'Control', alpha =0.2) +
  ggtitle('pk before and after') +
  scale_fill_manual(name = "Group", values = c("blue", "green"), labels = c("Control", "Control"))

control_plot_2

combined_plot <- ggplot() +
  geom_density(data = control_grp, aes(x = pk5, fill = "Control"), alpha = 0.2) +
  geom_density(data = treatment_grp, aes(x = pk5, fill = "Treatment"), alpha = 0.2) +
  ggtitle("pk5 in control vs treatment") +
  scale_fill_manual(name = "Group", values = c("blue", "green"), labels = c("Control", "Treatment"))

combined_plot


# Convert data to long format
control_grp_long <- pivot_longer(control_grp, cols = c("pk1", "pk5"), names_to = "variable", values_to = "value")

# Create plot
control_plot_2 <- ggplot(control_grp_long, aes(x = value, fill = variable)) + 
  geom_density(alpha = 0.2) +
  ggtitle("Control pk before and after") +
  scale_fill_manual(name = "Variable", values = c("orange", "purple"), labels = c("pk1", "pk5")) +
  labs(x = "pk value", y = "density") +
  theme_classic()

# Show plot
control_plot_2


# Convert data to long format
treatment_grp_long <- pivot_longer(treatment_grp, cols = c("pk1", "pk5"), names_to = "variable", values_to = "value")

# Create plot
treatment_plot_2 <- ggplot(treatment_grp_long, aes(x = value, fill = variable)) + 
  geom_density(alpha = 0.2) +
  ggtitle("Treatment pk before and after") +
  scale_fill_manual(name = "Variable", values = c("orange", "purple"), labels = c("pk1", "pk5")) +
  labs(x = "pk value", y = "density") +
  theme_classic()

# Show plot
treatment_plot_2


#stats tests

kruskal.test(pk1 ~ pk5)
dunnTest(pk1~pk5, method = bh)

wilcox.test(treatment_grp$pk1, treatment_grp$pk5, paired = TRUE)
wilcox.test(control_grp$pk5, control_grp$pk1, paired = TRUE)

?wilcox.test

#regression to see if accupuncture is assoicated with reductions in severity 
model_linear = glm(response ~ group, family = binomial(link = "logit"), data = Book1)
summary(model_linear)

abc<- ggplot(data=Book1, aes(x=group, y=response, color=group)) +
  geom_point() + 
  geom_smooth(method = "glm", method.args=list(family=('binomial')))
abc
summary(abc)

table(Book1$group[Book1$group == "0"], Book1$response[Book1$group == "0"])

table(Book1$group[Book1$group == "1"], Book1$response[Book1$group == "1"])


counts <- aggregate(response ~ group, data = Book1, function(x) table(x))
counts_1 <- counts[-group]

chisq.test(counts_1)
pairwiseNominalIndependence(counts_1,compare = 'row', fisher=FALSE, gtest=FALSE, chisq = TRUE, method = 'fdr', digits = 3)

##############################
##############################

headache <-subset(Book1, migraine ==0)
headache_txt <- subset(headache, group == 1)
headache_control <- subset(headache, group == 0)


migraine_1 <- subset(Book1,migraine ==1)
migraine_txt <- subset(migraine_1, group == 1)
migraine_control <- subset(migraine_1, group == 0)

accupuncture <- subset(Book1, group ==1)

# Convert data to long format
headache_txt_grp_long <- pivot_longer(headache_txt, cols = c("pk1", "pk5"), names_to = "variable", values_to = "value")

headache_control_grp_long <- pivot_longer(headache_control, cols = c("pk1", "pk5"), names_to = "variable", values_to = "value")

# Create plot
headache_txt_plot_2 <- ggplot(headache_txt_grp_long, aes(x = value, fill = variable)) + 
  geom_density(alpha = 0.2) +
  ggtitle("Headache txt pk before and after") +
  scale_fill_manual(name = "Variable", values = c("orange", "purple"), labels = c("pk1", "pk5")) +
  labs(x = "pk value", y = "density") +
  theme_classic()

headache_control_plot_2 <- ggplot(headache_control_grp_long, aes(x = value, fill = variable)) + 
  geom_density(alpha = 0.2) +
  ggtitle("Headache control pk before and after") +
  scale_fill_manual(name = "Variable", values = c("orange", "purple"), labels = c("pk1", "pk5")) +
  labs(x = "pk value", y = "density") +
  theme_classic()



# Show plot
headache_txt_plot_2

headache_control_plot_2

# Convert data to long format
migraine_txt_1_grp_long <- pivot_longer(migraine_txt, cols = c("pk1", "pk5"), names_to = "variable", values_to = "value")

migraine_control_1_grp_long <- pivot_longer(migraine_control, cols = c("pk1", "pk5"), names_to = "variable", values_to = "value")

# Create plot
migraine_txt_1_plot_2 <- ggplot(migraine_txt_1_grp_long, aes(x = value, fill = variable)) + 
  geom_density(alpha = 0.2) +
  ggtitle("Migraine txt pk before and after") +
  scale_fill_manual(name = "Variable", values = c("orange", "purple"), labels = c("pk1", "pk5")) +
  labs(x = "pk value", y = "density") +
  theme_classic()

migraine_control_1_plot_2 <- ggplot(migraine_control_1_grp_long, aes(x = value, fill = variable)) + 
  geom_density(alpha = 0.2) +
  ggtitle("Migraine control pk before and after") +
  scale_fill_manual(name = "Variable", values = c("orange", "purple"), labels = c("pk1", "pk5")) +
  labs(x = "pk value", y = "density") +
  theme_classic()

# Show plot
migraine_txt_1_plot_2
migraine_control_1_plot_2



wilcox.test(headache_txt$pk1, headache_txt$pk5, paired = TRUE)
wilcox.test(headache_control$pk1, headache_control$pk5, paired = TRUE)

wilcox.test(headache_txt$response, migraine_txt$response, paired = TRUE)

wilcox.test(migraine_txt$pk5, migraine_txt$pk1, paired = TRUE)
wilcox.test(migraine_control$pk5, migraine_control$pk1, paired = TRUE)


counts_mig <- aggregate(response ~ migraine, data = accupuncture, function(x) table(x))
counts_migs <- counts_mig[-migraine]

chisq.test(counts_migs)
counts_migs

pairwiseNominalIndependence(counts_mig,compare = 'row', fisher=FALSE, gtest=FALSE, chisq = TRUE, method = 'fdr', digits = 3)

##############################
##############################

Male <-subset(Book1, sex ==0)
Male_txt <- subset(Male, group == 1)

female <- subset(Book1,sex ==1)
female_txt <- subset(female, group ==1)


counts_gender <- aggregate(response ~ sex, data = , function(x) table(x))

# Convert data to long format
male_grp_long <- pivot_longer(Male_txt, cols = c("pk1", "pk5"), names_to = "variable", values_to = "value")

male_plot_2 <- ggplot(male_grp_long, aes(x = value, fill = variable)) + 
  geom_density(alpha = 0.2) +
  ggtitle("Male txt pk before and after") +
  scale_fill_manual(name = "Variable", values = c("orange", "purple"), labels = c("pk1", "pk5")) +
  labs(x = "pk value", y = "density") +
  theme_classic()
male_plot_2


female_grp_long <- pivot_longer(female_txt, cols = c("pk1", "pk5"), names_to = "variable", values_to = "value")

female_plot_2 <- ggplot(female_grp_long, aes(x = value, fill = variable)) + 
  geom_density(alpha = 0.2) +
  ggtitle("female txt pk before and after") +
  scale_fill_manual(name = "Variable", values = c("orange", "purple"), labels = c("pk1", "pk5")) +
  labs(x = "pk value", y = "density") +
  theme_classic()
female_plot_2

wilcox.test(female_txt$pk1, female_txt$pk5, paired = TRUE)
wilcox.test(Male_txt$pk1, Male_txt$pk5, paired = TRUE)

success <- subset(Book1, group ==1)

counts <- aggregate(response ~ sex, data = success, function(x) table(x))
counts_1 <- counts[-sex]
counts_1

chisq.test(counts_1)

####################

gen_grp_long <- pivot_longer(success, cols = c("gen1", "gen5"), names_to = "variable", values_to = "value")


gen_plot_2 <- ggplot(gen_grp_long, aes(x = value, fill = variable)) + 
  geom_density(alpha = 0.2) +
  ggtitle("SF36 txt before and after") +
  scale_fill_manual(name = "Variable", values = c("orange", "purple"), labels = c("gen1", "gen5")) +
  labs(x = "gen value", y = "density") +
  theme_classic()
gen_plot_2

shapiro.test(gen1)



wilcox.test(success$gen1, success$gen5, paired = TRUE)

wilcox.test(success$painmedspk1, success$painmedspk5, paired = TRUE)

pp_grp_long <- pivot_longer(success, cols = c("painmedspk1", "painmedspk5"), names_to = "variable", values_to = "value")


pp_plot_2 <- ggplot(pp_grp_long, aes(x = value, fill = variable)) + 
  geom_density(alpha = 0.2) +
  ggtitle("Painpacks txt before and after") +
  scale_fill_manual(name = "Variable", values = c("orange", "purple"), labels = c("painmedspk1", "painmedspk5")) +
  labs(x = "painmedspk value", y = "density") +
  theme_classic()
pp_plot_2
