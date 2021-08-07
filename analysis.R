trait <- "BP"
trait <- "SCZ"
dat <- read.csv(paste0(trait,"-results.csv"))
names(dat)[1] <- "gene_name"
library(dplyr)
library(ggplot2)

twas <- read.csv(paste0(trait,"-twas.csv"))
dat <- merge(dat, twas[,c("gene_name","TWAS.Z","TWAS.P")], by="gene_name")

thr <- c("BP"=1.96, "SCZ"=2.58)

dat <- dat %>%
  mutate(z = mean/sd, sig = abs(z) > thr[trait]) %>%
  arrange(desc(abs(z))) %>%
  filter(!is.na(sig))

sig <- dat %>% filter(sig)
head(dat)
cols <- c("TRUE"="#FF0000FF", "FALSE"="#00000022")

ggplot(dat, aes(TWAS.Z, mean, col=sig)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=mean - 1.96*sd, ymax=mean + 1.96*sd), width=.5) +
  geom_hline(yintercept=0, alpha=.5, lty=2) + geom_vline(xintercept=0, alpha=.5, lty=2) +
  theme(legend.position="none") +
  scale_colour_manual(values = cols) +
  xlim(-10,10) +
  coord_cartesian(ylim=c(-1.5,1.5))
  ## geom_label(data=sig, aes(TWAS.Z, mean, label=gene_name),
  ##            hjust = "inward", vjust = "center",
  ##            nudge_x = c(.2,-.2,-.2),
  ##            nudge_y = c(.15,-.15,-.15))

ggplot(dat, aes(twmr, mean, col=sig)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=mean - 1.96*sd, ymax=mean + 1.96*sd), width=.025) +
  geom_errorbarh(aes(xmin=twmr - 1.96*twmrSE, xmax=twmr + 1.96*twmrSE), height=.1) +
  geom_abline(slope=1,intercept=0) +
  geom_hline(yintercept=0, alpha=.5, lty=2) + geom_vline(xintercept=0, alpha=.5, lty=2) +
  theme(legend.position="none") +
  scale_colour_manual(values = cols) +
  coord_cartesian(ylim=c(-1.5,1.5))
  ## geom_label(data=sig, aes(twmr, mean, label=gene_name),
  ##            hjust = "inward", vjust = "inward",
  ##            nudge_x = c(.025, -.025, -.025),
  ##            nudge_y = c(.1, -.1, -.1))

