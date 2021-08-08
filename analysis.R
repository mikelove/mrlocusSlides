trait <- "BP"
trait <- "SCZ"
dat <- read.csv(paste0(trait,"-results.csv"))
names(dat)[c(1:2,11)] <- c("gene_name","MRLocus","TWMR")

library(dplyr)
library(ggplot2)
library(ggrepel)

twas <- read.csv(paste0(trait,"-twas.csv"))
dat <- merge(dat, twas[,c("gene_name","TWAS.Z","TWAS.P")], by="gene_name")

thr <- c("BP"=1.96, "SCZ"=2.81)

dat <- dat %>%
  mutate(z = MRLocus/sd, sig = abs(z) > thr[trait]) %>%
  arrange(abs(z)) %>%
  filter(!is.na(sig))

sig <- dat %>% filter(sig)
head(dat)
cols <- c("TRUE"="#FF0000FF", "FALSE"="#00000022")

ggplot(dat, aes(TWAS.Z, MRLocus, col=sig)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=MRLocus - 1.96*sd, ymax=MRLocus + 1.96*sd), width=.5) +
  geom_hline(yintercept=0, alpha=.5, lty=2) + geom_vline(xintercept=0, alpha=.5, lty=2) +
  theme(legend.position="none") +
  scale_colour_manual(values = cols) +
  xlim(-15,15) +
  coord_cartesian(ylim=c(-1,1)) +
  geom_text_repel(data=sig, aes(TWAS.Z, MRLocus, label=gene_name),
                   size=4, color="blue", 
                   box.padding = unit(1, "lines"),
                   point.padding = unit(1.5, "lines"))

dat %>% filter(twmrP < .01) %>%
  ggplot(aes(TWMR, MRLocus, col=sig)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=MRLocus - 1.96*sd, ymax=MRLocus + 1.96*sd), width=.025) +
  geom_errorbarh(aes(xmin=TWMR - 1.96*twmrSE, xmax=TWMR + 1.96*twmrSE), height=.1) +
  geom_abline(slope=1,intercept=0) +
  geom_hline(yintercept=0, alpha=.5, lty=2) + geom_vline(xintercept=0, alpha=.5, lty=2) +
  theme(legend.position="none") +
  scale_colour_manual(values = cols) +
  coord_cartesian(ylim=c(-1.5,1.5)) +
  geom_text_repel(data=sig, aes(TWMR, MRLocus, label=gene_name),
                   size=4, color="blue", 
                   box.padding = unit(2, "lines"),
                   point.padding = unit(1.5, "lines"))

sig %>% 
  ggplot(aes(TWMR, MRLocus, col="red")) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=MRLocus - 1.96*sd, ymax=MRLocus + 1.96*sd), width=.025) +
  geom_errorbarh(aes(xmin=TWMR - 1.96*twmrSE, xmax=TWMR + 1.96*twmrSE), height=.1) +
  geom_abline(slope=1,intercept=0) +
  geom_hline(yintercept=0, alpha=.5, lty=2) + geom_vline(xintercept=0, alpha=.5, lty=2) +
  theme(legend.position="none") +
  geom_text_repel(aes(TWMR, MRLocus, label=gene_name),
                   size=4, color="blue", 
                   box.padding = unit(2, "lines"),
                   point.padding = unit(1.5, "lines"))

