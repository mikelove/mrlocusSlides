dat <- read.csv("BP-results.csv")
library(dplyr)
library(ggplot2)
dat <- dat %>%
  mutate(z = mean/sd) %>%
  arrange(desc(abs(z))) %>%
  filter(!is.na(sig))
sig <- dat %>% filter(sig)
head(dat)
cols <- c("TRUE"="#FF0000FF", "FALSE"="#00000033")
ggplot(dat, aes(twmr, mean, col=sig)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=mean - 1.96*sd, ymax=mean + 1.96*sd), width=.025) +
  geom_errorbarh(aes(xmin=twmr - 1.96*twmrSE, xmax=twmr + 1.96*twmrSE), height=.025) +
  geom_abline(slope=1,intercept=0) +
  geom_hline(yintercept=0, alpha=.5, lty=2) + geom_vline(xintercept=0, alpha=.5, lty=2) +
  theme(legend.position="none") +
  scale_colour_manual(values = cols) +
  geom_label(data=sig, aes(twmr, mean, label=X),
             hjust = 0, vjust = 0,
             nudge_x = 0.02, nudge_y = 0.05)
