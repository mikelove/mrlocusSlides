meths <- c("causal","twmr","ptwas",
               "mrlocus","ecv-mrl")
cols <- unname(palette.colors())[-c(1,5)]
cols <- cols[c(1,3:5,5)]
names(cols) <- meths
shps <- c(24,17,15,16,10)
names(shps) <- meths
dat <- read.delim("paper_results.tsv")
library(ggplot2)
png(file="sim_perf.png", width=1200, height=500, res=150)
ggplot(dat, aes(h2med, rmae, col=method, shape=method)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=cols) +
  scale_shape_manual(values=shps) +
  facet_wrap(~h2g, labeller=label_both) +
  ylab("RMAE = |alpha-hat - alpha| / alpha") +
  scale_x_continuous(breaks = c(.001, .005, .01)) +
  coord_cartesian(xlim = c(0,0.011))
dev.off()
