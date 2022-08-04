library(tidyverse);
library(ggplot2);
library(ggrepel);


## Load ny and fl data and test of correlation results
dd = "2022-06-30";
ny = readRDS(sprintf("../data/NY.%s.rds", dd));
fl = readRDS(sprintf("../data/FL.%s.rds", dd));
load(sprintf("../output/test-correlation-ny.%s.Rdata", dd));
load(sprintf("../output/test-correlation-fl.%s.Rdata", dd));

## Indices to the covarites
jj = c(18:38);
names(fl)[jj];
names(ny)[jj];


make_cfr_plot = function(i, jj, obj, tc) {
## make_cfr_plot = function(i, jj, obj, res1, res2, res3, cor1, cor2, cor3) {
  ## p1 = sprintf("Phase 1, r=%6.4f, p=%6.4f", cor1[i,1], res1[i,4]);
  ## p2 = sprintf("Phase 2, r=%6.4f, p=%6.4f", cor2[i,1], res2[i,4]);
  ## p3 = sprintf("Phase 3, r=%6.4f, p=%6.4f", cor3[i,1], res3[i,4]);
  
  p1 = sprintf("Phase 1, r=%6.4f, p=%6.4f", tc[i, 1], tc[i, 2]);
  p2 = sprintf("Phase 2, r=%6.4f, p=%6.4f", tc[i, 3], tc[i, 4]);
  p3 = sprintf("Phase 3, r=%6.4f, p=%6.4f", tc[i, 5], tc[i, 6]);
  
  df = data.frame(x = rep(obj[,jj[i]], 3), 
                  fips = rep(obj$fips, 3), 
                  CFR = c(obj$cfr1, obj$cfr2, obj$cfr3),
                  phase = rep(c(p1, p2, p3), each = length(obj$cfr1))
  );
  
  cn = rownames(tc);
  
  p = ggplot(df, aes(x=x, y=CFR)) + 
    geom_point() + geom_text_repel(aes(label=fips %% 1000), size=2)+ 
    geom_smooth() +
    facet_wrap(.~phase) + 
    labs(x = cn[i]);
  
}

for (i in 1:21) { 
  p = make_cfr_plot(i, jj, ny, ny_spearman_combined)
  print(p);
  
  file.png = sprintf("../figures/ny.%d-%s.png", i, names(ny)[jj][i])
  ## ggsave(file.png, p, width=12, height=4)
  ggsave(file.png, p, width=12, height=3)
}

for (i in 1:21) { 
  p = make_cfr_plot(i, jj, fl, fl_spearman_combined)
  print(p);
  
  file.png = sprintf("../figures/fl.%d-%s.png", i, names(fl)[jj][i])
  ## ggsave(file.png, p, width=12, height=4)
  ggsave(file.png, p, width=12, height=3)
}


