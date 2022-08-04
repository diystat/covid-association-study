## Test for correlation between reported CFR and each covariate

library(tidyverse);
library(xtable);

cor.tests.1 = function(y, x) {
  cp = cor.test(x, y);
  ck = cor.test(x, y, method="kendall")
  cs = cor.test(x, y, method="spearman")
  data.frame(r = cp$estimate, p = cp$p.value, rk = ck$estimate, pk = ck$p.value, rs = cs$estimate, ps = cs$p.value);
}

cor.tests = function(i, jj, df) {
  res = t(sapply(jj, function(j){cor.tests.1(df[,i], df[,j])}));
  rownames(res) = names(df[,jj]);
  res
}

ny = readRDS("../data/NY.2022-06-30.rds");
fl = readRDS("../data/FL.2022-06-30.rds");

## Identify the columns of the covariates
## jj = c(18:42);
jj = c(18:38);
names(fl)[jj];
names(ny)[jj];

## NY correlation tests
ny_cor1 = cor.tests("cfr1", jj, ny);
ny_cor2 = cor.tests("cfr2", jj, ny);
ny_cor3 = cor.tests("cfr3", jj, ny);

print(ny_cor1, digits=4);
print(ny_cor2, digits=4);
print(ny_cor3, digits=4);

if (FALSE) {
  ny_cor_combined = cbind(ny_cor1[, c(1:2, 5:6)], ny_cor2[, c(1:2, 5:6)], ny_cor3[, c(1:2, 5:6)]);
  print(ny_cor_combined, digits=4)
  
  file.out = sprintf("../output/%s.ny-correlations-table.tex", Sys.Date());
  sink(file.out);
  xtable(ny_cor_combined, digits=3)
  sink();
}

highlight_p_value = function(x, cols, alpha = 0.05) {
  x[] = format(round(unlist(x), 3), nsmall=3);
  x[, cols] = ifelse(x[, cols] < alpha, paste0("{\\bf",  x[, cols], "}"), x[, cols]);
  x 
}


## Create and save latex table for Pearson correlation coefficients and test p-values
ny_pearson_combined = cbind(ny_cor1[, 1:2], ny_cor2[, 1:2], ny_cor3[, 1:2]);
print(ny_pearson_combined, digits=4);

nyrp = highlight_p_value(ny_pearson_combined, c(2, 4, 6));
print(nyrp);

file.pearson = sprintf("../output/%s.ny-pearson-correlations-table.tex", Sys.Date());
sink(file.pearson);
xt = xtable(nyrp)
print(xt, sanitize.text.function=identity)
sink();

## Create and save latex table for Pearson correlation coefficients and test p-values
ny_spearman_combined = cbind(ny_cor1[, 5:6], ny_cor2[, 5:6], ny_cor3[, 5:6]);
print(ny_spearman_combined, digits=4)

nyrs = highlight_p_value(ny_spearman_combined, c(2, 4, 6));
print(nyrs);
  
file.spearman = sprintf("../output/%s.ny-spearman-correlations-table.tex", Sys.Date());
sink(file.spearman);
xt = xtable(nyrs);
print(xt, sanitize.text.function=identity)
sink();

file.Rdata = sprintf("../output/test-correlation-ny.%s.Rdata", Sys.Date());
save(file = file.Rdata, ny_pearson_combined, ny_spearman_combined)

## FL correlation tests
fl_cor1_o = cor.tests("cfr1", jj, fl);
fl_cor2_o = cor.tests("cfr2", jj, fl);
fl_cor3_o = cor.tests("cfr3", jj, fl);

print(fl_cor1_o, digits=4);
print(fl_cor2_o, digits=4);
print(fl_cor3_o, digits=4);

## Create and save latex table for Pearson correlation coefficients and test p-values
fl_pearson_combined = cbind(fl_cor1_o[, 1:2], fl_cor2_o[, 1:2], fl_cor3_o[, 1:2]);
print(fl_pearson_combined, digits=4)
flrp = highlight_p_value(fl_pearson_combined, c(2, 4, 6));
print(flrp);

file.pearson = sprintf("../output/%s.fl-pearson-correlations-table.tex", Sys.Date());
sink(file.pearson);
xt = xtable(flrp);
print(xt, sanitize.text.function=identity)
sink();

## Create and save latex table for Pearson correlation coefficients and test p-values
fl_spearman_combined = cbind(fl_cor1_o[, 5:6], fl_cor2_o[, 5:6], fl_cor3_o[, 5:6]);
print(fl_spearman_combined, digits=4)
flrs = highlight_p_value(fl_spearman_combined, c(2, 4, 6));
print(flrs);

file.spearman = sprintf("../output/%s.fl-spearman-correlations-table.tex", Sys.Date());
sink(file.spearman);
xt = xtable(flrs);
print(xt, sanitize.text.function=identity)
sink();

file.Rdata = sprintf("../output/test-correlation-fl.%s.Rdata", Sys.Date());
save(file = file.Rdata, fl_pearson_combined, fl_spearman_combined)


## Florida without union county
fl_cor1 = cor.tests("cfr1", jj, fl[-63, ]);
fl_cor2 = cor.tests("cfr2", jj, fl[-63, ]);
fl_cor3 = cor.tests("cfr3", jj, fl[-63, ]);

print(fl_cor1, digits=4);
print(fl_cor2, digits=4);
print(fl_cor3, digits=4);

## Create and save latex table for Pearson correlation coefficients and test p-values
fl_pearson_combined = cbind(fl_cor1[, 1:2], fl_cor2[, 1:2], fl_cor3[, 1:2]);
print(fl_pearson_combined, digits=4)

flrp = highlight_p_value(fl_pearson_combined, c(2, 4, 6));
print(flrp);

file.pearson = sprintf("../output/%s.fl-1-pearson-correlations-table.tex", Sys.Date());
sink(file.pearson);
xt = xtable(flrp);
print(xt, sanitize.text.function=identity)
sink();


## Create and save latex table for Pearson correlation coefficients and test p-values
fl_spearman_combined = cbind(fl_cor1[, 5:6], fl_cor2[, 5:6], fl_cor3[, 5:6]);
print(fl_spearman_combined, digits=4);
flrs = highlight_p_value(fl_spearman_combined, c(2, 4, 6));
print(flrs);

file.spearman = sprintf("../output/%s.fl-1-spearman-correlations-table.tex", Sys.Date());
sink(file.spearman);
xt = xtable(flrs);
print(xt, sanitize.text.function=identity)
sink();




