---
title: "R Notebook"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 

```{r}
par(mfrow=c(2,2))
plot.legend <- c("Normal","gamma","lognormal", "Weibull","Pareto")
denscomp(list(prmt_normal,prmt_gamma, prmt_lognormal, prmt_weibull, prmt_weibull), legendtext = plot.legend)
cdfcomp (list(prmt_normal,prmt_gamma, prmt_lognormal, prmt_weibull, prmt_weibull), legendtext = plot.legend)
qqcomp  (list(prmt_normal,prmt_gamma, prmt_lognormal, prmt_weibull, prmt_weibull), legendtext = plot.legend)
ppcomp  (list(prmt_normal,prmt_gamma, prmt_lognormal, prmt_weibull, prmt_weibull), legendtext = plot.legend)
```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
