a <- FALSE
for (m in names(fits)) {
  rmdtxt  <- paste0(
"## Matching : ", m, "\n
```{r, fig.height=3, warning=FALSE}
z <- fits[['", m, "']]
grid.arrange(z$fig1, z$fig2, nrow = 1)
```\n
\\begin{center}
```{r, echo=FALSE, results='asis'}
stargazer(z$fit1, z$fit1a,
          dep.var.labels=c('DX58LeanMassTotPt'), single.row=TRUE,
          float = FALSE, header = FALSE, intercept.bottom = FALSE,
          intercept.top = TRUE, model.numbers = FALSE)
```\n
\\end{center}\n
\\begin{center}
```{r, echo=FALSE, results='asis'}
stargazer(z$fit2, z$fit2a,
          dep.var.labels=c('DX62ALMI'), single.row=TRUE,
          float = FALSE, header = FALSE, intercept.bottom = FALSE,
          intercept.top = TRUE, model.numbers = FALSE)
```\n
\\end{center}\n
\\newpage\n\n"
  )
  cat(rmdtxt, file = "/tmp/osteolaus_cavitd.Rmd", append = a)
  a <- TRUE
}
