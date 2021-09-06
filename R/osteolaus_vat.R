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
stargazer(z$fits[[1]], z$fits[[2]],
          dep.var.labels=c('DX55VAT'), single.row=TRUE,
          float = FALSE, header = FALSE, intercept.bottom = FALSE,
          intercept.top = TRUE, model.numbers = FALSE)
```\n
\\end{center}\n
\\begin{center}
```{r, echo=FALSE, results='asis'}
stargazer(z$fits[[3]], z$fits[[4]],
          dep.var.labels=c('DX55VAT'), single.row=TRUE,
          float = FALSE, header = FALSE, intercept.bottom = FALSE,
          intercept.top = TRUE, model.numbers = FALSE)
```\n
\\end{center}\n
\\newpage\n\n"
  )
  cat(rmdtxt, file = "/tmp/osteolaus_vat.Rmd", append = a)
  a <- TRUE
}
