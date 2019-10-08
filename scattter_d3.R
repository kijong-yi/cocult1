---
	title: "R Notebook"
output: html_notebook
---
	
	
	```{r}
library(tidyverse)
```

```{r}
read_migec <- function(dir) {
	list(
		A = read_tsv(paste(dir,"cdr_A_filter","TCRA.filtered.cdrblast.txt", sep = "/"),
					 col_types = "ddcccccdddddddd"),
		B = read_tsv(paste(dir,"cdr_B_filter","TCRB.filtered.cdrblast.txt", sep = "/"),
					 col_types = "ddcccccdddddddd")
	)
}

compare_scatter <- function(X, Y, chains = c("A","B"), par = T, abline = T){
	if(par) {
		par(mfrow = c(1,length(chains)))
	}
	for(chain in chains){
		X1 <- X[[chain]]$Percentage * 100
		Y1 <- Y[[chain]]$Percentage * 100
		names(X1) <- X[[chain]]$`CDR3 amino acid sequence`
		names(Y1) <- Y[[chain]]$`CDR3 amino acid sequence`
		u <- unique(c(names(X1), names(Y1)))
		Xu <- X1[u]
		Yu <- Y1[u]
		Xu[is.na(Xu)] <- 0
		Yu[is.na(Yu)] <- 0
		m <- max(c(X1,Y1,3.5))
		smoothScatter(Xu, Yu, xlab = deparse(substitute(X)), ylab = deparse(substitute(Y)), main = chain, xlim = c(0,m), ylim = c(0,m),bandwidth = 0.01)
		if(abline) {abline(0,1,lty = 2, col = "grey")}
	}
}


compare_scatter2 <- function(X, Y, abline = T, plot = T) {
	XA <- X$A$Percentage*100
	XB <- X$B$Percentage*100
	YA <- Y$A$Percentage*100
	YB <- Y$B$Percentage*100
	names(XA) <- X$A$`CDR3 amino acid sequence`
	names(XB) <- X$B$`CDR3 amino acid sequence`
	names(YA) <- Y$A$`CDR3 amino acid sequence`
	names(YB) <- Y$B$`CDR3 amino acid sequence`
	XX <- c(XA, XB)
	YY <- c(YA, YB)
	u <- unique(c(names(XX), names(YY)))
	Xu <- XX[u]
	Yu <- YY[u]
	Xu[is.na(Xu)] <- 0
	Yu[is.na(Yu)] <- 0
	m <- max(c(Xu,Yu,3.5))
	if (plot) {
		smoothScatter(Xu, Yu, xlab = deparse(substitute(X)), ylab = deparse(substitute(Y)), xlim = c(0,m), ylim = c(0,m),bandwidth = 0.01)
		if(abline) {abline(0,1,lty = 2, col = "grey")}
	} else {
		return(cbind(X = Xu, Y = Yu))
	}
}



```

```{r}
m <- function(X){a <- X$A$Percentage; names(a) <- X$A$`CDR3 amino acid sequence`; b <- X$B$Percentage; names(b) <- X$B$`CDR3 amino acid sequence`; c(a,b)}
merge_cbind <- function(...){
	l <- list(...)
	u <- unique(unlist(lapply(l, names)))
	ll <- lapply(l, function(x){o <- x[u]; o[is.na(o)] <- 0; names(o) <- u; o})
	lm <- do.call(cbind, ll)
	# lm[is.na(lm)] <- 0
	colnames(lm) <- unlist(lapply(substitute(list(...))[-1], deparse))
	lm
}
x1 <- merge_ab(rest1)
x2 <- merge_ab(rest2)
x3 <- merge_ab(base1)
l <- list(rest1,rest2,base1)

xxx <- merge_cbind(rest1, rest2, base1)

pairsD3(xx, opacity = 0.9, cex = 3, width = 600)

```



```{r}
rest1 <- read_migec("183rest3E5-2wr1")
rest2 <- read_migec("183rest3E5-2wr2")
base1 <- read_migec("183rest-3hr1")
c1516_70k <- "c1516-7E4-2w-r1" %>% read_migec
c1516_100k <- "c1516-1E5-2w-r1" %>% read_migec
c1516_2w_r1 <- "c1516-3E5-2w-r1" %>% read_migec
c1516_2w_r2 <- "c1516-3E5-2w-r2" %>% read_migec
c1516_3w_r1 <- "c1516-3E5-3w-r1" %>% read_migec
c1516_3w_r2 <- "c1516-3E5-3w-r2" %>% read_migec

rest1 <- read_migec("183rest3E5-2wr1") %>% m
rest2 <- read_migec("183rest3E5-2wr2") %>% m
base1 <- read_migec("183rest-3hr1") %>% m
c1516_70k <- "c1516-7E4-2w-r1" %>% read_migec %>% m
c1516_100k <- "c1516-1E5-2w-r1" %>% read_migec %>% m
c1516_2w_r1 <- "c1516-3E5-2w-r1" %>% read_migec %>% m
c1516_2w_r2 <- "c1516-3E5-2w-r2" %>% read_migec %>% m
c1516_3w_r1 <- "c1516-3E5-3w-r1" %>% read_migec %>% m
c1516_3w_r2 <- "c1516-3E5-3w-r2" %>% read_migec %>% m

xx <- merge_cbind(rest1,
				  rest2,
				  base1,
				  c1516_70k,
				  c1516_100k,
				  c1516_2w_r1,
				  c1516_2w_r2,
				  c1516_3w_r1,
				  c1516_3w_r2)
xx %>% unique() %>% pairsD3(opacity = 0.5, cex = 3, width = 1300) %>% savePairs(file = 'cocult1.html')
xx <- xx %>% unique()
xx_singletone <- apply(xx, 1, function(x){ifelse(sum(x>0) > 1, "sibling", "orphan")})
pairsD3(xx, group = xx_singletone, opacity = 0.5, cex = 3, width = 1300) %>% savePairs(file = 'cocult1.html')

```


```{r}
compare_scatter(rest1, base1)
compare_scatter(c1516_2w_r1, c1516_2w_r2)
compare_scatter(c1516_3w_r1, c1516_3w_r2)
compare_scatter(c1516_2w_r1, c1516_3w_r1)
compare_scatter(c1516_2w_r2, c1516_3w_r1)
compare_scatter(c1516_2w_r2, c1516_3w_r2)
compare_scatter(c1516_2w_r1, c1516_3w_r2)
compare_scatter(c1516_2w_r1, base1)

```


```{r}
compare_scatter(rest1, rest2)
compare_scatter(base1, rest1)
compare_scatter(base1, rest2)
```

```{r}
compare_scatter(rest1, c1516_70k)
compare_scatter(rest1, c1516_100k)
compare_scatter(rest1, c1516_2w_r1)
compare_scatter(rest1, c1516_2w_r2)
compare_scatter(rest1, c1516_3w_r1)
compare_scatter(rest1, c1516_3w_r2)
```


```{r}
compare_scatter(rest2, c1516_70k)
compare_scatter(rest2, c1516_100k)
compare_scatter(rest2, c1516_2w_r1)
compare_scatter(rest2, c1516_2w_r2)
compare_scatter(rest2, c1516_3w_r1)
compare_scatter(rest2, c1516_3w_r2)
```



```{r}
compare_scatter(base1, c1516_70k)
compare_scatter(base1, c1516_100k)
compare_scatter(base1, c1516_2w_r1)
compare_scatter(base1, c1516_2w_r2)
compare_scatter(base1, c1516_3w_r1)
compare_scatter(base1, c1516_3w_r2)
```


```{r}
# compare_scatter2(base1, c1516_70k)
# compare_scatter2(base1, c1516_100k)
compare_scatter(base1, c1516_2w_r1)
compare_scatter2(base1, c1516_2w_r1)
# compare_scatter2(base1, c1516_2w_r2)
# compare_scatter2(base1, c1516_3w_r1)
# compare_scatter2(base1, c1516_3w_r2)
smoothScatter(base1$A$Percentage, base1$A$Percentage,bandwidth = 0.0001)
```


```{r}
library(pairsD3)

pairsD3(x[,c(1,2,3)], group = x[,5], opacity = 0.9, cex = 3, width = 600)
```






