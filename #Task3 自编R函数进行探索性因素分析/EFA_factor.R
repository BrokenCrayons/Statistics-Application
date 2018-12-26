EFA_factor <- function(data)
{
  library(ggplot2)
  library(psych)
  library(GPArotation)
  
  # Next we’ll find out the number of factors that we’ll be selecting for factor analysis. 
  # Parallel Analysis / eigenvalue（特征值） 
  parallel <- fa.parallel(data, fm = 'minres', fa = 'fa')
  n_fact <- parallel$nfact
  
  # Factor Analysis
  # r  ??? Raw data or correlation or covariance matrix
  # nfactors ??? Number of factors to extract
  # rotate ??? Although there are various types rotations, `Varimax` and `Oblimin` are most popular
  # fm ??? One of the factor extraction techniques like `Minimum Residual (OLS)`, `Maximum Liklihood`, `Principal Axis` etc.
  
  # In this case, we will select oblique rotation (rotate = “oblimin”) as we believe that there is correlation in the 
  # factors. Note that Varimax rotation is used under the assumption that the factors are completely uncorrelated. 
  # We will use `Ordinary Least Squared/Minres` factoring (fm = “minres”), as it is known to provide results similar to
  # `Maximum Likelihood` without assuming multivariate normal distribution and derives solutions through iterative 
  # eigendecomposition like principal axis.
  RMSR <- c()
  RMSEA <- c()
  TLI <- c()
  my_list <- list()
  # 创建一系列变量名
  code <- as.character(sprintf("%01d",2:(n_fact+1)))
  # 这部分是统一命名一个方便你提取的变量名
  varname <- paste("factor_",code,sep="")
  n <- 2
  while(n <= (n_fact+1))
  {
    factor_opt <- fa(data,nfactors = n,rotate = "oblimin",fm="minres")
    factor_opt_cut <- print(factor_opt$loadings,cutoff = 0.3)
    fa.diagram(factor_opt)
    my_list[[n-1]] <- factor_opt_cut
    RMSR <- c(RMSR,factor_opt$rms)
    RMSEA <- c(RMSEA,factor_opt$RMSEA[1])
    TLI <- c(TLI,factor_opt$TLI)
    n <- n+1
  }
  RMSR <- round(RMSR,3)
  RMSEA <- round(RMSEA,3)
  TLI <- round(TLI,3)
  data_output <- data.frame(cbind(varname,RMSR, RMSEA,TLI))
  output_list <- list("factors_compare" = data_output, "loadings" = my_list)
  return(output_list) 
}


