z.test <- function (x, y = NULL, sigma1, sigma2 = NULL, alternative = "two-sided", mu = 0) 
{
  if (missing(x)) # Test again and again
  {
    stop("You must specify a data column for x")
  }
  else
  {
    xname <- deparse(substitute(x))
    n1 <- length(x)
    xbar <- mean(x)
    if (missing(y))
    {
      if (missing(sigma1))
      {
        stop("You must specify a value for sigma1")
      }
      else if (sigma1 <= 0)
      {
        stop("sigma1 must be positive")
      }
      else
      {
        z <- (xbar - mu)/(sigma1/sqrt(n1))
      }
    }
    else
    {
      if (missing(sigma1))
      {
        stop("You must specify a value for sigma1")
      }
      else if (sigma1 <= 0)
      {
        stop("sigma1 must be positive")
      }
      else if (missing(sigma2))
      {
        stop("If y is not NULL, you must specify a value for sigma2")
      }
      else if (sigma2 <= 0)
      {
        stop("sigma2 must be positive")
      }
      else
      {
        yname <- deparse(substitute(y))
        n2 <- length(y)
        ybar <- mean(y)
        z <- ((xbar - ybar) - mu)/(sqrt((sigma1^2/n1)+(sigma2^2/n2)))
      }
    }
    if (alternative == "two-sided")
    {
      if (z > 0)
      {
        p <- 2 * pnorm(-z)
      }
      else
      {
        p <- 2 * pnorm(z)
      }
    }
    else if (alternative == "greater")
    {
      p <- pnorm(-z)
    }
    else
    {
      p <- pnorm(z)
    }
    printf <- function(...) cat(sprintf(...))
    printf("\n")
    if (missing(y))
    {
      printf("\t One Sample z-test\n")
    }
    else
    {
      printf("\t Two Sample z-test\n")
    }
    printf("\n")
    if (missing(y))
    {
      printf("data: ")
      cat(xname)
      printf("\n")
    }
    else
    {
      printf("data: ")
      cat(xname)
      cat(" and ")
      cat(yname)
      printf("\n")
    }
    printf("z = %.4f, p-value = %.5f\n", z,p)
    if (alternative == "two-sided")
    {
      printf("alternative hypothesis: true mean is not equal to %f\n", mu)
    }
    else if (alternative == "greater")
    {
      printf("alternative hypothesis: true mean is greater than %f\n", mu)
    }
    else
    {
      printf("alternative hypothesis: true mean is less than %f\n", mu)
    }
    printf("95 percent confidence interval:\n")
    if (missing(y))
    {
      if (alternative == "two-sided")
      {
        printf("\t%.7f %.7f\n", xbar-(1.960*(sigma1/sqrt(n1))), xbar+(1.960*(sigma1/sqrt(n1))))
      }
      else if (alternative == "greater")
      {
        printf("\t%.7f Inf\n", xbar-(1.645*(sigma1/sqrt(n1))))
      }
      else
      {
        printf("\t-Inf %.7f\n", xbar+(1.645*(sigma1/sqrt(n1))))
      }
    }
    else
    {
      if (alternative == "two-sided")
      {
        printf("\t%.7f %.7f\n", ((xbar - ybar) - mu)-(1.960*(sqrt((sigma1^2/n1)+(sigma2^2/n2)))), ((xbar - ybar) - mu)+(1.960*(sqrt((sigma1^2/n1)+(sigma2^2/n2)))))
      }
      else if (alternative == "greater")
      {
        printf("\t%.7f Inf\n", ((xbar - ybar) - mu)-(1.645*(sqrt((sigma1^2/n1)+(sigma2^2/n2)))))
      }
      else
      {
        printf("\t-Inf %.7f\n", ((xbar - ybar) - mu)+(1.645*(sqrt((sigma1^2/n1)+(sigma2^2/n2)))))
      }
    }
    printf("sample estimates:\n")
    if (missing(y))
    {
      printf("mean of x\n")
      printf("\t%.3f\n", xbar)
    }
    else
    {
      printf("mean of x mean of y\n")
      printf("\t%.3f\t %.3f\n", xbar, ybar)
    }
  }
}