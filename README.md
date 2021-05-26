[![CRAN status](https://www.r-pkg.org/badges/version/job)](https://cran.r-project.org/package=job)
[![R-CMD-check](https://github.com/lindeloev/job/workflows/R-CMD-check/badge.svg)](https://github.com/lindeloev/job/actions)
<!-- [![job CRAN downloads](https://cranlogs.r-pkg.org/badges/job)](https://cran.r-project.org/package=job) -->

# job: free your RStudio console

Use `job::job()` to run chunks of R code in an [RStudio](https://www.rstudio.com/) job instead of the console. This frees your console while the job(s) go brrrrr in the background. By default, the result is returned to the global environment when the job completes.

Install:

```r
remotes::install_github("lindeloev/job")
```

## Addins
Two [RStudio Addins](https://rstudio.github.io/rstudioaddins/) are installed with `job`. Simply select some code code in your editor and click one of the Addins to run it as a job. The results are returned to the global environment once the job completes.

![](https://raw.githubusercontent.com/lindeloev/job/master/man/figures/addins.png)

 * *"Run selection as job"* calls `job::job()`. It imports everything from your environment, so it feels like home. It returns all new or changed variables job.
 * *"Run selection as job in empty session"* calls `job::empty()`. It imports nothing from your environment, so the code can run in clean isolation. All variables are returned.


## Typical usage

Write your script as usual. Then wrap parts of it using `job::job({<your code>})` to run that chunk as a job:

```r
job::job({
  foo = 10
  bar = rnorm(5)
})
```

When the job completes, it silently saves `foo` and `bar` to your global environment.


Another use case: We all love `brms` but compilation and sampling takes time. Let's run it as a job!

```r
library(brms)
data = mtcars[mtcars$hp > 100, ]
model = mpg ~ hp * wt

# Send long-running code to a job
job::job(brm_result = {
  options(mc.cores = 3)  # Set options inside the job
  fit = brm(model, data)
  fit = add_criterion(fit, "loo")
  
  print(summary(fit))  # Show a summary in the job
  the_test = hypothesis(fit, "hp > 0")
  job::export(c(fit, the_test))  # Only return these
})

cat("I'm free now! Thank you.
    Sincerely, Console.")
```

Now you can follow the progress in the jobs pane and your console is free. Because we named the code block `brm_result`, it will return the contents as an `environment()` called `brm_result` (or whatever you called it).`brm_result` behaves much like a `list()`:

![](https://raw.githubusercontent.com/lindeloev/job/master/man/figures/return_environment.png)


## Turn RStudio's Jobs into high-level history

Often, the results of the long-running chunks are the most interesting. But they easily get buried among the other outputs in the console and are eventually lost due to the line limit. RStudio's jobs history can be used as a nice overview. Make sure to print informative results within the job and give your jobs an appropriate `title`, e.g., (`job::job({<code here>}, title = "Unit test: first attempt at feature X")`.

![](https://raw.githubusercontent.com/lindeloev/job/master/man/figures/joblist.png)


## Finer control

See the [documentation](https://lindeloev.github.io/job/reference/job.html) how you can fine-control the job environment and what results are returned. The [job::job() website](https://lindeloev.github.io/job/) has worked examples, where finer control is beneficial, including:

 - [Controlling import](https://lindeloev.github.io/job/articles/articles/import.html) of variables, packages, and options.
 - [Controlling export](https://lindeloev.github.io/job/articles/articles/export.html) using the `job::export()` function.
 - Using `job::job()` to [test code chunks in an isolated environment](https://lindeloev.github.io/job/articles/articles/testing.html).
 - Using `job::job()` to [render large plots](https://lindeloev.github.io/job/articles/articles/plot.html).


## Use cases
The primary use case for `job::job()` are heavy statistical and numerical functions, including MCMC sampling, cross-validation, etc. 

But sometimes our flow is disturbed by semi-slow routine tasks too. Try running `devtools::test()`, `knitr::knit()`, `pkgdown::build_site()`, or `upgrade.packages()` as a job and see whether that's an improvement. Here's [a list of semi-slow functions](https://lindeloev.github.io/job/articles/articles/routines.html) that I regularly run as jobs.


## See also
`job::job()` is aimed at easing interactive development within RStudio. For tasks that don't benefit from running in the Jobs pane of RStudio, check out:

 * The [`future` package](https://cran.r-project.org/web/packages/future/vignettes/future-1-overview.html)'s `%<-` operator combined with `plan(multisession)`.
 
 * The [`callr` package](https://callr.r-lib.org/) is a general tool to run code in new R sessions.
