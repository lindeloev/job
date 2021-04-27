# job: free your RStudio console

Use `job::job()` to run chunks of R code in an [RStudio](https://www.rstudio.com/) job instead of the console. This frees your console while the job(s) go brrrrr in the background. By default, the result is returned to the global environment when the job completes.

Install:

```r
remotes::install_github("lindeloev/job")
```

## Addins
Two [RStudio Addins](https://rstudio.github.io/rstudioaddins/) are installed with `job`. Simply select some code code in your editor and click one of the Addins to run it as a job. The results are returned once the job completes.

![](https://raw.githubusercontent.com/lindeloev/job/master/man/figures/addins.png)

Import and export differently:

 * "Run selection as job" imports everything from your environment, so it feels like home. It will only return variables names that are not already in your environment upon submitting the job, so assign to novel names if you want them to return.
 * "Run selection as job in empty session" imports nothing from your environment, so the code can run in clean isolation from the mess of a long-running session. All variables are returned, and overwritten if they already exist in your environment.


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
  fit = brm(model, data)
  fit = add_criterion(fit, "loo")
  print(summary(fit))  # Show a summary in the job
  the_test = hypothesis(fit, "hp > 0")
})

cat("I'm free now! Thank you.
    Sincerely, Console.")
```

Now you can follow the progress in the jobs pane and your console is free. Because we named the code block `brm_result`, it will return the contents as an `environment()` called `brm_result` (or whatever you called it).`brm_result` behaves much like a `list()`:

![](https://raw.githubusercontent.com/lindeloev/job/master/man/figures/return_environment.png)


## Turn RStudio's Jobs into a record

Often, the results of the long-running chunks are the most interesting. But they easily get buried among the other outputs in the console and are eventually lost due to the line limit. RStudio's jobs history can be used as a nice record. Make sure to print informative results within the job and give your jobs an appropriate `title`, e.g., (`job::job({<code here>}, title = "Unit test: first attempt at feature X")`.

![](https://raw.githubusercontent.com/lindeloev/job/master/man/figures/joblist.png)


## Finer control

RStudio jobs spin up a new session, i.e., a new environment. By default, `job::job()` will make this environment identical to your current one. But you can fine control this:

-   `import`: the default "auto" setting imports all objects that are referenced by the code into the job. Control this using `job::job({}, import = c(model, data))`. You can also import everything (`import = "all"`) or nothing (`import = NULL`).

-   `packages`: by default, all attached packages are attached in the job. Control this using `job::job({}, packages = c("brms"))` or set `packages = NULL` to load nothing. If `brms` is not loaded in your current session, adding `library("brms")` to the job code may be more readable.

-   `options`: by default, all options are overwritten/inserted to the job. Control this using, e.g., `job::job({}, opts = list(mc.cores = 2)` or set `opts = NULL` to use default options. If you want to set job-specific options, adding `options(mc.cores = 2)` to the job code may be more readable.

-   `export`: in the example above, we assigned the job environment to `brm_result` upon completion. Naturally, you can choose any name, e.g., `job::job(fancy_name = {a = 5})`. To return nothing, use an unnamed code chunk (insert results to `globalenv()` and remove everything before return: (`job::job({a = 5; rm(list=ls())})`. Returning nothing is useful when

    1.  your main result is a text output or a file on the disk, or

    2.  when the return is a very large object. The underlying `rstudioapi::jobRunScript()` is slow in the back-transfer so it's usually faster to `saveRDS(obj, filename)` them in the job and `readRDS(filename)` into your current session.


## Some use cases

-   Model training, cross validation, or hyperparameter tuning: train multiple models simultaneously, each in their own job. If one fails, the others continue.
-   Heavy I/O tasks, like processing large files. Save the results to disk and return nothing.
-   Run unit tests and other code in an empty environment. By default, `devtools::test()` runs in the current environment, including manually defined variables (e.g., from the last test-run) and attached packages. Call `job::job({devtools::test()}, import = NULL, packages = NULL, opts = NULL)` to run the test in complete isolation.
-   Upgrading packages


## See also
`job::job()` is aimed at easing interactive development within RStudio. For larger problems, production code, and solutions that work outside of RStudio, check out:

 * The [`future` package](https://cran.r-project.org/web/packages/future/vignettes/future-1-overview.html)'s `%<-` operator combined with `plan(multisession)`.
 
 * The [`callr` package](https://callr.r-lib.org/) is a general tool to run code in new R sessions.
