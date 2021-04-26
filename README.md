# job: free your RStudio console

Use `job::job()` to run chunks of R code in an [RStudio](https://www.rstudio.com/) job instead of the console. This frees your console while the job(s) go brrrrr in the background. By default, the result is returned to the global environment when the job completes.

Install:

```r
remotes::install_github("lindeloev/job")
```

## Typical usage

We all love `brms` but compilation and sampling takes time. Let's run it as a job!

```r
library(brms)
data = mtcars[mtcars$hp > 100, ]
model = mpg ~ hp * wt

# Send long-running code to a job
job::job(brm_result = {
  fit = brm(model, data)
  fit = add_loo(fit)
  print(summary(fit))  # Show a summary in the job
  the_test = hypothesis(fit, "hp > 0")
})

cat("I'm free now! Thank you.
    Sincerely, Console.")
```

Now you can follow the progress in the jobs pane and your console is free. When the job completes, it saves `brm_result` to your environment. `brm_result` is itself an `environment()` and behaves much like a `list()`:

```r
brm_result$fit
brm_result$the_test
```


## Turn RStudio's Jobs into a record

Often, the results of the long-running chunks are the most interesting. But they easily get buried among the other outputs in the console and are eventually lost due to the line limit. RStudio's jobs history can be used as a nice record. Make sure to print informative results within the job and give your jobs an appropriate `title`, e.g., (`job::job({<code here>}, title = "Unit test: first attempt at feature X")`.

![](https://raw.githubusercontent.com/lindeloev/job/master/man/figures/joblist.png)


## Finer control

RStudio jobs spin up a new session, i.e., a new environment. By default, `job::job()` will make this environment identical to your current one. But you can fine control this:

-   `import`: by default, all objects are loaded into the job. Control this using `job::job({}, import = c(model, data))`, e.g., if you have a big global environment. Set `import = NULL` to import nothing.

-   `packages`: by default, all attached packages are attached in the job. Control this using `job::job({}, packages = c("brms"))` or set `packages = NULL` to load nothing. If `brms` is not loaded in your current session, adding `library("brms")` to the job code may be more readable than attaching via the `packages` argument.

-   `options`: by default, all options are overwritten/inserted to the job. Control this using, e.g., `job::job({}, opts = list(mc.cores = 2)` or set `opts = NULL` to use default options. If you want to set job-specific options, adding `options(mc.cores = 2)` to the job code may be more readable than setting via the `opts` argument.

-   `export`: in the example above, we assigned the job environment to `brm_result` upon completion. Naturally, you can choose any name, e.g., `job::job(fancy_name = {a = 5})`. To return nothing, simply omit the name (`job::job({a = 5})`. Returning nothing is useful when

    1.  your main result is a text output or a file on the disk, or

    2.  when the return is a very large object. The underlying `rstudioapi::jobRunScript()` is slow in the back-transfer so it's faster to `saveRDS(obj, filename)` them in the job and `readRDS(filename)` into your current session.


## Some use cases

-   Model training, cross validation, or hyperparameter tuning: train multiple models simultaneously, each in their own job. If one fails, the others continue.
-   Heavy I/O tasks, like processing large files. Save the results to disk and return nothing.
-   Run unit tests and other code in an empty environment. By default, `devtools::test()` runs in the current environment, including manually defined variables (e.g., from the last test-run) and attached packages. Call `job::job({devtools::test()}, import = NULL, packages = NULL, opts = NULL)` to run the test in complete isolation.
-   Upgrading packages
