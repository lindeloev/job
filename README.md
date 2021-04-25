# job: free your RStudio console

`job::job()` runs R code in an RStudio job instead of the console. This frees your console to continue coding while the job(s) go brrrrr in the background. By default, the result is returned to your environment when the job completes.

Install:

``` {.r}
remotes::install_github("lindeloev/job")
```

## Typical usage

We all love `brms` but compilation and sampling takes time. Let's run it as a job!

``` {.r}
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
```

Now you can follow the progress in the jobs pane. When the job completes, it saves `brm_result` to your environment. `brm_result` is itself an `environment()` and behaves much like a `list()`:

``` {.r}
brm_result$fit
brm_result$the_test
```

You can control `job::job()` behavior:

-   `import`: by default, all objects are loaded into the job. Control this using `job::job({}, import = c(model, data))`, e.g., if you have a big global environment. Set `import = NULL` to import nothing, i.e., run the job in a clean session.
-   `packages`: by default, all attached packages are also attached in the job. Control this using `job::job({}, packages = c("brms"))` or set `packages = NULL` to load nothing. You can also simply call `library("brms")` etc. inside the job chunk.
-   `export`: in the example above, we assigned the job environment to `brm_result` upon completion. Naturally, you can choose any name, e.g., `job::job(fancy_name = {a = 5})`. To return nothing, simply omit the name (`job::job({a = 5})`. This is useful when printing the result to console or if you save results to a file.

## Some use cases

-   Model training, cross validation, or hyperparameter tuning: train multiple models simultaneously, each in their own job. If one fails, the others continue.
-   Heavy I/O tasks, like processing large files. Save the results to disk and return nothing.
-   Run unit tests and other code in an empty environment. By default, `devtools::test()` runs in the current environment, including manually defined variables (e.g., from the last test-run) and attached packages. Call `job::job({devtools::test()}, import = NULL, packages = NULL)` to run the test in complete isolation.

## Turn RStudio's Jobs into a record

Often, the results of the long-running chunks are the most interesting. But they easily get buried among the other outputs in the console and are eventually lost due to the line limit. RStudio's jobs history can be used as a nice record. Make sure to print informative results within the job and give your jobs an appropriate `title`, e.g., (`job::job({}, title = "Unit test: first attempt at feature X")`.
