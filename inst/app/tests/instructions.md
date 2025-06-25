
You need to have the `shinytest2` package installed. If you haven't installed it yet, you can do so with:
``` r
install.packages("shinytest2")
```

To run all the tests do
``` r
test_app("inst/app")
```

If any tests fail, run
``` r
testthat::snapshot_review('shinytest2/', path="inst/app/tests/testthat")
```
to review the changes and either accept them or reject them or change the code and rerun the test until it passes.

After you have accepted changes, commit the changed snapshot files.

To record a new test, you can use the `record_app` function:
``` r
record_app("inst/app")
```
This will open the app in a browser window, where you can interact with it and add screenshots. When you are done, close the browser window and the test will be recorded. It will be added to the script in `inst/app/tests/testthat/test-shinytest2.R`. You can edit that file directly, for example to remove tests you no longer want.
