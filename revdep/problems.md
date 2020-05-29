# animint2

<details>

* Version: 2019.7.3
* Source code: https://github.com/cran/animint2
* URL: https://github.com/tdhock/animint2
* BugReports: https://github.com/tdhock/animint2/issues
* Date/Publication: 2019-07-18 06:36:12 UTC
* Number of recursive dependencies: 134

Run `revdep_details(,"animint2")` for more info

</details>

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘animint2-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: aes
    > ### Title: Define aesthetic mappings.
    > ### Aliases: aes
    > 
    > ### ** Examples
    > 
    > aes(x = mpg, y = wt)
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 732 | SKIPPED: 7 | WARNINGS: 14 | FAILED: 19 ]
      1. Failure: grid: facet order follows default data frame order (@test-compiler-facet-locate.r#118) 
      2. Failure: grid: facet order follows default data frame order (@test-compiler-facet-locate.r#119) 
      3. Failure: grid: facet order follows default data frame order (@test-compiler-facet-locate.r#126) 
      4. Failure: grid: facet order follows default data frame order (@test-compiler-facet-locate.r#127) 
      5. Failure: grid: facet order follows default data frame order (@test-compiler-facet-locate.r#134) 
      6. Failure: grid: facet order follows default data frame order (@test-compiler-facet-locate.r#135) 
      7. Failure: wrap: facet order follows default data frame order (@test-compiler-facet-locate.r#148) 
      8. Failure: wrap: facet order follows default data frame order (@test-compiler-facet-locate.r#154) 
      9. Failure: wrap: facet order follows default data frame order (@test-compiler-facet-locate.r#160) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        data   3.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘lazyeval’ ‘tibble’
      All declared Imports should be used.
    ```

# aoristic

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/aoristic
* Date/Publication: 2020-04-24 16:10:13 UTC
* Number of recursive dependencies: 68

Run `revdep_details(,"aoristic")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘foreign’
      All declared Imports should be used.
    ```

# bea.R

<details>

* Version: 1.0.6
* Source code: https://github.com/cran/bea.R
* URL: https://github.com/us-bea/bea.R
* Date/Publication: 2018-02-23 19:30:19 UTC
* Number of recursive dependencies: 74

Run `revdep_details(,"bea.R")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rcpp’ ‘chron’ ‘colorspace’ ‘gtable’ ‘htmltools’ ‘htmlwidgets’
      ‘httpuv’ ‘magrittr’ ‘munsell’ ‘plyr’ ‘scales’ ‘stringi’ ‘xtable’
      ‘yaml’
      All declared Imports should be used.
    ```

# BETS

<details>

* Version: 0.4.9
* Source code: https://github.com/cran/BETS
* URL: https://github.com/nmecsys/BETS
* BugReports: https://github.com/nmecsys/BETS/issues
* Date/Publication: 2018-09-28 17:10:03 UTC
* Number of recursive dependencies: 141

Run `revdep_details(,"BETS")` for more info

</details>

## In both

*   checking whether the package can be unloaded cleanly ... WARNING
    ```
    Error: package or namespace load failed for ‘BETS’ in dyn.load(file, DLLpath = DLLpath, ...):
     unable to load shared object '/Users/barret/Documents/git/rstudio/htmltools/htmltools/revdep/library.noindex/BETS/RMySQL/libs/RMySQL.so':
      dlopen(/Users/barret/Documents/git/rstudio/htmltools/htmltools/revdep/library.noindex/BETS/RMySQL/libs/RMySQL.so, 6): Symbol not found: _GSS_C_NT_USER_NAME
      Referenced from: /Users/barret/Documents/git/rstudio/htmltools/htmltools/revdep/library.noindex/BETS/RMySQL/libs/RMySQL.so
      Expected in: flat namespace
     in /Users/barret/Documents/git/rstudio/htmltools/htmltools/revdep/library.noindex/BETS/RMySQL/libs/RMySQL.so
    Execution halted
    ```

*   checking whether the namespace can be loaded with stated dependencies ... WARNING
    ```
    Error in dyn.load(file, DLLpath = DLLpath, ...) : 
      unable to load shared object '/Users/barret/Documents/git/rstudio/htmltools/htmltools/revdep/library.noindex/BETS/RMySQL/libs/RMySQL.so':
      dlopen(/Users/barret/Documents/git/rstudio/htmltools/htmltools/revdep/library.noindex/BETS/RMySQL/libs/RMySQL.so, 6): Symbol not found: _GSS_C_NT_USER_NAME
      Referenced from: /Users/barret/Documents/git/rstudio/htmltools/htmltools/revdep/library.noindex/BETS/RMySQL/libs/RMySQL.so
      Expected in: flat namespace
     in /Users/barret/Documents/git/rstudio/htmltools/htmltools/revdep/library.noindex/BETS/RMySQL/libs/RMySQL.so
    Calls: <Anonymous> ... namespaceImport -> loadNamespace -> library.dynam -> dyn.load
    Execution halted
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Error: package or namespace load failed for ‘BETS’ in dyn.load(file, DLLpath = DLLpath, ...):
     unable to load shared object '/Users/barret/Documents/git/rstudio/htmltools/htmltools/revdep/library.noindex/BETS/RMySQL/libs/RMySQL.so':
      dlopen(/Users/barret/Documents/git/rstudio/htmltools/htmltools/revdep/library.noindex/BETS/RMySQL/libs/RMySQL.so, 6): Symbol not found: _GSS_C_NT_USER_NAME
      Referenced from: /Users/barret/Documents/git/rstudio/htmltools/htmltools/revdep/library.noindex/BETS/RMySQL/libs/RMySQL.so
      Expected in: flat namespace
     in /Users/barret/Documents/git/rstudio/htmltools/htmltools/revdep/library.noindex/BETS/RMySQL/libs/RMySQL.so
    Call sequence:
    6: stop(msg, call. = FALSE, domain = NA)
    5: value[[3L]](cond)
    4: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    3: tryCatchList(expr, classes, parentenv, handlers)
    2: tryCatch({
           attr(package, "LibPath") <- which.lib.loc
           ns <- loadNamespace(package, lib.loc)
           env <- attachNamespace(ns, pos = pos, de
    Execution halted
    ```

# billboarder

<details>

* Version: 0.2.8
* Source code: https://github.com/cran/billboarder
* URL: https://github.com/dreamRs/billboarder
* BugReports: https://github.com/dreamRs/billboarder/issues
* Date/Publication: 2020-01-09 18:30:26 UTC
* Number of recursive dependencies: 76

Run `revdep_details(,"billboarder")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc   3.4Mb
    ```

# bioacoustics

<details>

* Version: 0.2.4
* Source code: https://github.com/cran/bioacoustics
* URL: https://github.com/wavx/bioacoustics/
* BugReports: https://github.com/wavx/bioacoustics/issues/
* Date/Publication: 2020-05-24 15:40:02 UTC
* Number of recursive dependencies: 23

Run `revdep_details(,"bioacoustics")` for more info

</details>

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# BiocPkgTools

<details>

* Version: 1.6.0
* Source code: https://github.com/cran/BiocPkgTools
* URL: https://github.com/seandavi/BiocPkgTools
* BugReports: https://github.com/seandavi/BiocPkgTools/issues/new
* Date/Publication: 2020-04-27
* Number of recursive dependencies: 114

Run `revdep_details(,"BiocPkgTools")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rappdirs’
      All declared Imports should be used.
    Unexported object imported by a ':::' call: ‘BiocManager:::.repositories’
      See the note in ?`:::` about the use of this operator.
    ```

# BiocStyle

<details>

* Version: 2.16.0
* Source code: https://github.com/cran/BiocStyle
* URL: https://github.com/Bioconductor/BiocStyle
* BugReports: https://github.com/Bioconductor/BiocStyle/issues
* Date/Publication: 2020-04-27
* Number of recursive dependencies: 23

Run `revdep_details(,"BiocStyle")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unexported object imported by a ':::' call: ‘rmarkdown:::partition_yaml_front_matter’
      See the note in ?`:::` about the use of this operator.
    ```

# bsplus

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/bsplus
* URL: https://github.com/ijlyttle/bsplus
* BugReports: https://github.com/ijlyttle/bsplus/issues
* Date/Publication: 2018-04-05 18:39:46 UTC
* Number of recursive dependencies: 57

Run `revdep_details(,"bsplus")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# CaPO4Sim

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/CaPO4Sim
* Date/Publication: 2019-04-11 14:55:39 UTC
* Number of recursive dependencies: 85

Run `revdep_details(,"CaPO4Sim")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘htmltools’
      All declared Imports should be used.
    ```

# CEMiTool

<details>

* Version: 1.12.0
* Source code: https://github.com/cran/CEMiTool
* Date/Publication: 2020-04-27
* Number of recursive dependencies: 170

Run `revdep_details(,"CEMiTool")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.9Mb
      sub-directories of 1Mb or more:
        data      3.1Mb
        doc       2.3Mb
        extdata   2.8Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    plot_ora,CEMiTool : <anonymous>: no visible global function definition
      for ‘head’
    plot_qq,CEMiTool: no visible binding for global variable ‘data’
    plot_sample_tree,CEMiTool: no visible global function definition for
      ‘hclust’
    plot_sample_tree,CEMiTool: no visible global function definition for
      ‘dist’
    plot_sample_tree,CEMiTool: no visible global function definition for
      ‘dev.off’
    save_plots,CEMiTool : <anonymous>: no visible global function
      definition for ‘dev.off’
    save_plots,CEMiTool: no visible global function definition for
      ‘dev.off’
    Undefined global functions or variables:
      ..eq.label.. ..rr.label.. := Mean Variance as.dist data dev.off dist
      hclust head modules num_genes setNames tail var
    Consider adding
      importFrom("grDevices", "dev.off")
      importFrom("stats", "as.dist", "dist", "hclust", "setNames", "var")
      importFrom("utils", "data", "head", "tail")
    to your NAMESPACE file.
    ```

# chromoMap

<details>

* Version: 0.2
* Source code: https://github.com/cran/chromoMap
* Date/Publication: 2019-04-10 19:27:11 UTC
* Number of recursive dependencies: 30

Run `revdep_details(,"chromoMap")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘htmltools’
      All declared Imports should be used.
    ```

# ClassifyR

<details>

* Version: 2.8.0
* Source code: https://github.com/cran/ClassifyR
* Date/Publication: 2020-04-27
* Number of recursive dependencies: 153

Run `revdep_details(,"ClassifyR")` for more info

</details>

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘measurements’
    samplesMetricMap,matrix: no visible binding for global variable ‘name’
    samplesMetricMap,matrix: no visible binding for global variable ‘type’
    samplesMetricMap,matrix: no visible binding for global variable
      ‘Metric’
    selectionPlot,list : <anonymous> : <anonymous>: no visible global
      function definition for ‘first’
    selectionPlot,list : <anonymous> : <anonymous>: no visible global
      function definition for ‘second’
    selectionPlot,list : <anonymous> : <anonymous> : <anonymous>: no
      visible global function definition for ‘first’
    selectionPlot,list : <anonymous> : <anonymous> : <anonymous>: no
      visible global function definition for ‘second’
    selectionPlot,list: no visible binding for global variable ‘Freq’
    Undefined global functions or variables:
      ..density.. .iteration Class Freq Group ID Metric Pairs dmvnorm first
      key legends grouping measurement measurements name second setsNodes
      type value
    Consider adding
      importFrom("base", "grouping")
    to your NAMESPACE file.
    ```

# codebook

<details>

* Version: 0.8.2
* Source code: https://github.com/cran/codebook
* URL: https://github.com/rubenarslan/codebook
* BugReports: https://github.com/rubenarslan/codebook/issues
* Date/Publication: 2020-01-09 16:20:07 UTC
* Number of recursive dependencies: 179

Run `revdep_details(,"codebook")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘graphics’ ‘jsonlite’ ‘pander’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 65 marked UTF-8 strings
    ```

# datadigest

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/datadigest
* URL: https://github.com/RhoInc/datadigest
* BugReports: https://github.com/RhoInc/datadigest/issues
* Date/Publication: 2018-09-03 11:10:06 UTC
* Number of recursive dependencies: 92

Run `revdep_details(,"datadigest")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘miniUI’
      All declared Imports should be used.
    ```

# DiagrammeR

<details>

* Version: 1.0.6.1
* Source code: https://github.com/cran/DiagrammeR
* URL: https://github.com/rich-iannone/DiagrammeR
* BugReports: https://github.com/rich-iannone/DiagrammeR/issues
* Date/Publication: 2020-05-08 21:40:02 UTC
* Number of recursive dependencies: 86

Run `revdep_details(,"DiagrammeR")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 14.1Mb
      sub-directories of 1Mb or more:
        doc           9.7Mb
        htmlwidgets   2.8Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# downloadthis

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/downloadthis
* URL: https://github.com/fmmattioni/downloadthis
* BugReports: https://github.com/fmmattioni/downloadthis/issues
* Date/Publication: 2020-05-04 19:10:02 UTC
* Number of recursive dependencies: 42

Run `revdep_details(,"downloadthis")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        assets   2.9Mb
        doc      2.0Mb
    ```

# EBImage

<details>

* Version: 4.30.0
* Source code: https://github.com/cran/EBImage
* URL: https://github.com/aoles/EBImage
* BugReports: https://github.com/aoles/EBImage/issues
* Date/Publication: 2020-04-27
* Number of recursive dependencies: 43

Run `revdep_details(,"EBImage")` for more info

</details>

## In both

*   checking whether package ‘EBImage’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      morphology.cpp:163:14: warning: expression result unused [-Wunused-value]
      morphology.cpp:166:14: warning: expression result unused [-Wunused-value]
      morphology.cpp:170:14: warning: expression result unused [-Wunused-value]
      morphology.cpp:196:14: warning: expression result unused [-Wunused-value]
      morphology.cpp:199:14: warning: expression result unused [-Wunused-value]
      morphology.cpp:203:14: warning: expression result unused [-Wunused-value]
    See ‘/Users/barret/Documents/git/rstudio/htmltools/htmltools/revdep/checks.noindex/EBImage/new/EBImage.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        doc      5.1Mb
        images   1.7Mb
    ```

# eph

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/eph
* URL: https://github.com/holatam/eph
* BugReports: https://github.com/holatam/eph/issues
* Date/Publication: 2020-05-24 18:40:03 UTC
* Number of recursive dependencies: 137

Run `revdep_details(,"eph")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘readr’ ‘tidyverse’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked Latin-1 string
      Note: found 721 marked UTF-8 strings
    ```

# epiflows

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/epiflows
* URL: https://www.repidemicsconsortium.org/epiflows
* BugReports: https://github.com/reconhub/epiflows/issues
* Date/Publication: 2018-08-14 12:40:03 UTC
* Number of recursive dependencies: 129

Run `revdep_details(,"epiflows")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘htmlwidgets’
      All declared Imports should be used.
    ```

# epivizrChart

<details>

* Version: 1.10.0
* Source code: https://github.com/cran/epivizrChart
* Date/Publication: 2020-04-27
* Number of recursive dependencies: 168

Run `revdep_details(,"epivizrChart")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 25.1Mb
      sub-directories of 1Mb or more:
        data   3.3Mb
        doc   17.3Mb
        www    3.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: 'epivizrServer'
      All declared Imports should be used.
    ```

# eqn2svg

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/eqn2svg
* URL: https://github.com/rich-iannone/eqn2svg
* BugReports: https://github.com/rich-iannone/eqn2svg/issues
* Date/Publication: 2019-10-06 12:50:05 UTC
* Number of recursive dependencies: 26

Run `revdep_details(,"eqn2svg")` for more info

</details>

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(eqn2svg)
      > 
      > test_check("eqn2svg")
      ── 1. Failure: SVG conversion works (@test-eqn2svg.R#5)  ───────────────────────
      `svg_text` does not match "^  <svg style.*".
      Actual value: "<svg style="vertical-align: -0\.566ex" xmlns="http://www\.w3\.org/2000/svg" width="15\.31ex" height="2\.262ex" role="img" focusable="false" viewBox="0 -750 6767\.2 1000"><g stroke="currentColor" fill="black" stroke-width="0" transform="matrix\(1 0 0 -1 0 0\)"><g data-mml-node="math"><g data-mml-node="TeXAtom" data-mjx-texclass="ORD"><g data-mml-node="msub"><g data-mml-node="mi"><path data-c="1D462" d="M21 287Q21 295 30 318T55 370T99 420T158 442Q204 442 227 417T250 358Q250 340 216 246T182 105Q182 62 196 45T238 27T291 44T328 78L339 95Q341 99 377 247Q407 367 413 387T427 416Q444 431 463 431Q480 431 488 421T496 402L420 84Q419 79 419 68Q419 43 426 35T447 26Q469 29 482 57T512 145Q514 153 532 153Q551 153 551 144Q550 139 549 130T540 98T523 55T498 17T462 -8Q454 -10 438 -10Q372 -10 347 46Q345 45 336 36T318 21T296 6T267 -6T233 -11Q189 -11 155 7Q103 38 103 113Q103 170 138 262T173 379Q173 380 173 381Q173 390 173 393T169 400T158 404H154Q131 404 112 385T82 344T65 302T57 280Q55 278 41 278H27Q21 284 21 287Z"></path></g><g data-mml-node="mi" transform="translate\(572, -150\) scale\(0\.707\)"><path data-c="1D450" d="M34 159Q34 268 120 355T306 442Q362 442 394 418T427 355Q427 326 408 306T360 285Q341 285 330 295T319 325T330 359T352 380T366 386H367Q367 388 361 392T340 400T306 404Q276 404 249 390Q228 381 206 359Q162 315 142 235T121 119Q121 73 147 50Q169 26 205 26H209Q321 26 394 111Q403 121 406 121Q410 121 419 112T429 98T420 83T391 55T346 25T282 0T202 -11Q127 -11 81 37T34 159Z"></path></g></g></g><g data-mml-node="mrow" transform="translate\(928\.2, 0\)"><g data-mml-node="mo"><path data-c="28" d="M94 250Q94 319 104 381T127 488T164 576T202 643T244 695T277 729T302 750H315H319Q333 750 333 741Q333 738 316 720T275 667T226 581T184 443T167 250T184 58T225 -81T274 -167T316 -220T333 -241Q333 -250 318 -250H315H302L274 -226Q180 -141 137 -14T94 250Z"></path></g><g data-mml-node="mi" transform="translate\(389, 0\)"><path data-c="1D461" d="M26 385Q19 392 19 395Q19 399 22 411T27 425Q29 430 36 430T87 431H140L159 511Q162 522 166 540T173 566T179 586T187 603T197 615T211 624T229 626Q247 625 254 615T261 596Q261 589 252 549T232 470L222 433Q222 431 272 431H323Q330 424 330 420Q330 398 317 385H210L174 240Q135 80 135 68Q135 26 162 26Q197 26 230 60T283 144Q285 150 288 151T303 153H307Q322 153 322 145Q322 142 319 133Q314 117 301 95T267 48T216 6T155 -11Q125 -11 98 4T59 56Q57 64 57 83V101L92 241Q127 382 128 383Q128 385 77 385H26Z"></path></g><g data-mml-node="mo" transform="translate\(750, 0\)"><path data-c="29" d="M60 749L64 750Q69 750 74 750H86L114 726Q208 641 251 514T294 250Q294 182 284 119T261 12T224 -76T186 -143T145 -194T113 -227T90 -246Q87 -249 86 -250H74Q66 -250 63 -250T58 -247T55 -238Q56 -237 66 -225Q221 -64 221 250T66 725Q56 737 55 738Q55 746 60 749Z"></path></g></g><g data-mml-node="mo" transform="translate\(2345, 0\)"><path data-c="3D" d="M56 347Q56 360 70 367H707Q722 359 722 347Q722 336 708 328L390 327H72Q56 332 56 347ZM56 153Q56 168 72 173H708Q722 163 722 153Q722 140 707 133H70Q56 140 56 153Z"></path></g><g data-mml-node="mi" transform="translate\(3400\.7, 0\)"><path data-c="1D462" d="M21 287Q21 295 30 318T55 370T99 420T158 442Q204 442 227 417T250 358Q250 340 216 246T182 105Q182 62 196 45T238 27T291 44T328 78L339 95Q341 99 377 247Q407 367 413 387T427 416Q444 431 463 431Q480 431 488 421T496 402L420 84Q419 79 419 68Q419 43 426 35T447 26Q469 29 482 57T512 145Q514 153 532 153Q551 153 551 144Q550 139 549 130T540 98T523 55T498 17T462 -8Q454 -10 438 -10Q372 -10 347 46Q345 45 336 36T318 21T296 6T267 -6T233 -11Q189 -11 155 7Q103 38 103 113Q103 170 138 262T173 379Q173 380 173 381Q173 390 173 393T169 400T158 404H154Q131 404 112 385T82 344T65 302T57 280Q55 278 41 278H27Q21 284 21 287Z"></path></g><g data-mml-node="mrow" transform="translate\(3972\.7, 0\)"><g data-mml-node="mo"><path data-c="28" d="M94 250Q94 319 104 381T127 488T164 576T202 643T244 695T277 729T302 750H315H319Q333 750 333 741Q333 738 316 720T275 667T226 581T184 443T167 250T184 58T225 -81T274 -167T316 -220T333 -241Q333 -250 318 -250H315H302L274 -226Q180 -141 137 -14T94 250Z"></path></g><g data-mml-node="TeXAtom" data-mjx-texclass="ORD" transform="translate\(389, 0\)"><g data-mml-node="mi"><path data-c="1D461" d="M26 385Q19 392 19 395Q19 399 22 411T27 425Q29 430 36 430T87 431H140L159 511Q162 522 166 540T173 566T179 586T187 603T197 615T211 624T229 626Q247 625 254 615T261 596Q261 589 252 549T232 470L222 433Q222 431 272 431H323Q330 424 330 420Q330 398 317 385H210L174 240Q135 80 135 68Q135 26 162 26Q197 26 230 60T283 144Q285 150 288 151T303 153H307Q322 153 322 145Q322 142 319 133Q314 117 301 95T267 48T216 6T155 -11Q125 -11 98 4T59 56Q57 64 57 83V101L92 241Q127 382 128 383Q128 385 77 385H26Z"></path></g><g data-mml-node="mo" transform="translate\(583\.2, 0\)"><path data-c="2212" d="M84 237T84 250T98 270H679Q694 262 694 250T679 230H98Q84 237 84 250Z"></path></g><g data-mml-node="mi" transform="translate\(1583\.4, 0\)"><path data-c="1D450" d="M34 159Q34 268 120 355T306 442Q362 442 394 418T427 355Q427 326 408 306T360 285Q341 285 330 295T319 325T330 359T352 380T366 386H367Q367 388 361 392T340 400T306 404Q276 404 249 390Q228 381 206 359Q162 315 142 235T121 119Q121 73 147 50Q169 26 205 26H209Q321 26 394 111Q403 121 406 121Q410 121 419 112T429 98T420 83T391 55T346 25T282 0T202 -11Q127 -11 81 37T34 159Z"></path></g></g><g data-mml-node="mo" transform="translate\(2405\.4, 0\)"><path data-c="29" d="M60 749L64 750Q69 750 74 750H86L114 726Q208 641 251 514T294 250Q294 182 284 119T261 12T224 -76T186 -143T145 -194T113 -227T90 -246Q87 -249 86 -250H74Q66 -250 63 -250T58 -247T55 -238Q56 -237 66 -225Q221 -64 221 250T66 725Q56 737 55 738Q55 746 60 749Z"></path></g></g></g></g></svg>"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 1 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: SVG conversion works (@test-eqn2svg.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# FastqCleaner

<details>

* Version: 1.6.0
* Source code: https://github.com/cran/FastqCleaner
* Date/Publication: 2020-04-27
* Number of recursive dependencies: 86

Run `revdep_details(,"FastqCleaner")` for more info

</details>

## In both

*   checking for portable file names ... NOTE
    ```
    Found the following non-portable file paths:
      FastqCleaner/inst/application/www/help/docs/articles/Overview_files/figure-html/unnamed-chunk-11-1.png
      FastqCleaner/inst/application/www/help/docs/articles/Overview_files/figure-html/unnamed-chunk-12-1.png
      FastqCleaner/inst/application/www/help/docs/articles/Overview_files/figure-html/unnamed-chunk-13-1.png
      FastqCleaner/inst/application/www/help/docs/articles/Overview_files/figure-html/unnamed-chunk-14-1.png
      FastqCleaner/inst/application/www/help/docs/articles/Overview_files/figure-html/unnamed-chunk-7-1.png
      FastqCleaner/inst/application/www/help/docs/articles/Overview_files/figure-html/unnamed-chunk-8-1.png
    
    Tarballs are only required to store paths of up to 100 bytes and cannot
    store those of more than 256 bytes, with restrictions including to 100
    bytes for the final component.
    See section ‘Package structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        application   4.2Mb
        doc           1.1Mb
    ```

# GenEst

<details>

* Version: 1.4.3
* Source code: https://github.com/cran/GenEst
* Date/Publication: 2020-05-21 04:20:02 UTC
* Number of recursive dependencies: 83

Run `revdep_details(,"GenEst")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘htmlwidgets’
      All declared Imports should be used.
    ```

# GeneTonic

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/GeneTonic
* URL: https://github.com/federicomarini/GeneTonic
* BugReports: https://github.com/federicomarini/GeneTonic/issues
* Date/Publication: 2020-04-27
* Number of recursive dependencies: 175

Run `revdep_details(,"GeneTonic")` for more info

</details>

## In both

*   checking examples ... ERROR
    ```
    ...
    'select()' returned 1:many mapping between keys and columns
    > 
    > # res object
    > data(res_de_macrophage, package = "GeneTonic")
    > res_de <- res_macrophage_IFNg_vs_naive
    > 
    > # res_enrich object
    > data(res_enrich_macrophage, package = "GeneTonic")
    > res_enrich <- shake_topGOtableResult(topgoDE_macrophage_IFNg_vs_naive)
    Found 500 gene sets in `topGOtableResult` object.
    Converting for usage in GeneTonic...
    > res_enrich <- get_aggrscores(res_enrich, res_de, anno_df)
    > 
    > ggs <- ggs_graph(res_enrich,
    +                  res_de,
    +                  anno_df
    +                 )
    Error in (function (classes, fdef, mtable)  : 
      unable to find an inherited method for function ‘Definition’ for signature ‘"NULL"’
    Calls: ggs_graph -> vapply -> FUN -> Definition -> <Anonymous>
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 13.9Mb
      sub-directories of 1Mb or more:
        doc  12.7Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘pcaExplorer’
    ```

# gt

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/gt
* URL: https://github.com/rstudio/gt
* BugReports: https://github.com/rstudio/gt/issues
* Date/Publication: 2020-05-23 07:00:02 UTC
* Number of recursive dependencies: 104

Run `revdep_details(,"gt")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        help   5.5Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 116 marked UTF-8 strings
    ```

# highcharter

<details>

* Version: 0.7.0
* Source code: https://github.com/cran/highcharter
* URL: http://jkunst.com/highcharter
* BugReports: https://github.com/jbkunst/highcharter/issues
* Date/Publication: 2019-01-15 16:50:03 UTC
* Number of recursive dependencies: 96

Run `revdep_details(,"highcharter")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.8Mb
      sub-directories of 1Mb or more:
        doc           3.7Mb
        htmlwidgets   4.0Mb
    ```

# hystReet

<details>

* Version: 0.0.1
* Source code: https://github.com/cran/hystReet
* URL: https://github.com/JohannesFriedrich/hystReet
* BugReports: https://github.com/JohannesFriedrich/hystReet/issues
* Date/Publication: 2020-04-01 09:50:15 UTC
* Number of recursive dependencies: 72

Run `revdep_details(,"hystReet")` for more info

</details>

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 236 marked UTF-8 strings
    ```

# i2dash

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/i2dash
* Date/Publication: 2020-05-27 15:00:02 UTC
* Number of recursive dependencies: 135

Run `revdep_details(,"i2dash")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        doc   5.7Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘assertive.types’ ‘flexdashboard’ ‘rmarkdown’ ‘stats’ ‘stringr’
      ‘yaml’ ‘ymlthis’
      All declared Imports should be used.
    ```

# idr2d

<details>

* Version: 1.2.2
* Source code: https://github.com/cran/idr2d
* URL: https://idr2d.mit.edu
* Date/Publication: 2020-05-07
* Number of recursive dependencies: 91

Run `revdep_details(,"idr2d")` for more info

</details>

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `expect_output(...)` threw an error with unexpected message.
      Expected match: "No module named 'straw'"
      Actual message: "ImportError: No module named straw"
      Backtrace:
        1. testthat::expect_error(...)
       15. idr2d::parse_juicer_matrix("wrong/path", use_python = "", use_virtualenv = "")
       16. reticulate::import("straw")
       17. reticulate:::py_module_import(module, convert = convert)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 131 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: parse_juicer_matrix (@test-hic.R#36) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# iSEE

<details>

* Version: 2.0.0
* Source code: https://github.com/cran/iSEE
* URL: https://github.com/iSEE/iSEE
* BugReports: https://github.com/iSEE/iSEE/issues
* Date/Publication: 2020-04-27
* Number of recursive dependencies: 153

Run `revdep_details(,"iSEE")` for more info

</details>

## In both

*   checking examples ... ERROR
    ```
    ...
    snapshotDate(): 2020-04-27
    see ?scRNAseq and browseVignettes('scRNAseq') for documentation
    loading from cache
    see ?scRNAseq and browseVignettes('scRNAseq') for documentation
    loading from cache
    see ?scRNAseq and browseVignettes('scRNAseq') for documentation
    loading from cache
    > class(sce)
    [1] "SingleCellExperiment"
    attr(,"package")
    [1] "SingleCellExperiment"
    > 
    > library(scater)
    Loading required package: ggplot2
    > sce <- logNormCounts(sce, exprs_values="tophat_counts")
    > 
    > sce <- runPCA(sce, ncomponents=4)
    > sce <- runTSNE(sce)
    Error in loadNamespace(name) : there is no package called ‘Rtsne’
    Calls: runTSNE ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      see ?scRNAseq and browseVignettes('scRNAseq') for documentation
      downloading 1 resources
      retrieving 1 resource
      loading from cache
      see ?scRNAseq and browseVignettes('scRNAseq') for documentation
      downloading 1 resources
      retrieving 1 resource
      loading from cache
      see ?scRNAseq and browseVignettes('scRNAseq') for documentation
      downloading 1 resources
      retrieving 1 resource
      loading from cache
      Error in loadNamespace(name) : there is no package called 'Rtsne'
      Calls: test_check ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 26.3Mb
      sub-directories of 1Mb or more:
        doc  24.6Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported object imported by a ':::' call: ‘S4Vectors:::selectSome’
      See the note in ?`:::` about the use of this operator.
    ```

# leafem

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/leafem
* URL: https://github.com/r-spatial/leafem
* BugReports: https://github.com/r-spatial/leafem/issues
* Date/Publication: 2020-04-05 11:40:02 UTC
* Number of recursive dependencies: 100

Run `revdep_details(,"leafem")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# leaflet.extras2

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/leaflet.extras2
* URL: https://trafficonese.github.io/leaflet.extras2, https://github.com/trafficonese/leaflet.extras2
* BugReports: https://github.com/trafficonese/leaflet.extras2/issues
* Date/Publication: 2020-05-18 15:10:06 UTC
* Number of recursive dependencies: 89

Run `revdep_details(,"leaflet.extras2")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘htmlwidgets’
      All declared Imports should be used.
    ```

# leafletCN

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/leafletCN
* Date/Publication: 2017-02-27 08:15:32
* Number of recursive dependencies: 64

Run `revdep_details(,"leafletCN")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.0Mb
      sub-directories of 1Mb or more:
        geojson  11.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rgeos’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1096 marked UTF-8 strings
    ```

# leafpm

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/leafpm
* URL: https://github.com/r-spatial/leafpm
* BugReports: https://github.com/r-spatial/leafpm/issues
* Date/Publication: 2019-03-13 12:10:03 UTC
* Number of recursive dependencies: 75

Run `revdep_details(,"leafpm")` for more info

</details>

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'geojsonio', 'mapedit', 'mapview'
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘htmlwidgets’ ‘jsonlite’ ‘sf’
      All declared Imports should be used.
    ```

# leafsync

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/leafsync
* URL: https://github.com/r-spatial/leafsync
* BugReports: https://github.com/r-spatial/leafsync/issues
* Date/Publication: 2019-03-05 15:10:03 UTC
* Number of recursive dependencies: 63

Run `revdep_details(,"leafsync")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘leaflet’
      All declared Imports should be used.
    ```

# mapedit

<details>

* Version: 0.6.0
* Source code: https://github.com/cran/mapedit
* URL: https://github.com/r-spatial/mapedit
* BugReports: https://github.com/r-spatial/mapedit/issues
* Date/Publication: 2020-02-02 17:20:02 UTC
* Number of recursive dependencies: 107

Run `revdep_details(,"mapedit")` for more info

</details>

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘geojsonio’
    ```

# mapview

<details>

* Version: 2.7.8
* Source code: https://github.com/cran/mapview
* URL: https://github.com/r-spatial/mapview
* BugReports: https://github.com/r-spatial/mapview/issues
* Date/Publication: 2020-04-07 10:00:16 UTC
* Number of recursive dependencies: 125

Run `revdep_details(,"mapview")` for more info

</details>

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘leafsync’, ‘slideview’
    ```

# metaseqR2

<details>

* Version: 1.0.11
* Source code: https://github.com/cran/metaseqR2
* URL: http://www.fleming.gr
* BugReports: https://github.com/pmoulos/metaseqR2/issues
* Date/Publication: 2020-04-29
* Number of recursive dependencies: 204

Run `revdep_details(,"metaseqR2")` for more info

</details>

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘metaseqR2-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: getAnnotation
    > ### Title: Annotation downloader
    > ### Aliases: getAnnotation
    > 
    > ### ** Examples
    > 
    > mm9Genes <- getAnnotation("mm9","gene")
    Using Ensembl host http://may2012.archive.ensembl.org
    Error in bmRequest(request = request, verbose = verbose) : 
      Not Found (HTTP 404).
    Calls: getAnnotation ... listMarts -> .listMarts -> bmRequest -> stop_for_status
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/runTests.R’ failed.
    Last 13 lines of output:
      metaseqR2 RUnit Tests - 2 test functions, 1 error, 0 failures
      ERROR in test_estimate_aufc_weights: Error in bmRequest(request = request, verbose = verbose) : 
        Not Found (HTTP 404).
      
      Test files with failing tests
      
         test_estimate_aufc_weights.R 
           test_estimate_aufc_weights 
      
      
      Error in BiocGenerics:::testPackage("metaseqR2") : 
        unit tests failed for package metaseqR2
      In addition: Warning message:
      The column that contains the gene biotypes ("embedCols$btCol") is missing with embedded annotation! Biotype filters and certain plots will not be available... 
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘TCC’
    ```

# mgwrsar

<details>

* Version: 0.1
* Source code: https://github.com/cran/mgwrsar
* Date/Publication: 2018-05-11 10:44:14 UTC
* Number of recursive dependencies: 78

Run `revdep_details(,"mgwrsar")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc    1.1Mb
        libs   4.0Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘McSpatial’
    ```

# motifStack

<details>

* Version: 1.32.0
* Source code: https://github.com/cran/motifStack
* Date/Publication: 2020-04-27
* Number of recursive dependencies: 109

Run `revdep_details(,"motifStack")` for more info

</details>

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘MotIV’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# Onassis

<details>

* Version: 1.10.0
* Source code: https://github.com/cran/Onassis
* Date/Publication: 2020-04-27
* Number of recursive dependencies: 124

Run `revdep_details(,"Onassis")` for more info

</details>

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘data.table’
    A package should be listed in only one of these fields.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘Rtsne’ ‘clusteval’ ‘dendextend’ ‘ggfortify’ ‘ggplot2’ ‘knitr’
      ‘stats’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    annotate,data.frame-character-character: no visible binding for global
      variable ‘sample_id’
    annotateDF,EntityFinder-data.frame-character-CMoptions: no visible
      binding for global variable ‘ID’
    annotateDF,EntityFinder-data.frame-character-CMoptions: no visible
      binding for global variable ‘NEW’
    collapse,Onassis: no visible global function definition for ‘as.dist’
    collapse,Onassis: no visible global function definition for ‘hclust’
    collapse,Onassis: no visible global function definition for ‘cutree’
    compare,Onassis: no visible global function definition for ‘p.adjust’
    Undefined global functions or variables:
      ID NEW as.dist cutree hclust p.adjust sample_id
    Consider adding
      importFrom("stats", "as.dist", "cutree", "hclust", "p.adjust")
    to your NAMESPACE file.
    ```

# oncrawlR

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/oncrawlR
* Date/Publication: 2020-01-31 16:40:03 UTC
* Number of recursive dependencies: 139

Run `revdep_details(,"oncrawlR")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘e1071’ ‘fs’ ‘pdp’
      All declared Imports should be used.
    ```

# packagedocs

<details>

* Version: 0.4.0
* Source code: https://github.com/cran/packagedocs
* URL: http://hafen.github.io/packagedocs, https://github.com/hafen/packagedocs
* BugReports: https://github.com/hafen/packagedocs/issues
* Date/Publication: 2020-01-01 11:37:19
* Number of recursive dependencies: 80

Run `revdep_details(,"packagedocs")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported objects:
      ‘devtools::use_build_ignore’ ‘devtools::use_travis’
    ```

# parlitools

<details>

* Version: 0.4.1
* Source code: https://github.com/cran/parlitools
* URL: https://docs.evanodell.com/parlitools, https://github.com/EvanOdell/parlitools/
* BugReports: https://github.com/EvanOdell/parlitools/issues
* Date/Publication: 2020-01-12 22:00:02 UTC
* Number of recursive dependencies: 122

Run `revdep_details(,"parlitools")` for more info

</details>

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 16 marked UTF-8 strings
    ```

# pcaExplorer

<details>

* Version: 2.14.1
* Source code: https://github.com/cran/pcaExplorer
* URL: https://github.com/federicomarini/pcaExplorer, https://federicomarini.github.io/pcaExplorer/
* BugReports: https://github.com/federicomarini/pcaExplorer/issues
* Date/Publication: 2020-05-26
* Number of recursive dependencies: 176

Run `revdep_details(,"pcaExplorer")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 21.3Mb
      sub-directories of 1Mb or more:
        doc  20.1Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    pcaExplorer: no visible binding for '<<-' assignment to
      ‘pcaexplorer_env’
    pcaExplorer : <anonymous>: no visible binding for global variable
      ‘airway’
    pcaExplorer : <anonymous>: no visible binding for global variable
      ‘pcaexplorer_env’
    Undefined global functions or variables:
      airway pcaexplorer_env
    ```

# phantasus

<details>

* Version: 1.8.0
* Source code: https://github.com/cran/phantasus
* URL: https://genome.ifmo.ru/phantasus, https://artyomovlab.wustl.edu/phantasus
* BugReports: https://github.com/ctlab/phantasus/issues
* Date/Publication: 2020-04-27
* Number of recursive dependencies: 141

Run `revdep_details(,"phantasus")` for more info

</details>

## In both

*   checking Rd \usage sections ... WARNING
    ```
    Documented arguments not in \usage in documentation object 'write.gct':
      ‘...’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 21.6Mb
      sub-directories of 1Mb or more:
        doc        3.0Mb
        testdata   4.3Mb
        www       14.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      'GEOquery:::getDirListing' 'opencpu:::rookhandler'
      'opencpu:::tmp_root' 'opencpu:::win_or_mac'
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    loadSession: no visible binding for global variable 'es'
    reproduceInR: no visible global function definition for 'object.size'
    safeDownload: no visible binding for global variable 'tempDestFile'
    Undefined global functions or variables:
      es object.size tempDestFile
    Consider adding
      importFrom("utils", "object.size")
    to your NAMESPACE file.
    ```

*   checking Rd files ... NOTE
    ```
    prepare_Rd: convertByAnnotationDB.Rd:27-32: Dropping empty section \examples
    ```

# pivottabler

<details>

* Version: 1.4.0
* Source code: https://github.com/cran/pivottabler
* URL: http://www.pivottabler.org.uk/, https://github.com/cbailiss/pivottabler
* BugReports: https://github.com/cbailiss/pivottabler/issues
* Date/Publication: 2020-05-11 15:20:02 UTC
* Number of recursive dependencies: 79

Run `revdep_details(,"pivottabler")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        R      2.0Mb
        data   2.0Mb
    ```

# plotGrouper

<details>

* Version: 1.6.0
* Source code: https://github.com/cran/plotGrouper
* URL: https://jdgagnon.github.io/plotGrouper/
* BugReports: https://github.com/jdgagnon/plotGrouper/issues
* Date/Publication: 2020-04-27
* Number of recursive dependencies: 137

Run `revdep_details(,"plotGrouper")` for more info

</details>

## In both

*   checking R code for possible problems ... NOTE
    ```
    gplot: no visible binding for global variable ‘max_value’
    gplot: no visible binding for global variable ‘max_error’
    Undefined global functions or variables:
      max_error max_value
    ```

# plotly

<details>

* Version: 4.9.2.1
* Source code: https://github.com/cran/plotly
* URL: https://plotly-r.com, https://github.com/ropensci/plotly#readme, https://plot.ly/r
* BugReports: https://github.com/ropensci/plotly/issues
* Date/Publication: 2020-04-04 19:50:02 UTC
* Number of recursive dependencies: 151

Run `revdep_details(,"plotly")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        htmlwidgets   3.7Mb
    ```

# pointblank

<details>

* Version: 0.3.1.1
* Source code: https://github.com/cran/pointblank
* URL: https://github.com/rich-iannone/pointblank
* BugReports: https://github.com/rich-iannone/pointblank/issues
* Date/Publication: 2020-04-02 08:40:03 UTC
* Number of recursive dependencies: 97

Run `revdep_details(,"pointblank")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scales’
      All declared Imports should be used.
    ```

# polmineR

<details>

* Version: 0.8.0
* Source code: https://github.com/cran/polmineR
* URL: https://www.github.com/PolMine/polmineR
* BugReports: https://github.com/PolMine/polmineR/issues
* Date/Publication: 2019-12-17 12:50:06 UTC
* Number of recursive dependencies: 100

Run `revdep_details(,"polmineR")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        R         2.0Mb
        extdata   1.9Mb
    ```

# processanimateR

<details>

* Version: 1.0.3
* Source code: https://github.com/cran/processanimateR
* URL: https://github.com/bupaverse/processanimateR/
* BugReports: https://github.com/bupaverse/processanimateR/issues
* Date/Publication: 2020-03-13 21:30:02 UTC
* Number of recursive dependencies: 104

Run `revdep_details(,"processanimateR")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.5Mb
      sub-directories of 1Mb or more:
        doc           8.8Mb
        htmlwidgets   2.5Mb
    ```

# psichomics

<details>

* Version: 1.13.1
* Source code: https://github.com/cran/psichomics
* URL: https://nuno-agostinho.github.io/psichomics/
* BugReports: https://github.com/nuno-agostinho/psichomics/issues
* Date/Publication: 2020-01-30
* Number of recursive dependencies: 236

Run `revdep_details(,"psichomics")` for more info

</details>

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      MIC
      
      EXSK
      
      MULTI
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 1280 | SKIPPED: 1 | WARNINGS: 1 | FAILED: 4 ]
      1. Failure: parseMisoEventID parses an event identifier from MISO (@testMisoEvents.R#28) 
      2. Failure: parseMisoEventID parses an event identifier from MISO (@testMisoEvents.R#29) 
      3. Failure: parseMisoEventID parses an event identifier from MISO (@testMisoEvents.R#31) 
      4. Failure: parseMisoEventID parses an event identifier from MISO (@testMisoEvents.R#32) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        R     2.0Mb
        doc   4.2Mb
    ```

# questionr

<details>

* Version: 0.7.1
* Source code: https://github.com/cran/questionr
* URL: https://juba.github.io/questionr/
* BugReports: https://github.com/juba/questionr/issues
* Date/Publication: 2020-05-26 11:30:05 UTC
* Number of recursive dependencies: 80

Run `revdep_details(,"questionr")` for more info

</details>

## In both

*   checking package dependencies ... ERROR
    ```
    Package required and available but unsuitable version: ‘haven’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# RagGrid

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/RagGrid
* URL: https://github.com/no-types/RagGrid/
* BugReports: https://github.com/no-types/RagGrid/issues
* Date/Publication: 2018-08-12 09:30:03 UTC
* Number of recursive dependencies: 32

Run `revdep_details(,"RagGrid")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

# rAmCharts

<details>

* Version: 2.1.13
* Source code: https://github.com/cran/rAmCharts
* URL: http://datastorm-open.github.io/introduction_ramcharts/
* BugReports: https://github.com/datastorm-open/rAmCharts/issues/
* Date/Publication: 2019-12-06 15:50:05 UTC
* Number of recursive dependencies: 52

Run `revdep_details(,"rAmCharts")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.8Mb
      sub-directories of 1Mb or more:
        api           3.1Mb
        htmlwidgets   7.0Mb
    ```

# rcrimeanalysis

<details>

* Version: 0.4.2
* Source code: https://github.com/cran/rcrimeanalysis
* Date/Publication: 2020-05-20 05:10:08 UTC
* Number of recursive dependencies: 109

Run `revdep_details(,"rcrimeanalysis")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rcpp’ ‘dplyr’
      All declared Imports should be used.
    ```

# regressoR

<details>

* Version: 1.1.8
* Source code: https://github.com/cran/regressoR
* URL: http://www.promidat.com
* Date/Publication: 2019-09-19 04:30:03 UTC
* Number of recursive dependencies: 120

Run `revdep_details(,"regressoR")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ROCR’ ‘colourpicker’ ‘corrplot’ ‘e1071’ ‘flexdashboard’ ‘gbm’
      ‘glmnet’ ‘kknn’ ‘neuralnet’ ‘rattle’ ‘rpart’ ‘shinyWidgets’
      ‘shinydashboardPlus’ ‘shinyjs’ ‘xgboost’ ‘zip’
      All declared Imports should be used.
    ```

# repr

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/repr
* URL: https://github.com/IRkernel/repr/
* BugReports: https://github.com/IRkernel/repr/issues/
* Date/Publication: 2020-01-28 12:20:03 UTC
* Number of recursive dependencies: 68

Run `revdep_details(,"repr")` for more info

</details>

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'vegalite', 'geojsonio'
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘geojsonio’, ‘vegalite’
    ```

# rgl

<details>

* Version: 0.100.54
* Source code: https://github.com/cran/rgl
* URL: https://r-forge.r-project.org/projects/rgl/
* BugReports: https://r-forge.r-project.org/projects/rgl/
* Date/Publication: 2020-04-14 14:40:02 UTC
* Number of recursive dependencies: 59

Run `revdep_details(,"rgl")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        doc     3.6Mb
        fonts   1.5Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘heplots’
    ```

# rhandsontable

<details>

* Version: 0.3.7
* Source code: https://github.com/cran/rhandsontable
* URL: http://jrowen.github.io/rhandsontable/
* BugReports: https://github.com/jrowen/rhandsontable/issues
* Date/Publication: 2018-11-20 05:50:03 UTC
* Number of recursive dependencies: 32

Run `revdep_details(,"rhandsontable")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.5Mb
      sub-directories of 1Mb or more:
        doc          10.2Mb
        htmlwidgets   1.2Mb
    ```

# rmarkdown

<details>

* Version: 2.1
* Source code: https://github.com/cran/rmarkdown
* URL: https://github.com/rstudio/rmarkdown
* BugReports: https://github.com/rstudio/rmarkdown/issues
* Date/Publication: 2020-01-20 19:30:02 UTC
* Number of recursive dependencies: 71

Run `revdep_details(,"rmarkdown")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.1Mb
      sub-directories of 1Mb or more:
        rmd   8.2Mb
    ```

# rpostgisLT

<details>

* Version: 0.6.0
* Source code: https://github.com/cran/rpostgisLT
* URL: https://github.com/mablab/rpostgisLT
* BugReports: https://github.com/mablab/rpostgisLT/issues
* Date/Publication: 2018-03-02 18:02:11 UTC
* Number of recursive dependencies: 117

Run `revdep_details(,"rpostgisLT")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘htmltools’
      All declared Imports should be used.
    ```

# rwebstat

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/rwebstat
* URL: https://developer.webstat.banque-france.fr,http://webstat.banque-france.fr
* BugReports: https://developer.webstat.banque-france.fr/contact
* Date/Publication: 2020-04-14 13:50:02 UTC
* Number of recursive dependencies: 62

Run `revdep_details(,"rwebstat")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘htmltools’
      All declared Imports should be used.
    ```

# RxODE

<details>

* Version: 0.9.2-0
* Source code: https://github.com/cran/RxODE
* URL: https://nlmixrdevelopment.github.io/RxODE/
* BugReports: https://github.com/nlmixrdevelopment/RxODE/issues
* Date/Publication: 2020-03-13 07:10:14 UTC
* Number of recursive dependencies: 132

Run `revdep_details(,"RxODE")` for more info

</details>

## Newly fixed

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Error building model on another thread.
      Backtrace:
       1. RxODE::RxODE(model = ode, modName = "m1", wd = "/tmp")
       2. .env$compile()
       4. base::with.default(...)
       5. [ base::eval(...) ] with 1 more call
       8. RxODE:::rxCompile.rxModelVars(...)
      
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 890 | SKIPPED: 122 | WARNINGS: 20 | FAILED: 1 ]
      1. Error: Issue #56 (@test-issue-56.R#10) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      'SnakeCharmR', 'installr'
    ```

# sass

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/sass
* URL: https://github.com/rstudio/sass
* BugReports: https://github.com/rstudio/sass/issues
* Date/Publication: 2020-03-18 15:50:09 UTC
* Number of recursive dependencies: 39

Run `revdep_details(,"sass")` for more info

</details>

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# shiny

<details>

* Version: 1.4.0.2
* Source code: https://github.com/cran/shiny
* URL: http://shiny.rstudio.com
* BugReports: https://github.com/rstudio/shiny/issues
* Date/Publication: 2020-03-13 10:00:02 UTC
* Number of recursive dependencies: 68

Run `revdep_details(,"shiny")` for more info

</details>

## Newly broken

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'HTML':
    HTML
      Code: function(text, ..., .noWS = NULL)
      Docs: function(text, ...)
      Argument names in code not in docs:
        .noWS
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.2Mb
      sub-directories of 1Mb or more:
        R     2.0Mb
        www   8.2Mb
    ```

# shiny.semantic

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/shiny.semantic
* BugReports: https://github.com/Appsilon/shiny.semantic/issues
* Date/Publication: 2020-03-13 07:10:02 UTC
* Number of recursive dependencies: 69

Run `revdep_details(,"shiny.semantic")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.2Mb
      sub-directories of 1Mb or more:
        www   8.0Mb
    ```

# shinyaframe

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/shinyaframe
* Date/Publication: 2017-11-26 15:29:43 UTC
* Number of recursive dependencies: 51

Run `revdep_details(,"shinyaframe")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘shiny’
      All declared Imports should be used.
    ```

# shinyBS

<details>

* Version: 0.61
* Source code: https://github.com/cran/shinyBS
* URL: https://ebailey78.github.io/shinyBS
* BugReports: https://github.com/ebailey78/shinyBS/issues
* Date/Publication: 2015-03-31 07:52:38
* Number of recursive dependencies: 17

Run `revdep_details(,"shinyBS")` for more info

</details>

## In both

*   checking R code for possible problems ... NOTE
    ```
    popify: no visible global function definition for ‘runif’
    tipify: no visible global function definition for ‘runif’
    Undefined global functions or variables:
      runif
    Consider adding
      importFrom("stats", "runif")
    to your NAMESPACE file.
    ```

# shinyEffects

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/shinyEffects
* URL: https://github.com/DivadNojnarg/shinyEffects, https://divadnojnarg.github.io/shinyEffects/
* BugReports: https://github.com/DivadNojnarg/shinyEffects/issues
* Date/Publication: 2018-11-18 18:40:03 UTC
* Number of recursive dependencies: 29

Run `revdep_details(,"shinyEffects")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘shinydashboard’
      All declared Imports should be used.
    ```

# shinyjs

<details>

* Version: 1.1
* Source code: https://github.com/cran/shinyjs
* URL: https://deanattali.com/shinyjs
* BugReports: https://github.com/daattali/shinyjs/issues
* Date/Publication: 2020-01-13 06:40:03 UTC
* Number of recursive dependencies: 49

Run `revdep_details(,"shinyjs")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘htmltools’
      All declared Imports should be used.
    ```

# shinyloadtest

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/shinyloadtest
* URL: https://rstudio.github.io/shinyloadtest/, https://github.com/rstudio/shinyloadtest
* BugReports: https://github.com/rstudio/shinyloadtest/issues
* Date/Publication: 2020-01-09 10:20:02 UTC
* Number of recursive dependencies: 92

Run `revdep_details(,"shinyloadtest")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘R6’ ‘getPass’ ‘svglite’ ‘websocket’
      All declared Imports should be used.
    ```

# shinypanels

<details>

* Version: 0.5.0
* Source code: https://github.com/cran/shinypanels
* URL: http://github.com/datasketch/shinypanels
* Date/Publication: 2020-01-26 10:30:08 UTC
* Number of recursive dependencies: 37

Run `revdep_details(,"shinypanels")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘shiny’ ‘shinyjs’
      All declared Imports should be used.
    ```

# sigmajs

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/sigmajs
* URL: http://sigmajs.john-coene.com/
* BugReports: https://github.com/JohnCoene/sigmajs/issues
* Date/Publication: 2019-04-09 11:10:09 UTC
* Number of recursive dependencies: 77

Run `revdep_details(,"sigmajs")` for more info

</details>

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 28 marked UTF-8 strings
    ```

# slickR

<details>

* Version: 0.4.9
* Source code: https://github.com/cran/slickR
* URL: https://github.com/metrumresearchgroup/slickR
* BugReports: https://github.com/metrumresearchgroup/slickR/issues
* Date/Publication: 2020-02-14 13:30:02 UTC
* Number of recursive dependencies: 83

Run `revdep_details(,"slickR")` for more info

</details>

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(slickR)
      > 
      > test_check("slickR")
      ── 1. Failure: slick div method: widget (@test-div_method.R#44)  ───────────────
      {
          ...
      } not equal to readRDS("../assets/slick_div_widget.Rds").
      Component "attribs": Component "srcdoc": 1 string mismatch
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 27 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: slick div method: widget (@test-div_method.R#44) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# slideview

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/slideview
* Date/Publication: 2019-03-06 15:00:03 UTC
* Number of recursive dependencies: 12

Run `revdep_details(,"slideview")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘viridisLite’
      All declared Imports should be used.
    ```

# SmarterPoland

<details>

* Version: 1.7
* Source code: https://github.com/cran/SmarterPoland
* Date/Publication: 2016-03-28 13:59:24
* Number of recursive dependencies: 55

Run `revdep_details(,"SmarterPoland")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.2Mb
      sub-directories of 1Mb or more:
        data   8.1Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1122 marked UTF-8 strings
    ```

# stringr

<details>

* Version: 1.4.0
* Source code: https://github.com/cran/stringr
* URL: http://stringr.tidyverse.org, https://github.com/tidyverse/stringr
* BugReports: https://github.com/tidyverse/stringr/issues
* Date/Publication: 2019-02-10 03:40:03 UTC
* Number of recursive dependencies: 46

Run `revdep_details(,"stringr")` for more info

</details>

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 3 marked UTF-8 strings
    ```

# summarytools

<details>

* Version: 0.9.6
* Source code: https://github.com/cran/summarytools
* URL: https://github.com/dcomtois/summarytools
* BugReports: https://github.com/dcomtois/summarytools/issues
* Date/Publication: 2020-03-02 07:20:02 UTC
* Number of recursive dependencies: 74

Run `revdep_details(,"summarytools")` for more info

</details>

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 78 marked UTF-8 strings
    ```

# svgPanZoom

<details>

* Version: 0.3.4
* Source code: https://github.com/cran/svgPanZoom
* URL: https://github.com/timelyportfolio/svgPanZoom
* BugReports: https://github.com/timelyportfolio/svgPanZoom/issues
* Date/Publication: 2020-02-15 21:20:02 UTC
* Number of recursive dependencies: 11

Run `revdep_details(,"svgPanZoom")` for more info

</details>

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘gridSVG’
    ```

# tableHTML

<details>

* Version: 2.0.0
* Source code: https://github.com/cran/tableHTML
* URL: https://github.com/LyzandeR/tableHTML
* BugReports: https://github.com/LyzandeR/tableHTML/issues
* Date/Publication: 2019-03-16 17:30:02 UTC
* Number of recursive dependencies: 50

Run `revdep_details(,"tableHTML")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        doc   5.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘shiny’
      All declared Imports should be used.
    ```

# tablerDash

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/tablerDash
* URL: https://rinterface.github.io/tablerDash/, https://github.com/RinteRface/tablerDash/
* BugReports: https://github.com/RinteRface/tablerDash/issues
* Date/Publication: 2019-03-08 16:00:03 UTC
* Number of recursive dependencies: 64

Run `revdep_details(,"tablerDash")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

# testextra

<details>

* Version: 0.1.0.1
* Source code: https://github.com/cran/testextra
* URL: https://github.com/RDocTaskForce/testextra
* BugReports: https://github.com/RDocTaskForce/testextra/issues
* Date/Publication: 2019-12-18 09:15:29 UTC
* Number of recursive dependencies: 83

Run `revdep_details(,"testextra")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘stringi’ ‘utils’
      All declared Imports should be used.
    ```

# texPreview

<details>

* Version: 1.4.4
* Source code: https://github.com/cran/texPreview
* URL: https://github.com/metrumresearchgroup/texPreview
* BugReports: https://github.com/metrumresearchgroup/texPreview/issues
* Date/Publication: 2020-02-16 06:10:03 UTC
* Number of recursive dependencies: 89

Run `revdep_details(,"texPreview")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘details’
      All declared Imports should be used.
    ```

# timevis

<details>

* Version: 0.5
* Source code: https://github.com/cran/timevis
* URL: https://github.com/daattali/timevis, http://daattali.com/shiny/timevis-demo/
* BugReports: https://github.com/daattali/timevis/issues
* Date/Publication: 2019-01-16 09:00:03 UTC
* Number of recursive dependencies: 55

Run `revdep_details(,"timevis")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lubridate’
      All declared Imports should be used.
    ```

# tosca

<details>

* Version: 0.2-0
* Source code: https://github.com/cran/tosca
* URL: https://github.com/Docma-TU/tosca, https://doi.org/10.5281/zenodo.3591068
* Date/Publication: 2020-03-10 15:20:02 UTC
* Number of recursive dependencies: 117

Run `revdep_details(,"tosca")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        data   5.3Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1946 marked UTF-8 strings
    ```

# trackViewer

<details>

* Version: 1.24.1
* Source code: https://github.com/cran/trackViewer
* Date/Publication: 2020-05-23
* Number of recursive dependencies: 146

Run `revdep_details(,"trackViewer")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.6Mb
      sub-directories of 1Mb or more:
        doc   7.7Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    plotGInteractions: no visible global function definition for
      'plot.default'
    Undefined global functions or variables:
      plot.default
    Consider adding
      importFrom("graphics", "plot.default")
    to your NAMESPACE file.
    ```

# ursa

<details>

* Version: 3.8.19
* Source code: https://github.com/cran/ursa
* URL: https://github.com/nplatonov/ursa
* BugReports: https://github.com/nplatonov/ursa/issues
* Date/Publication: 2020-03-29 08:00:03 UTC
* Number of recursive dependencies: 124

Run `revdep_details(,"ursa")` for more info

</details>

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘rje’, ‘magick’
    ```

# VarSelLCM

<details>

* Version: 2.1.3
* Source code: https://github.com/cran/VarSelLCM
* URL: http://varsellcm.r-forge.r-project.org/
* Date/Publication: 2018-08-29 10:24:28 UTC
* Number of recursive dependencies: 73

Run `revdep_details(,"VarSelLCM")` for more info

</details>

## In both

*   checking line endings in C/C++/Fortran sources/headers ... NOTE
    ```
    ...
      inst/include/AlgorithmCategorical.h
      inst/include/AlgorithmContinuous.h
      inst/include/AlgorithmInteger.h
      inst/include/AlgorithmMixed.h
      inst/include/Data.h
      inst/include/DataCategorical.h
      inst/include/DataContinuous.h
      inst/include/DataInteger.h
      inst/include/DataMixed.h
      inst/include/Param.h
      inst/include/ParamCategorical.h
      inst/include/ParamContinuous.h
      inst/include/ParamInteger.h
      inst/include/ParamMixed.h
      inst/include/XEM.h
      inst/include/XEMCategorical.h
      inst/include/XEMContinuous.h
      inst/include/XEMInteger.h
      inst/include/XEMMixed.h
      inst/include/XEMPen.h
    Some compilers warn on such files.
    ```

# vdiffr

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/vdiffr
* URL: https://github.com/r-lib/vdiffr
* BugReports: https://github.com/r-lib/vdiffr/issues
* Date/Publication: 2019-06-24 16:30:03 UTC
* Number of recursive dependencies: 102

Run `revdep_details(,"vdiffr")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘freetypeharfbuzz’
      All declared Imports should be used.
    ```

# ViSEAGO

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/ViSEAGO
* URL: https://www.bioconductor.org/packages/release/bioc/html/ViSEAGO.html, https://forgemia.inra.fr/UMR-BOA/ViSEAGO
* BugReports: https://forgemia.inra.fr/UMR-BOA/ViSEAGO/issues
* Date/Publication: 2020-04-27
* Number of recursive dependencies: 148

Run `revdep_details(,"ViSEAGO")` for more info

</details>

## In both

*   checking dependencies in R code ... WARNING
    ```
    'library' or 'require' call not declared from: ‘topGO’
    'library' or 'require' call to ‘topGO’ in package code.
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        doc       3.9Mb
        extdata   2.0Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      variable ‘ENTREZID’
    annotate,character-genomic_ressource: no visible binding for global
      variable ‘GO’
    annotate,character-genomic_ressource: no visible binding for global
      variable ‘EVIDENCE’
    annotate,character-genomic_ressource: no visible binding for global
      variable ‘ONTOLOGY’
    compute_SS_distances,ANY-character: no visible binding for global
      variable ‘N’
    compute_SS_distances,ANY-character: no visible binding for global
      variable ‘IC’
    merge_enrich_terms,list : <anonymous> : esummary : <anonymous>: no
      visible binding for global variable ‘start’
    merge_enrich_terms,list : <anonymous> : esummary : <anonymous>: no
      visible binding for global variable ‘end’
    Undefined global functions or variables:
      . ENTREZID EVIDENCE GO GO.ID IC N ONTOLOGY end start text
    Consider adding
      importFrom("graphics", "text")
      importFrom("stats", "end", "start")
    to your NAMESPACE file.
    ```

# visNetwork

<details>

* Version: 2.0.9
* Source code: https://github.com/cran/visNetwork
* URL: http://datastorm-open.github.io/visNetwork/
* BugReports: https://github.com/datastorm-open/visNetwork/issues
* Date/Publication: 2019-12-06 08:50:02 UTC
* Number of recursive dependencies: 93

Run `revdep_details(,"visNetwork")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.1Mb
      sub-directories of 1Mb or more:
        doc           4.3Mb
        docjs         1.4Mb
        htmlwidgets   3.9Mb
    ```

# weathercan

<details>

* Version: 0.3.4
* Source code: https://github.com/cran/weathercan
* URL: https://docs.ropensci.org/weathercan, https://github.com/ropensci/weathercan
* BugReports: https://github.com/ropensci/weathercan/issues
* Date/Publication: 2020-04-17 12:10:02 UTC
* Number of recursive dependencies: 129

Run `revdep_details(,"weathercan")` for more info

</details>

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 72 marked UTF-8 strings
    ```

# XKCDdata

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/XKCDdata
* Date/Publication: 2017-10-11 12:07:59 UTC
* Number of recursive dependencies: 44

Run `revdep_details(,"XKCDdata")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

