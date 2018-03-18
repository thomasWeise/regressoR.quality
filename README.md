# R Package Providing Quality Metrics for Input/Output Regression

[<img alt="Travis CI Build Status" src="https://img.shields.io/travis/thomasWeise/regressoR.quality/master.svg" height="20"/>](https://travis-ci.org/thomasWeise/regressoR.quality/)

## Introduction
This package provides quality metrics to measure the error of regression models that map an input variable `x` to an output variable `y`. The goal is to encapsulate primitives for such metrics into a separate package so that they can easily be extended and improved in the near future.

Each quality measure is an instance of S4 class `RegressionQualityMetric`, which provides several fields, including a function for computing the metric, the original `x` and `y` data, and optionally functions for computing the Jacobian, the residuals, and a weight vector. Such instances are created by functions which accept two vectors `x`
and `y` as parameter and return, well, an instance of `RegressionQualityMetric`.

The way the quality metrics are structured allows for linear and non-linear regression as well as for using numerical optimization methods to fit parametric models.

Currently, we only support some simple weighted root-mean-square-error based measures, where the default one weights samples based on their inverse square root of the absolute value (in order to accommodate values differing vastly in scale) with fixes for zero values. However, in the future, the creation of the quality metrics could involve outlier detection and decrease their weights accordingly. This would improve the robustness of the regression procedure.
    
## Installation

You can install the package directl from GitHub by using the package
[`devtools`](http://cran.r-project.org/web/packages/devtools/index.html) as
follows:

    library(devtools)
    install_github("thomasWeise/regressoR.quality")

If `devtools` is not yet installed on your machine, you need to FIRST do

    install.packages("devtools")
    
## License

The copyright holder of this package is Prof. Dr. Thomas Weise (see Contact).
The package is licensed under the  GNU LESSER GENERAL PUBLIC LICENSE Version 3, 29 June 2007.
    
## Contact

If you have any questions or suggestions, please contact
[Prof. Dr. Thomas Weise](http://iao.hfuu.edu.cn/team/director) of the
[Institute of Applied Optimization](http://iao.hfuu.edu.cn/) at
[Hefei University](http://www.hfuu.edu.cn) in
Hefei, Anhui, China via
email to [tweise@hfuu.edu.cn](mailto:tweise@hfuu.edu.cn).
