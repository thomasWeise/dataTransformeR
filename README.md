# R Package for Normalizing and Transforming Numerical Data

[<img alt="Travis CI Build Status" src="https://img.shields.io/travis/thomasWeise/dataTransformeR/master.svg" height="20"/>](https://travis-ci.org/thomasWeise/dataTransformeR/)

## Introduction

When we fit models to data, we often do not want to use the raw data as-is.
Instead, we usually want to fit models to normalized or log-scaled versions of the
data. If all data elements are in `[0,1]`, this makes it easier to pick
initial parameter values for models. If there are exponential relationships present
in data sets, we may want to get rid of them by log-scaling the data.
This means that, after the models have been fitted, we need to transform the
model back by applying the inverse of the data transformation to the model.

This package uses our [functionComposeR](http://www.github.com/thomasWeise/functionComposeR) package to construct and apply such bijective transformations. The core of this
package are the `Transformation` and `TransformedData` S4 classes and the routines
to construct instances of them.

`Transformation` allows us to specify a bijection, i.e., `foward` and `backward`
function for which `backward(forward(x)) = x` holds (at least within the prescribed
domain). Such transformations can be constructed and composed with this package.
For instance, `Transformation.mapIntervals(a, b, c, d)` creates a `Transformation`
whose `forward` function maps the elements from interval `[a, b]` to interval
`[c, d]` (whereas the `backward` function maps `[c, d]` back to `[a, b]`).
`Transformation.andThen1(first, after)` creates a new `Transformation` whose
`forward` function corresponds to `after@forward(before@forward(x))` and whose
`backward` function corresponds to `before@backward(after@backward(x))`.

Instances of `TransformedData` hold a transformed data vector along with the
`Transformation` that was used to create it. Several functions are defined
which create such transformed dataset. For example, `Transformation.normalize(d)`
creates a `TransformedData` where the `data` vector is a version of `d` normalized
into `[0,1]`. `Transformation.log(d)`, on the other hand, will first log-scale all
elements of `d` and then normalize the result. If some elements in `d` are less or
equal to zero, `d` is shifted into the positive domain first, by using the
`Transformation` created by `Transformation.makePositive(d)`.

Using `Transformation.applyAll(data, transformations)` you can apply a list
`transformations` of transformations to a `data` set and obtain a list of
resulting `TransformedData` instances. This list is automatically pruned to
not contain two identical transformed data vectors.
`Transformation.applyDefault(data)` will apply a set of default transformations,
including plain normalization to `[0,1]`, the negated normalization (the smallest
data value maps to `1`, the largest to `0`), a normalized logarithmic scaling, and
a negated-normalized logarithmic scaling. Both `Transformation.applyAll` and
`Transformation.applyDefault` can add a `TransformedData` instance corresponding
to the orginal data (using the identity `Transformation`) and do so by default.

The goal is to provide a toolbox which will allow you to automatically obtain
normalized and scaled versions of data vectors along with functions to convert
back and forth between the original and transformed data representation.

A transformation is furthermore accompanied by a positive `complexity`.
Only `Transformation.identity` has complexity `0L`. All other
transformations should have a larger complexity. If we simply log-scale some
data (e.g., via `Transformation.log`) by just applying the
`log` function, this could have complexity `1L`. If we have a
transformation involving `n` variables whose values we decide upon,
then we should pick complexity `n+1L`: If we first move the data by
3 units and then divide it by 2, i.e., apply something like `(x+3)/`},
this transformation should have a complexity of `3` - we chose two
values and applied them in a function.

## Motivating Example

Assume that you have the data vector

    data <- c(-1, 0, 2, 6, 14, 30)

You can obtain a log-scaled and normalized version of this data by doing
    
    Transformation.log(data)
    # An object of class "TransformedData"
    # Slot "transformation":
    # An object of class "Transformation"
    # Slot "forward":
    # function (x)
    # log(x + 2) * 0.288539008177793
    # <environment: 0x3b5cc18>
    # 
    # Slot "complexity":
    # [1] 5
    #
    # Slot "backward":
    # function (x)
    # exp(x = x * 3.46573590279973) - 2
    # <environment: 0x3c19460>
    #
    # Slot "data":
    # [1] 0.0 0.2 0.4 0.6 0.8 1.0

The resulting `data` vector shown above has a lot of nice properties.
First, you know that all elements are in `[0, 1]`, which will help when looking for
initial values when fitting models. Second, in this example, the data became beautifully
linear. If you were fitting a linear model to this, you can then translate this model
back into the original data space easily using the `backward` and `forward` functions.
Furthermore, we also get some measure of the complexity that the log scaling has
involved. We may then decide to apply a model fitting approach to both the raw and
the transformed data. When deciding which of the two resulting models to choose, we
may not just consider the model performance, but also that the model on the transformed
data is actually more complex than the one of the raw data.

By the way, did you notice the beautiful readble bodies of the transformation functions? They do not contain any unresolved variables or nested, opaque functions (apart from the system functions log and exp). They are constructed with the support of our [functionComposeR](http://www.github.com/thomasWeise/functionComposeR) package. 
    
## Installation

You can install the package directl from GitHub by using the package
[`devtools`](http://cran.r-project.org/web/packages/devtools/index.html) as
follows:

    library(devtools)
    install_github("thomasWeise/dataTransformeR")

If `devtools` is not yet installed on your machine, you need to FIRST do

    install.packages("devtools")
    
## License

The copyright holder of this package is Prof. Dr. Thomas Weise (see Contact).
The package is licensed under the  GNU LESSER GENERAL PUBLIC LICENSE Version 3, 29 June 2007.
    
## Contact

For more information, see our corresponding[blog post](http://iao.hfuu.edu.cn/blogs/programming-blog/133).

If you have any questions or suggestions, please contact
[Prof. Dr. Thomas Weise](http://iao.hfuu.edu.cn/team/director) of the
[Institute of Applied Optimization](http://iao.hfuu.edu.cn/) at
[Hefei University](http://www.hfuu.edu.cn) in
Hefei, Anhui, China via
email to [tweise@hfuu.edu.cn](mailto:tweise@hfuu.edu.cn).
