# DavyFeyn
_DavyFeyn_ is a _Mathematica_ package for automated analytical Feynman diagram computations. The whole project is composed by three packages: 

 * _ElementaryTensorCalculus_
 * _GammaMatricesAlgebra_
 * _DavyFeyn_

Check the tutorials to fully understand how to use these packages.

## ElementaryTensorCalculus 
Defines the notions of a metric tensor, vectors, the Levi-Civita symbol, as well as a notion of derivative with respect to vectors. This package is inspired on notebooks found at the webpage of the [MSSTP](http://msstp.org).

## GammaMatricesAlgebra
Is a package to compute the trace of gamma matrices in arbitrary dimensions. This package uses the package _ElementaryTensorCalculus_.

## DavyFeyn
_DavyFeyn_ also depends on the notions defined in _ElementaryTensorCalculus_. The fundamental idea of the package is that, given the numerator of a Feynman integral, there is a function that returns the full result of the integral.  
The package _GammaMatricesAlgebra_ is useful generate the numerator of Feynman integrals in the case of theories with fermions.

### Tensor reduction
To perform the tensorial reduction of of Feynman integrals, the approach that A. I. Davydychev proposes in the paper [A Simple formula for reducing Feynman diagrams to scalar integrals](http://inspirehep.net/record/316371) is implemented.

### Scalar integrals

#### Massive case
The package current supports only computations of 2-point functions in the massive case with only one mass parameter.

#### Massless case
The computation of the scalar integrals that appears in 3-point functions is done by using their triple-K representation. The ideas and methods presented in [Evaluation of conformal integrals](http://inspirehep.net/record/1403571) are implemented.

## Installation
The quickest way to start using these packages is to create a new Mathematica notebook in the same folder in which you have saved all the files of _DavyFeyn_. In this case, start your notebook with:

```
SetDirectory[NotebookDirectory[]];  
<<DavyFeyn`
```

By loading _DavyFeyn_, _ElementaryTensorCalculus_ will be also loaded. If you only want to use the package _ElemetaryTensorCalculs_ you can simply type:

```
SetDirectory[NotebookDirectory[]];  
<<ElementaryTensorCalculus`
```

If you also need to compute traces of gamma matrices, you will have to load the package _GammaMatricesAlgebra_ separately:

```
<<GammaMatricesAlgebra`
```

Another possibility is to save _DavyFeyn_ together with your _Mathematica_ installation. To discover where you have to put the files you can run the command  

```
$UserBaseDirectory
```

on a Mathematica notebook. The result will be the path of your local installation. You should put the folders of _DavyFeyn_ in the folder **Applications**.