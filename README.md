# nntopr: Towards a mathematical framework to inform Neural Network modelling via Polynomial Regression
This repository contains all the code needed to implement the formulas, the simulations and the figures used in the following paper: 

* **Title**: "Towards a mathematical framework to inform Neural Network modelling via Polynomial Regression"
* **Authors :** Pablo Morala, Jenny Alexandra Cifuentes, Rosa E. Lillo, Iñaki Ucar
* **Institutions**: uc3m-Santander Big Data Institute, Universidad Carlos III de Madrid and Department of Statistics, Universidad Carlos III de Madrid.
* **Date:** February 2021
* **Keywords**: Polynomial Regression, Neural Networks, Machine Learning.
* **arXiv**: https://arxiv.org/abs/2102.03865

### Abstract
Even when neural networks are widely used in a large number of applications, they are still considered as black boxes and present some difficulties for dimensioning or evaluating their prediction error. This has led to an increasing interest in the overlapping area between neural networks and more traditional statistical methods, which can help overcome those problems. In this article, a mathematical framework relating neural networks and polynomial regression is explored by building an explicit expression for the coefficients of a polynomial regression from the weights of a given neural network, using a Taylor expansion approach. This is achieved for single hidden layer neural networks in regression problems. The validity of the proposed method depends on different factors like the distribution of the synaptic potentials or the chosen activation function. The performance of this method is empirically tested via simulation of synthetic data generated from polynomials to train neural networks with different structures and hyperparameters, showing that almost identical predictions can be obtained when certain conditions are met. Lastly, when learning from polynomial generated data, the proposed method produces polynomials that approximate correctly the data locally.

### License
This project is licensed under the terms of the MIT license. See the [LICENSE](LICENSE) file for license rights and limitations.
