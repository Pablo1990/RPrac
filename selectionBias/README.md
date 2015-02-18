Selection bias problem
========

We create two main programs, in which simulate the selection bias problem with the random forest algorithm. In the mainGood we do it in a good way, the whole pipiline would be like this:

1. Create dataset
2. Generate k-folding index
3. Divide in train and test
4. <b>Feature selection</b>
5. Random forest with training data
6. Predict with testing data
7. Visualize data

And the second one, we do it in a bad way, doing the feature selection before the kfolding:

1. Create dataset
2. <b>Feature selection</b>
3. Generate k-folding index
4. Divide in train and test
5. Random forest with training data
6. Predict with testing data
7. Visualize data

========

Authors: Claudia Buhigas, Pablo Vicente and Jose Alejandro Romero

MSc Bioinformatics 2014-2015 - R & statistics exercise

Presentation - https://docs.zoho.com/show/publish/pnmm5185d5e8a3ce8459e9903c0954bdfb2d9
