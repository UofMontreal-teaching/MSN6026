IntrotoBayes_binomial_example
================
Jérôme Lavoué
October 24, 2021

binomial example, the false die - figure 1

![](IntrotoBayes_binomial_example_files/figure-gfm/binomial1%20-1.png)<!-- -->

binomial example, the false die - figure 2

![](IntrotoBayes_binomial_example_files/figure-gfm/binomial2%20-1.png)<!-- -->

binomial example, the false die - figure 3

![](IntrotoBayes_binomial_example_files/figure-gfm/binomial3%20-1.png)<!-- -->

binomial example, hygienist height - figure 1 ( prior ) application du
modèle conjugué beta-binomial

Information à priori : proba moyenne 0.3, sd = 0.1

![](IntrotoBayes_binomial_example_files/figure-gfm/binomial4%20-1.png)<!-- -->

vraisemblance : 11 succès sur 27 tirages

![](IntrotoBayes_binomial_example_files/figure-gfm/binomial5%20-1.png)<!-- -->

Information à posteriori

![](IntrotoBayes_binomial_example_files/figure-gfm/binomial6%20-1.png)<!-- -->

vérification numérique entre le calcul empirique et théorique

|   x |   y.prior |     y.lik | y.post.theo | y.post.theo.s | y.post.emp |
|----:|----------:|----------:|------------:|--------------:|-----------:|
| 0.0 | 0.0000000 | 0.0000000 |   0.0000000 |     0.0000000 |  0.0000000 |
| 0.1 | 0.9073787 | 0.0000242 |   0.0003293 |     0.0000329 |  0.0000329 |
| 0.2 | 2.8069397 | 0.0075158 |   0.3169127 |     0.0316907 |  0.0316907 |
| 0.3 | 3.1354429 | 0.0767556 |   3.6152488 |     0.3615180 |  0.3615180 |
| 0.4 | 2.0458788 | 0.1542721 |   4.7412916 |     0.4741201 |  0.4741201 |
| 0.5 | 0.8613462 | 0.0971399 |   1.2569116 |     0.1256888 |  0.1256888 |
| 0.6 | 0.2260047 | 0.0203157 |   0.0689728 |     0.0068971 |  0.0068971 |
| 0.7 | 0.0314021 | 0.0011098 |   0.0005235 |     0.0000523 |  0.0000523 |
| 0.8 | 0.0015033 | 0.0000073 |   0.0000002 |     0.0000000 |  0.0000000 |
| 0.9 | 0.0000059 | 0.0000000 |   0.0000000 |     0.0000000 |  0.0000000 |
| 1.0 | 0.0000000 | 0.0000000 |   0.0000000 |     0.0000000 |  0.0000000 |
