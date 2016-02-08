# Clustering by compression

Thanks Davy Landman for the parallel ncd script. [ncd](https://github.com/DavyLandman).

## Steps to clustering data files

Put all your data items in one folder, called `my_example` below. For examples of data that can be clustered, see the folder `data`.

Create a distance matrix with:

    python ncd/ncds.py data/my_example/* > distance_matrices/my_example.csv

In `lib/clustering.R`, set this line:

    distances <- "distance_matrices/my_example.csv" 

And this one:

    pdf("plots/my_example.pdf")

And run the script with:

    Rscript lib/clustering.R
