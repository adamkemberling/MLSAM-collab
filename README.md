
<img src='logo.png' align="right" height="44" />

# ML SAM - Machine Learning Stock Assesment Model

This is the formal documentation for the data and code associated with
the NSF accelerator project.

## General workflow

Proper coordination between contributors is necessary to ensure a stable
code base. To ensure that all major modeling files and supporting
functions are up to date it is important to follow the agreed upon
workflow.

That workflow is as follows:

1.  The `gulfofmaine/MLSAM-collab` repository will be maintained as the
    “gold-standard” repository. The most up-to-date features can be
    found there.  
2.  Contributors should start by setting up their own working branches
    by forking, then cloning copies of the `gulfofmaine/MLSAM-collab`
    repository  
3.  It is then recommended contributors create a “working branch” from
    which to test new features and code changes. This way, any new
    updates from the `gulfofmaine/MLSAM-collab` master branch can be
    pulled safely to personal repositories as needed  
4.  The `gulfofmaine/MLSAM-collab` repo can then be set as the upstream
    remote by typing `git add remote upstream
    https://github.com/gulfofmaine/MLSAM-collab.git` from the terminal
    when working locally in the forked repositories  
5.  When features are complete, or when changes to the upstream
    repository are necessary, they will be added using a pull-request  
6.  New updates can then be pulled to individual forks using `git pull
    upstream master` in the
terminal

## ML Stock Assesment Model Pipeline

<img src="README_files/figure-gfm/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />
