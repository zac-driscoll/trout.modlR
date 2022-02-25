## trout.modlR

<hr style ="width:75%" align="left">

<img src='man/figures/mmsd.wq_hex.png' width="120" height="140" 
style="border: none" align="right"/>

  
  

<b>trout.modlR</b> is a custom R package that can be used to query,
transform, analyze,and visualize MMSDs water quality data. The package
is authored, maintained, and utilized by Freshwater Resource Monitoring
Staff. See <a href="https://mmsdgit.github.io/mmsd.wq/"> package
documentation</a> for full details on usage.

## Installation and updating

The easiest way install the package is via its Github repo using the
devtools R Package.

    install.packages(devtools)
    devtools::install_github("MMSDGIT/mmsd.wq")

Once the package is installed it can be updated using an internal
package function. This will pull and install the most up to date package
version from the Github repo.

    library(mmsd.wq)
    update_mmsd.wq()
