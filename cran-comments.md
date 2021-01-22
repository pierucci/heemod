## Test environments
* local ubuntu 20.10 install, R 4.0.3
* win-builder (devel, release and old-release)
* r-hub (windows-server, Fedora Linux R-devel, clang, gfortran)

## R CMD check results

There were no ERRORs or WARNINGs. 1 NOTE: Possibly mis-spelled words in DESCRIPTION:
  Siebert (17:5)
  al (15:62, 17:20)
  et (15:59, 17:17)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2021-01-21 as check problems were not corrected in time.
  
Dear Uwe, the m1MAC issue resolved; I'm trying to make the vignettes as least computing-intensive as possible
to reduce the overall check time, but I think that I need to learn C.
    

    