## Test environments
* local ubuntu 18.04 install, R 3.5.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

In this version, I corrected the problems identified by CRAN, that I was not 
able to reproduce. I think it was due to permission problems during the tests 
(now the files are created in the temporary directory).
