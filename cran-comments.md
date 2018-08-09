
A small update to address problems with a too large number of file handles on solaris as reported by Prof. Ripley. A more solid fix will have to come from the lvec package which is responsible for opening the files. I am working on that. I checked the package locally with the maximum number of file handles of processes set to 200 (ulimit -n 200) and the packaged checked fine. 


## Test environments
* local ubuntu 18.04 install, R 3.4.4
* local ubuntu 18.04 install, R 3.4.4 - with valgrin
* R-devel and R-release on windows using the R-builder


## R CMD check results

0 errors | 0 warnings | 0 notes


## Reverse dependencies

None

