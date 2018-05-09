## Submission details

This is a first submission of the package.

## Test environments
* Ubuntu 16.04 LTS (local install), R 3.4.4
* macOS 10.11 El Capitan (64-bit) (on R-hub), R 3.5.0 (2018-04-23)
* Windows Server 2008 R2 SP1 (32/64 bit) (on R-hub), R-devel (2018-05-05 r74699)
* win-builder, R Under development (unstable) (2018-05-05 r74699)
* Debian Linux (on R-hub), R-devel (2018-05-05 r74699), GCC

## R CMD check results

0 errors | 0 warnings | 1 note

* Note is for the new submission.

---

On some platforms on R-hub there was WARNING:

* checking top-level files ... WARNING
Conversion of ‘README.md’ failed:
pandoc: Could not fetch https://travis-ci.org/echasnovski/comperes.svg?branch=master

  This seems like pandoc issue on particular platforms. Local Ubuntu check doesn't have that.

---

On some platforms on R-hub there was a NOTE:

* Found the following (possibly) invalid URLs:
     URL: https://CRAN.R-project.org/package=rlang
       From: man/comperes-package.Rd
       Status: Error
       Message: libcurl error code 7:
         	Failed to connect to CRAN.R-project.org port 443: Timed out

  This link seems to be valid, just opens slowly.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.
