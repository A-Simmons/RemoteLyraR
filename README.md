RemoteLyraR
[![Build Status](https://travis-ci.org/A-Simmons/RemoteLyraR.svg?branch=master)](https://travis-ci.org/A-Simmons/RemoteLyraR.svg?branch=master)

### This package is still in alpha and feature-incomplete 

Fully fledged package to handle the pipeline of submitting jobs to Lyra from a local machine on QUT's intranet (either on campus or with VPN from a remote device).

Handles the following:
  1. Lexical search of your project identifying:
    1. Local scripts and data files that need to be transferred to the HPC file server
    2. Libraries that need to be installed (Will download and install into a personal library on the HPC-filesver)
    3. Modules that need to be loaded (such as jags and gdal)
  2. Batch processing of multiple jobs using a sapply, lapply and mapply approach.
  3. Allow user to get updates on progress and transfer results when the jobs complete.
  4. Works on Unix natively and Windows through the installation of Cygwin (which can be completely within R)
