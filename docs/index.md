easy-convert-bag-to-deposit
===========
[![Build Status](https://travis-ci.org/DANS-KNAW/easy-convert-bag-to-deposit.png?branch=master)](https://travis-ci.org/DANS-KNAW/easy-convert-bag-to-deposit)

<!-- Remove this comment and extend the descriptions below -->


SYNOPSIS
--------

    easy-convert-bag-to-deposit { -sip | -sips } <directory> -t { URN | DOI } [ -o <staged-IP-dir> ]

DESCRIPTION
-----------

Add deposit.properties to directories(s) with a bag


ARGUMENTS
---------

    Options:

         -t, --dataverse-identifier-type  <arg>   the field to be used as Dataverse identifier, either doi or urn:nbn
         -o, --output-dir  <arg>                  Empty directory that will receive completed SIPs with atomic moves.
                                                  It will be created if it does not exist.
             --sip  <arg>                         A directory containing nothing but a bag
             --sips  <arg>                        A directory with directories containing nothing but a bag
         -h, --help                               Show help message
         -v, --version                            Show version of this program
    ---

EXAMPLES
--------

    easy-bag-store -d 04e638eb-3af1-44fb-985d-36af12fccb2d 04e638eb-3af1-44fb-985d-36af12fccb2d
    easy-convert-bag-to-deposit --sip 04e638eb-3af1-44fb-985d-36af12fccb2d -t DOI

    easy-bag-store -d xyz/04e638eb-3af1-44fb-985d-36af12fccb2d 04e638eb-3af1-44fb-985d-36af12fccb2d
    easy-bag-store -d xyz/b55abcfa-ec6b-4290-af6b-e93f35aefd20 b55abcfa-ec6b-4290-af6b-e93f35aefd20
    easy-convert-bag-to-deposit --sips xyz -t DOI

INSTALLATION AND CONFIGURATION
------------------------------
Currently this project is built as an RPM package for RHEL7/CentOS7 and later. The RPM will install the binaries to
`/opt/dans.knaw.nl/easy-convert-bag-to-deposit` and the configuration files to `/etc/opt/dans.knaw.nl/easy-convert-bag-to-deposit`. 

To install the module on systems that do not support RPM, you can copy and unarchive the tarball to the target host.
You will have to take care of placing the files in the correct locations for your system yourself. For instructions
on building the tarball, see next section.

BUILDING FROM SOURCE
--------------------
Prerequisites:

* Java 8 or higher
* Maven 3.3.3 or higher
* RPM

Steps:
    
    git clone https://github.com/DANS-KNAW/easy-convert-bag-to-deposit.git
    cd easy-convert-bag-to-deposit 
    mvn clean install

If the `rpm` executable is found at `/usr/local/bin/rpm`, the build profile that includes the RPM 
packaging will be activated. If `rpm` is available, but at a different path, then activate it by using
Maven's `-P` switch: `mvn -Pprm install`.

Alternatively, to build the tarball execute:

    mvn clean install assembly:single
