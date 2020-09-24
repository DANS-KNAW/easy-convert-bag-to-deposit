easy-vault-export-ip
===========
[![Build Status](https://travis-ci.org/DANS-KNAW/easy-vault-export-ip.png?branch=master)](https://travis-ci.org/DANS-KNAW/easy-vault-export-ip)

<!-- Remove this comment and extend the descriptions below -->


SYNOPSIS
--------

    easy-vault-export-ip { -u <id> | -i <input-file> } -o <staged-IP-dir> -t [ URN | DOI ] [-l <log-file>]


DESCRIPTION
-----------

Export bags from the Vault as information packages


ARGUMENTS
---------

    Options:

       -t, --dataverse-identifier-type  <arg>   the field to be used as Dataverse identifier, either doi or urn:nbn
         -i, --input-file  <arg>                  File containing a newline-separated list of ids of the bags to be
                                                  exported
         -l, --log-file  <arg>                    The name of the logfile in csv format. If not provided a file
                                                  easy-vault-export-ip-<timestamp>.csv will be created in the
                                                  home-dir of the user.
                                                  (default = /Users/jokep/easy-vault-export-ip-2020-02-02T20:20:02.000Z.csv)
         -o, --output-dir  <arg>                  Empty directory in which to stage the created IPs.
         -u, --UUID  <arg>                        the id of the bag to be exported
         -h, --help                               Show help message
         -v, --version                            Show version of this program
    ---

EXAMPLES
--------

    easy-vault-export-ip -o value

INSTALLATION AND CONFIGURATION
------------------------------
Currently this project is built as an RPM package for RHEL7/CentOS7 and later. The RPM will install the binaries to
`/opt/dans.knaw.nl/easy-vault-export-ip` and the configuration files to `/etc/opt/dans.knaw.nl/easy-vault-export-ip`. 

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
    
    git clone https://github.com/DANS-KNAW/easy-vault-export-ip.git
    cd easy-vault-export-ip 
    mvn clean install

If the `rpm` executable is found at `/usr/local/bin/rpm`, the build profile that includes the RPM 
packaging will be activated. If `rpm` is available, but at a different path, then activate it by using
Maven's `-P` switch: `mvn -Pprm install`.

Alternatively, to build the tarball execute:

    mvn clean install assembly:single
