easy-vault-export-ip
===========
[![Build Status](https://travis-ci.org/DANS-KNAW/easy-vault-export-ip.png?branch=master)](https://travis-ci.org/DANS-KNAW/easy-vault-export-ip)

<!-- Remove this comment and extend the descriptions below -->


SYNOPSIS
--------

    easy-vault-export-ip (synopsis of command line parameters)
    easy-vault-export-ip (... possibly multiple lines for subcommands)


DESCRIPTION
-----------

Export bags from the Vault as information packages


ARGUMENTS
---------

    Options:

       -h, --help      Show help message
       -v, --version   Show version of this program

    Subcommand: run-service - Starts EASY Vault Export Ip as a daemon that services HTTP requests
       -h, --help   Show help message
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
