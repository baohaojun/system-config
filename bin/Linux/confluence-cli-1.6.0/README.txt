Command line interface clients for Atlassian products

This is a command line interface (CLI) for remotely accessing Atlassian products.
This is a combination of product specific clients that can be used directly with your installation.
It uses product specific SOAP and/or REST remote APIs. It also serves as an example for writing a
Java SOAP and/or REST client

Installation
- Unzip the distribution package and put the enclosed directory in a convenient location

Java Requirements
- Requires a JRE version 1.5 or higher on the client
- Run java -version from a command line
  - ensure it shows 1.5 or higher

Usage - where xxxxxxxx is a particular Atlassian product (confluence, jira, ...)
- On a command line, cd to the directory where you installed the client
- On Windows
-- Run xxxxxxxx
- On Linux, Mac, or Unix
-- Run ./xxxxxxxx.sh
- On any system supporting Java
-- Run java -jar release/xxxxxxxx-cli-x.x.x.jar
- This will show help text for the command line interface client
- The client defaults to use a user of automation. Either add this user with all the authorities required to do the actions you want or specify a different user parameter
- It is recommended that you open the confluene.bat or xxxxxxxx.sh file with an editor and customize it for your environment by adding server, user, and password parameters.
  Follow the example in the comments and make sure you do not remove the %* at the end of the line.

License
- The software provided for this tool has a BSD style license
- The distribution ships binaries with various licenses (BSD, LGPL, and Apache)
- Look in the license directory for detailed license information