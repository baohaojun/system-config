#!/bin/bash
#
# Test the import code
#

source $REG_DIR/scaffold

cmd setup_repo

cmd touch foo foo:baz

# invalid character
shouldfail guilt import -P foo:bar foo

# non-existant file & invalid character
shouldfail guilt import -P foo:bar foo2

# non-existant file
shouldfail guilt import -P foo foo2

# ok
cmd guilt import -P foo3 foo

# duplicate patch name
shouldfail guilt import -P foo3 foo

# ok
cmd guilt import -P foo2 foo

# ok
shouldfail guilt import foo

# duplicate patch name (implicit)
shouldfail guilt import foo

# check that bug 47 doesn't come back
cmd guilt import -P foo,bar foo

# implicitly bad patch name - invalid char
shouldfail guilt import foo:baz
