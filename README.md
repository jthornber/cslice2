# Introduction

cslice is a C to C compiler.  It's main use case is cutting slices out
of C programs to enable you to unit test small portions of legacy projects.

# Status

The code is still under initial development and is not ready for anyone
to use yet.

# Architecture

 

# Building

cslise is written in Haskell.  It uses the *stack* tool to manage installing
the compiler tools, and building.

To install stack run:

    curl -sSL https://get.haskellstack.org/ | sh
    
Once *stack* in installed you can build cslice with:

    stack build
    
This will take a long time the first time you run it.

You can install with:

    stack install

By default this will install it for just the local user.  See
'''stack install --help''' for more information.
