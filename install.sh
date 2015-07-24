#!/bin/bash

mkdir deps
cd deps
git clone https://github.com/Drezil/yesod-auth-oauth2
git clone https://github.com/Drezil/eve-api
cd ..
cabal sandbox init
cabal sandbox add-source deps/yesod-auth-oauth2
cabal sandbox add-source deps/eve-api
cabal install --only-dependencies
