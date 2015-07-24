#!/bin/bash

mkdir deps
cd deps
git clone https://github.com/Drezil/yesod-auth-oauth2
cd ..
cabal sandbox init
cabal sandbox add-source deps/yesod-auth-oauth2
cabal install --only-dependencies --reorder-goals
