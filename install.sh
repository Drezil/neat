#!/bin/bash

mkdir deps
cd deps
git clone https://github.com/Drezil/yesod-auth-oauth2
git clone https://github.com/Drezil/eve-api
cd ..
#cabal install --only-dependencies --reorder-goals
