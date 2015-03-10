# New Eden Accounting Tool

This is a complete rewrite of the New Eden Accounting Tool currently found at [http://pwning.de/neat]().

At the moment there is nothing much to see here but a bit of playing around with yesod. Pull-Requests are welcome anyway.

## Getting stuff to run

1. Install Haskell like [http://www.stackage.org/Install]()
2. Go into a folder and do
    
    ```
    wget http://www.stackage.org/lts/cabal.config
    sudo apt-get install -y build-essential zlib1g-dev
    cabal update                       # download package list
    cabal install alex happy yesod-bin # install build tools
    ```

3. clone this repository into the same folder
4. install postgres (i use v. 9.3)

    ```
    sudo apt-get install -y postgresql-9.3 libpg-dev
    ```

5. create user and database inside a psql-shell as user postgres:

    ```
    CREATE ROLE neat;
    ALTER ROLE neat LOGIN;
    CREATE DATABASE neat;
    GRANT ALL ON DATABASE neat TO neat;
    ```

6. run yesod with ```yesod devel```
