# New Eden Accounting Tool

This is a complete rewrite of the New Eden Accounting Tool currently found at [http://pwning.de/neat]().

At the moment there is nothing much to see here but a bit of playing around with yesod. Pull-Requests are welcome anyway.

## Getting stuff to run

1. Install Stack like [http://docs.haskellstack.org/en/stable/README.html]()
2. install postgres (i use v. 9.3)

    ```
    sudo apt-get install -y postgresql-9.3 libpg-dev
    ```

3. create user and database inside a psql-shell as user postgres:

    ```
    CREATE ROLE neat;
    ALTER ROLE neat LOGIN;
    CREATE DATABASE neat;
    GRANT ALL ON DATABASE neat TO neat;
    ```
4. clone this repository, invoke the install.sh to also download the dependencies and install yesod-bin and build the project

    ```
    git clone https://github.com/Drezil/neat
    cd neat
    ./install.sh
    stack setup
    stack install yesod-bin
    stack build
    ```

7. Get the current postgres-data-dump from `https://www.fuzzwork.co.uk/dump/postgres-latest.dmp.bz2` and restore it into the `neat` database:

   ```
   sudo su postgres
   cd /tmp
   wget https://www.fuzzwork.co.uk/dump/postgres-latest.dmp.bz2
   bzip2 -d postgres-latest.dmp.bz2
   pg_restore -d neat postgres-latest.dmp
   ```

8. run yesod in development-mode using stack with ```stack exec yesod devel```
