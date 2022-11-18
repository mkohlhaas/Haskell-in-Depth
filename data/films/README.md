- Installation of Postgresql server and client software.

```shell
sudo xbps-install postgresql14
sudo xbps-install postgresql14-client
sudo ln -s /etc/sv/postgresql14/ /var/service/
sudo sv start  postgresql14
sudo sv status postgresql14                                             # should be running by now
```

- Logoff and login again so PATH is setup properly and you can use `psql`.
- Create database user, database, import data.

```shell
sudo chsh -s /bin/bash postgres
sudo su - postgres
psql
CREATE USER schmidh WITH PASSWORD 'my_password';                        # change password; user is the same as OS user
CREATE DATABASE sakila_films;
GRANT ALL PRIVILEGES ON DATABASE sakila_films to schmidh;
Ctrl-D                                                                  # logout of psql
rm .psql_history
Ctrl-D                                                                  # logout of postgres user session
cd Haskell-in-Depth/data/films/
psql -d sakila_films -a -f films-schema.sql
psql -d sakila_films -a -f films-insert.sql
psql -d sakila_films -c 'select * from film order by film_id;'          # 1000 rows
psql -d sakila_films -c 'select * from film_category order by film_id;' # 1000 rows
psql -d sakila_films -c 'select * from category;'                       # 16 rows
```

- Optional: Installation of graphical administration tool for Postgresql.

```shell
sudo xbps-install dbeaver
dbeaver
```

- Configure new connection according to the [Screenshot](`Haskell-in-Depth/data/films/DBbeaver.png`).
