<div align="center"><img height="180" src="https://gitlab.iscpif.fr/gargantext/main/raw/master/images/logo.png"></div>

&nbsp;
# Gargantext with Haskell (Backend instance)

![Haskell](https://img.shields.io/badge/Code-Haskell-informational?style=flat&logo=haskell&color=6144b3)&nbsp;&nbsp;![Nix](https://img.shields.io/badge/Package%20manager-Nix-informational?style=flat&logo=nixos&color=6586c8)&nbsp;&nbsp;![Cabal](https://img.shields.io/badge/Tools-Cabal-informational?style=flat&logo=cabal&color=567dd9)&nbsp;&nbsp;![Stack](https://img.shields.io/badge/Tools-Stack-informational?style=flat&logo=stack&color=6144b3)&nbsp;&nbsp;![GHC](https://img.shields.io/badge/Tools-GHC-informational?style=flat&logo=&color=2E677B)&nbsp;&nbsp;![Docker](https://img.shields.io/badge/Tools-Docker-informational?style=flat&logo=docker&color=003f8c)

#### Table of Contents
1. [About the project](#about)
2. [Installation](#install)
3. [Initialization](#init)
4. [Launch & develop GarganText](#launch)
5. [Uses cases](#use-cases)
6. [GraphQL](#graphql)
7. [PostgreSQL](#postgresql)

## About the project <a name="about"></a>

GarganText is a collaborative web-decentralized-based macro-service platform for the exploration of unstructured texts. It combines tools from natural language processing, text-data-mining bricks, complex networks analysis algorithms and interactive data visualization tools to pave the way toward new kinds of interactions with your textual and digital corpora.

This software is free (as "Libre" in French) software, developed by the CNRS Complex Systems Institute of Paris Île-de-France (ISC-PIF) and its partners.

GarganText Project: this repo builds the backend for the frontend server built by [backend](https://gitlab.iscpif.fr/gargantext/haskell-gargantext).


## Installation <a name="install"></a>

Disclaimer: since this project is still in development, this document remains in progress. Please report and improve this documentation if you encounter any issues.

#### Prerequisites

- Install:
  - git (https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
  - curl (https://everything.curl.dev/get)
- Clone the project.
  ```shell
  git clone https://gitlab.iscpif.fr/gargantext/haskell-gargantext.git
  cd haskell-gargantext
  ```
### Installation

This project can be built with either Stack or Cabal. We keep up-to-date the `cabal.project` (which allows us
to build with `cabal` by default) but we support `stack` thanks to thanks to
[cabal2stack](https://github.com/iconnect/cabal2stack), which allows us to generate a valid `stack.yaml` from
a `cabal.project`. Due to the fact gargantext requires a particular set of system dependencies (C++ libraries,
toolchains, etc) we use [nix](https://nixos.org/) to setup an environment with all the required system
dependencies, in a sandboxed and isolated fashion.

#### Install Nix 

As said, Gargantext requires [Nix](https://github.com/NixOS/nix) to provide system dependencies (for example, C libraries), but its use is limited to that. In order to install [Nix](https://nixos.org/download.html):

```shell
sh <(curl -L https://nixos.org/nix/install) --daemon
```

Verify the installation is complete with
```shell
nix-env --version
nix-env (Nix) 2.19.2
```

**Important:** Before building the project with either `stack` or `cabal` you need to be in the correct Nix shell, which will fetch all the required system dependencies. To do so, just type **inside your haskell-gargantext folder**:

```shell
nix-shell
```

This will take a bit of time as it has to download/build the dependencies, but this will be needed only the first time.

### Build: choose cabal (new) or stack (old)

#### With Cabal (recommanded)

##### Turning off optimization flags

Create a `cabal.project.local` file (don't commit it to git!):
```
package gargantext
    ghc-options: -fwrite-ide-info -hiedir=".stack-work/hiedb" -O0

package gargantext-admin
    ghc-options: -O0

package gargantext-cli
    ghc-options: -O0

package gargantext-db-obfuscation
    ghc-options: -O0

package gargantext-import
    ghc-options: -O0

package gargantext-init
    ghc-options: -O0

package gargantext-invitations
    ghc-options: -O0

package gargantext-phylo
    ghc-options: -O0

package gargantext-server
    ghc-options: -O0

package gargantext-upgrade
    ghc-options: -O0

package gargantext-graph
    ghc-options: -O0

package hmatrix
    ghc-options: -O0

package sparse-linear
    ghc-options: -O0
```

##### Building

First, into `nix-shell`:
```shell
cabal update
cabal install
```

Alternatively, if you want to run the command "from the outside", in your current shell:

```
nix-shell --run "cabal update"
nix-shell --run "cabal install"
```

#### With Stack

Install [Stack (or Haskell Tool Stack)](https://docs.haskellstack.org/en/stable/):

```shell
curl -sSL https://get.haskellstack.org/ | sh
```

Verify the installation is complete with
```shell
stack --version
Version 2.9.1
```

NOTE: Default build (with optimizations) requires large amounts of RAM (16GB at least). To avoid heavy compilation times and swapping out your machine, it is recommended to `stack build` with the `--fast` flag, i.e.:

```shell
stack build --fast
```


#### Keeping the stack.yaml updated with the cabal.project

(Section for Developers using stack only)

Once you have a valid version of `stack`, building requires generating a valid `stack.yaml`.
This can be obtained by installing `cabal2stack`:

```shell
git clone https://github.com/iconnect/cabal2stack.git
cd cabal2stack
```

Then, depending on what build system you are using, either build with `cabal install --overwrite-policy=always` or `stack install`.

And finally:

```shell
cabal2stack --system-ghc --allow-newer --resolver lts-21.17 --resolver-file devops/stack/lts-21.17.yaml -o stack.yaml
stack build
```

The good news is that you don't have to do all of this manually; during development, after modifying the
`cabal.project`, it's enough to do:

```shell
./bin/update-project-dependencies
```

## Initialization <a name="init"></a>

#### 1. Docker-compose will configure your database and some NLP bricks (such as CoreNLP):

``` sh
# If docker is not installed:
# curl -sSL https://gitlab.iscpif.fr/gargantext/haskell-gargantext/raw/dev/devops/docker/install_docker | sh
cd devops/docker
docker compose up
```
Initialization schema should be loaded automatically (from `devops/postgres/schema.sql`).

##### (Optional) If using stack, then install:
``` sh
stack install
```

#### 2. Copy the configuration file:
``` sh
cp gargantext.ini_toModify gargantext.ini
```
> Do not worry, `.gitignore` avoids adding this file to the repository by mistake, then you can change the passwords in gargantext.ini safely.

#### 3. A user have to be created first as instance:
``` sh
~/.local/bin/gargantext-init "gargantext.ini"
```
Now, `user1` is created with password `1resu`

#### 4. Clone FRONTEND repository:

From the Backend root folder (haskell-gargantext):

```shell
git clone ssh://git@gitlab.iscpif.fr:20022/gargantext/purescript-gargantext.git
```
&nbsp;

## Launch & develop GarganText <a name="launch"></a>

>  **Note:** here, the method with Cabal is used as default


From the Backend root folder (haskell-gargantext):

``` shell
./start
# The start script runs following commands:
# - `./bin/install` to update and build the project
# - `docker compose up` to run the Docker for postgresql from devops/docker folder
# - `cabal run gargantext-server -- --ini gargantext.ini --run Prod` to run other services through `nix-shell`
```

For frontend development and compilation, see the [Frontend Readme.md](https://gitlab.iscpif.fr/gargantext/purescript-gargantext#dev)

### Running tests

From nix shell:

```
cabal v2-test --test-show-details=streaming
```

Or, from "outside":
```
nix-shell --run "cabal v2-test --test-show-details=streaming"
```
### Working on libraries

When a devlopment is needed on libraries (for instance, the HAL crawler in https://gitlab.iscpif.fr/gargantext/crawlers):

1. Ongoing devlopment (on local repo):
   1. In `cabal.project`:
      - add `../hal` to `packages:`
      - turn off (temporarily) the `hal` in `source-repository-package` 
   2. When changes work and tests are OK, commit in repo `hal`
2. When changes are commited / merged:
   1. Get the hash id, and edit `cabal.project` with the **new commit id**
   2. run `./bin/update-project-dependencies`
      - get an error that sha256 don't match, so update the `./bin/update-project-dependencies` with new sha256 hash
      - run again `./bin/update-project-dependencies` (to make sure it's a fixed point now)

> Note: without `stack.yaml` we would have to only fix `cabal.project` -> `source-repository-package` commit id. Sha256 is there to make sure CI reruns the tests.

## Use Cases <a name="use-cases"></a>

### Multi-User with Graphical User Interface (Server Mode)

``` sh
~/.local/bin/stack --docker exec gargantext-server -- --ini "gargantext.ini" --run Prod
```

Then you can log in with `user1` / `1resu`


### Command Line Mode tools

#### Simple cooccurrences computation and indexation from a list of Ngrams

``` sh
stack --docker exec gargantext-cli -- CorpusFromGarg.csv ListFromGarg.csv Ouput.json
```

### Analyzing the ngrams table repo

We store the repository in directory `repos` in the [CBOR](https://cbor.io/) file format. To decode it to JSON and analyze, say, using [jq](https://shapeshed.com/jq-json/), use the following command:

``` sh
cat repos/repo.cbor.v5 | stack exec gargantext-cbor2json | jq .
```
### Documentation

To build documentation, run:

```sh
stack build --haddock --no-haddock-deps --fast
```

(in `.stack-work/dist/x86_64-linux-nix/Cabal-3.2.1.0/doc/html/gargantext`).

## GraphQL <a name="graphql"></a>

Some introspection information.

Playground is located at http://localhost:8008/gql

### List all GraphQL types in the Playground

```
{
  __schema {
    types {
      name
    }
  }
}
```

### List details about a type in GraphQL

```
{
  __type(name:"User") {
  	fields {
    	name
      description
      type {
        name
      }
  	}
	}
}
```
## PostgreSQL <a name="pgsql"></a>

### Upgrading using Docker

https://www.cloudytuts.com/tutorials/docker/how-to-upgrade-postgresql-in-docker-and-kubernetes/

To upgrade PostgreSQL in Docker containers, for example from 11.x to 14.x, simply run:
```sh
docker exec -it <container-id> pg_dumpall -U gargantua > 11-db.dump
```

Then, shut down the container, replace `image` section in `devops/docker/docker-compose.yaml` with `postgres:14`. Also, it is a good practice to create a new volume, say `garg-pgdata14` and bind the new container to it. If you want to keep the same volume, remember about removing it like so:
```sh
docker-compose rm postgres
docker volume rm docker_garg-pgdata
```

Now, start the container and execute:
```sh
# need to drop the empty DB first, since schema will be created when restoring the dump
docker exec -i <new-container-id> dropdb -U gargantua gargandbV5
# recreate the db, but empty with no schema
docker exec -i <new-container-id> createdb -U gargantua gargandbV5
# now we can restore the dump
docker exec -i <new-container-id> psql -U gargantua -d gargandbV5 < 11-db.dump
```

### Upgrading using 

There is a solution using pgupgrade_cluster but you need to manage the clusters version 14 and 13. Hence here is a simple solution to upgrade.

First save your data:
```
sudo su postgres
pg_dumpall > gargandb.dump
```

Upgrade postgresql:
```
sudo apt install postgresql-server-14 postgresql-client-14
sudo apt remove --purge postgresql-13
```
Restore your data:
```
sudo su postgres
psql < gargandb.dump
```

Maybe you need to restore the gargantua password
```
ALTER ROLE gargantua PASSWORD 'yourPasswordIn_gargantext.ini'
```
Maybe you need to change the port to 5433 for database connection in your gargantext.ini file.




