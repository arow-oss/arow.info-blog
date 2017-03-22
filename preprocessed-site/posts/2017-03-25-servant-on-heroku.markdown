---
title: ARoW.info Blog -- Releasing a Servant App on Heroku
headingBackgroundImage: ../img/post-bg.jpg
headingDivClass: post-heading
heading: Releasing a Servant App on Heroku
subHeading: Using Heroku to serve a Servant App with Docker
postedBy: <a href="http://functor.tokyo">Dennis Gosnell</a>
draft: true
---

Releasing Haskell web applications on Heroku has become much easier with Heroku's Docker support.  This article explains how to deploy a Servant application on Heroku using Docker.

I've prepared
an [example application](https://github.com/cdepillabout/servant-on-heroku) you
can use to try deploying to Heroku. The first section explains how to run the
application locally. The second section explains how to run the application locally in Docker.  The third section talks about how to deploy this application
to Heroku. If you just want to deploy to Heroku without running locally first,
feel free to just skim through the first and second sections.

## Running the application locally WITHOUT Docker

The example application is a simple application. You are able to submit comments
and read back past submitted comments. The comments are saved to a PostgreSQL
database.  This is similar to an anonymized Twitter.

The following will walk through how to build and run the application locally, without involving Docker or Heroku.

### Build the application locally

First, clone the repo and build the application.

```sh
$ git clone https://github.com/cdepillabout/servant-on-heroku
$ cd servant-on-heroku/
$ stack setup  # install the required version of ghc on your system
$ stack build  # install all dependencies and build the application
```

You may get an error when building the application because of missing PostgreSQL libraries.

On Arch Linux you can install these libraries with the following command:

```sh
$ pacman -Ss postgresql-libs
```

On Ubuntu, you can use the following command:

```sh
$ apt-get install libpq-dev
```

Once you've installed the required PostgreSQL libraries, try running `stack build` again.  It should succeed this time.

Now try running the application:

```sh
$ stack exec -- servant-on-heroku-api
```

Oops!  It should fail with the following error:

```
servant-on-heroku-api: libpq: failed (could not connect to server: Connection refused
        Is the server running on host "localhost" (::1) and accepting
        TCP/IP connections on port 5432?
        could not connect to server: Connection refused
        Is the server running on host "localhost" (127.0.0.1) and accepting
        TCP/IP connections on port 5432?
)
```

The example application stores comments in PostgreSQL, so we need PostgreSQL running locally.

### Setup PostgreSQL

Most OSs and distrobutions will have a different way of installing PostgreSQL.  Check with your platform documentation on how to install PostgreSQL.

For example, [here]() is the Arch Linux documentation for installing PostgreSQL.  [Here]() is the Ubuntu documentation.

Once you have PostgreSQL installed and running, you can try running our application again:

```sh
$ stack exec -- servant-on-heroku-api
```

Another oops!  It should fail with the following error:

```
servant-on-heroku-api: libpq: failed (FATAL:  role "mydbuser" does not exist
)
```

Looks like we need to setup a PostgreSQL user and database for our application.  If you checkout the application source, you can see that it is reading in the `DATABASE_URL` environment variable and using it to connect to the PostgreSQL server.

If the `DATABASE_URL` environment variable is not specified, the application defaults to using:

```
postgres://mydbuser:mydbpass@localhost:5432/mydb
```

It is trying to use the user `mydbuser` with password `mydbpass` to access the
database named `mydb`. Let's create this user and database in PostgreSQL. The
following commands are specific to Arch Linux. They may differ slightly if you
are on a different platform. Check your platform documentation if they don't
seem to be working.

First, create the `mydbuser` user with password `mydbpass`:

```sh
$ sudo -u postgres -- psql --command "CREATE ROLE mydbuser NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT LOGIN ENCRYPTED PASSWORD 'mydbpass'"
```

Create a database named `mydb`:

```sh
$ sudo -u postgres -- createdb mydb
```

Make sure that `mydbuser` can access the `mydb` database:

```sh
$ sudo -u postgres -- psql --command "GRANT ALL PRIVILEGES ON DATABASE mydb TO mydbuser"
```

Now you might need to restart PostgreSQL:

```sh
$ sudo systemctl restart postgresql
```

You should now be able to connect to the `mydb` database locally as the `mydbuser` user:

```sh
$ psql -U mydbuser -d mydb -h 127.0.0.1
```

### Testing the API

Now that PostgreSQL is setup correctly, you should be able to run the application:

```sh
$ stack exec -- servant-on-heroku-api
running servant-on-heroku on port 8080...
```

Let's try sending a comment.  With the application still running, try the following command:

```sh
$ curl --request POST \
    --header 'Content-Type: application/json' \
    --data '{"author": "DG", "text": "Pretty good"}' \
    'http://localhost:8080/add-comment'
{ "text": "Pretty good", "author": "DG" }
```

Now let's list all comments:

```sh
$ curl --request GET \
    --header 'Content-Type: application/json' \
    'http://localhost:8080/get-comments'
[ { "text": "Pretty good", "author": "DG" } ]
```

Looks like it's working.  Now let's try with Docker!

## Running the application locally WITH Docker

Docker is used to build and run the application inside a container.

### Installing Docker

Docker can be installed differently on different platforms. Check your platform
documentation for more advice. For instance, here are the instructions for
installing on [Arch Linux]() and [Ubuntu]().

After installing Docker, you can make sure it is functioning with the following command:

```sh
$ docker info
```

### Building with Docker

We will build our application inside of Docker and create a docker image for our
application.

Use `docker build` to build the application:

```sh
$ docker build -t servant-on-heroku .
```

This uses the [`Dockerfile`]() in the current directory to build the
application. The `Dockerfile` lists all the steps to build the application and
create a reusable image.

If you take a look at the `Dockerfile` you can see that it is performing the
following steps:

1.  Install required packages with `apt-get`.
2.  Install `stack`.
3.  Install GHC using `stack` based on the application's `stack.yaml` file.
4.  Install dependencies for the application using the `.cabal` file.
5.  Building the application with `stack`.
6.  Create a non-root user to use to run the application.
7.  Run the application.

`docker build` can take up to one hour to finish creating the
`servant-on-heroku` image.

These seven steps are slightly complicated. Ideally, it should be possible to
install GHC, install all the application dependencies, and build the application
in just one command. However, I have separated it into multiple commands to take
advantage of Docker's caching ability. When re-running `docker build`, only
commands where the input has changed will be re-run.

For example, if you change the `servant-on-heroku.cabal` file and re-run `docker
build`, it will rebuild the image from (4) with installing dependencies from the
application's `.cabal` file. `docker build` does not have to re-run (1), (2), or
(3). It uses cached versions of the image.

This means that if all you change is the application source code under `src/`
and re-run `docker build`, all `docker build` has to do is re-run (5), (6), and
(7). It doesn't have to install GHC or the application's Haskell dependencies.
This reduces a large part of the build-time. Future builds will take just a few
minutes, instead of tens of minutes.

### Testing the API with Docker

Once `docker build` finishes, you can use `docker images` to list all local images:

```sh
$ docker images
REPOSITORY           TAG       IMAGE ID       CREATED        SIZE
servant-on-heroku    latest    ff591d372461   6 days ago     3.92 GB
heroku/heroku        16        cc0caac6a5c5   2 weeks ago    464.7 MB
```

You can see the `servant-on-heroku` image.

Let's try running the `servant-on-heroku` image.  This will run the application in Docker:

```sh
$ docker run --interactive --tty --rm servant-on-heroku
```

Oh no!  It looks like our PostgreSQL problem is back:

```
servant-on-heroku-api: libpq: failed (could not connect to server: Connection refused
        Is the server running on host "localhost" (::1) and accepting
        TCP/IP connections on port 5432?
could not connect to server: Connection refused
        Is the server running on host "localhost" (127.0.0.1) and accepting
        TCP/IP connections on port 5432?
)
```

What's happening here? Well, since the `servant-on-heroku` container is running
as a Docker container, by default it can't see our local network. It can't see
that PostgreSQL is running on `localhost:5432`.

Here's a small trick we can use. When we run the `servant-on-heroku` container,
we can tell Docker to just let it use our local network. That way, it can see
PostgreSQL:

```sh
$ docker run --interactive --tty --rm --network host servant-on-heroku
running servant-on-heroku on port 8080...
```

With the `servant-on-heroku` container running, you can try the `curl` commands
from the previous section:

```sh
$ curl --request POST \
    --header 'Content-Type: application/json' \
    --data '{"author": "DG", "text": "Pretty good"}' \
    'http://localhost:8080/add-comment'
{ "text": "Not enough CT", "author": "EK" }
$ curl --request GET \
    --header 'Content-Type: application/json' \
    'http://localhost:8080/get-comments'
[ { "text": "Pretty good", "author": "DG" }, { "text": "Not enough CT", "author": "EK" } ]
```

By the way, if you just want to open a shell and inspect our container, you can use a command like this:

```sh
$ docker run --interactive --tty --rm --network host servant-on-heroku /bin/bash
```

Now that we know our application works in Docker, it's time for Heroku.

## Heroku

Once we have the application building successfully in Docker, it's easy to move to Heroku.  The first step is creating a Heroku account.

### Creating an Account

Go [here](https://signup.heroku.com) to sign up for a Heroku account. If you
already have a Heroku account, you can skip this step.

We will deploy out application using Heroku's "Free" tier, so you don't need to
worry about registering a credit card.

The majority of the instructions in this section are condensed from Heroku's [own documentation](https://devcenter.heroku.com/articles/container-registry-and-runtime) on integrating with Docker.  Checkout their documentation is anything is unclear.

### Install the Heroku CLI Application

Heroku provides a CLI application to make it easy to work with their service.
This is similar to [AWS's CLI](https://aws.amazon.com/cli)
or [Digital Ocean's CLI](https://github.com/digitalocean/doctl).

On Arch Linux Heroku's CLI application can be installed with the following
command:

```sh
$ yaort -S heroku-toolbelt
```

Instructions for other platforms can be found
on [Heroku's site](https://devcenter.heroku.com/articles/heroku-cli).

Once you're downloaded the CLI, you can use it login and authenticate with
Heroku's API:

```sh
$ heroku login
```

You will be asked for the username and password of the account you just created.

### Create an app in Heroku

The first step of releasing our Servant API to Heroku is to create a Heroku
Application.

The following command will create a new Heroku application called `servant-on-heroku`:

```sh
$ heroku apps:create servant-on-heroku
```

We can list information about the app we just created (although it won't be too
interesting yet):

```sh
$ heroku apps:info servant-on-heroku
```

### Install Heroku Docker Plugin

The Heroku CLI app has a plugin architecture.  It allows you to install plugins that can be used to access different parts of Heroku's API.

There is a plugin for using Heroku's [container registry](https://devcenter.heroku.com/articles/container-registry-and-runtime).

You can use the following command to intall the plugin:

```sh
$ heroku plugins:install heroku-container-registry
```

After installing the plugin, you can make sure it works by using the following command:

```sh
$ heroku container
```

It should return the version string for the plugin.

In order to actually use the plugin, you also must login to Heroku's container registery.  That can be accomplished with the following command:

```sh
$ heroku container:login
```

This adds login information to Heroku's container registery to the file `~/.docker/config.json`:

```sh
$ cat ~/.docker/config.json
{
  "auths": {
    "registry.heroku.com": {
      "auth": "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx="
    }
  }
}
```

### Get the Application Running on Heroku

In order to get your application actually running on Heroku, you need to send the Docker image built in a previous step to Heroku's container registery:

```sh
$ heroku container:push web
```

Now lets check `heroku apps:info` again:

```sh
$ heroku apps:info servant-on-heroku
=== servant-on-heroku
Auto Cert Mgmt: false
Dynos:
Git URL:        https://git.heroku.com/servant-on-heroku.git
Owner:          me@gmail.com
Region:         us
Repo Size:      0 B
Slug Size:      0 B
Stack:          cedar-14
Web URL:        https://servant-on-heroku.herokuapp.com/
```

Hmm, that's not right.  See where it says `Dynos:   `?  A "dyno" is Heroku-lingo for a server that runs your web application.  This line means that you don't have any servers running your application.

In order to fix that we can use the `heroku ps:scale` command to give you one dyno:

```sh
$ heroku ps:scale web=1
```

This gives us one "web" dyno, which works as a web api.  There are multiple different kinds of dynos, but we don't need to worry about that here.

Now run the following command to make sure your dyno is actually running:

```sh
$ heroku ps
Free dyno hours quota remaining this month: 549h 2m (99%)
For more information on dyno sleeping and how to upgrade, see:
https://devcenter.heroku.com/articles/dyno-sleeping

=== web (Free): /bin/sh -c /opt/servant-on-heroku/bin/servant-on-heroku-api (1)
web.1: starting 2017/03/22 19:05:04 +0900 (~ 8s ago)
```

The output is somewhat noisy, but you can tell that we have one web dyno running.

Now we are ready to go back to our app URL (that you can find in the output of
`heroku apps:info`) and try accessing it with curl:

```sh
$ curl --request POST \
    --header 'Content-Type: application/json' \
    --data '{"author": "MS", "text": "Gotta make it professional"}' \
    'https://servant-on-heroku.herokuapp.com/add-comment'
```

That's strange, there appears to be an error.  Let's see how to check application errors.

### Debugging Application Errors

Heroku has a really nice log system. We can check the application's log with the
following command and try to figure out why our app doesn't appear to be
working:

```sh
$ heroku logs
2017-03-22T10:05:49 heroku[web.1]: proc start `/opt/servant-on-heroku/bin/servant-on-heroku-api`
2017-03-22T10:05:52 app[web.1]: servant-on-heroku-api: libpq: failed (could not connect to server: Connection refused
2017-03-22T10:05:52 app[web.1]:    Is the server running on host "localhost" (127.0.0.1) and accepting
2017-03-22T10:05:52 app[web.1]:    TCP/IP connections on port 5432?
2017-03-22T10:05:52 app[web.1]: )
2017-03-22T10:05:52 heroku[web.1]: State changed from starting to crashed
```

Oh no!  It's the same error that has been plaguing us this whole time.  Why is it occuring again?  Well, it's because we haven't setup a PostgreSQL database!

### PostgreSQL on Heroku

Heroku has [nice support](https://devcenter.heroku.com/articles/heroku-postgresql) for PostgreSQL.  Heroku provides a PostgreSQL database that can be used free-of-charge.

The following command can be used enable the PostgreSQL database addon for our app:

```sh
$ heroku addons:create heroku-postgresql:hobby-dev
```

This enables the `heroku-postgresql` addon in the `hobby-dev` tier (which is free).

After enabling it, we can make sure it has been successfully created:

```sh
$ heroku addons:info heroku-postgresql
=== postgresql-tetrahedral-44549
Attachments:  servant-on-heroku::DATABASE
Installed at: Wed Mar 22 2017 19:22:14 GMT+0900 (JST)
Owning app:   servant-on-heroku
Plan:         heroku-postgresql:hobby-dev
Price:        free
State:        created
```

We can also check the database info with the `pg:info` command:

```sh
$ heroku pg:info
=== DATABASE_URL
Plan:        Hobby-dev
Status:      Available
Connections: 0/20
PG Version:  9.6.1
Created:     2017-03-22 10:22 UTC
Data Size:   7.2 MB
Tables:      1
Rows:        0/10000 (In compliance)
Fork/Follow: Unsupported
Rollback:    Unsupported
Add-on:      postgresql-tetrahedral-44549
```

## Future Work

- Use a slimmer image as the base for Dockerfile.  Maybe alpine linux?
- base the image on something with `stack`, `ghc`, and popular Haskell libraries
  already installed.
- remove stack, ghc, and all haskell libraries from the docker image to reduce
  the size.

## Conclusion



*** TODO 新規にHerokuにデプロイする場合の手順もドキュメントにまとめてください
    https://devcenter.heroku.com/articles/container-registry-and-runtime

    https://www.reddit.com/r/haskell/comments/3iql3f/heroku_buildpack_using_stack/

**** add database
     - If you want to connect to the database remotely.
       $ heroku config
       $ heroku config:get DATABASE_URL
**** subsequent releases
     - All you need to do is push the app to heroku
       $ heroku container:push web

In Haskell, when building a project, the normal advice is to "just
use `stack`".[^1] 

## Footnotes

[^1]: It used to be, "Just use `cabal` and sandboxes.  For multi-project
builds... maybe try a small shell script?"
