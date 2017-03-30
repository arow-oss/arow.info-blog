---
title: ARoW.info Blog -- Releasing a Haskell Web App on Heroku
headingBackgroundImage: ../img/post-bg.jpg
headingDivClass: post-heading
heading: Releasing a Haskell Web App on Heroku
subHeading: Using Heroku to serve a Servant App with Docker
postedBy: <a href="http://functor.tokyo">Dennis Gosnell</a>
---

Releasing Haskell web applications on Heroku has become much easier with
Heroku's Docker support. This article explains how to deploy a Servant
application on Heroku using Docker.

I've prepared an [example
application](https://github.com/cdepillabout/servant-on-heroku) you can use to
try deploying to Heroku. This article is divided into three sections. The first
section explains how to run the example application locally. The second section
explains how to run the example application locally using Docker. The third
section explains how to deploy this application to Heroku.

If you want to deploy to Heroku without running locally first, feel free to
skim through the first and second sections.  However, if you're new to Haskell
development, I recommend going through all three sections.

## Running the application locally WITHOUT Docker

The example application is a small JSON API. It provides two APIs. One is to
submit simple comments. The other is to display all comments that have been
submitted. The comments are saved to a PostgreSQL database.

The following will walk through how to build and run the application locally,
without involving Docker or Heroku.

### Build the application locally

First, clone the repository and build the application.

```sh
$ git clone https://github.com/cdepillabout/servant-on-heroku
$ cd servant-on-heroku/
$ stack setup  # install the required version of ghc on your system
$ stack build  # install all dependencies and build the application
```

An error may occur when building the application because of missing PostgreSQL
libraries.

On Arch Linux, these missing PostgreSQL libraries can be installed with the
following command:

```sh
$ sudo pacman -Ss postgresql-libs
```

On Ubuntu, the following command can be used:

```sh
$ sudo apt-get install libpq-dev
```

Other platforms may use a different command to install these libraries.

Once the required PostgreSQL libraries have been installed, try running `stack
build` again. It should succeed this time.

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

The example application is trying to connect to PostgreSQL. Comments are stored
in PostgreSQL, so you need PostgreSQL running locally.

### Setup PostgreSQL

Most OSs and distributions will have different ways of installing PostgreSQL.
Check with your platform documentation on how to install PostgreSQL.

For example, [here](https://wiki.archlinux.org/index.php/PostgreSQL#Installing_PostgreSQL) is the Arch Linux documentation for installing
PostgreSQL. [Here](https://help.ubuntu.com/community/PostgreSQL#Installation) is the Ubuntu documentation.

Once PostgreSQL is installed and running, try running the application again:

```sh
$ stack exec -- servant-on-heroku-api
```

Another oops!  It should fail with the following error:

```
servant-on-heroku-api: libpq: failed (FATAL:  role "mydbuser" does not exist
)
```

Looks like a PostgreSQL user and database need to be setup for our application.
If you check out the application source code (`src/Lib.hs`), you can see that it
is reading in the `DATABASE_URL` environment variable and using it to connect to
the PostgreSQL server.

If the `DATABASE_URL` environment variable is not specified, the application
defaults to using:

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

PostgreSQL might need to be restarted:

```sh
$ sudo systemctl restart postgresql
```

It should now be possible to connect to the `mydb` database locally as the
`mydbuser` user:

```sh
$ psql -U mydbuser -d mydb -h 127.0.0.1
```

### Testing the API

Now that PostgreSQL is setup correctly, the application can be run with the
following command:

```sh
$ stack exec -- servant-on-heroku-api
running servant-on-heroku on port 8080...
```

Let's try sending a comment. With the application still running, try the
following command:

```sh
$ curl --request POST \
    --header 'Content-Type: application/json' \
    --data '{"author": "DG", "text": "Pretty good"}' \
    'http://localhost:8080/add-comment'
{ "text": "Pretty good", "author": "DG" }
```

Now let's list all the comments:

```sh
$ curl --request GET \
    --header 'Content-Type: application/json' \
    'http://localhost:8080/get-comments'
[{"text":"Pretty good","author":"DG"} ]
```

Looks like it's working.  Now let's try with Docker!

## Running the application locally WITH Docker

[Docker](https://www.docker.com/) is used to build and run the application inside a container. The
following section assumes basic familiarity with Docker.

### Installing Docker

Docker is installed differently on different platforms. Check your platform
documentation for more advice. For instance, here are the instructions for
installing on [Arch Linux](https://wiki.archlinux.org/index.php/Docker#Installation)
and [Ubuntu](https://docs.docker.com/engine/installation/linux/ubuntu/).

After installing Docker, make sure it is running with the following command:

```sh
$ docker info
```

### Building with Docker

We will build the application inside of Docker and create a docker image for the
application.

Use `docker build` to build a Docker image for the application:

```sh
$ docker build -t servant-on-heroku .
```

This uses the [`Dockerfile`](https://github.com/cdepillabout/servant-on-heroku/blob/master/Dockerfile)
in the current directory to build the application. The `Dockerfile` lists all
the steps to build the application and create a reusable image.

If you take a look at the `Dockerfile`, you can see that it is performing the
following steps:

1.  Install required packages with `apt-get`.
2.  Install `stack`.
3.  Install GHC using `stack` based on the application's `stack.yaml` file.
4.  Install Haskell dependencies for the application using the application's
    `.cabal` file.
5.  Building the application with `stack`.
6.  Create a non-root user to use to run the application.
7.  Run the application.

`docker build` can take up to one hour to finish creating the
`servant-on-heroku` image.[^1]

### Testing the API with Docker

Once `docker build` finishes, `docker images` can be used to list all local
images:

```sh
$ docker images
REPOSITORY           TAG       IMAGE ID       CREATED         SIZE
servant-on-heroku    latest    ff591d372461   30 seconds ago  3.92 GB
...
```

You can see the `servant-on-heroku` image that was just created.

Let's try running the `servant-on-heroku` image. This will run the application
in Docker:

```sh
$ docker run --interactive --tty --rm servant-on-heroku
```

Oh no!  It looks like the PostgreSQL problem is back:

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

Here's a small trick we can use. When running the `servant-on-heroku` container,
we can tell Docker to just let the container use our local network interface.
That way, it can see PostgreSQL:

```sh
$ docker run --interactive --tty --rm --network host servant-on-heroku
running servant-on-heroku on port 8080...
```

With the `servant-on-heroku` container running, let's try the `curl` commands
from the previous section.  Posting a comment:

```sh
$ curl --request POST \
    --header 'Content-Type: application/json' \
    --data '{"author": "EK", "text": "Not enough CT"}' \
    'http://localhost:8080/add-comment'
{ "text": "Not enough CT", "author": "EK" }
```

Getting the comments:

```sh
$ curl --request GET \
    --header 'Content-Type: application/json' \
    'http://localhost:8080/get-comments'
[{"text":"Pretty good","author": "DG"},{"text":"Not enough CT","author":"EK"}]
```

By the way, in order to open a shell and inspect the image by hand, the
following command can be used:

```sh
$ docker run --interactive --tty --rm --network host servant-on-heroku /bin/bash
```

Now that we are confident our application works in Docker, it's time for Heroku.

## Heroku

Once we have the application building and running successfully in Docker, it's
easy to move to Heroku. The first step is creating a Heroku account.

### Creating an Account

Go [here](https://signup.heroku.com) to sign up for a Heroku account. If you
already have a Heroku account, you can skip this step.

We will deploy the application using Heroku's "Free" tier, so you don't need to
worry about registering a credit card.

The majority of the instructions in this section are condensed from
Heroku's
[own documentation](https://devcenter.heroku.com/articles/container-registry-and-runtime) on
integrating with Docker. Check out their documentation if anything is unclear.

### Install the Heroku CLI Application

Heroku provides a CLI application to make it easy to work with their service.
This is similar to [AWS's CLI](https://aws.amazon.com/cli)
or [Digital Ocean's CLI](https://github.com/digitalocean/doctl).

On Arch Linux, Heroku's CLI application can be installed with the following
command:

```sh
$ yaourt -S heroku-toolbelt
```

This installs the `heroku` binary to the system.

Instructions for other platforms can be found
on [Heroku's site](https://devcenter.heroku.com/articles/heroku-cli).

Once the CLI application has been downloaded, it can be used to login and
authenticate with Heroku's API:

```sh
$ heroku login
```

You will be asked for the username and password of the account you just created.

### Create an Application on Heroku

The first step of releasing our Servant API to Heroku is to create a Heroku
Application.

The following command will create a new Heroku application called
`servant-on-heroku`. You may need to use a different name for your own
application:

```sh
$ heroku apps:create servant-on-heroku
```

The following command lists information about the application just created
(although it won't be too interesting yet):

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

Make sure to take note of the `Web URL`. It will come in handy later.

### Install Heroku Docker Plugin

The Heroku CLI application has a plugin architecture. This allows the user to
install plugins that can be used to access different parts of Heroku's API.

There is a plugin for using
Heroku's
[Docker Container Registry](https://devcenter.heroku.com/articles/container-registry-and-runtime).

The following command can be used to install the plugin:

```sh
$ heroku plugins:install heroku-container-registry
```

After installing the plugin, the following command can be used to make sure it
works:

```sh
$ heroku container
4.1.1
```

It should return the version string for the plugin.

In order to actually use the plugin, the following command can be used to login
to Heroku's container registry.

```sh
$ heroku container:login
```

This adds login information for Heroku's container registry to the file
`~/.docker/config.json`:

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

In order to get the application actually running on Heroku, the following
command is used:

```sh
$ heroku container:push web
```

This builds a Docker image for the application based on the `Dockerfile` in the
current directory. Internally, `docker build` is used to do this. If the image
was already built in the previous step (when running `docker build` from the
command line), then this `heroku container:push` command will just use the
previously built image. The image is sent to Docker's Container Registry.

Now let's check `heroku apps:info` again:

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

Hmm, that's not right. See where it says `Dynos: `? A "dyno" is Heroku-lingo for
a server that runs the web application. This line means that there aren't any
servers running the application.

In order to fix this, the `heroku ps:scale` command can be used to spin up one
dyno to run the application:

```sh
$ heroku ps:scale web=1
```

This creates one "web" dyno, which will run the Servant API.[^3]

Now run the following command to make sure the dyno is actually running:

```sh
$ heroku ps
Free dyno hours quota remaining this month: 549h 2m (99%)
For more information on dyno sleeping and how to upgrade, see:
https://devcenter.heroku.com/articles/dyno-sleeping

=== web (Free): /bin/sh -c /opt/servant-on-heroku/bin/servant-on-heroku-api (1)
web.1: starting 2017/03/22 19:05:04 +0900 (~ 8s ago)
```

The output is somewhat noisy, but you can tell that there is now one web dyno
running.

Now that the application is running, the following command can be used to access
the application's `Web URL` with curl. (The application `Web URL` can be found in
the output of `heroku apps:info`.)

```sh
$ curl --request POST \
    --header 'Content-Type: application/json' \
    --data '{"author": "MS", "text": "Gotta make it professional"}' \
    'https://servant-on-heroku.herokuapp.com/add-comment'
```

That's strange, there appears to be another error. Let's see how to investigate
application errors on Heroku.

### Debugging Application Errors

Heroku has a really nice log system. The application's `stdout` and `stderr`
logs can be inspected with the following command:

```sh
$ heroku logs
2017-03-22T10:05:49 heroku[web.1]: proc start `/opt/servant-on-heroku/bin/servant-on-heroku-api`
2017-03-22T10:05:52 app[web.1]: servant-on-heroku-api: libpq: failed (could not connect to server: Connection refused
2017-03-22T10:05:52 app[web.1]:    Is the server running on host "localhost" (127.0.0.1) and accepting
2017-03-22T10:05:52 app[web.1]:    TCP/IP connections on port 5432?
2017-03-22T10:05:52 app[web.1]: )
2017-03-22T10:05:52 heroku[web.1]: State changed from starting to crashed
```

Oh no! It's the same error that has been plaguing us this whole time. Why is it
occurring again?

Well, it's because we haven't setup a PostgreSQL database on Heroku!

### PostgreSQL on Heroku

Heroku
has [nice support](https://devcenter.heroku.com/articles/heroku-postgresql) for
PostgreSQL. Heroku provides a PostgreSQL database that can be used
free-of-charge.

The following command can be used enable the PostgreSQL database add-on for the
application:

```sh
$ heroku addons:create heroku-postgresql:hobby-dev
```

This enables the `heroku-postgresql` add-on in the `hobby-dev` tier (which is
free).

After enabling it, the following command can be used to make sure the PostgreSQL
database has been successfully created:

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

The database info can be checked with the `pg:info` command:

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

### Restart the App

Now that the PostgreSQL database is up and running, let's try restarting the
application:

```sh
$ heroku ps:restart
```

Let's take a look at the application logs again:

```sh
$ heroku logs
2017-03-22T10:22:15 heroku[web.1]: State changed from crashed to starting
2017-03-22T10:22:54 heroku[web.1]: proc start `/opt/servant-on-heroku/bin/servant-on-heroku-api`
2017-03-22T10:22:56 app[web.1]: Migrating: CREATe TABLE "comment"("id" SERIAL8  PRIMARY KEY UNIQUE,"author" VARCHAR NOT NULL,"text" VARCHAR NOT NULL)
2017-03-22T10:22:57 heroku[web.1]: State changed from starting to up
```

Looks like it worked this time!  Finally!

Let's try accessing the app using `curl` again:

```sh
$ curl --request POST \
    --header 'Content-Type: application/json' \
    --data '{"author": "SPJ", "text": "Avoid heroku-at-all-costs"}' \
    'https://servant-on-heroku.herokuapp.com/add-comment'
{"text":"Avoid heroku-at-all-costs","author":"SPJ"}
```

And once more:

```sh
$ curl --request GET \
    --header 'Content-Type: application/json' \
    'https://servant-on-heroku.herokuapp.com/get-comments'
[{"text":"Avoid heroku-at-all-costs","author":"SPJ"}]
```

Success! Looks like everything is working well!

### How does the app on Heroku know how to connect to the database?

You may be wondering how the application running on Heroku knows how to
connect to the database. Well, Heroku has configuration variables that it passes
to the application as environment variables.

These configuration variables can be inspected with the following command:

```sh
$ heroku config
=== servant-on-heroku Config Vars
DATABASE_URL: postgres://someusername:somepassword@ec2-12-12-234-123.compute-1.amazonaws.com:5432/databasename
```

Setting up the PostgreSQL database creates a configuration variable
called `DATABASE_URL`. Heroku passes this configuration variable to the
application on startup as an environment variable. As discussed in a previous
section, the application uses `DATABASE_URL` to connect to the correct
database[^2].

Heroku's `DATABASE_URL` can also be used to connect to the database on the
command line:

```sh
$ psql "$(heroku config:get DATABASE_URL)"
psql (9.6.1)
SSL connection (protocol: TLSv1.2, cipher: ECDHE-RSA-AES256-GCM-SHA384, bits: 256, compression: off)

databasename=> select * from comment;
 id | author |           text
----|--------|---------------------------
  1 | SPJ    | Avoid heroku-at-all-costs
(1 row)
```

### Future (Normal) Releases

Performing future releases of the application is extremely easy. Just run the
following command:

```sh
$ heroku container:push web
```

This rebuilds the docker image for the application and pushes it to Heroku's
container repository. It then restarts the dynos so they are running with the
new code for the application.

## Future Work

This application works pretty well, but there are a couple places for
improvements. The lowest hanging fruit would probably be the `Dockerfile`.
Here are a couple ideas that would make the `Dockerfile` a little better:

- Use a slimmer image as the base image for the `Dockerfile`. Right now it is
  using [Heroku's images](https://hub.docker.com/r/heroku/heroku/), but I don't
  think there is any reason that something like
  [Alpine Linux](https://hub.docker.com/_/alpine/) couldn't be used.
- Base the image on something with `stack`, GHC, and popular Haskell libraries
  already installed. This would greatly reduce the time it takes to do the very
  initial `docker build`.
- At the very end of the `Dockerfile`, remove `stack`, GHC, and all Haskell
  libraries. This would hopefully make the docker image a little smaller. It
  would take less bandwidth to send the image to Heroku's container repository.

It would also be nice to use something like `docker-compose` to setup the
PostgreSQL database using Docker when running locally.

## Alternatives to Docker for Deploying on Heroku

The only strong alternative to using Docker for deploying Haskell code to Heroku
is [haskellonheroku](https://haskellonheroku.com/). This is a normal
Heroku [buildpack](https://devcenter.heroku.com/articles/buildpacks) for
Haskell. With this buildpack, you are able to use Heroku like you would with
dynamic languages. All you need to do is `git push` your code to Heroku's remote
git repository. The new code is automatically compiled and deployed.

This sounds really good in theory, but in pracitce haskellonheroku has two big
drawbacks:

1.  Heroku build times are limited to 15 minutes. haskellonheroku gets around
    this in a complicated way,
    [requiring use](https://haskellonheroku.com/tutorial/#build-the-sandbox) of
    Amazon S3 to upload prebuilt libraries before doing a `git push`.
2.  haskellonheroku uses [`halcyon`](https://halcyon.sh/) internally to
    accomplish most build steps. `halcyon` is a tool similar to `stack` and
    `nix`. However, it appears that development has stopped 2 years ago.
    `halcyon` does not support any of the latest GHC versions.

`halcyon` might have been nice a few years ago before `stack` existed. But now
that `stack` is regularly used for Haskell development, moving to an alternative
build tool doesn't seem like a good decision.[^4]

## Related Work

-   [Alternative Dockerfile](https://www.reddit.com/r/haskell/comments/3iql3f/heroku_buildpack_using_stack/cujd263/)
    for deploying with Docker

## Conclusion

As long as you have Docker running on your local machine, it's pretty easy to
get your Haskell code on Heroku. Heroku's free plan is nice for testing
application ideas and showing them to others. It may not work for any sort of
business application, but as a proof-of-concept, it's great!

If you decide your proof-of-concept works well and you want to release it, it's
easy to add a credit card to Heroku and start running on their cheapest paid
tier.  Heroku has a very nice upgrade path.

## Footnotes

[^1]: These seven steps are slightly complicated. Ideally, it should be possible
    to install GHC, install all the application dependencies, and build the
    application in just one command. However, I have separated it into multiple
    commands to take advantage of Docker's caching ability. When re-running
    `docker build`, only commands where the input has changed will be re-run.

    For example, if you change the `servant-on-heroku.cabal` file and re-run
    `docker build`, it will rebuild the image from (4), starting with installing
    dependencies from the application's `.cabal` file. `docker build` does not
    have to re-run (1), (2), or (3). It uses cached versions of the image.

    This means that if all you change is the application source code under
    `src/` and re-run `docker build`, all `docker build` has to do is re-run
    (5), (6), and (7). It doesn't have to install GHC or the application's
    Haskell dependencies. This reduces a large part of the build-time. Future
    builds will take just a few minutes, instead of one hour.

[^2]: Heroku also makes use of the `PORT` environment variable for telling your
    application which port to listen on.

[^3]: There are
    [multiple kinds](https://devcenter.heroku.com/articles/dynos#dyno-configurations)
    of dynos. However, it's not something that we need to worry about for our
    simple web API.

[^4]: Unless it is a tool that gives substantial extra power, like `nix`.
