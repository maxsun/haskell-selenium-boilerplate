# Haskell-Selenium Boilerplate

This repo contains code to help you quickly start using [Selenium](https://www.seleniumhq.org/projects/webdriver/) with [Haskell](https://www.haskell.org/).

The repo uses the [webdriver](http://hackage.haskell.org/package/webdriver-0.9.0.1) Haskell library to interface with Selenium. For a list of all the commands, see [here](https://hackage.haskell.org/package/webdriver-0.6.0.4/docs/Test-WebDriver-Commands.html).

### Installation & Building

Before you begin, make sure you have the following installed:

- [Stack](https://docs.haskellstack.org/en/stable/README/)
- [Selenium Standalone Server](https://www.seleniumhq.org/download/)

Next, run (this could take several minutes):
```stack build```

After building, we can start the Selenium Server and run our compiled Haskell executable.

To start the Selenium Server, navigate to the downloaded Jar and run:
```java -jar selenium-server-standalone-x.x.x.jar```
(where `x.x.x` is the version you downloaded. I've tested on 3.141.59)

After the server is running, you can check [localhost:4444](http://localhost:4444) to verify.

Finally, we can run our executable with:
`stack run`


### Using Docker

Make sure you have [Docker](https://docs.docker.com/) and Docker-Compose installed. You can check the later with `docker-compose --version`.