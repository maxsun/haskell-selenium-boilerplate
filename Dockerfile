FROM haskell:8.6 as build-env

COPY stack.yaml /opt/haskell-selenium-boilerplate/
COPY package.yaml /opt/haskell-selenium-boilerplate/
RUN cd /opt/haskell-selenium-boilerplate && stack build --only-dependencies

FROM haskell:8.6
COPY --from=build-env /opt/haskell-selenium-boilerplate/ /opt/haskell-selenium-boilerplate/ 
COPY --from=build-env  /root/.stack /root/.stack
ADD . /opt/haskell-selenium-boilerplate/ 
RUN cd /opt/haskell-selenium-boilerplate && stack build --copy-bins .
RUN mv /root/.local/bin/haskell-selenium-boilerplate-exe /bin/

RUN chmod +x /opt/haskell-selenium-boilerplate/wait-for-it.sh

CMD bin/haskell-selenium-boilerplate-exe
