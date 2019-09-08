FROM haskell:8.6 as build-env

COPY stack.yaml /opt/selenium-example/
COPY package.yaml /opt/selenium-example/
RUN cd /opt/selenium-example && stack build --only-dependencies

FROM haskell:8.6
COPY --from=build-env /opt/selenium-example/ /opt/selenium-example/ 
COPY --from=build-env  /root/.stack /root/.stack
ADD . /opt/selenium-example/ 
RUN cd /opt/selenium-example && stack build --copy-bins .
RUN mv /root/.local/bin/selenium-example-exe /bin/

RUN ["chmod", "+x", "/opt/selenium-example/wait-for-it.sh"]

# CMD bin/selenium-example-exe
CMD /opt/selenium-example/wait-for-it.sh www.google.com:80 -- echo "google is up"
