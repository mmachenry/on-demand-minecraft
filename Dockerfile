FROM haskell:8 as build
WORKDIR /opt/build
COPY StopServerWithNoPlayers.hs /opt/build/
RUN stack --resolver lts-12.5 --install-ghc ghc -- -optl-static StopServerWithNoPlayers.hs

FROM itzg/minecraft-server
ENV EULA TRUE
COPY log4j2.xml /
ENV JVM_OPTS -Dlog4j.configurationFile=/log4j2.xml

COPY conditional-shutdown.* /etc/systemd/system/
COPY --from=build /opt/build/StopServerWithNoPlayers /usr/local/bin/
RUN chmod 755 /usr/local/bin/StopServerWithNoPlayers
