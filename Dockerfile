FROM itzg/minecraft-server
ENV EULA TRUE
COPY log4j2.xml /
ENV JVM_OPTS -Dlog4j.configurationFile=/log4j2.xml
