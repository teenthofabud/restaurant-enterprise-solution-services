FROM adoptopenjdk:11-jre-hotspot-focal
MAINTAINER Anirban Das <anirbandas18@live.com>
RUN mkdir ~p -p /opt/restaurant-enterprise-solutions/restaurant-solution-naming-service
COPY target/*.jar /opt/restaurant-enterprise-solutions/restaurant-solution-naming-service/naming-app.jar
EXPOSE 8761
CMD ["java", "-jar", \
"-DNAMING_SERVER_PORT=${PORT}", \
"-DNAMING_LOGSTASH_ENABLED=${LOGSTASH_ENABLED}", \
"-DLOGBACK_LOGGING_LEVEL=${LOGGING_LEVEL}", \
"-DNAMING_LOGSTASH_HOST=${LOGSTASH_HOST}", \
"-DNAMING_LOGSTASH_PORT=${LOGSTASH_PORT}", \
"/opt/restaurant-enterprise-solutions/restaurant-solution-naming-service/naming-app.jar"]