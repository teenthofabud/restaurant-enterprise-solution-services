FROM adoptopenjdk:11-jre-hotspot-focal
MAINTAINER Anirban Das <anirbandas18@live.com>
RUN mkdir ~p -p /opt/restaurant-enterprise-solutions/restaurant-solution-configuration-service
COPY target/*.jar /opt/restaurant-enterprise-solutions/restaurant-solution-configuration-service/configuration-app.jar
EXPOSE 10001
CMD ["java", "-jar", \
"-DCONFIGURATION_SERVER_PORT=${PORT}", \
"-DCONFIGURATION_LOGSTASH_ENABLED=${LOGSTASH_ENABLED}", \
"-DLOGBACK_LOGGING_LEVEL=${LOGGING_LEVEL}", \
"-DCONFIGURATION_LOGSTASH_HOST=${LOGSTASH_HOST}", \
"-DCONFIGURATION_LOGSTASH_PORT=${LOGSTASH_PORT}", \
"-DCONFIGURATION_EUREKA_CLIENT_ENABLED=${EUREKA_CLIENT_ENABLED}", \
"-DCONFIGURATION_EUREKA_HOST=${EUREKA_HOST}", \
"-DCONFIGURATION_EUREKA_PORT=${EUREKA_PORT}", \
"/opt/restaurant-enterprise-solutions/restaurant-solution-configuration-service/configuration-app.jar"]