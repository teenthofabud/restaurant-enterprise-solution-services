FROM adoptopenjdk:11-jre-hotspot-focal
MAINTAINER Anirban Das <anirbandas18@live.com>
RUN mkdir ~p -p /opt/restaurant-enterprise-solutions/restaurant-solution-inventory-service
COPY target/*.jar /opt/restaurant-enterprise-solutions/restaurant-solution-inventory-service/inventory-app.jar
EXPOSE 15001
CMD ["java", "-jar", \
"-DINVENTORY_SPRING_PROFILES_ACTIVE=${SPRING_PROFILES_ACTIVE}", \
"-DINVENTORY_EUREKA_CLIENT_ENABLED=${EUREKA_CLIENT_ENABLED}", \
"-DINVENTORY_ZIPKIN_CLIENT_ENABLED=${ZIPKIN_CLIENT_ENABLED}", \
"-DINVENTORY_SWAGGER_ENABLED=${SWAGGER_ENABLED}", \
"-DINVENTORY_LOGSTASH_ENABLED=${LOGSTASH_ENABLED}", \
"-DINVENTORY_LOGBACK_LOGGING_LEVEL=${LOGBACK_LOGGING_LEVEL}", \
"-DINVENTORY_SPRING_CLOUD_CONFIG_ENABLED=${SPRING_CLOUD_CONFIG_ENABLED}", \
"-DINVENTORY_LOGSTASH_HOST=${LOGSTASH_HOST}", \
"-DINVENTORY_LOGSTASH_PORT=${LOGSTASH_PORT}", \
"-DINVENTORY_SPRING_CLOUD_CONFIG_HOST=${SPRING_CLOUD_CONFIG_HOST}", \
"-DINVENTORY_SPRING_CLOUD_CONFIG_PORT=${SPRING_CLOUD_CONFIG_PORT}", \
"-DINVENTORY_LOGGING_LEVEL=${LOGGING_LEVEL}", \
"-DINVENTORY_DATABASE_HOST=${DATABASE_HOST}", \
"-DINVENTORY_DATABASE_PORT=${DATABASE_PORT}", \
"-DINVENTORY_EUREKA_HOST=${EUREKA_HOST}", \
"-DINVENTORY_EUREKA_PORT=${EUREKA_PORT}", \
"-DINVENTORY_ZIPKIN_HOST=${ZIPKIN_HOST}", \
"-DINVENTORY_ZIPKIN_PORT=${ZIPKIN_PORT}", \
"-DINVENTORY_GATEWAY_HOST=${GATEWAY_HOST}", \
"-DINVENTORY_GATEWAY_PORT=${GATEWAY_PORT}", \
"-DINVENTORY_PORT=${PORT}", \
"-DINVENTORY_FEIGN_LOGGING_LEVEL=${FEIGN_LOGGING_LEVEL}", \
"-DINVENTORY_METADATA_GENDER_FEIGN_LOGGING_LEVEL=${METADATA_GENDER_FEIGN_LOGGING_LEVEL}", \
"-DINVENTORY_CIRCUIT_BREAKER_ENABLED=${CIRCUIT_BREAKER_ENABLED}", \
"/opt/restaurant-enterprise-solutions/restaurant-solution-inventory-service/inventory-app.jar"]