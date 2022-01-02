#!/usr/bin/env zsh
java \
-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:6331 \
-jar \
-DINVENTORY_SPRING_PROFILES_ACTIVE=development \
-DINVENTORY_EUREKA_CLIENT_ENABLED=true \
-DINVENTORY_SPRING_CLOUD_CONFIG_ENABLED=true \
-DINVENTORY_ZIPKIN_CLIENT_ENABLED=false \
-DINVENTORY_LOGSTASH_ENABLED=false \
-DINVENTORY_SWAGGER_ENABLED=true \
-DINVENTORY_LOGBACK_LOGGING_LEVEL=info \
-DINVENTORY_LOGSTASH_HOST=localhost \
-DINVENTORY_LOGSTASH_PORT=4560 \
-DINVENTORY_SPRING_CLOUD_CONFIG_HOST=localhost \
-DINVENTORY_SPRING_CLOUD_CONFIG_PORT=8888 \
-DINVENTORY_LOGGING_LEVEL=INFO \
-DINVENTORY_DATABASE_HOST=localhost \
-DINVENTORY_DATABASE_PORT=3306 \
-DINVENTORY_EUREKA_HOST=localhost \
-DINVENTORY_EUREKA_PORT=8761 \
-DINVENTORY_ZIPKIN_HOST=localhost \
-DINVENTORY_ZIPKIN_PORT=9411 \
-DINVENTORY_GATEWAY_HOST=localhost \
-DINVENTORY_GATEWAY_PORT=8081 \
-DINVENTORY_FEIGN_LOGGING_LEVEL=basic \
-DINVENTORY_METADATA_GENDER_FEIGN_LOGGING_LEVEL=full \
-DINVENTORY_CIRCUIT_BREAKER_ENABLED=true \
target/inventory-app.jar