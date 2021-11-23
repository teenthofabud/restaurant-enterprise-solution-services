#!/usr/bin/env zsh
java \
-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:6331 \
-jar \
-DMENU_SPRING_PROFILES_ACTIVE=development \
-DMENU_EUREKA_CLIENT_ENABLED=true \
-DMENU_SPRING_CLOUD_CONFIG_ENABLED=true \
-DMENU_ZIPKIN_CLIENT_ENABLED=false \
-DMENU_LOGSTASH_ENABLED=false \
-DMENU_SWAGGER_ENABLED=true \
-DMENU_LOGBACK_LOGGING_LEVEL=info \
-DMENU_LOGSTASH_HOST=localhost \
-DMENU_LOGSTASH_PORT=4560 \
-DMENU_SPRING_CLOUD_CONFIG_HOST=localhost \
-DMENU_SPRING_CLOUD_CONFIG_PORT=8888 \
-DMENU_LOGGING_LEVEL=INFO \
-DMENU_DATABASE_HOST=localhost \
-DMENU_DATABASE_PORT=3306 \
-DMENU_EUREKA_HOST=localhost \
-DMENU_EUREKA_PORT=8761 \
-DMENU_ZIPKIN_HOST=localhost \
-DMENU_ZIPKIN_PORT=9411 \
-DMENU_GATEWAY_HOST=localhost \
-DMENU_GATEWAY_PORT=8081 \
-DMENU_FEIGN_LOGGING_LEVEL=basic \
-DMENU_METADATA_GENDER_FEIGN_LOGGING_LEVEL=full \
-DMENU_CIRCUIT_BREAKER_ENABLED=true \
target/menu-app.jar