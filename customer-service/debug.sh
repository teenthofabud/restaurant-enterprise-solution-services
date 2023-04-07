#!/usr/bin/env zsh
java \
-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:6333 \
-jar \
-DCUSTOMER_SPRING_PROFILES_ACTIVE=local \
-DCUSTOMER_EUREKA_CLIENT_ENABLED=true \
-DCUSTOMER_SPRING_CLOUD_CONFIG_ENABLED=false \
-DCUSTOMER_ZIPKIN_CLIENT_ENABLED=false \
-DCUSTOMER_LOGSTASH_ENABLED=false \
-DCUSTOMER_SWAGGER_ENABLED=true \
-DCUSTOMER_LOGBACK_LOGGING_LEVEL=info \
-DCUSTOMER_LOGSTASH_HOST=localhost \
-DCUSTOMER_LOGSTASH_PORT=4560 \
-DCUSTOMER_SPRING_CLOUD_CONFIG_HOST=localhost \
-DCUSTOMER_SPRING_CLOUD_CONFIG_PORT=8888 \
-DCUSTOMER_LOGGING_LEVEL=INFO \
-DCUSTOMER_SERVER_PORT=11001 \
-DCUSTOMER_DATABASE_HOST=localhost \
-DCUSTOMER_DATABASE_PORT=3306 \
-DCUSTOMER_EUREKA_HOST=localhost \
-DCUSTOMER_EUREKA_PORT=8761 \
-DCUSTOMER_ZIPKIN_HOST=localhost \
-DCUSTOMER_ZIPKIN_PORT=9411 \
-DCUSTOMER_GATEWAY_HOST=localhost \
-DCUSTOMER_GATEWAY_PORT=8081 \
-DCUSTOMER_FEIGN_LOGGING_LEVEL=basic \
-DCUSTOMER_METADATA_GENDER_FEIGN_LOGGING_LEVEL=full \
-DCUSTOMER_CIRCUIT_BREAKER_ENABLED=true \
target/customer-app.jar