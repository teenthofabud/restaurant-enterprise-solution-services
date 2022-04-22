java -jar \
-DCOOKBOOK_SPRING_PROFILES_ACTIVE=local \
-DCOOKBOOK_EUREKA_CLIENT_ENABLED=true \
-DCOOKBOOK_SPRING_CLOUD_CONFIG_ENABLED=false \
-DCOOKBOOK_ZIPKIN_CLIENT_ENABLED=false \
-DCOOKBOOK_LOGSTASH_ENABLED=false \
-DCOOKBOOK_SWAGGER_ENABLED=true \
-DCOOKBOOK_LOGBACK_LOGGING_LEVEL=info \
-DCOOKBOOK_SERVER_PORT=16001 \
-DCOOKBOOK_LOGSTASH_HOST=localhost \
-DCOOKBOOK_LOGSTASH_PORT=4560 \
-DCOOKBOOK_SPRING_CLOUD_CONFIG_HOST=localhost \
-DCOOKBOOK_SPRING_CLOUD_CONFIG_PORT=8888 \
-DCOOKBOOK_LOGGING_LEVEL=INFO \
-DCOOKBOOK_DATABASE_HOST=localhost \
-DCOOKBOOK_DATABASE_PORT=3306 \
-DCOOKBOOK_EUREKA_HOST=localhost \
-DCOOKBOOK_EUREKA_PORT=8761 \
-DCOOKBOOK_ZIPKIN_HOST=localhost \
-DCOOKBOOK_ZIPKIN_PORT=9411 \
-DCOOKBOOK_GATEWAY_HOST=localhost \
-DCOOKBOOK_GATEWAY_PORT=8081 \
-DCOOKBOOK_FEIGN_LOGGING_LEVEL=basic \
-DCOOKBOOK_METADATA_GENDER_FEIGN_LOGGING_LEVEL=basic \
-DCOOKBOOK_CIRCUIT_BREAKER_ENABLED=true \
target/cookbook-app.jar