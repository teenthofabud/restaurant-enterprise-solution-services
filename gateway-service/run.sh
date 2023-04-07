java -jar \
-DGATEWAY_SPRING_PROFILES_ACTIVE=local \
-DGATEWAY_EUREKA_CLIENT_ENABLED=true \
-DGATEWAY_SPRING_CLOUD_CONFIG_ENABLED=false \
-DGATEWAY_ZIPKIN_CLIENT_ENABLED=false \
-DGATEWAY_LOGSTASH_ENABLED=false \
-DGATEWAY_SWAGGER_ENABLED=true \
-DGATEWAY_LOGBACK_LOGGING_LEVEL=info \
-DGATEWAY_SERVER_PORT=8083 \
-DGATEWAY_LOGSTASH_HOST=localhost \
-DGATEWAY_LOGSTASH_PORT=4560 \
-DGATEWAY_SPRING_CLOUD_CONFIG_HOST=localhost \
-DGATEWAY_SPRING_CLOUD_CONFIG_PORT=8888 \
-DGATEWAY_LOGGING_LEVEL=INFO \
-DGATEWAY_DATABASE_HOST=localhost \
-DGATEWAY_DATABASE_PORT=3306 \
-DGATEWAY_EUREKA_HOST=localhost \
-DGATEWAY_EUREKA_PORT=8761 \
-DGATEWAY_GATEWAY_HOST=localhost \
-DGATEWAY_GATEWAY_PORT=8081 \
-DGATEWAY_FEIGN_LOGGING_LEVEL=basic \
-DGATEWAY_CIRCUIT_BREAKER_ENABLED=true \
target/gateway-app.jar