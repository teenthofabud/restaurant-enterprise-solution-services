java -jar ^
-DBOOKING_SPRING_PROFILES_ACTIVE=development ^
-DBOOKING_EUREKA_CLIENT_ENABLED=true ^
-DBOOKING_SPRING_CLOUD_CONFIG_ENABLED=true ^
-DBOOKING_ZIPKIN_CLIENT_ENABLED=false ^
-DBOOKING_LOGSTASH_ENABLED=false ^
-DBOOKING_SWAGGER_ENABLED=true ^
-DBOOKING_LOGBACK_LOGGING_LEVEL=info ^
-DBOOKING_SERVER_PORT=17001 ^
-DBOOKING_LOGSTASH_HOST=localhost ^
-DBOOKING_LOGSTASH_PORT=4560 ^
-DBOOKING_SPRING_CLOUD_CONFIG_HOST=localhost ^
-DBOOKING_SPRING_CLOUD_CONFIG_PORT=8888 ^
-DBOOKING_LOGGING_LEVEL=INFO ^
-DBOOKING_DATABASE_HOST=localhost ^
-DBOOKING_DATABASE_PORT=3306 ^
-DBOOKING_EUREKA_HOST=localhost ^
-DBOOKING_EUREKA_PORT=8761 ^
-DBOOKING_ZIPKIN_HOST=localhost ^
-DBOOKING_ZIPKIN_PORT=9411 ^
-DBOOKING_GATEWAY_HOST=localhost ^
-DBOOKING_GATEWAY_PORT=8081 ^
-DBOOKING_FEIGN_LOGGING_LEVEL=basic ^
-DBOOKING_METADATA_GENDER_FEIGN_LOGGING_LEVEL=basic ^
-DBOOKING_CIRCUIT_BREAKER_ENABLED=true ^
target\booking-app.jar