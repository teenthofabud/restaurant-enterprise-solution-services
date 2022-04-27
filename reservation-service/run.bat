java -jar ^
-DRESERVATION_SPRING_PROFILES_ACTIVE=local ^
-DRESERVATION_EUREKA_CLIENT_ENABLED=false ^
-DRESERVATION_SPRING_CLOUD_CONFIG_ENABLED=false ^
-DRESERVATION_ZIPKIN_CLIENT_ENABLED=false ^
-DRESERVATION_LOGSTASH_ENABLED=false ^
-DRESERVATION_SWAGGER_ENABLED=true ^
-DRESERVATION_LOGBACK_LOGGING_LEVEL=info ^
-DRESERVATION_LOGSTASH_HOST=localhost ^
-DRESERVATION_LOGSTASH_PORT=4560 ^
-DRESERVATION_SPRING_CLOUD_CONFIG_HOST=localhost ^
-DRESERVATION_SPRING_CLOUD_CONFIG_PORT=8888 ^
-DRESERVATION_LOGGING_LEVEL=INFO ^
-DRESERVATION_DATABASE_HOST=localhost ^
-DRESERVATION_DATABASE_PORT=3306 ^
-DRESERVATION_EUREKA_HOST=localhost ^
-DRESERVATION_EUREKA_PORT=8761 ^
-DRESERVATION_ZIPKIN_HOST=localhost ^
-DRESERVATION_ZIPKIN_PORT=9411 ^
-DRESERVATION_GATEWAY_HOST=localhost ^
-DRESERVATION_GATEWAY_PORT=8081 ^
-DRESERVATION_FEIGN_LOGGING_LEVEL=basic ^
-DRESERVATION_METADATA_GENDER_FEIGN_LOGGING_LEVEL=basic ^
-DRESERVATION_CIRCUIT_BREAKER_ENABLED=true ^
target\reservation-app.jar