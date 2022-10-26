java -jar \
-DSETTINGS_SPRING_PROFILES_ACTIVE=development \
-DSETTINGS_EUREKA_CLIENT_ENABLED=true \
-DSETTINGS_SPRING_CLOUD_CONFIG_ENABLED=true \
-DSETTINGS_ZIPKIN_CLIENT_ENABLED=false \
-DSETTINGS_LOGSTASH_ENABLED=false \
-DSETTINGS_SWAGGER_ENABLED=true \
-DSETTINGS_LOGBACK_LOGGING_LEVEL=info \
-DSETTINGS_SERVER_PORT=13001 \
-DSETTINGS_LOGSTASH_HOST=localhost \
-DSETTINGS_LOGSTASH_PORT=4560 \
-DSETTINGS_SPRING_CLOUD_CONFIG_HOST=localhost \
-DSETTINGS_SPRING_CLOUD_CONFIG_PORT=8888 \
-DSETTINGS_LOGGING_LEVEL=INFO \
-DSETTINGS_DATABASE_HOST=localhost \
-DSETTINGS_DATABASE_PORT=3306 \
-DSETTINGS_EUREKA_HOST=localhost \
-DSETTINGS_EUREKA_PORT=8761 \
-DSETTINGS_ZIPKIN_HOST=localhost \
-DSETTINGS_ZIPKIN_PORT=9411 \
-DSETTINGS_GATEWAY_HOST=localhost \
-DSETTINGS_GATEWAY_PORT=8081 \
-DSETTINGS_FEIGN_LOGGING_LEVEL=basic \
-DSETTINGS_METADATA_GENDER_FEIGN_LOGGING_LEVEL=basic \
-DSETTINGS_CIRCUIT_BREAKER_ENABLED=true \
target/settings-app.jar