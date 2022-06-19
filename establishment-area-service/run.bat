java -jar ^
-DESTABLISHMENT_AREA_SPRING_PROFILES_ACTIVE=local ^
-DESTABLISHMENT_AREA_EUREKA_CLIENT_ENABLED=true ^
-DESTABLISHMENT_AREA_SPRING_CLOUD_CONFIG_ENABLED=false ^
-DESTABLISHMENT_AREA_ZIPKIN_CLIENT_ENABLED=false ^
-DESTABLISHMENT_AREA_LOGSTASH_ENABLED=false ^
-DESTABLISHMENT_AREA_SWAGGER_ENABLED=true ^
-DESTABLISHMENT_AREA_LOGBACK_LOGGING_LEVEL=DEBUG ^
-DESTABLISHMENT_AREA_SERVER_PORT=10002 ^
-DESTABLISHMENT_AREA_LOGSTASH_HOST=localhost ^
-DESTABLISHMENT_AREA_LOGSTASH_PORT=4560 ^
-DESTABLISHMENT_AREA_SPRING_CLOUD_CONFIG_HOST=localhost ^
-DESTABLISHMENT_AREA_SPRING_CLOUD_CONFIG_PORT=8888 ^
-DESTABLISHMENT_AREA_LOGGING_LEVEL=DEBUG ^
-DESTABLISHMENT_AREA_DATABASE_HOST=localhost ^
-DESTABLISHMENT_AREA_DATABASE_PORT=3306 ^
-DESTABLISHMENT_AREA_EUREKA_HOST=localhost ^
-DESTABLISHMENT_AREA_EUREKA_PORT=8761 ^
-DESTABLISHMENT_AREA_ZIPKIN_HOST=localhost ^
-DESTABLISHMENT_AREA_ZIPKIN_PORT=9411 ^
target\establishment-area-app.jar