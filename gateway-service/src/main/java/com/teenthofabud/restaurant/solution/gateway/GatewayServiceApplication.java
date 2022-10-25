package com.teenthofabud.restaurant.solution.gateway;

//import com.teenthofabud.core.common.configuration.TOABAutoConfiguration;
//import com.teenthofabud.core.common.configuration.TOABMongoAutoConfiguration;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.netflix.eureka.EnableEurekaClient;

@SpringBootApplication//(exclude = {TOABAutoConfiguration.class, TOABMongoAutoConfiguration.class})
@EnableEurekaClient
public class GatewayServiceApplication {
    public static void main(String[] args) {
        SpringApplication.run(GatewayServiceApplication.class);
    }
}