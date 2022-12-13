package com.teenthofabud.restaurant.solution.encounter;

import com.teenthofabud.core.common.configuration.TOABMongoAutoConfiguration;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.security.servlet.UserDetailsServiceAutoConfiguration;
import org.springframework.cloud.netflix.eureka.EnableEurekaClient;

@SpringBootApplication(exclude = { TOABMongoAutoConfiguration.class, UserDetailsServiceAutoConfiguration.class })
@EnableEurekaClient
public class EncounterServiceApplication {
    public static void main(String[] args) {

        System.out.println(System.getProperty("ENCOUNTER_SPRING_CLOUD_CONFIG_ENABLED"));
        SpringApplication.run(EncounterServiceApplication.class);
    }
}
