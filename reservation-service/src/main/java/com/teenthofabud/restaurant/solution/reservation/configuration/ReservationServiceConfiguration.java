package com.teenthofabud.restaurant.solution.reservation.configuration;

import com.teenthofabud.core.common.repository.TOABBaseMongoRepositoryImpl;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.netflix.eureka.EnableEurekaClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.data.mongodb.config.EnableMongoAuditing;
import org.springframework.data.mongodb.repository.config.EnableMongoRepositories;

@Configuration
@EnableEurekaClient
@EnableMongoAuditing
@EnableMongoRepositories(basePackages = {"com.teenthofabud.restaurant.solution.reservation.category.repository",
        "com.teenthofabud.restaurant.solution.reservation.booking.repository",
        "com.teenthofabud.restaurant.solution.reservation.engagement.repository"},
        repositoryBaseClass = TOABBaseMongoRepositoryImpl.class)
public class ReservationServiceConfiguration {

    @Profile("!test")
    @Bean
    public OpenAPI menuServiceAPI(@Value("${spring.application.name}") String applicationName,
                                      @Value("${res.reservation.description}") String applicationDescription,
                                      @Value("${res.reservation.version}") String applicationVersion) {
        return new OpenAPI()
                .info(new Info().title(applicationName)
                        .description(applicationDescription)
                        .version(applicationVersion));
    }

}
