package com.teenthofabud.restaurant.solution.session.configuration;

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
@EnableMongoRepositories(basePackages = { "com.teenthofabud.restaurant.solution.session.experience.repository",
        "com.teenthofabud.restaurant.solution.session.association.repository"},
        repositoryBaseClass = TOABBaseMongoRepositoryImpl.class)
public class SessionServiceConfiguration {

    @Profile("!test")
    @Bean
    public OpenAPI menuServiceAPI(@Value("${spring.application.name}") String applicationName,
                                      @Value("${res.session.description}") String applicationDescription,
                                      @Value("${res.session.version}") String applicationVersion) {
        return new OpenAPI()
                .info(new Info().title(applicationName)
                        .description(applicationDescription)
                        .version(applicationVersion));
    }

}
