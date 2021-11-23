package com.teenthofabud.restaurant.solution.menu.configuration;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.netflix.eureka.EnableEurekaClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@Configuration
@EnableEurekaClient
@EnableJpaAuditing
@EnableJpaRepositories(basePackages = { "com.teenthofabud.restaurant.solution.menu.category.repository",
        "com.teenthofabud.restaurant.solution.menu.item.repository",
        "com.teenthofabud.restaurant.solution.menu.price.repository" })
@EnableTransactionManagement
public class MenuServiceConfiguration {

    @Profile("!test")
    @Bean
    public OpenAPI menuServiceAPI(@Value("${spring.application.name}") String applicationName,
                                      @Value("${res.menu.description}") String applicationDescription,
                                      @Value("${res.menu.version}") String applicationVersion) {
        return new OpenAPI()
                .info(new Info().title(applicationName)
                        .description(applicationDescription)
                        .version(applicationVersion));
    }

}
