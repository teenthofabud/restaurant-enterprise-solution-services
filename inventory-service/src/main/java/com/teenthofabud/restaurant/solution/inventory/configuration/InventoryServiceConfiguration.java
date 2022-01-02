package com.teenthofabud.restaurant.solution.inventory.configuration;

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
@EnableJpaRepositories(basePackages = { "com.teenthofabud.restaurant.solution.inventory.category.repository",
        "com.teenthofabud.restaurant.solution.inventory.product.repository",
        "com.teenthofabud.restaurant.solution.inventory.quantity.repository"})
@EnableTransactionManagement
public class InventoryServiceConfiguration {

    @Profile("!test")
    @Bean
    public OpenAPI menuServiceAPI(@Value("${spring.application.name}") String applicationName,
                                      @Value("${res.inventory.description}") String applicationDescription,
                                      @Value("${res.inventory.version}") String applicationVersion) {
        return new OpenAPI()
                .info(new Info().title(applicationName)
                        .description(applicationDescription)
                        .version(applicationVersion));
    }

}
