package com.teenthofabud.restaurant.solution.customer.configuration;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.netflix.eureka.EnableEurekaClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@Configuration
@EnableEurekaClient
@EnableJpaAuditing
@EnableJpaRepositories(basePackages = { "com.teenthofabud.restaurant.solution.customer.account.repository",
        "com.teenthofabud.restaurant.solution.customer.address.repository" })
@EnableTransactionManagement
public class CustomerServiceConfiguration {

    private String applicationName;
    private String applicationDescription;
    private String applicationVersion;

    @Value("${spring.application.name}")
    public void setApplicationName(String applicationName) {
        this.applicationName = applicationName;
    }

    @Value("${res.customer.description}")
    public void setApplicationDescription(String applicationDescription) {
        this.applicationDescription = applicationDescription;
    }

    @Value("${res.customer.version}")
    public void setApplicationVersion(String applicationVersion) {
        this.applicationVersion = applicationVersion;
    }

    @Bean
    public OpenAPI customerServiceAPI() {
        return new OpenAPI()
                .info(new Info().title(applicationName)
                        .description(applicationDescription)
                        .version(applicationVersion));
    }

}
