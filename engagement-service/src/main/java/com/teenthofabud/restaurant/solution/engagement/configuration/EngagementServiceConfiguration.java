package com.teenthofabud.restaurant.solution.engagement.configuration;

import com.teenthofabud.core.common.repository.TOABBaseMongoRepositoryImpl;
import com.teenthofabud.core.common.repository.TOABInheritanceAwareMongoRepositoryFactoryBean;
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
@EnableMongoRepositories(basePackages = {"com.teenthofabud.restaurant.solution.engagement.category.repository",
        "com.teenthofabud.restaurant.solution.engagement.booking.repository",
        "com.teenthofabud.restaurant.solution.engagement.engagement.repository"},
        repositoryBaseClass = TOABBaseMongoRepositoryImpl.class,
        repositoryFactoryBeanClass = TOABInheritanceAwareMongoRepositoryFactoryBean.class)
public class EngagementServiceConfiguration {

    @Profile("!test")
    @Bean
    public OpenAPI menuServiceAPI(@Value("${spring.application.name}") String applicationName,
                                      @Value("${res.engagement.description}") String applicationDescription,
                                      @Value("${res.engagement.version}") String applicationVersion) {
        return new OpenAPI()
                .info(new Info().title(applicationName)
                        .description(applicationDescription)
                        .version(applicationVersion));
    }

}
