package com.teenthofabud.restaurant.solution.encounter.configuration;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.teenthofabud.restaurant.solution.encounter.delivery.service.impl.DeliveryServiceImpl;
import com.teenthofabud.restaurant.solution.encounter.pickup.service.impl.PickUpServiceImpl;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.cloud.netflix.eureka.EnableEurekaClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.context.annotation.Scope;

@Configuration
@EnableEurekaClient
public class EncounterServiceConfiguration {

    @Profile("!test")
    @Bean
    public OpenAPI encounterServiceAPI(@Value("${spring.application.name}") String applicationName,
                                      @Value("${res.encounter.description}") String applicationDescription,
                                      @Value("${res.encounter.version}") String applicationVersion) {
        return new OpenAPI()
                .info(new Info().title(applicationName)
                        .description(applicationDescription)
                        .version(applicationVersion));
    }

    @Bean
    @Scope(BeanDefinition.SCOPE_PROTOTYPE)
    public DeliveryServiceImpl deliveryServiceImpl() {
        return new DeliveryServiceImpl();
    }

    @Bean
    @Scope(BeanDefinition.SCOPE_PROTOTYPE)
    public PickUpServiceImpl pickUpServiceImpl() {
        return new PickUpServiceImpl();
    }

    @Profile("test")
    @Bean
    public ObjectMapper om() {
        return new ObjectMapper();
    }

}
