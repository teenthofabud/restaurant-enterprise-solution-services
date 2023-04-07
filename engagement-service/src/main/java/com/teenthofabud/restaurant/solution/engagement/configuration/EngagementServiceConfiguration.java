package com.teenthofabud.restaurant.solution.engagement.configuration;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.impl.ReservationServiceImpl;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.impl.WalkInServiceImpl;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.cloud.netflix.eureka.EnableEurekaClient;
import org.springframework.context.annotation.*;

@Configuration
@EnableEurekaClient
public class EngagementServiceConfiguration {

    @Profile("!test")
    @Bean
    public OpenAPI engagementServiceAPI(@Value("${spring.application.name}") String applicationName,
                                      @Value("${res.engagement.description}") String applicationDescription,
                                      @Value("${res.engagement.version}") String applicationVersion) {
        return new OpenAPI()
                .info(new Info().title(applicationName)
                        .description(applicationDescription)
                        .version(applicationVersion));
    }

    @Bean
    @Scope(BeanDefinition.SCOPE_PROTOTYPE)
    public WalkInServiceImpl walkInServiceImpl() {
        return new WalkInServiceImpl();
    }

    @Bean
    @Scope(BeanDefinition.SCOPE_PROTOTYPE)
    public ReservationServiceImpl reservationServiceImpl() {
        return new ReservationServiceImpl();
    }

    @Profile("test")
    @Bean
    public ObjectMapper om() {
        return new ObjectMapper();
    }

}
