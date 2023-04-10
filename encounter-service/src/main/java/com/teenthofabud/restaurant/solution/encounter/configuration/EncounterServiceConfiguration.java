package com.teenthofabud.restaurant.solution.encounter.configuration;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
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
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

@Configuration
@EnableEurekaClient
public class EncounterServiceConfiguration {

    private String encounterAuditTimestampFormat;

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

    @Bean
    public ObjectMapper om() {
        Jdk8Module jdk8Module = new Jdk8Module();
        JavaTimeModule javaTimeModule = new JavaTimeModule();
        LocalDateTimeDeserializer localDateTimeDeserializer = new LocalDateTimeDeserializer(DateTimeFormatter.ofPattern(encounterAuditTimestampFormat));
        javaTimeModule.addDeserializer(LocalDateTime.class, localDateTimeDeserializer);
        return Jackson2ObjectMapperBuilder.json()
                .modules(javaTimeModule, jdk8Module)
                .featuresToDisable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS)
                .build();
    }

    @Value("${res.encounter.audit.timestamp.format}")
    public void setEngagementAuditTimestampFormat(String encounterAuditTimestampFormat) {
        this.encounterAuditTimestampFormat = encounterAuditTimestampFormat;
    }
}
