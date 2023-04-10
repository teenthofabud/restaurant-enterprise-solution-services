package com.teenthofabud.restaurant.solution.engagement.configuration;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.impl.ReservationServiceImpl;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.impl.WalkInServiceImpl;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.cloud.netflix.eureka.EnableEurekaClient;
import org.springframework.context.annotation.*;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

@Configuration
@EnableEurekaClient
public class EngagementServiceConfiguration {

    private String engagementAuditTimestampFormat;
    @Value("${res.engagement.audit.timestamp.format}")
    public void setEngagementAuditTimestampFormat(String engagementAuditTimestampFormat) {
        this.engagementAuditTimestampFormat = engagementAuditTimestampFormat;
    }

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

    @Bean
    public ObjectMapper objectMapper() {
        Jdk8Module jdk8Module = new Jdk8Module();
        JavaTimeModule javaTimeModule = new JavaTimeModule();
        LocalDateTimeDeserializer localDateTimeDeserializer = new LocalDateTimeDeserializer(DateTimeFormatter.ofPattern(engagementAuditTimestampFormat));
        javaTimeModule.addDeserializer(LocalDateTime.class, localDateTimeDeserializer);
        ObjectMapper objectMapper = Jackson2ObjectMapperBuilder.json()
                .modules(javaTimeModule, jdk8Module)
                .featuresToDisable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS)
                .build();
        return objectMapper;
    }

}
