package com.teenthofabud.restaurant.solution.settings.configuration;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
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
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

@Configuration
@EnableEurekaClient
@EnableMongoAuditing
@EnableMongoRepositories(basePackages = { "com.teenthofabud.restaurant.solution.settings.paymentmethod.repository",
        "com.teenthofabud.restaurant.solution.settings.charge.repository",
        "com.teenthofabud.restaurant.solution.settings.discount.repository",
        "com.teenthofabud.restaurant.solution.settings.deliverypartner.repository",
        "com.teenthofabud.restaurant.solution.settings.template.repository",
        "com.teenthofabud.restaurant.solution.settings.device.repository" },
        repositoryBaseClass = TOABBaseMongoRepositoryImpl.class)
public class SettingsServiceConfiguration {

    @Profile("!test")
    @Bean
    public OpenAPI menuServiceAPI(@Value("${spring.application.name}") String applicationName,
                                      @Value("${res.settings.description}") String applicationDescription,
                                      @Value("${res.settings.version}") String applicationVersion) {
        return new OpenAPI()
                .info(new Info().title(applicationName)
                        .description(applicationDescription)
                        .version(applicationVersion));
    }

    /*@Bean
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
    }*/

}
