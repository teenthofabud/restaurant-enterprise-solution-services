package com.teenthofabud.restaurant.solution.engagement.configuration;

import com.teenthofabud.restaurant.solution.engagement.checkin.service.impl.ReservationServiceImpl;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.impl.WalkInServiceImpl;
import com.teenthofabud.restaurant.solution.engagement.handler.EngagementRequestMappingHandlerMapping;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration;
import org.springframework.cloud.netflix.eureka.EnableEurekaClient;
import org.springframework.context.annotation.*;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.format.support.FormattingConversionService;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.web.accept.ContentNegotiationManager;
import org.springframework.web.servlet.config.annotation.DelegatingWebMvcConfiguration;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;
import org.springframework.web.servlet.resource.ResourceUrlProvider;

@Configuration
@EnableEurekaClient
@EnableJpaAuditing(auditorAwareRef = "baseAuditPropertyHandler")
@EnableJpaRepositories(basePackages = {"com.teenthofabud.restaurant.solution.engagement.checkin.repository" })
@EnableTransactionManagement
public class EngagementServiceConfiguration extends DelegatingWebMvcConfiguration {

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


    @Override
    protected RequestMappingHandlerMapping createRequestMappingHandlerMapping() {
        return new EngagementRequestMappingHandlerMapping();
    }

    @Bean
    @Primary
    @Override
    public RequestMappingHandlerMapping requestMappingHandlerMapping(ContentNegotiationManager contentNegotiationManager, FormattingConversionService conversionService, ResourceUrlProvider resourceUrlProvider) {
        return super.requestMappingHandlerMapping(contentNegotiationManager, conversionService, resourceUrlProvider);
    }

    @Configuration
    public static class UnconditionalWebMvcAutoConfiguration extends WebMvcAutoConfiguration {//forces @EnableWebMvc
    }

}
