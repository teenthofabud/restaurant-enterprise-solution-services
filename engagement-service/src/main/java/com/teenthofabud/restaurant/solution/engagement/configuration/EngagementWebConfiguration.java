package com.teenthofabud.restaurant.solution.engagement.configuration;

import com.teenthofabud.restaurant.solution.engagement.handler.EngagementRequestMappingHandlerMapping;
import org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration;
import org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory;
import org.springframework.boot.web.servlet.server.ServletWebServerFactory;
import org.springframework.context.annotation.*;
import org.springframework.format.support.FormattingConversionService;
import org.springframework.web.accept.ContentNegotiationManager;
import org.springframework.web.servlet.config.annotation.DelegatingWebMvcConfiguration;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;
import org.springframework.web.servlet.resource.ResourceUrlProvider;

@Configuration
public class EngagementWebConfiguration extends DelegatingWebMvcConfiguration {

    @Profile("test")
    @Bean
    public ServletWebServerFactory servletWebServerFactory() {
        return new TomcatServletWebServerFactory();
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
