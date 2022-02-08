package com.teenthofabud.restaurant.solution.booking.configuration;

import com.teenthofabud.core.common.factory.TOABFeignErrorDecoderFactory;
import feign.codec.ErrorDecoder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@Slf4j
@EnableFeignClients(basePackages = {"com.teenthofabud.restaurant.solution.booking.integration.establishmentarea.proxy",
        "com.teenthofabud.restaurant.solution.booking.integration.customer.proxy"})
public class BookingIntegrationConfiguration {

    private ApplicationContext applicationContext;

    @Autowired
    public void setApplicationContext(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }

    @Bean
    public ErrorDecoder errorDecoder() {
        String[] feignBasePackages = { "com.teenthofabud.restaurant.solution.booking.integration.establishmentarea.proxy",
                "com.teenthofabud.restaurant.solution.booking.integration.customer.proxy" };
        return new TOABFeignErrorDecoderFactory(applicationContext, feignBasePackages);
    }
}
