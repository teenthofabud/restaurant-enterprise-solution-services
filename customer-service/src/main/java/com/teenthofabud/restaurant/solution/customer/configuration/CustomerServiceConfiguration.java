package com.teenthofabud.restaurant.solution.customer.configuration;

import org.springframework.cloud.netflix.eureka.EnableEurekaClient;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@Configuration
@EnableEurekaClient
@EnableJpaAuditing
@EnableJpaRepositories(basePackages = { "com.teenthofabud.restaurant.solution.customer.account.repository" })
@EnableTransactionManagement
public class CustomerServiceConfiguration {
}
