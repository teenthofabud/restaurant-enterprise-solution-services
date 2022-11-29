package com.teenthofabud.restaurant.solution.engagement.configuration;

import org.springframework.context.annotation.*;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@Configuration
@EnableJpaAuditing(auditorAwareRef = "baseAuditPropertyHandler")
@EnableJpaRepositories(basePackages = {
        "com.teenthofabud.restaurant.solution.engagement.checkin.repository",
        "com.teenthofabud.restaurant.solution.engagement.tableallocation.repository"})
@EnableTransactionManagement
public class EngagementJPAConfiguration {

}
