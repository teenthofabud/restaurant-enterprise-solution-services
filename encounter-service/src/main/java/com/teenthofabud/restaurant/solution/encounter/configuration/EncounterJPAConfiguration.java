package com.teenthofabud.restaurant.solution.encounter.configuration;

import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@Configuration
@EnableJpaAuditing(auditorAwareRef = "baseAuditPropertyHandler")
@EnableJpaRepositories(basePackages = {
        "com.teenthofabud.restaurant.solution.encounter.meeting.repository",
        "com.teenthofabud.restaurant.solution.encounter.pickup.repository",
        "com.teenthofabud.restaurant.solution.encounter.delivery.repository"})
@EnableTransactionManagement
public class EncounterJPAConfiguration {

}
