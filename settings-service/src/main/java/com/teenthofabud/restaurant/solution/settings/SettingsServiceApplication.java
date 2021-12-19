package com.teenthofabud.restaurant.solution.settings;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.security.servlet.UserDetailsServiceAutoConfiguration;

@SpringBootApplication(exclude = { UserDetailsServiceAutoConfiguration.class })
public class SettingsServiceApplication {
    public static void main(String[] args) {
        SpringApplication.run(SettingsServiceApplication.class);
    }
}
