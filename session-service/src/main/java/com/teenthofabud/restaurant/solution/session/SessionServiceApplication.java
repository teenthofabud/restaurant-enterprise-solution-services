package com.teenthofabud.restaurant.solution.session;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.security.servlet.UserDetailsServiceAutoConfiguration;

@SpringBootApplication(exclude = { UserDetailsServiceAutoConfiguration.class })
public class SessionServiceApplication {
    public static void main(String[] args) {
        SpringApplication.run(SessionServiceApplication.class);
    }
}
