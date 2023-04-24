package com.teenthofabud.restaurant.solution.menu.configuration;

import com.teenthofabud.core.common.factory.TOABDataJPABeanFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@Configuration
@EnableTransactionManagement
@EnableJpaRepositories(
        basePackages = {
                "com.teenthofabud.restaurant.solution.menu.category",
                "com.teenthofabud.restaurant.solution.menu.item",
                "com.teenthofabud.restaurant.solution.menu.price"
        },
        entityManagerFactoryRef = "toabEntityManager",
        transactionManagerRef = "toabTransactionManager"
)
public class MenuJPAConfiguration {

        @Bean
        public TOABDataJPABeanFactory toabDataJPABeanFactory() {
                String[] packagesToScan = new String[] {
                        "com.teenthofabud.restaurant.solution.menu.category",
                        "com.teenthofabud.restaurant.solution.menu.item",
                        "com.teenthofabud.restaurant.solution.menu.price"
                };
                return new TOABDataJPABeanFactory(packagesToScan);
        }

}
