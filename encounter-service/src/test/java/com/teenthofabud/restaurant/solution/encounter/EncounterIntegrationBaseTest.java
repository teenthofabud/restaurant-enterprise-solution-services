package com.teenthofabud.restaurant.solution.encounter;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.specto.hoverfly.junit.core.Hoverfly;
import io.specto.hoverfly.junit.core.HoverflyConfig;
import io.specto.hoverfly.junit.core.HoverflyMode;
import io.specto.hoverfly.junit.core.SimulationSource;
import io.specto.hoverfly.junit.core.config.LocalHoverflyConfig;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.test.web.servlet.MockMvc;

import java.net.URISyntaxException;
import java.nio.file.Path;
import java.nio.file.Paths;

public abstract class EncounterIntegrationBaseTest {

    protected ObjectMapper om;
    protected MockMvc mockMvc;
    private Hoverfly hoverfly;
    private Integer integrationGatewayPort;

    @Value("${res.encounter.integration.gateway.port}")
    public void setIntegrationGatewayPort(Integer integrationGatewayPort) {
        this.integrationGatewayPort = integrationGatewayPort;
    }

    @Autowired
    public void setObjectMapper(ObjectMapper objectMapper) {
        this.om = objectMapper;
    }

    @Autowired
    public void setMockMvc(MockMvc mockMvc) {
        this.mockMvc = mockMvc;
    }

    @BeforeAll
    private void setUp() throws URISyntaxException {

        LocalHoverflyConfig localHoverflyConfig = HoverflyConfig.localConfigs();
        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        Path simulationBaseLocationPath = Paths.get(classLoader.getResource("integration").toURI());

        localHoverflyConfig
                .addCommands("-response-body-files-path", simulationBaseLocationPath.toString())
                .disableTlsVerification()
                .asWebServer()
                .proxyPort(integrationGatewayPort);

        hoverfly = new Hoverfly(localHoverflyConfig, HoverflyMode.SIMULATE);
        hoverfly.start();
        Path simulationFilePath = Paths.get(simulationBaseLocationPath.toString(), "simulation.json");
        SimulationSource simulationSource = SimulationSource.file(simulationFilePath);
        hoverfly.simulate(simulationSource);
    }

    @AfterAll
    private void tearDown() {
        if(hoverfly != null) {
            hoverfly.close();
        }
    }

}
