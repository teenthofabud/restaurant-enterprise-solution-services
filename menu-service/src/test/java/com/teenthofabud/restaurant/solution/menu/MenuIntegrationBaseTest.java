package com.teenthofabud.restaurant.solution.menu;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import io.specto.hoverfly.junit.core.Hoverfly;
import io.specto.hoverfly.junit.core.HoverflyConfig;
import io.specto.hoverfly.junit.core.HoverflyMode;
import io.specto.hoverfly.junit.core.SimulationSource;
import io.specto.hoverfly.junit.core.config.LocalHoverflyConfig;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.web.servlet.MockMvc;

import java.net.URISyntaxException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public abstract class MenuIntegrationBaseTest {

    protected ObjectMapper om;
    protected MockMvc mockMvc;
    private Hoverfly hoverfly;

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setMockMvc(MockMvc mockMvc) {
        this.mockMvc = mockMvc;
    }

    @BeforeAll
    private void setUp() throws URISyntaxException {
        om.registerModule(new Jdk8Module());
        om.registerModule(new JavaTimeModule());

        LocalHoverflyConfig localHoverflyConfig = HoverflyConfig.localConfigs();

        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        Path simulationBaseLocationPath = Paths.get(classLoader.getResource(getSimulationBaseLocation()).toURI());

        localHoverflyConfig
                .addCommands("-response-body-files-path", simulationBaseLocationPath.toString())
                .disableTlsVerification()
                .asWebServer()
                .proxyPort(getServicePort());

        hoverfly = new Hoverfly(localHoverflyConfig, HoverflyMode.SIMULATE);
        hoverfly.start();
        String[] simulationFilePaths = getSimulationFilePaths();
        if(simulationFilePaths != null && simulationFilePaths.length > 0) {
            List<SimulationSource> simulationSources = Arrays.stream(simulationFilePaths)
                    .map(sfp -> SimulationSource.classpath(sfp))
                    .collect(Collectors.toList());
            hoverfly.simulate(simulationSources.get(0), simulationSources.subList(1,
                    simulationSources.size()).toArray(new SimulationSource[simulationSources.size() - 1]));
        }

    }

    public abstract String getSimulationBaseLocation();

    public abstract Integer getServicePort();

    public abstract String[] getSimulationFilePaths();

    @AfterAll
    private void tearDown() {
        if(hoverfly != null) {
            hoverfly.close();
        }
    }
}
