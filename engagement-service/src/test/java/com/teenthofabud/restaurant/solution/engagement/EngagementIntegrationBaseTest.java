package com.teenthofabud.restaurant.solution.engagement;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.constants.CheckInType;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.WalkInEntity;
import com.teenthofabud.restaurant.solution.engagement.integration.customer.data.AccountVo;
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
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

public abstract class EngagementIntegrationBaseTest {

    protected ObjectMapper objectMapper;
    protected MockMvc mockMvc;
    private Hoverfly hoverfly;

    @Autowired
    public void setObjectMapper(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

    @Autowired
    public void setMockMvc(MockMvc mockMvc) {
        this.mockMvc = mockMvc;
    }

    @BeforeAll
    private void setUp() throws URISyntaxException {
        LocalHoverflyConfig localHoverflyConfig = HoverflyConfig.localConfigs();

        Optional<String> optSimulationBaseLocation = Optional.empty();
        Optional<Integer> optServicePort = Optional.empty();
        Optional<String[]> optSimulationFilePaths = Optional.empty();

        try {
            optSimulationBaseLocation = Optional.of(getSimulationBaseLocation());
            optServicePort = Optional.of(getServicePort());
            optSimulationFilePaths = Optional.of(getSimulationFilePaths());
        }  catch (UnsupportedOperationException e) {

        }

        if(optSimulationBaseLocation.isPresent() && optServicePort.isPresent() && optSimulationFilePaths.isPresent()) {
            ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
            Path simulationBaseLocationPath = Paths.get(classLoader.getResource(optSimulationBaseLocation.get()).toURI());

            localHoverflyConfig
                    .addCommands("-response-body-files-path", simulationBaseLocationPath.toString())
                    .disableTlsVerification()
                    .asWebServer()
                    .proxyPort(optServicePort.get());

            hoverfly = new Hoverfly(localHoverflyConfig, HoverflyMode.SIMULATE);
            hoverfly.start();
            String[] simulationFilePaths = optSimulationFilePaths.get();
            if (simulationFilePaths != null && simulationFilePaths.length > 0) {
                List<SimulationSource> simulationSources = Arrays.stream(simulationFilePaths)
                        .map(sfp -> SimulationSource.classpath(sfp))
                        .collect(Collectors.toList());
                hoverfly.simulate(simulationSources.get(0), simulationSources.subList(1,
                        simulationSources.size()).toArray(new SimulationSource[simulationSources.size() - 1]));
            }
        }

    }

    public abstract String getSimulationBaseLocation() throws UnsupportedOperationException;

    public abstract Integer getServicePort() throws UnsupportedOperationException;

    public abstract String[] getSimulationFilePaths() throws UnsupportedOperationException;

    @AfterAll
    private void tearDown() {
        if(hoverfly != null) {
            hoverfly.close();
        }
    }

    protected AccountVo accountVo(String id, String firstName, String lastName, boolean active) {
        AccountVo accountVo = new AccountVo();
        accountVo.setActive(active);
        accountVo.setId(id);
        accountVo.setFirstName(firstName);
        accountVo.setLastName(lastName);
        return accountVo;
    }

    protected WalkInEntity walkInEntity(String accountId, String sequence, Integer noOfPersons, String notes, boolean active, String name, String phoneNumber, String emailId) {
        WalkInEntity walkInEntity = new WalkInEntity();
        walkInEntity.setName(name);
        walkInEntity.setSequence(sequence);
        walkInEntity.setPhoneNumber(phoneNumber);
        walkInEntity.setEmailId(emailId);
        walkInEntity.setNotes(notes);
        walkInEntity.setNoOfPersons(noOfPersons);
        walkInEntity.setAccountId(accountId);
        walkInEntity.setActive(active);
        walkInEntity.setType(CheckInType.WALK_IN);
        return walkInEntity;
    }

    protected ReservationEntity reservationEntity(String accountId, String sequence, Integer noOfPersons, String notes, boolean active, LocalDate date, LocalTime time) {
        ReservationEntity reservationEntity = new ReservationEntity();
        reservationEntity.setTime(time);
        reservationEntity.setSequence(sequence);
        reservationEntity.setDate(date);
        reservationEntity.setNotes(notes);
        reservationEntity.setNoOfPersons(noOfPersons);
        reservationEntity.setAccountId(accountId);
        reservationEntity.setActive(active);
        reservationEntity.setType(CheckInType.RESERVATION);
        return reservationEntity;
    }

    protected String name() {
        return String.join(" ", "Name", UUID.randomUUID().toString());
    }

    protected String phoneNumber() {
        return String.valueOf(System.nanoTime());
    }

    protected String emailId() {
        return String.join("@", String.valueOf(System.nanoTime()), "email.com");
    }


}
