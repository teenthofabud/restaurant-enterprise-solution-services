package com.teenthofabud.restaurant.solution.establishmentarea;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.web.servlet.MockMvc;

public abstract class EstablishmentAreaIntegrationBaseTest {

    protected ObjectMapper om;
    protected MockMvc mockMvc;

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setMockMvc(MockMvc mockMvc) {
        this.mockMvc = mockMvc;
    }
}
