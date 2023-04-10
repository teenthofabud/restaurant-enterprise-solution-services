package com.teenthofabud.restaurant.solution.engagement.checkin;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.engagement.EngagementIntegrationBaseTest;
import com.teenthofabud.restaurant.solution.engagement.EngagementServiceApplication;
import com.teenthofabud.restaurant.solution.engagement.checkin.constants.CheckInType;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.WalkInEntity2VoConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.WalkInEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.WalkInForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.WalkInVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.WalkInRepository;
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import com.teenthofabud.restaurant.solution.engagement.integration.customer.data.AccountVo;
import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@ContextConfiguration(classes = { EngagementServiceApplication.class })
@TestMethodOrder(MethodOrderer.Alphanumeric.class)
public class WalkInIntegrationTest extends EngagementIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";
    private static final String WALK_IN_URI = "/checkIn/walkIn";
    private static final String WALK_IN_URI_BY_ID = String.join("/", WALK_IN_URI, "{id}");
    private static final String WALK_IN_URI_BY_SEQUENCE = String.join("/", WALK_IN_URI, "sequence", "{sequence}");
    private static final String WALK_IN_URI_PRIMARY_FILTER = String.join("/", WALK_IN_URI, "primaryFilter");
    private static final String WALK_IN_URI_SECONDARY_FILTER = String.join("/", WALK_IN_URI, "secondaryFilter");

    private WalkInRepository walkInRepository;

    private WalkInEntity2VoConverter walkInEntity2VoConverter;

    private Integer integrationPort;

    private String walkInTimeFormat;

    @Value("${res.engagement.checkIn.walkIn.timestamp.format}")
    public void setWalkInTimeFormat(String walkInTimeFormat) {
        this.walkInTimeFormat = walkInTimeFormat;
    }

    @Value("${res.engagement.integration.gateway.port}")
    public void setIntegrationPort(Integer integrationPort) {
        this.integrationPort = integrationPort;
    }

    @Autowired
    public void setWalkInRepository(WalkInRepository walkInRepository) {
        this.walkInRepository = walkInRepository;
    }

    @Autowired
    public void setWalkInEntity2VoConverter(WalkInEntity2VoConverter walkInEntity2VoConverter) {
        this.walkInEntity2VoConverter = walkInEntity2VoConverter;
    }

    private AccountVo accountVo1;
    private AccountVo accountVo2;
    private AccountVo accountVo22;
    private AccountVo accountVo4;

    private WalkInForm walkInForm;
    private WalkInVo walkInVo1;
    private WalkInVo walkInVo2;
    private WalkInVo walkInVo3;
    private WalkInVo walkInVo4;
    private WalkInVo walkInVo5;
    private WalkInVo walkInVo6;
    private WalkInEntity walkInEntity1;
    private WalkInEntity walkInEntity2;
    private WalkInEntity walkInEntity3;
    private WalkInEntity walkInEntity4;
    private WalkInEntity walkInEntity5;

    private List<PatchOperationForm> patches;

    /*private AccountVo accountVo(String id, String firstName, String lastName, boolean active) {
        AccountVo accountVo = new AccountVo();
        accountVo.setActive(active);
        accountVo.setId(id);
        accountVo.setFirstName(firstName);
        accountVo.setLastName(lastName);
        return accountVo;
    }

    private WalkInEntity walkInEntity(String accountId, String sequence, Integer noOfPersons, String notes, boolean active, String name, String phoneNumber, String emailId) {
        WalkInEntity walkInEntity = new WalkInEntity();
        walkInEntity.setName(name);
        walkInEntity.setSequence(sequence);
        walkInEntity.setPhoneNumber(phoneNumber);
        walkInEntity.setEmailId(emailId);
        walkInEntity.setNotes(notes);
        walkInEntity.setNoOfPersons(noOfPersons);
        walkInEntity.setAccountId(accountId);
        walkInEntity.setActive(active);
        return walkInEntity;
    }*/

    @BeforeEach
    private void init() {

        /**
         * Account
         */

        accountVo1 = this.accountVo("1", "Account 1", "Account 1", true);
        accountVo2 = this.accountVo("2", "Account 2", "Account 2", true);
        accountVo22 = this.accountVo("22", "Account 22", "Account 22", false);
        accountVo4 = this.accountVo("4", "Account 4", "Account 4", true);

        /**
         * WalkIn
         */

        walkInForm = new WalkInForm();
        walkInForm.setAccountId(accountVo4.getId());
        walkInForm.setNotes("New Notes");
        walkInForm.setName("New Name");
        walkInForm.setPhoneNumber("1234569781");
        walkInForm.setEmailId("new@email.com");
        walkInForm.setSequence(UUID.randomUUID().toString());
        walkInForm.setNoOfPersons(45);
        walkInForm.setType(CheckInType.WALK_IN.name());

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", "4"),
                new PatchOperationForm("replace", "/notes", "patched notes"));

        walkInEntity1 = this.walkInEntity(accountVo1.getId(), UUID.randomUUID().toString(), 2, "walkIn 1 notes", true, name(), phoneNumber(), emailId());
        walkInEntity1 = walkInRepository.save(walkInEntity1);
        walkInVo1 = this.walkInEntity2VoConverter.convert(walkInEntity1);

        walkInEntity2 = this.walkInEntity(accountVo2.getId(), UUID.randomUUID().toString(), 21, "walkIn 2 notes", true, name(), phoneNumber(), emailId());
        walkInEntity2 = walkInRepository.save(walkInEntity2);
        walkInVo2 = this.walkInEntity2VoConverter.convert(walkInEntity2);

        walkInEntity3 = this.walkInEntity(accountVo1.getId(), UUID.randomUUID().toString(), 28, "walkIn 3 notes", true, name(), phoneNumber(), emailId());
        walkInEntity3 = walkInRepository.save(walkInEntity3);
        walkInVo3 = this.walkInEntity2VoConverter.convert(walkInEntity3);

        walkInEntity4 = this.walkInEntity(accountVo22.getId(), UUID.randomUUID().toString(), 12, "walkIn 4 notes", false, name(), phoneNumber(), emailId());
        walkInEntity4 = walkInRepository.save(walkInEntity4);
        walkInVo4 = this.walkInEntity2VoConverter.convert(walkInEntity4);

        walkInEntity5 = this.walkInEntity(accountVo4.getId(), UUID.randomUUID().toString(), 244, "walkIn 5 notes", true, name(), phoneNumber(), emailId());
        walkInEntity5 = walkInRepository.save(walkInEntity5);
        walkInVo5 = this.walkInEntity2VoConverter.convert(walkInEntity5);

        walkInVo6 = new WalkInVo();
        walkInVo6.setActive(true);
        walkInVo6.setSequence(walkInForm.getSequence());
        walkInVo6.setCreatedOn(LocalDateTime.now());
        walkInVo6.setNotes(walkInForm.getNotes());
        walkInVo6.setNoOfPersons(walkInForm.getNoOfPersons());
        walkInVo6.setAccount(accountVo4);
        walkInVo6.setType(walkInForm.getType());
        walkInVo6.setName(walkInForm.getName());
        walkInVo6.setPhoneNumber(walkInForm.getPhoneNumber());
        walkInVo6.setEmailId(walkInForm.getEmailId());
        walkInVo6.setId("6");

    }

    @AfterEach
    private void destroy() {
        if(walkInEntity1 != null && walkInEntity1.getId() != null) {
            walkInRepository.deleteById(walkInEntity1.getId());
        }
        if(walkInEntity2 != null && walkInEntity2.getId() != null) {
            walkInRepository.deleteById(walkInEntity2.getId());
        }
        if(walkInEntity3 != null && walkInEntity3.getId() != null) {
            walkInRepository.deleteById(walkInEntity3.getId());
        }
        if(walkInEntity4 != null && walkInEntity4.getId() != null) {
            walkInRepository.deleteById(walkInEntity4.getId());
        }
        if(walkInEntity5 != null && walkInEntity5.getId() != null) {
            walkInRepository.deleteById(walkInEntity5.getId());
        }
    }

    @Test
    public void test_WalkIn_Post_ShouldReturn_201Response_And_NewWalkInId_WhenPosted_WithValidWalkInForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(WALK_IN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_WalkIn_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithEmptyAccountId(String accountId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "accountId";
        walkInForm.setAccountId(accountId);

        mvcResult = mockMvc.perform(post(WALK_IN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

    }

    @Test
    public void test_WalkIn_Post_ShouldReturn_500Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithInvalidAccountId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-CUST-001";
        String field = "id";
        String message = "invalid";
        walkInForm.setAccountId("r");

        mvcResult = mockMvc.perform(post(WALK_IN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_WalkIn_Post_ShouldReturn_500Response_And_ErrorCode_RES_CUST_003_WhenRequested_WithAbsentAccountId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-CUST-002";
        String field = "id";
        String message = "unavailable";
        walkInForm.setAccountId("3");

        mvcResult = mockMvc.perform(post(WALK_IN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_WalkIn_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithInactiveAccountId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "accountId";
        String message = "invalid";
        walkInForm.setAccountId("22");

        mvcResult = mockMvc.perform(post(WALK_IN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_WalkIn_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithEmptySequence(String sequence) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "sequence";
        walkInForm.setSequence(sequence);

        mvcResult = mockMvc.perform(post(WALK_IN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

    }

    @Test
    public void test_WalkIn_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithEmptyNoOfPersons() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "noOfPersons";
        String message = "invalid";
        walkInForm.setNoOfPersons(null);

        mvcResult = mockMvc.perform(post(WALK_IN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @ParameterizedTest
    @ValueSource(ints = { 0, -1 })
    public void test_WalkIn_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithInvalidNoOfPersons(Integer noOfPersons) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "noOfPersons";
        walkInForm.setNoOfPersons(noOfPersons);

        mvcResult = mockMvc.perform(post(WALK_IN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

    }

    @ParameterizedTest
    @ValueSource(strings = { "", "" })
    public void test_WalkIn_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithEmptyType(String type) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "type";
        String message = "invalid";
        walkInForm.setType(type);

        mvcResult = mockMvc.perform(post(WALK_IN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_WalkIn_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithInvalidType() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "type";
        walkInForm.setType("type");

        mvcResult = mockMvc.perform(post(WALK_IN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

    }

    @Test
    public void test_WalkIn_Post_ShouldReturn_201Response_And_NewWalkInId_WhenPosted_WithEmptyNotes() throws Exception {
        MvcResult mvcResult = null;
        walkInForm.setAccountId("2");
        walkInForm.setNotes("");

        mvcResult = mockMvc.perform(post(WALK_IN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", "" })
    public void test_WalkIn_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithEmptyName(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "name";
        String message = "invalid";
        walkInForm.setName(name);

        mvcResult = mockMvc.perform(post(WALK_IN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @ParameterizedTest
    @ValueSource(strings = { "", "" })
    public void test_WalkIn_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithEmptyPhoneNumber(String phoneNumber) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "phoneNumber";
        String message = "invalid";
        walkInForm.setPhoneNumber(phoneNumber);

        mvcResult = mockMvc.perform(post(WALK_IN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @ParameterizedTest
    @ValueSource(strings = { "", "" })
    public void test_WalkIn_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithEmptyEmailId(String emailId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "emailId";
        String message = "invalid";
        walkInForm.setEmailId(emailId);

        mvcResult = mockMvc.perform(post(WALK_IN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_WalkIn_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithInvalidEmailId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "emailId";
        walkInForm.setEmailId("emailId");

        mvcResult = mockMvc.perform(post(WALK_IN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));

    }

    @Test
    public void test_WalkIn_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithInvalidPhoneNumber() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "phoneNumber";
        walkInForm.setPhoneNumber("phoneNumber");

        mvcResult = mockMvc.perform(post(WALK_IN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));

    }

    @Test
    public void test_WalkIn_Post_ShouldReturn_409Response_And_ErrorCode_RES_ENGMNT_004_WhenPosted_WithDuplicateWalkIn() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_EXISTS.getErrorCode();
        String fieldAccountId = "accountId";
        String fieldSequence = "sequence";
        String message = "already exists";
        walkInForm.setAccountId(walkInEntity1.getAccountId());
        walkInForm.setSequence(walkInEntity1.getSequence());

        mvcResult = mockMvc.perform(post(WALK_IN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldSequence));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_WalkIn_Post_ShouldReturn_422Response_And_ErrorCode_RES_ENGMNT_003_WhenPosted_WithNoWalkInForm() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldAccountId = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(WALK_IN_URI)
                        .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_WalkIn_Get_ShouldReturn_200Response_And_WalkInListNaturallyOrdered_WhenRequested_ForAllWalkIns() throws Exception {
        MvcResult mvcResult = null;
        List<WalkInVo> walkInList = Arrays.asList(walkInVo3, walkInVo1, walkInVo2, walkInVo4, walkInVo5);

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_WalkIn_Get_Primary_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_EmptyAccountIdOnly(String accountId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "filters";

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_PRIMARY_FILTER).queryParam("accountId", accountId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_WalkIn_Get_Primary_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_EmptyNotesOnly(String notes) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "filters";

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_PRIMARY_FILTER).queryParam("notes", notes))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_WalkIn_Get_Primary_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_EmptySequenceOnly(String sequence) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "filters";

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_PRIMARY_FILTER).queryParam("sequence", sequence))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
    }

    @Test
    public void test_WalkIn_Get_Primary_ShouldReturn_200Response_And_EmptyWalkInList_WhenRequestedBy_AbsentAccountId() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_PRIMARY_FILTER).queryParam("accountId", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Primary_ShouldReturn_200Response_And_EmptyWalkInList_WhenRequestedBy_AbsentNotes() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_PRIMARY_FILTER).queryParam("notes", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Primary_ShouldReturn_200Response_And_EmptyWalkInList_WhenRequestedBy_AbsentSequence() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_PRIMARY_FILTER).queryParam("sequence", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Primary_ShouldReturn_200Response_And_WalkInListNaturallyOrdered_WhenRequested_ForWalkIns_WithAccountId() throws Exception {
        MvcResult mvcResult = null;
        List<WalkInVo> walkInList = new ArrayList<>(Arrays.asList(walkInVo5));

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_PRIMARY_FILTER)
                        .queryParam("accountId", "4"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Primary_ShouldReturn_200Response_And_WalkInListNaturallyOrdered_WhenRequested_ForWalkIns_WithNotes() throws Exception {
        MvcResult mvcResult = null;
        List<WalkInVo> walkInList = new ArrayList<>(Arrays.asList(walkInVo2));

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_PRIMARY_FILTER)
                        .queryParam("notes", "walkIn 2"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Primary_ShouldReturn_200Response_And_WalkInListNaturallyOrdered_WhenRequested_ForWalkIns_WithSequence() throws Exception {
        MvcResult mvcResult = null;
        List<WalkInVo> walkInList = new ArrayList<>(Arrays.asList(walkInVo2));

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_PRIMARY_FILTER)
                        .queryParam("sequence",  walkInVo2.getSequence()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Primary_ShouldReturn_200Response_And_WalkInListNaturallyOrdered_WhenRequested_ForWalkIns_WithAccountIdAndNotes() throws Exception {
        MvcResult mvcResult = null;
        Set<WalkInVo> walkInList = new TreeSet<>(Arrays.asList(walkInVo1));

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_PRIMARY_FILTER)
                        .queryParam("accountId", walkInVo1.getAccountId())
                        .queryParam("notes", "1 notes"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Primary_ShouldReturn_200Response_And_WalkInListNaturallyOrdered_WhenRequested_ForWalkIns_WithAccountIdAndSequence() throws Exception {
        MvcResult mvcResult = null;
        Set<WalkInVo> walkInList = new TreeSet<>(Arrays.asList(walkInVo1));

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_PRIMARY_FILTER)
                        .queryParam("accountId", walkInVo1.getAccountId())
                        .queryParam("sequence", walkInVo1.getSequence()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Primary_ShouldReturn_200Response_And_WalkInListNaturallyOrdered_WhenRequested_ForWalkIns_WithSequenceAndNotes() throws Exception {
        MvcResult mvcResult = null;
        Set<WalkInVo> walkInList = new TreeSet<>(Arrays.asList(walkInVo1));

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_PRIMARY_FILTER)
                        .queryParam("sequence", walkInVo1.getSequence())
                        .queryParam("notes", "1 notes"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Primary_ShouldReturn_200Response_And_WalkInListNaturallyOrdered_WhenRequested_ForWalkIns_WithAccountIdAndSequenceAndNotes() throws Exception {
        MvcResult mvcResult = null;
        Set<WalkInVo> walkInList = new TreeSet<>(Arrays.asList(walkInVo1));

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_PRIMARY_FILTER)
                        .queryParam("accountId", walkInVo1.getAccountId())
                        .queryParam("sequence", walkInVo1.getSequence())
                        .queryParam("notes", "1 notes"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Primary_ShouldReturn_200Response_And_EmptyWalkInList_WhenRequested_ForWalkIns_WithAbsent_WithAccountIdAndNotes() throws Exception {
        MvcResult mvcResult = null;
        Set<WalkInVo> walkInList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_PRIMARY_FILTER)
                        .queryParam("accountId", "4")
                        .queryParam("notes", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Primary_ShouldReturn_200Response_And_EmptyWalkInList_WhenRequested_ForWalkIns_WithAbsent_WithAccountIdAndSequence() throws Exception {
        MvcResult mvcResult = null;
        Set<WalkInVo> walkInList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_PRIMARY_FILTER)
                        .queryParam("accountId", "4")
                        .queryParam("sequence", "WalkIn 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Primary_ShouldReturn_200Response_And_EmptyWalkInList_WhenRequested_ForWalkIns_WithAbsent_WithSequenceAndNotes() throws Exception {
        MvcResult mvcResult = null;
        Set<WalkInVo> walkInList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_PRIMARY_FILTER)
                        .queryParam("notes", "5 notes")
                        .queryParam("sequence", "WalkIn 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Primary_ShouldReturn_200Response_And_EmptyWalkInList_WhenRequested_ForWalkIns_WithAbsent_WithAccountIdAndSequenceAndNotes() throws Exception {
        MvcResult mvcResult = null;
        Set<WalkInVo> walkInList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_PRIMARY_FILTER)
                        .queryParam("accountId", "4")
                        .queryParam("notes", UUID.randomUUID().toString())
                        .queryParam("sequence", "1 notes"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    /**
     *
     */

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_WalkIn_Get_Sequence_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_EmptySequence_AndDate(String sequence) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldSequence = "sequence";
        String date = LocalDateTime.now().format(DateTimeFormatter.ofPattern(walkInTimeFormat));

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_BY_SEQUENCE, sequence).queryParam("date", date))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldSequence));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_WalkIn_Get_Sequence_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_SequenceAnd_EmptyDate(String date) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldDate = "date";
        String sequence = UUID.randomUUID().toString();

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_BY_SEQUENCE, sequence).queryParam("date", date))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldDate));
    }

    @Test
    public void test_WalkIn_Get_Sequence_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_SequenceAnd_InvalidDate() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldDate = "date";
        String sequence = UUID.randomUUID().toString();
        String date = "Hey";

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_BY_SEQUENCE, sequence).queryParam("date", date))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldDate));
    }

    @Test
    public void test_WalkIn_Get_Sequence_ShouldReturn_404Response_And_ErrorCode_RES_ENGMNT_002_WhenRequested_ByAbsentSequence_AndDate() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
        String fieldSequence = "sequence";
        String sequence = UUID.randomUUID().toString();
        String date = walkInEntity1.getCreatedOn().format(DateTimeFormatter.ofPattern(walkInTimeFormat));

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_BY_SEQUENCE, sequence).queryParam("date", date))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldSequence));
    }

    @Test
    public void test_WalkIn_Get_Sequence_ShouldReturn_404Response_And_ErrorCode_RES_ENGMNT_002_WhenRequested_BySequence_AndAbsentDate() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
        String fieldDate = "date";
        String sequence = walkInEntity1.getSequence();
        String date = LocalDateTime.now().plusDays(2).format(DateTimeFormatter.ofPattern(walkInTimeFormat));

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_BY_SEQUENCE, sequence).queryParam("date", date))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldDate));
    }

    @Test
    public void test_WalkIn_Get_Sequence_ShouldReturn_200Response_And_WalkInListNaturallyOrdered_WhenRequested_ForWalkIns_WithSequenceAndDate() throws Exception {
        MvcResult mvcResult = null;
        String sequence = walkInEntity1.getSequence();
        String date = walkInEntity1.getCreatedOn().format(DateTimeFormatter.ofPattern(walkInTimeFormat));

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_BY_SEQUENCE, sequence).queryParam("date", date))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(objectMapper.writeValueAsString(walkInVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(walkInVo1.getId(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo.class).getId());
    }

    /**
     *
     */

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_WalkIn_Get_Secondary_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_EmptyNameOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldDate = "filters";

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_SECONDARY_FILTER).queryParam("name", name))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldDate));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_WalkIn_Get_Secondary_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_EmptyPhoneNumberOnly(String phoneNumber) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldTime = "filters";

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_SECONDARY_FILTER).queryParam("phoneNumber", phoneNumber))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldTime));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_WalkIn_Get_Secondary_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_EmptyEmailIdOnly(String emailId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldTime = "filters";

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_SECONDARY_FILTER).queryParam("emailId", emailId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldTime));
    }

    @ParameterizedTest
    @ValueSource(strings = { "Hey", "123" })
    public void test_WalkIn_Get_Secondary_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_InvalidPhoneNumber(String phoneNumber) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldDate = "phoneNumber";

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_SECONDARY_FILTER).queryParam("phoneNumber", phoneNumber))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldDate));
    }

    @ParameterizedTest
    @ValueSource(strings = { "Hey", "123@" })
    public void test_WalkIn_Get_Secondary_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_InvalidEmailId(String emailId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldDate = "emailId";

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_SECONDARY_FILTER).queryParam("emailId", emailId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldDate));
    }

    @Test
    public void test_WalkIn_Get_Secondary_ShouldReturn_200Response_And_EmptyWalkInList_WhenRequestedBy_AbsentPhoneNumber() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_SECONDARY_FILTER).queryParam("phoneNumber", "1234567890"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Secondary_ShouldReturn_200Response_And_EmptyWalkInList_WhenRequestedBy_AbsentEmailId() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_SECONDARY_FILTER).queryParam("emailId", "abc@email.com"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Secondary_ShouldReturn_200Response_And_WalkInListNaturallyOrdered_WhenRequested_ForWalkIns_WithName() throws Exception {
        MvcResult mvcResult = null;
        List<WalkInVo> walkInList = new ArrayList<>(Arrays.asList(walkInVo5));

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_SECONDARY_FILTER).queryParam("name", walkInVo5.getName()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Secondary_ShouldReturn_200Response_And_WalkInListNaturallyOrdered_WhenRequested_ForWalkIns_WithPhoneNumber() throws Exception {
        MvcResult mvcResult = null;
        List<WalkInVo> walkInList = new ArrayList<>(Arrays.asList(walkInVo2));

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_SECONDARY_FILTER).queryParam("phoneNumber", walkInVo2.getPhoneNumber()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Secondary_ShouldReturn_200Response_And_WalkInListNaturallyOrdered_WhenRequested_ForWalkIns_WithEmailId() throws Exception {
        MvcResult mvcResult = null;
        List<WalkInVo> walkInList = new ArrayList<>(Arrays.asList(walkInVo2));

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_SECONDARY_FILTER).queryParam("emailId", walkInVo2.getEmailId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Secondary_ShouldReturn_200Response_And_WalkInListNaturallyOrdered_WhenRequested_ForWalkIns_WithNameAndPhoneNumber() throws Exception {
        MvcResult mvcResult = null;
        Set<WalkInVo> walkInList = new TreeSet<>(Arrays.asList(walkInVo1));

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_SECONDARY_FILTER)
                        .queryParam("name", walkInVo1.getName())
                        .queryParam("phoneNumber", walkInVo1.getPhoneNumber()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Secondary_ShouldReturn_200Response_And_WalkInListNaturallyOrdered_WhenRequested_ForWalkIns_WithNameAndEmailId() throws Exception {
        MvcResult mvcResult = null;
        Set<WalkInVo> walkInList = new TreeSet<>(Arrays.asList(walkInVo1));

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_SECONDARY_FILTER)
                        .queryParam("name", walkInVo1.getName())
                        .queryParam("emailId", walkInVo1.getEmailId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Secondary_ShouldReturn_200Response_And_WalkInListNaturallyOrdered_WhenRequested_ForWalkIns_WithPhoneNumberAndEmailId() throws Exception {
        MvcResult mvcResult = null;
        Set<WalkInVo> walkInList = new TreeSet<>(Arrays.asList(walkInVo1));

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_SECONDARY_FILTER)
                        .queryParam("phoneNumber", walkInVo1.getPhoneNumber())
                        .queryParam("emailId", walkInVo1.getEmailId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    @Test
    public void test_WalkIn_Get_Secondary_ShouldReturn_200Response_And_WalkInListNaturallyOrdered_WhenRequested_ForWalkIns_WithNameAndPhoneNumberAndEmailId() throws Exception {
        MvcResult mvcResult = null;
        Set<WalkInVo> walkInList = new TreeSet<>(Arrays.asList(walkInVo1));

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_SECONDARY_FILTER)
                        .queryParam("name", walkInVo1.getName())
                        .queryParam("phoneNumber", walkInVo1.getPhoneNumber())
                        .queryParam("emailId", walkInVo1.getEmailId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo[].class).length);
    }

    /**
     *
     */

    @Test
    public void test_WalkIn_Get_ShouldReturn_200Response_And_WalkInDetails_WhenRequested_ById() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(objectMapper.writeValueAsString(walkInVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(walkInVo1.getId(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_WalkIn_Get_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_EmptyId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_WalkIn_Get_ShouldReturn_404Response_And_ErrorCode_RES_ENGMNT_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_WalkIn_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = walkInEntity3.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(WALK_IN_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_WalkIn_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInVo1.getId(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo.class).getId());
        Assertions.assertEquals(walkInVo1.getAccountId(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo.class).getAccountId());
        Assertions.assertEquals(walkInVo1.getNotes(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo.class).getNotes());
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo.class).getActive()));
    }

    @Test
    public void test_WalkIn_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        walkInVo1.setAccount(accountVo1);

        mvcResult = this.mockMvc.perform(get(WALK_IN_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(walkInVo1.getId(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo.class).getId());
        Assertions.assertNull(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo.class).getAccountId());
        Assertions.assertEquals(walkInVo1.getAccount(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo.class).getAccount());
        Assertions.assertEquals(walkInVo1.getNotes(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo.class).getNotes());
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), WalkInVo.class).getActive()));
    }

    @Test
    public void test_WalkIn_Delete_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001__WhenDeleted_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(delete(WALK_IN_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_WalkIn_Delete_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_003_WhenDeleted_ByInvalidId() throws Exception {
        String id = " ";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(delete(WALK_IN_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_WalkIn_Delete_ShouldReturn_422Response_And_ErrorCode_RES_ENGMNT_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = walkInEntity4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(WALK_IN_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_WalkIn_Delete_ShouldReturn_404Response_And_ErrorCode_RES_ENGMNT_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(delete(WALK_IN_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_WalkIn_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndWalkInDetails() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        walkInForm.setAccountId(accountVo4.getId());

        mvcResult = this.mockMvc.perform(put(WALK_IN_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_WalkIn_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenUpdatedBy_EmptyId_AndWalkInDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(put(WALK_IN_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_WalkIn_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenUpdated_ByInvalidId_AndWalkInDetails() throws Exception {
        String id = "id";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(put(WALK_IN_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_WalkIn_Put_ShouldReturn_404Response_And_ErrorCode_RES_ENGMNT_002_WhenUpdated_ByAbsentId_AndWalkInDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(put(WALK_IN_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_WalkIn_Put_ShouldReturn_422Response_And_ErrorCode_RES_ENGMNT_005_WhenUpdated_ByInactiveId_AndWalkInDetails() throws Exception {
        String id = walkInEntity4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(WALK_IN_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_WalkIn_Put_ShouldReturn_422Response_And_ErrorCode_RES_ENGMNT_003_WhenUpdated_ById_AndNoWalkInDetails() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldAccountId = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(WALK_IN_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_WalkIn_Put_ShouldReturn_409Response_And_ErrorCode_RES_ENGMNT_004_WhenUpdated_ById_AndDuplicateWalkInDetails() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_EXISTS.getErrorCode();
        String fieldAccountId = "accountId";
        String fieldSequence = "sequence";
        String message = "already exists";
        walkInForm.setAccountId(walkInEntity1.getAccountId());
        walkInForm.setSequence(walkInEntity1.getSequence());

        mvcResult = this.mockMvc.perform(put(WALK_IN_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldSequence));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r", "3", "22" })
    public void test_WalkIn_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ById_AndInvalidAccountId(String accountId) throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "accountId";
        walkInForm.setAccountId("");

        mvcResult = mockMvc.perform(put(WALK_IN_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_WalkIn_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ById_AndInvalidNotes() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "notes";
        walkInForm.setNotes("");

        mvcResult = mockMvc.perform(put(WALK_IN_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_WalkIn_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ById_AndInvalidSequence() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "sequence";
        walkInForm.setSequence("");

        mvcResult = mockMvc.perform(put(WALK_IN_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @ParameterizedTest
    @ValueSource(ints = { 0, -1 })
    public void test_WalkIn_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ById_AndInvalidNoOfPersons(Integer noOfPersons) throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "noOfPersons";
        walkInForm.setNoOfPersons(noOfPersons);

        mvcResult = mockMvc.perform(put(WALK_IN_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " ", "r" })
    public void test_WalkIn_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ById_AndInvalidEmptyType(String type) throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "type";
        walkInForm.setType(type);

        mvcResult = mockMvc.perform(put(WALK_IN_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_WalkIn_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ById_AndEmptyName(String name) throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "name";
        walkInForm.setName(name);

        mvcResult = mockMvc.perform(put(WALK_IN_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " ", "r", "123" })
    public void test_WalkIn_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ById_AndInvalidEmptyPhoneNumber(String phoneNumber) throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "phoneNumber";
        walkInForm.setPhoneNumber(phoneNumber);

        mvcResult = mockMvc.perform(put(WALK_IN_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " ", "r" })
    public void test_WalkIn_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ById_AndInvalidEmptyEmailId(String emailId) throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "emailId";
        walkInForm.setEmailId(emailId);

        mvcResult = mockMvc.perform(put(WALK_IN_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(walkInForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
    }

    @Test
    public void test_WalkIn_Put_ShouldReturn_422Response_And_ErrorCode_RES_ENGMNT_003_WhenUpdated_ById_AndEmptyWalkInDetails() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldAccountId = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(WALK_IN_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(new WalkInForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    /**
     * Patch
     */

    @Test
    public void test_WalkIn_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndWalkInDetails() throws Exception {
        String id = walkInEntity4.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(WALK_IN_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_WalkIn_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenUpdated_ByEmptyId_AndWalkInDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(patch(WALK_IN_URI_BY_ID, " ")
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_WalkIn_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenUpdated_ByInvalidId_AndWalkInDetails() throws Exception {
        String id = "x";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldId = "id";
        String message = "invalid";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", "2"));

        mvcResult = this.mockMvc.perform(patch(WALK_IN_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_WalkIn_Patch_ShouldReturn_404Response_And_ErrorCode_RES_ENGMNT_002_WhenUpdated_ByAbsentId_AndWalkInDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", "2"));

        mvcResult = this.mockMvc.perform(patch(WALK_IN_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_WalkIn_Patch_ShouldReturn_404Response_And_ErrorCode_RES_ENGMNT_002_WhenUpdated_ByInactiveId_AndWalkInDetails() throws Exception {
        String id = "4";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", "2"));

        mvcResult = this.mockMvc.perform(patch(WALK_IN_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_WalkIn_Patch_ShouldReturn_422Response_And_ErrorCode_RES_ENGMNT_003_WhenUpdated_ById_AndNoWalkInDetails() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldAccountId = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(WALK_IN_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_WalkIn_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(WALK_IN_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

    }

    @Test
    public void test_WalkIn_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyAccountId() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", " "));

        mvcResult = mockMvc.perform(patch(WALK_IN_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

    }

    @Test
    public void test_WalkIn_Patch_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_ById_AndInvalidAccountId() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = "RES-CUST-001";
        String fieldId = "id";
        String message = "invalid";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", "r"));

        mvcResult = mockMvc.perform(patch(WALK_IN_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_WalkIn_Patch_ShouldReturn_400Response_And_ErrorCode_RES_CUST_002_WhenRequested_ById_AndAbsentAccountId() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = "RES-CUST-002";
        String fieldId = "id";
        String accountId = "3";
        String message = "unavailable";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", accountId));

        mvcResult = mockMvc.perform(patch(WALK_IN_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(accountId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_WalkIn_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ById_AndInactiveAccountId() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode. ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "accountId";
        String message = "invalid";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", "22"));

        mvcResult = mockMvc.perform(patch(WALK_IN_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_WalkIn_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptySequence() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String field = "value";
        String message = "invalid";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/sequence", " "));

        mvcResult = mockMvc.perform(patch(WALK_IN_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_WalkIn_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyNoOfPersons() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String field = "value";
        String message = "invalid";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/noOfPersons", " "));

        mvcResult = mockMvc.perform(patch(WALK_IN_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_WalkIn_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ById_AndInvalidNoOfPersons() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "noOfPersons";
        String message = "invalid";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/noOfPersons", "x"));

        mvcResult = mockMvc.perform(patch(WALK_IN_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @ParameterizedTest
    @ValueSource(strings = { "r", "123" })
    public void test_WalkIn_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ById_AndInvalidPhoneNumber(String phoneNumber) throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "phoneNumber";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/phoneNumber", phoneNumber));


        mvcResult = mockMvc.perform(patch(WALK_IN_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
    }

    @ParameterizedTest
    @ValueSource(strings = { "rt@", "r" })
    public void test_WalkIn_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ById_AndInvalidEmailId(String emailId) throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "emailId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/emailId", emailId));


        mvcResult = mockMvc.perform(patch(WALK_IN_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
    }

    @Test
    public void test_WalkIn_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ById_AndInvalidDefinitionOfWalkInAttribute() throws Exception {
        String id = walkInEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(WALK_IN_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

    }

    @Override
    public String getSimulationBaseLocation() {
        return "integration";
    }

    @Override
    public Integer getServicePort() throws UnsupportedOperationException {
        return integrationPort;
    }

    @Override
    public String[] getSimulationFilePaths() {
        return new String[] { String.join("/", getSimulationBaseLocation(), "simulation.json") };
    }
}
