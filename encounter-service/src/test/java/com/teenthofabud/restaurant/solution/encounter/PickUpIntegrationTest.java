package com.teenthofabud.restaurant.solution.encounter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.encounter.constants.EncounterErrorCode;
import com.teenthofabud.restaurant.solution.encounter.integration.customer.data.AccountVo;
import com.teenthofabud.restaurant.solution.encounter.pickup.converter.PickUpEntity2VoConverter;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpEntity;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpForm;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpVo;
import com.teenthofabud.restaurant.solution.encounter.pickup.repository.PickUpRepository;
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

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.*;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@ContextConfiguration(classes = { EncounterServiceApplication.class })
@TestMethodOrder(MethodOrderer.Alphanumeric.class)
public class PickUpIntegrationTest extends EncounterIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";
    private static final String PICK_UP_URI = "/meeting/pickUp";
    private static final String PICK_UP_URI_BY_ID = String.join("/", PICK_UP_URI, "{id}");
    private static final String PICK_UP_URI_BY_SEQUENCE = String.join("/", PICK_UP_URI, "sequence", "{sequence}");
    private static final String PICK_UP_URI_PRIMARY_FILTER = String.join("/", PICK_UP_URI, "primaryFilter");
    private static final String PICK_UP_URI_SECONDARY_FILTER = String.join("/", PICK_UP_URI, "secondaryFilter");

    private PickUpRepository pickUpRepository;

    private PickUpEntity2VoConverter pickUpEntity2VoConverter;

    private String pickUpDateFormat;

    private String pickUpTimeFormat;

    private String encounterAuditTimestampFormat;

    @Value("${res.encounter.audit.timestamp.format}")
    public void setEngagementAuditTimestampFormat(String encounterAuditTimestampFormat) {
        this.encounterAuditTimestampFormat = encounterAuditTimestampFormat;
    }

    @Value("${res.encounter.meeting.date.format}")
    public void setPickUpDateFormat(String pickUpDateFormat) {
        this.pickUpDateFormat = pickUpDateFormat;
    }

    @Value("${res.encounter.meeting.time.format")
    public void setPickUpTimeFormat(String pickUpTimeFormat) {
        this.pickUpTimeFormat = pickUpTimeFormat;
    }

    @Autowired
    public void setPickUpRepository(PickUpRepository pickUpRepository) {
        this.pickUpRepository = pickUpRepository;
    }

    @Autowired
    public void setPickUpEntity2VoConverter(PickUpEntity2VoConverter pickUpEntity2VoConverter) {
        this.pickUpEntity2VoConverter = pickUpEntity2VoConverter;
    }

    private AccountVo accountVo1;
    private AccountVo accountVo2;
    private AccountVo accountVo3;
    private AccountVo accountVo4;

    private PickUpForm pickUpForm;
    private PickUpVo pickUpVo1;
    private PickUpVo pickUpVo2;
    private PickUpVo pickUpVo3;
    private PickUpVo pickUpVo4;
    private PickUpVo pickUpVo5;
    private PickUpVo pickUpVo6;
    private PickUpEntity pickUpEntity1;
    private PickUpEntity pickUpEntity2;
    private PickUpEntity pickUpEntity3;
    private PickUpEntity pickUpEntity4;
    private PickUpEntity pickUpEntity5;

    private List<PatchOperationForm> patches;

    private AccountVo accountVo(String id, String firstName, String lastName, boolean active) {
        AccountVo accountVo = new AccountVo();
        accountVo.setActive(active);
        accountVo.setId(id);
        accountVo.setFirstName(firstName);
        accountVo.setLastName(lastName);
        return accountVo;
    }

    private PickUpEntity pickUpEntity(String accountId, String sequence, String phoneNo, String name, boolean active) {
        PickUpEntity pickUpEntity = new PickUpEntity();
        pickUpEntity.setSequence(sequence);
        pickUpEntity.setAccountId(accountId);
        pickUpEntity.setPhoneNo(phoneNo);
        pickUpEntity.setName(name);
        pickUpEntity.setActive(active);
        return pickUpEntity;
    }

    @BeforeEach
    private void init() {

        /**
         * Account
         */

        accountVo1 = this.accountVo("1", "AccountOne", "AccountOne", true);
        accountVo2 = this.accountVo("2", "AccountTwo", "AccountTwo", true);
        accountVo3 = this.accountVo("3", "AccountThree", "AccountThree", false);
        accountVo4 = this.accountVo("4", "AccountFour", "AccountFour", true);

        /**
         * PickUp
         */

        pickUpForm = new PickUpForm();
        pickUpForm.setAccountId("1");
        pickUpForm.setSequence(UUID.randomUUID().toString());
        pickUpForm.setName("FormOne");
        pickUpForm.setPhoneNo("1234567890");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "patched name"),
                new PatchOperationForm("replace", "/phoneNo", "patched phoneNo"));

        pickUpEntity1 = this.pickUpEntity(accountVo1.getId(), UUID.randomUUID().toString(), "222222222", "pickUpOne", true);
        pickUpEntity1 = pickUpRepository.save(pickUpEntity1);
        pickUpVo1 = this.pickUpEntity2VoConverter.convert(pickUpEntity1);

        pickUpEntity2 = this.pickUpEntity(accountVo2.getId(), UUID.randomUUID().toString(), "222222222", "pickUpTwo", true);
        pickUpEntity2 = pickUpRepository.save(pickUpEntity2);
        pickUpVo2 = this.pickUpEntity2VoConverter.convert(pickUpEntity2);

        pickUpEntity3 = this.pickUpEntity(accountVo3.getId(), UUID.randomUUID().toString(), "111111111", "pickUpThree", true);
        pickUpEntity3 = pickUpRepository.save(pickUpEntity3);
        pickUpVo3 = this.pickUpEntity2VoConverter.convert(pickUpEntity3);

        pickUpEntity4 = this.pickUpEntity(accountVo4.getId(), UUID.randomUUID().toString(), "333333333", "pickUpFour", true);
        pickUpEntity4 = pickUpRepository.save(pickUpEntity4);
        pickUpVo4 = this.pickUpEntity2VoConverter.convert(pickUpEntity4);

        pickUpEntity5 = this.pickUpEntity(accountVo1.getId(), UUID.randomUUID().toString(), "555555555", "pickUpFive", false);
        pickUpEntity5 = pickUpRepository.save(pickUpEntity5);
        pickUpVo5 = this.pickUpEntity2VoConverter.convert(pickUpEntity5);

        pickUpVo6 = new PickUpVo();
        pickUpVo6.setActive(true);
        pickUpVo6.setSequence(pickUpForm.getSequence());
        pickUpVo6.setAccount(accountVo4);
        pickUpVo6.setName(pickUpForm.getName());
        pickUpVo6.setPhoneNo(pickUpForm.getPhoneNo());
        pickUpVo6.setId("6");
    }

    @AfterEach
    private void destroy() {
        pickUpRepository.deleteById(pickUpEntity1.getId());
        pickUpRepository.deleteById(pickUpEntity2.getId());
        pickUpRepository.deleteById(pickUpEntity3.getId());
        pickUpRepository.deleteById(pickUpEntity4.getId());
        pickUpRepository.deleteById(pickUpEntity5.getId());
    }

    @Test
    public void test_PickUp_Post_ShouldReturn_201Response_And_NewPickUpId_WhenPosted_WithValidPickUpForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(PICK_UP_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(pickUpForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_PickUp_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequested_WithEmptyAccountId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "accountId";
        pickUpForm.setAccountId("");

        mvcResult = mockMvc.perform(post(PICK_UP_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(pickUpForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

    }

    @Test
    public void test_PickUp_Post_ShouldReturn_400Response_And_NewPickUpId_WhenPosted_WithEmptyNameAndPhoneNo() throws Exception {
        MvcResult mvcResult = null;
        pickUpForm.setAccountId("2");
        pickUpForm.setName("");
        pickUpForm.setPhoneNo("");

        mvcResult = mockMvc.perform(post(PICK_UP_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(pickUpForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_PickUp_Post_ShouldReturn_422Response_And_ErrorCode_RES_ENCTR_003_WhenPosted_WithNoPickUpForm() throws Exception {
        String id = pickUpEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldAccountId = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(PICK_UP_URI)
                        .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_PickUp_Get_ShouldReturn_200Response_And_PickUpListNaturallyOrdered_WhenRequested_ForAllPickUps() throws Exception {
        MvcResult mvcResult = null;
        List<PickUpVo> pickUpList = Arrays.asList(pickUpVo3, pickUpVo1, pickUpVo2, pickUpVo4, pickUpVo5);

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(pickUpList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_PickUp_Get_Primary_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_EmptyAccountIdOnly(String accountId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String field = "filters";

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_PRIMARY_FILTER).queryParam("accountId", accountId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_PickUp_Get_Primary_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_EmptyNamesOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String field = "filters";

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_PRIMARY_FILTER).queryParam("name", name))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_PickUp_Get_Primary_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_EmptyPhoneNoOnly(String phoneNo) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String field = "filters";

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_PRIMARY_FILTER).queryParam("phoneNo", phoneNo))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_PickUp_Get_Primary_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_EmptySequenceOnly(String sequence) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String field = "filters";

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_PRIMARY_FILTER).queryParam("sequence", sequence))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
    }

    @Test
    public void test_PickUp_Get_Primary_ShouldReturn_200Response_And_EmptyPickUpList_WhenRequestedBy_AbsentAccountId() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_PRIMARY_FILTER).queryParam("accountId", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
    }

    @Test
    public void test_PickUp_Get_Primary_ShouldReturn_200Response_And_EmptyPickUpList_WhenRequestedBy_AbsentSequence() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_PRIMARY_FILTER).queryParam("sequence", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
    }

    @Test
    public void test_PickUp_Get_Primary_ShouldReturn_200Response_And_PickUpListNaturallyOrdered_WhenRequested_ForPickUps_WithAccountId() throws Exception {
        MvcResult mvcResult = null;
        List<PickUpVo> pickUpList = new ArrayList<>(Arrays.asList(pickUpVo5));

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_PRIMARY_FILTER)
                        .queryParam("accountId", pickUpVo5.getAccountId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
    }

    @Test
    public void test_PickUp_Get_Primary_ShouldReturn_200Response_And_PickUpListNaturallyOrdered_WhenRequested_ForPickUps_WithSequence() throws Exception {
        MvcResult mvcResult = null;
        List<PickUpVo> pickUpList = new ArrayList<>(Arrays.asList(pickUpVo2));

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_PRIMARY_FILTER)
                        .queryParam("sequence", pickUpVo2.getSequence()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
    }


    @Test
    public void test_PickUp_Get_Primary_ShouldReturn_200Response_And_PickUpListNaturallyOrdered_WhenRequested_ForPickUps_WithAccountIdAndSequence() throws Exception {
        MvcResult mvcResult = null;
        Set<PickUpVo> pickUpList = new TreeSet<>(Arrays.asList(pickUpVo1));

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_PRIMARY_FILTER)
                        .queryParam("accountId", pickUpVo1.getAccountId())
                        .queryParam("sequence", pickUpVo1.getSequence()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(pickUpList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
    }

    @Test
    public void test_PickUp_Get_Primary_ShouldReturn_200Response_And_EmptyPickUpList_WhenRequested_ForPickUps_WithAbsent_WithAccountIdAndSequence() throws Exception {
        MvcResult mvcResult = null;
        Set<PickUpVo> pickUpList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_PRIMARY_FILTER)
                        .queryParam("accountId", "4")
                        .queryParam("sequence", "PickUp 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(pickUpList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_PickUp_Get_Secondary_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_EmptyNameOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_SECONDARY_FILTER).queryParam("name", name))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_PickUp_Get_Secondary_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_EmptyPhoneNoOnly(String phoneNo) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldPhoneNo = "filters";

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_SECONDARY_FILTER).queryParam("phoneNo", phoneNo))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldPhoneNo));
    }

    @Test
    public void test_PickUp_Get_Secondary_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_InvalidName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_SECONDARY_FILTER).queryParam("name", "@#$"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_PickUp_Get_Secondary_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_InvalidPhoneNo() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldPhoneNo = "phoneNumber";

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_SECONDARY_FILTER).queryParam("phoneNumber", "sdasd"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldPhoneNo));
    }

    @Test
    public void test_PickUp_Get_Secondary_ShouldReturn_200Response_And_EmptyPickUpList_WhenRequestedBy_AbsentName() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_SECONDARY_FILTER).queryParam("name", "sss"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
    }

    @Test
    public void test_PickUp_Get_Secondary_ShouldReturn_200Response_And_EmptyPickUpList_WhenRequestedBy_AbsentPhoneNo() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_SECONDARY_FILTER).queryParam("phoneNumber", "0000000000"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
    }

    @Test
    public void test_PickUp_Get_Secondary_ShouldReturn_200Response_And_PickUpListNaturallyOrdered_WhenRequested_ForPickUps_WithNameOnly() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_SECONDARY_FILTER).queryParam("name", pickUpVo5.getName()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
    }

    @Test
    public void test_PickUp_Get_Secondary_ShouldReturn_200Response_And_PickUpListNaturallyOrdered_WhenRequested_ForPickUps_WithPhoneNoOnly() throws Exception {
        MvcResult mvcResult = null;
        
        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_SECONDARY_FILTER).queryParam("phoneNumber", pickUpVo2.getPhoneNo()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
    }

    @Test
    public void test_PickUp_Get_Secondary_ShouldReturn_200Response_And_PickUpListNaturallyOrdered_WhenRequested_ForPickUps_WithNameAndPhoneNo() throws Exception {
        MvcResult mvcResult = null;
        Set<PickUpVo> pickUpList = new TreeSet<>(Arrays.asList(pickUpVo1));

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_SECONDARY_FILTER)
                        .queryParam("name", pickUpVo1.getName())
                        .queryParam("phoneNumber", pickUpVo1.getPhoneNo()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(pickUpList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
    }

    @Test
    public void test_PickUp_Get_Secondary_ShouldReturn_200Response_And_EmptyPickUpList_WhenRequested_ForPickUps_WithAbsent_NameAndPhoneNo() throws Exception {
        MvcResult mvcResult = null;
        Set<PickUpVo> pickUpList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_SECONDARY_FILTER)
                        .queryParam("name", "sdsdsd")
                        .queryParam("phoneNumber", "44444444444"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(pickUpList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
    }

    @Test
    public void test_PickUp_Get_ShouldReturn_200Response_And_PickUpDetails_WhenRequested_ById() throws Exception {
        String id = pickUpEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(pickUpVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(pickUpVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_PickUp_Get_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_EmptyId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_PickUp_Get_ShouldReturn_404Response_And_ErrorCode_RES_ENCTR_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "9";
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_PickUp_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = pickUpEntity3.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(PICK_UP_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_PickUp_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = pickUpEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(pickUpVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getId());
        Assertions.assertEquals(pickUpVo1.getAccountId(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getAccountId());
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getCreatedBy()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getModifiedBy()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getCreatedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getModifiedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getActive()));
    }

    @Test
    public void test_PickUp_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = pickUpEntity1.getId().toString();
        MvcResult mvcResult = null;
        pickUpVo1.setAccount(accountVo1);

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(pickUpVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getId());
        Assertions.assertNull(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getAccountId());
        Assertions.assertEquals(pickUpVo1.getAccount(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getAccount());
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getCreatedBy()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getModifiedBy()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getCreatedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getModifiedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getActive()));
    }

    @Test
    public void test_PickUp_Delete_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001__WhenDeleted_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(delete(PICK_UP_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_PickUp_Delete_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_003_WhenDeleted_ByInvalidId() throws Exception {
        String id = " ";
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(delete(PICK_UP_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_PickUp_Delete_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = pickUpEntity5.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(PICK_UP_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_PickUp_Delete_ShouldReturn_404Response_And_ErrorCode_RES_ENCTR_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "6";
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(delete(PICK_UP_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_PickUp_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndPickUpDetails() throws Exception {
        String id = pickUpEntity1.getId().toString();
        MvcResult mvcResult = null;
        pickUpForm.setAccountId(accountVo4.getId());

        mvcResult = this.mockMvc.perform(put(PICK_UP_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(pickUpForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_PickUp_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenUpdatedBy_EmptyId_AndPickUpDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(put(PICK_UP_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(pickUpForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_PickUp_Put_ShouldReturn_404Response_And_ErrorCode_RES_ENCTR_002_WhenUpdated_ByAbsentId_AndPickUpDetails() throws Exception {
        String id = "6";
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(put(PICK_UP_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(pickUpForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_PickUp_Put_ShouldReturn_422Response_And_ErrorCode_RES_ENCTR_005_WhenUpdated_ByInactiveId_AndPickUpDetails() throws Exception {
        String id = pickUpEntity5.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(PICK_UP_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(pickUpForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }
//
    @Test
    public void test_PickUp_Put_ShouldReturn_422Response_And_ErrorCode_RES_ENCTR_003_WhenUpdated_ById_AndNoPickUpDetails() throws Exception {
        String id = pickUpEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_NOT_FOUND.getErrorCode();
        String fieldAccountId = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(PICK_UP_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_PickUp_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequested_ById_AndInvalidAccountId() throws Exception {
        String id = pickUpEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "accountId";
        pickUpForm.setAccountId("");

        mvcResult = mockMvc.perform(put(PICK_UP_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(pickUpForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_PickUp_Put_ShouldReturn_422Response_And_ErrorCode_RES_ENCTR_003_WhenUpdated_ById_AndEmptyPickUpDetails() throws Exception {
        String id = pickUpEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldAccountId = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(PICK_UP_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(new PickUpForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_PickUp_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndPickUpDetails() throws Exception {
        String id = pickUpEntity4.getId().toString();
        MvcResult mvcResult = null;
        /*patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", "5"),
                new PatchOperationForm("replace", "/notes", "patched notes"));*/

        mvcResult = this.mockMvc.perform(patch(PICK_UP_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_PickUp_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenUpdated_ByEmptyId_AndPickUpDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(patch(PICK_UP_URI_BY_ID, " ")
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_PickUp_Patch_ShouldReturn_404Response_And_ErrorCode_RES_ENCTR_002_WhenUpdated_ByAbsentId_AndPickUpDetails() throws Exception {
        String id = "6";
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", "2"));

        mvcResult = this.mockMvc.perform(patch(PICK_UP_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_PickUp_Patch_ShouldReturn_422Response_And_ErrorCode_RES_ENCTR_003_WhenUpdated_ById_AndNoPickUpDetails() throws Exception {
        String id = pickUpEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldAccountId = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(PICK_UP_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_PickUp_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = pickUpEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(PICK_UP_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

    }

    @Test
    public void test_PickUp_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidAccountId() throws Exception {
        String id = pickUpEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", " "));

        mvcResult = mockMvc.perform(patch(PICK_UP_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

    }

    @Test
    public void test_PickUp_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyName() throws Exception {
        String id = pickUpEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(PICK_UP_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_PickUp_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequested_ById_AndInvalidDefinitionOfPickUpAttribute() throws Exception {
        String id = pickUpEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(PICK_UP_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

    }

    @Test
    public void test_PickUp_Get_ShouldReturn_200Response_And_PickUpDetails_WhenRequested_ByValid_SequenceAndCreatedDate() throws Exception {
        String sequence = pickUpEntity1.getSequence();
        MvcResult mvcResult = null;
        String strDate = LocalDate.now().format(DateTimeFormatter.ofPattern(pickUpDateFormat));

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_BY_SEQUENCE, sequence)
                .queryParam("date", strDate))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(pickUpVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(pickUpVo1.getSequence(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getSequence());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_PickUp_Get_Sequence_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_EmptySequence_AndDate(String sequence) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldSequence = "sequence";
        String date = LocalDate.now().format(DateTimeFormatter.ofPattern(pickUpDateFormat));

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_BY_SEQUENCE, sequence).queryParam("date", date))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldSequence));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_PickUp_Get_Sequence_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_SequenceAnd_EmptyDate(String date) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldDate = "date";
        String sequence = UUID.randomUUID().toString();

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_BY_SEQUENCE, sequence).queryParam("date", date))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldDate));
    }

    @Test
    public void test_PickUp_Get_Sequence_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_SequenceAnd_InvalidDate() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldDate = "date";
        String sequence = UUID.randomUUID().toString();
        String date = "Hey";

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_BY_SEQUENCE, sequence).queryParam("date", date))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldDate));
    }

    @Test
    public void test_PickUp_Get_Sequence_ShouldReturn_404Response_And_ErrorCode_RES_ENCTR_002_WhenRequested_ByAbsentSequence_AndDate() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_NOT_FOUND.getErrorCode();
        String fieldSequence = "sequence";
        String sequence = UUID.randomUUID().toString();
        String date = pickUpEntity1.getCreatedOn().format(DateTimeFormatter.ofPattern(pickUpDateFormat));

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_BY_SEQUENCE, sequence).queryParam("date", date))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldSequence));
    }

    @Test
    public void test_PickUp_Get_Sequence_ShouldReturn_404Response_And_ErrorCode_RES_ENCTR_002_WhenRequested_BySequence_AndAbsentDate() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_NOT_FOUND.getErrorCode();
        String fieldDate = "date";
        String sequence = pickUpEntity1.getSequence();
        String date = LocalDate.now().plusDays(2).format(DateTimeFormatter.ofPattern(pickUpDateFormat));

        mvcResult = this.mockMvc.perform(get(PICK_UP_URI_BY_SEQUENCE, sequence).queryParam("date", date))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldDate));
    }

}
