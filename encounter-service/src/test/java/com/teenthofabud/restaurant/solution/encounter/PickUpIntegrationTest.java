package com.teenthofabud.restaurant.solution.encounter;


import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
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

import java.util.*;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@ContextConfiguration(classes = { EncounterServiceApplication.class })
public class PickUpIntegrationTest extends EncounterIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String MEETING_URI = "/meeting/pickUp";
    private static final String MEETING_URI_BY_ID = "/meeting/pickUp/{id}";
    private static final String MEETING_URI_PRIMARY_FILTER = "/meeting/pickUp/primaryFilter";

    private PickUpRepository pickUpRepository;

    private PickUpEntity2VoConverter pickUpEntity2VoConverter;

    private Integer integrationPort;

    private String pickUpDateFormat;

    private String pickUpTimeFormat;

    @Value("${res.encounter.meeting.date.format}")
    public void setPickUpDateFormat(String pickUpDateFormat) {
        this.pickUpDateFormat = pickUpDateFormat;
    }

    @Value("${res.encounter.meeting.time.format")
    public void setPickUpTimeFormat(String pickUpTimeFormat) {
        this.pickUpTimeFormat = pickUpTimeFormat;
    }

    @Value("${res.encounter.integration.gateway.port}")
    public void setIntegrationPort(Integer integrationPort) {
        this.integrationPort = integrationPort;
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
    private PickUpEntity pickUpEntity1;
    private PickUpEntity pickUpEntity2;
    private PickUpEntity pickUpEntity3;
    private PickUpEntity pickUpEntity4;

    private List<PatchOperationForm> patches;

    private AccountVo accountVo(String id, String firstName, String lastName, boolean active) {
        AccountVo accountVo = new AccountVo();
        accountVo.setActive(active);
        accountVo.setId(id);
        accountVo.setFirstName(firstName);
        accountVo.setLastName(lastName);
        return accountVo;
    }

    private PickUpEntity pickUpEntity() {
        return null;
    }

    @BeforeEach
    private void init() {

        /**
         * Account
         */

        accountVo1 = this.accountVo("1", "Account 1", "Account 1", true);
        accountVo2 = this.accountVo("2", "Account 2", "Account 2", true);
        accountVo3 = this.accountVo("3", "Account 3", "Account 3", false);
        accountVo4 = this.accountVo("4", "Account 4", "Account 4", true);

        /**
         * PickUp
         */

        pickUpForm = new PickUpForm();
        pickUpForm.setAccountId("1");
        pickUpForm.setSequence(String.valueOf(System.nanoTime()));
        pickUpForm.setName("Form 1");
        pickUpForm.setPhoneNo("1234567890");


        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "patched name"),
                new PatchOperationForm("replace", "/phoneNo", "patched phoneNo"));

        pickUpEntity1 = new PickUpEntity();
        pickUpEntity1.setAccountId("1");
        pickUpEntity1.setSequence(String.valueOf(System.nanoTime()));
        pickUpEntity1.setPhoneNo("11111111");
        pickUpEntity1.setName("Name 1");

        pickUpEntity1 = pickUpRepository.save(pickUpEntity1);

        pickUpVo1 = new PickUpVo();
        pickUpVo1.setId(pickUpEntity1.getId().toString());
        pickUpVo1.setAccountId(pickUpEntity1.getAccountId());
        pickUpVo1.setAccount(accountVo1);
        pickUpVo1.setSequence(pickUpEntity1.getSequence());
        pickUpVo1.setName(pickUpEntity1.getName());
        pickUpVo1.setPhoneNo(pickUpEntity1.getPhoneNo());

        pickUpEntity2 = new PickUpEntity();
        pickUpEntity2.setAccountId("2");
        pickUpEntity2.setSequence(String.valueOf(System.nanoTime()));
        pickUpEntity2.setPhoneNo("22222222");
        pickUpEntity2.setName("Name 2");

        pickUpEntity2 = pickUpRepository.save(pickUpEntity2);

        pickUpVo2 = new PickUpVo();
        pickUpVo2.setId(pickUpEntity2.getId().toString());
        pickUpVo2.setAccountId(pickUpEntity2.getAccountId());
        pickUpVo2.setAccount(accountVo2);
        pickUpVo2.setSequence(pickUpEntity2.getSequence());
        pickUpVo2.setName(pickUpEntity2.getName());
        pickUpVo2.setPhoneNo(pickUpEntity2.getPhoneNo());

        pickUpEntity3 = new PickUpEntity();
        pickUpEntity3.setAccountId("3");
        pickUpEntity3.setSequence(String.valueOf(System.nanoTime()));
        pickUpEntity3.setPhoneNo("33333333");
        pickUpEntity3.setName("Name 3");

        pickUpEntity3 = pickUpRepository.save(pickUpEntity3);

        pickUpVo3 = new PickUpVo();
        pickUpVo3.setId(pickUpEntity3.getId().toString());
        pickUpVo3.setAccountId(pickUpEntity3.getAccountId());
        pickUpVo3.setAccount(accountVo3);
        pickUpVo3.setSequence(pickUpEntity3.getSequence());
        pickUpVo3.setName(pickUpEntity3.getName());
        pickUpVo3.setPhoneNo(pickUpEntity3.getPhoneNo());

        pickUpEntity4 = new PickUpEntity();
        pickUpEntity4.setAccountId("4");
        pickUpEntity4.setSequence(String.valueOf(System.nanoTime()));
        pickUpEntity4.setPhoneNo("444444444");
        pickUpEntity4.setName("Name 4");

        pickUpEntity4 = pickUpRepository.save(pickUpEntity4);

        pickUpVo4 = new PickUpVo();
        pickUpVo4.setId(pickUpEntity4.getId().toString());
        pickUpVo4.setAccountId(pickUpEntity4.getAccountId());
        pickUpVo4.setAccount(accountVo1);
        pickUpVo4.setSequence(pickUpEntity4.getSequence());
        pickUpVo4.setName(pickUpEntity4.getName());
        pickUpVo4.setPhoneNo(pickUpEntity4.getPhoneNo());

        pickUpEntity1 = pickUpRepository.save(pickUpEntity1);
        pickUpEntity2 = pickUpRepository.save(pickUpEntity2);
        pickUpEntity3 = pickUpRepository.save(pickUpEntity3);
        pickUpEntity4 = pickUpRepository.save(pickUpEntity4);
    }

    @AfterEach
    private void destroy() {
        pickUpRepository.deleteById(pickUpEntity1.getId());
        pickUpRepository.deleteById(pickUpEntity2.getId());
        pickUpRepository.deleteById(pickUpEntity3.getId());
        pickUpRepository.deleteById(pickUpEntity4.getId());
    }

    @Test
    public void test_PickUp_Post_ShouldReturn_201Response_And_NewPickUpId_WhenPosted_WithValidPickUpForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(MEETING_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(pickUpForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

//    @Test
//    public void test_PickUp_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenRequested_WithEmptyAccountId() throws Exception {
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
//        String fieldAccountId = "accountId";
//        pickUpForm.setAccountId("");
//
//        mvcResult = mockMvc.perform(post(MEETING_URI)
//                        .contentType(MediaType.APPLICATION_JSON)
//                        .content(om.writeValueAsString(pickUpForm)))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//
//    }
//
//    @Test
//    public void test_PickUp_Post_ShouldReturn_201Response_And_NewPickUpId_WhenPosted_WithEmptyNotes() throws Exception {
//        MvcResult mvcResult = null;
//        pickUpForm.setAccountId("New PickUp AccountId");
//        pickUpForm.setNotes("");
//
//        mvcResult = mockMvc.perform(post(MEETING_URI)
//                        .contentType(MediaType.APPLICATION_JSON)
//                        .content(om.writeValueAsString(pickUpForm)))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
//    }
//
//    @Test
//    public void test_PickUp_Post_ShouldReturn_422Response_And_ErrorCode_RES_ENGAGEMENT_003_WhenPosted_WithNoPickUpForm() throws Exception {
//        String id = pickUpEntity1.getId().toString();
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED.getErrorCode();
//        String fieldAccountId = "form";
//        String message = "not provided";
//
//        mvcResult = mockMvc.perform(post(MEETING_URI)
//                        .contentType(MediaType.APPLICATION_JSON))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
//
//    }
//
//    @Test
//    public void test_PickUp_Get_ShouldReturn_200Response_And_PickUpListNaturallyOrdered_WhenRequested_ForAllPickUps() throws Exception {
//        MvcResult mvcResult = null;
//        Set<PickUpVo> pickUpList = new TreeSet<>(Arrays.asList(pickUpVo1, pickUpVo2, pickUpVo3, pickUpVo4));
//
//        mvcResult = this.mockMvc.perform(get(MEETING_URI))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(pickUpList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
//    }
//
//    @ParameterizedTest
//    @ValueSource(strings = { "", " " })
//    public void test_PickUp_Get_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenRequestedBy_EmptyAccountIdOnly(String accountId) throws Exception {
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
//        String fieldAccountId = "filters";
//
//        mvcResult = this.mockMvc.perform(get(MEETING_URI_PRIMARY_FILTER).queryParam("accountId", accountId))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//    }
//
//    @ParameterizedTest
//    @ValueSource(strings = { "", " " })
//    public void test_PickUp_Get_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenRequestedBy_EmptyNotesOnly(String notes) throws Exception {
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
//        String fieldAccountId = "filters";
//
//        mvcResult = this.mockMvc.perform(get(MEETING_URI_PRIMARY_FILTER).queryParam("notes", notes))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//    }
//
//    @Test
//    public void test_PickUp_Get_ShouldReturn_200Response_And_EmptyPickUpList_WhenRequestedBy_AbsentAccountId() throws Exception {
//        MvcResult mvcResult = null;
//
//        mvcResult = this.mockMvc.perform(get(MEETING_URI_PRIMARY_FILTER).queryParam("accountId", "Hey"))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
//    }
//
//    @Test
//    public void test_PickUp_Get_ShouldReturn_200Response_And_EmptyPickUpList_WhenRequestedBy_AbsentNotes() throws Exception {
//        MvcResult mvcResult = null;
//
//        mvcResult = this.mockMvc.perform(get(MEETING_URI_PRIMARY_FILTER).queryParam("notes", "Hey"))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
//    }
//
//    @Test
//    public void test_PickUp_Get_ShouldReturn_200Response_And_PickUpListNaturallyOrdered_WhenRequested_ForPickUps_WithAccountId() throws Exception {
//        MvcResult mvcResult = null;
//        List<PickUpVo> pickUpList = new ArrayList<>(Arrays.asList(pickUpVo1, pickUpVo2, pickUpVo3, pickUpVo4, pickUpVo5));
//
//        mvcResult = this.mockMvc.perform(get(MEETING_URI_PRIMARY_FILTER)
//                        .queryParam("accountId", "PickUp"))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(pickUpList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
//    }
//
//    @Test
//    public void test_PickUp_Get_ShouldReturn_200Response_And_PickUpListNaturallyOrdered_WhenRequested_ForPickUps_WithNotes() throws Exception {
//        MvcResult mvcResult = null;
//        List<PickUpVo> pickUpList = new ArrayList<>(Arrays.asList(pickUpVo2));
//
//        mvcResult = this.mockMvc.perform(get(MEETING_URI_PRIMARY_FILTER)
//                        .queryParam("notes", "PickUp 2"))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(pickUpList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
//    }
//
//    @Test
//    public void test_PickUp_Get_ShouldReturn_200Response_And_PickUpListNaturallyOrdered_WhenRequested_ForPickUps_WithAccountIdAndNotes() throws Exception {
//        MvcResult mvcResult = null;
//        Set<PickUpVo> pickUpList = new TreeSet<>(Arrays.asList(pickUpVo1));
//
//        mvcResult = this.mockMvc.perform(get(MEETING_URI_PRIMARY_FILTER)
//                        .queryParam("accountId", "PickUp 1")
//                        .queryParam("notes", "PickUp 1"))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(pickUpList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
//    }
//
//    @Test
//    public void test_PickUp_Get_ShouldReturn_200Response_And_EmptyPickUpList_WhenRequested_ForPickUps_WithAbsent_WithAccountIdAndNotes() throws Exception {
//        MvcResult mvcResult = null;
//        Set<PickUpVo> pickUpList = new TreeSet<>();
//
//        mvcResult = this.mockMvc.perform(get(MEETING_URI_PRIMARY_FILTER)
//                        .queryParam("accountId", "PickUp 1")
//                        .queryParam("notes", UUID.randomUUID().toString()))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(pickUpList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo[].class).length);
//    }
//
//    @Test
//    public void test_PickUp_Get_ShouldReturn_200Response_And_PickUpDetails_WhenRequested_ById() throws Exception {
//        String id = pickUpEntity1.getId().toString();
//        MvcResult mvcResult = null;
//
//        mvcResult = this.mockMvc.perform(get(MEETING_URI_BY_ID, id))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(om.writeValueAsString(pickUpVo1), mvcResult.getResponse().getContentAsString());
//        Assertions.assertEquals(pickUpVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getId());
//    }
//
//    @ParameterizedTest
//    @ValueSource(strings = { " " })
//    public void test_PickUp_Get_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenRequestedBy_EmptyId(String id) throws Exception {
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
//        String fieldAccountId = "id";
//
//        mvcResult = this.mockMvc.perform(get(MEETING_URI_BY_ID, id))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//    }
//
//    @Test
//    public void test_PickUp_Get_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_002_WhenRequested_ByAbsentId() throws Exception {
//        String id = "5";
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
//        String fieldAccountId = "id";
//
//        mvcResult = this.mockMvc.perform(get(MEETING_URI_BY_ID, id))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//    }
//
//    @Test
//    public void test_PickUp_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
//        String id = pickUpEntity3.getId().toString();
//        MvcResult mvcResult = null;
//
//        mvcResult = this.mockMvc.perform(delete(MEETING_URI_BY_ID, id))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
//    }
//
//    @Test
//    public void test_PickUp_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
//        String id = pickUpEntity1.getId().toString();
//        MvcResult mvcResult = null;
//
//        mvcResult = this.mockMvc.perform(get(MEETING_URI_BY_ID, id)
//                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
//                .andDo(print())
//                .andReturn();
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(pickUpVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getId());
//        Assertions.assertEquals(pickUpVo1.getAccountId(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getAccountId());
//        Assertions.assertEquals(pickUpVo1.getNotes(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getNotes());
//        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getCreatedBy()));
//        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getModifiedBy()));
//        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getCreatedOn()));
//        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getModifiedOn()));
//        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getActive()));
//    }
//
//    @Test
//    public void test_PickUp_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
//        String id = pickUpEntity1.getId().toString();
//        MvcResult mvcResult = null;
//
//        mvcResult = this.mockMvc.perform(get(MEETING_URI_BY_ID, id)
//                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
//                .andDo(print())
//                .andReturn();
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(pickUpVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getId());
//        Assertions.assertEquals(pickUpVo1.getAccountId(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getAccountId());
//        Assertions.assertEquals(pickUpVo1.getNotes(), om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getNotes());
//        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getCreatedBy()));
//        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getModifiedBy()));
//        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getCreatedOn()));
//        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getModifiedOn()));
//        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PickUpVo.class).getActive()));
//    }
//
//    @Test
//    public void test_PickUp_Delete_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001__WhenDeleted_ByEmptyId() throws Exception {
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
//        String fieldAccountId = "id";
//
//        mvcResult = this.mockMvc.perform(delete(MEETING_URI_BY_ID, " "))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//    }
//
//    @Test
//    public void test_PickUp_Delete_ShouldReturn_422Response_And_ErrorCode_RES_ENGAGEMENT_003_WhenDeleted_ByInvalidId() throws Exception {
//        String id = " ";
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
//        String fieldAccountId = "id";
//
//        mvcResult = this.mockMvc.perform(delete(MEETING_URI_BY_ID, id))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//    }
//
//    @Test
//    public void test_PickUp_Delete_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_005_WhenDeleted_ByInactiveId() throws Exception {
//        String id = pickUpEntity4.getId().toString();
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_INACTIVE.getErrorCode();
//
//        mvcResult = this.mockMvc.perform(delete(MEETING_URI_BY_ID, id))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
//    }
//
//    @Test
//    public void test_PickUp_Delete_ShouldReturn_404Response_And_ErrorCode_RES_ENGAGEMENT_002_WhenDeleted_ByAbsentId() throws Exception {
//        String id = "5";
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
//        String fieldAccountId = "id";
//
//        mvcResult = this.mockMvc.perform(delete(MEETING_URI_BY_ID, id))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//    }
//
//    @Test
//    public void test_PickUp_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndPickUpDetails() throws Exception {
//        String id = pickUpEntity1.getId().toString();
//        MvcResult mvcResult = null;
//        pickUpForm.setAccountId("Ferran");
//
//        mvcResult = this.mockMvc.perform(put(MEETING_URI_BY_ID, id)
//                        .contentType(MediaType.APPLICATION_JSON)
//                        .content(om.writeValueAsString(pickUpForm)))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
//    }
//
//    @ParameterizedTest
//    @ValueSource(strings = { " " })
//    public void test_PickUp_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenUpdatedBy_EmptyId_AndPickUpDetails(String id) throws Exception {
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
//        String fieldAccountId = "id";
//
//        mvcResult = this.mockMvc.perform(put(MEETING_URI_BY_ID, id)
//                        .contentType(MediaType.APPLICATION_JSON)
//                        .content(om.writeValueAsString(pickUpForm)))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//    }
//
//    @Test
//    public void test_PickUp_Put_ShouldReturn_404Response_And_ErrorCode_RES_ENGAGEMENT_002_WhenUpdated_ByAbsentId_AndPickUpDetails() throws Exception {
//        String id = "5";
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
//        String fieldAccountId = "id";
//
//        mvcResult = this.mockMvc.perform(put(MEETING_URI_BY_ID, id)
//                        .contentType(MediaType.APPLICATION_JSON)
//                        .content(om.writeValueAsString(pickUpForm)))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
//    }
//
//    @Test
//    public void test_PickUp_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_005_WhenUpdated_ByInactiveId_AndPickUpDetails() throws Exception {
//        String id = pickUpEntity4.getId().toString();
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_INACTIVE.getErrorCode();
//
//        mvcResult = this.mockMvc.perform(put(MEETING_URI_BY_ID, id)
//                        .contentType(MediaType.APPLICATION_JSON)
//                        .content(om.writeValueAsString(pickUpForm)))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
//    }
//
//    @Test
//    public void test_PickUp_Put_ShouldReturn_422Response_And_ErrorCode_RES_ENGAGEMENT_003_WhenUpdated_ById_AndNoPickUpDetails() throws Exception {
//        String id = pickUpEntity1.getId().toString();
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED.getErrorCode();
//        String fieldAccountId = "form";
//        String message = "not provided";
//
//        mvcResult = this.mockMvc.perform(put(MEETING_URI_BY_ID, id)
//                        .contentType(MediaType.APPLICATION_JSON))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
//    }
//
//    @Test
//    public void test_PickUp_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenRequested_ById_AndInvalidAccountId() throws Exception {
//        String id = pickUpEntity1.getId().toString();
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
//        String fieldAccountId = "accountId";
//        pickUpForm.setAccountId("");
//
//        mvcResult = mockMvc.perform(put(MEETING_URI_BY_ID, id)
//                        .contentType(MediaType.APPLICATION_JSON)
//                        .content(om.writeValueAsString(pickUpForm)))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//    }
//
//    @Test
//    public void test_PickUp_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenRequested_ById_AndInvalidNotes() throws Exception {
//        String id = pickUpEntity1.getId().toString();
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
//        String fieldAccountId = "notes";
//        pickUpForm.setNotes("");
//
//        mvcResult = mockMvc.perform(put(MEETING_URI_BY_ID, id)
//                        .contentType(MediaType.APPLICATION_JSON)
//                        .content(om.writeValueAsString(pickUpForm)))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//    }
//
//    @Test
//    public void test_PickUp_Put_ShouldReturn_422Response_And_ErrorCode_RES_ENGAGEMENT_003_WhenUpdated_ById_AndEmptyPickUpDetails() throws Exception {
//        String id = pickUpEntity1.getId().toString();
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED.getErrorCode();
//        String fieldAccountId = "form";
//        String message = "fields are expected with new values";
//
//        mvcResult = this.mockMvc.perform(put(MEETING_URI_BY_ID, id)
//                        .contentType(MediaType.APPLICATION_JSON)
//                        .content(om.writeValueAsString(new PickUpForm())))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
//    }
//
//    @Test
//    public void test_PickUp_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndPickUpDetails() throws Exception {
//        String id = pickUpEntity4.getId().toString();
//        MvcResult mvcResult = null;
//
//        mvcResult = this.mockMvc.perform(patch(MEETING_URI_BY_ID, id)
//                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
//                        .content(om.writeValueAsString(patches)))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
//    }
//
//    @Test
//    public void test_PickUp_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenUpdated_ByEmptyId_AndPickUpDetails() throws Exception {
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
//        String fieldAccountId = "id";
//
//        mvcResult = this.mockMvc.perform(patch(MEETING_URI_BY_ID, " ")
//                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
//                        .content(om.writeValueAsString(patches)))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//    }
//
//    @Test
//    public void test_PickUp_Patch_ShouldReturn_404Response_And_ErrorCode_RES_ENGAGEMENT_002_WhenUpdated_ByAbsentId_AndPickUpDetails() throws Exception {
//        String id = "5";
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
//        String fieldAccountId = "id";
//
//        mvcResult = this.mockMvc.perform(patch(MEETING_URI_BY_ID, id)
//                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
//                        .content(om.writeValueAsString(patches)))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
//    }
//
//    @Test
//    public void test_PickUp_Patch_ShouldReturn_422Response_And_ErrorCode_RES_ENGAGEMENT_003_WhenUpdated_ById_AndNoPickUpDetails() throws Exception {
//        String id = pickUpEntity1.getId().toString();
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED.getErrorCode();
//        String fieldAccountId = "patch";
//        String message = "not provided";
//
//        mvcResult = this.mockMvc.perform(patch(MEETING_URI_BY_ID, id))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
//    }
//
//    @Test
//    public void test_PickUp_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenRequested_ById_AndInvalidActive() throws Exception {
//        String id = pickUpEntity1.getId().toString();
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
//        String fieldAccountId = "active";
//        patches = Arrays.asList(
//                new PatchOperationForm("replace", "/active", "x"));
//
//        mvcResult = mockMvc.perform(patch(MEETING_URI_BY_ID, id)
//                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
//                        .content(om.writeValueAsString(patches)))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//
//    }
//
//    @Test
//    public void test_PickUp_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidAccountId() throws Exception {
//        String id = pickUpEntity1.getId().toString();
//        MvcResult mvcResult = null;
//        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
//        String fieldAccountId = "value";
//        patches = Arrays.asList(
//                new PatchOperationForm("replace", "/accountId", " "));
//
//        mvcResult = mockMvc.perform(patch(MEETING_URI_BY_ID, id)
//                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
//                        .content(om.writeValueAsString(patches)))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//
//    }
//
//    @Test
//    public void test_PickUp_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyRate() throws Exception {
//        String id = pickUpEntity1.getId().toString();
//        MvcResult mvcResult = null;
//        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
//        String fieldAccountId = "value";
//        patches = Arrays.asList(
//                new PatchOperationForm("replace", "/rate", " "));
//
//        mvcResult = mockMvc.perform(patch(MEETING_URI_BY_ID, id)
//                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
//                        .content(om.writeValueAsString(patches)))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//
//    }
//
//    @Test
//    public void test_PickUp_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenRequested_ById_AndInvalidDefinitionOfPickUpAttribute() throws Exception {
//        String id = pickUpEntity1.getId().toString();
//        MvcResult mvcResult = null;
//        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
//        String fieldAccountId = "path";
//        patches = Arrays.asList(
//                new PatchOperationForm("replace", "/x", "x"));
//
//        mvcResult = mockMvc.perform(patch(MEETING_URI_BY_ID, id)
//                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
//                        .content(om.writeValueAsString(patches)))
//                .andDo(print())
//                .andReturn();
//
//        Assertions.assertNotNull(mvcResult);
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
//        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
//        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
//
//    }

    @Override
    public String getSimulationBaseLocation() {
        return "simulation/customer-service";
    }

    @Override
    public Integer getServicePort() throws UnsupportedOperationException {
        return integrationPort;
    }

    @Override
    public String[] getSimulationFilePaths() {
        return new String[] { String.join("/", getSimulationBaseLocation(), "simulation-v3.json") };
    }
}
