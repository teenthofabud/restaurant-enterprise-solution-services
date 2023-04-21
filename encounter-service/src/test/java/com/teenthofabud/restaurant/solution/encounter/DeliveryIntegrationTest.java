package com.teenthofabud.restaurant.solution.encounter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.encounter.constants.EncounterErrorCode;
import com.teenthofabud.restaurant.solution.encounter.delivery.converter.DeliveryEntity2VoConverter;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryEntity;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryForm;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryVo;
import com.teenthofabud.restaurant.solution.encounter.delivery.repository.DeliveryRepository;
import com.teenthofabud.restaurant.solution.encounter.integration.customer.data.AccountVo;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpVo;
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
public class DeliveryIntegrationTest extends EncounterIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";
    private static final String DELIVERY_URI = "/meeting/delivery";
    private static final String DELIVERY_URI_BY_ID = String.join("/", DELIVERY_URI, "{id}");
    private static final String DELIVERY_URI_BY_SEQUENCE = String.join("/", DELIVERY_URI, "sequence", "{sequence}");
    private static final String DELIVERY_URI_PRIMARY_FILTER = String.join("/", DELIVERY_URI, "primaryFilter");
    private static final String DELIVERY_URI_SECONDARY_FILTER = String.join("/", DELIVERY_URI, "secondaryFilter");

    private DeliveryRepository deliveryRepository;

    private DeliveryEntity2VoConverter deliveryEntity2VoConverter;

    private String deliveryDateFormat;

    private String deliveryTimeFormat;

    @Value("${res.encounter.meeting.date.format}")
    public void setDeliveryDateFormat(String deliveryDateFormat) {
        this.deliveryDateFormat = deliveryDateFormat;
    }

    @Value("${res.encounter.meeting.time.format")
    public void setDeliveryTimeFormat(String deliveryTimeFormat) {
        this.deliveryTimeFormat = deliveryTimeFormat;
    }

    @Autowired
    public void setDeliveryRepository(DeliveryRepository deliveryRepository) {
        this.deliveryRepository = deliveryRepository;
    }

    @Autowired
    public void setDeliveryEntity2VoConverter(DeliveryEntity2VoConverter deliveryEntity2VoConverter) {
        this.deliveryEntity2VoConverter = deliveryEntity2VoConverter;
    }

    private AccountVo accountVo1;
    private AccountVo accountVo2;
    private AccountVo accountVo3;
    private AccountVo accountVo4;

    private DeliveryForm deliveryForm;
    private DeliveryVo deliveryVo1;
    private DeliveryVo deliveryVo2;
    private DeliveryVo deliveryVo3;
    private DeliveryVo deliveryVo4;
    private DeliveryVo deliveryVo5;
    private DeliveryVo deliveryVo6;
    private DeliveryEntity deliveryEntity1;
    private DeliveryEntity deliveryEntity2;
    private DeliveryEntity deliveryEntity3;
    private DeliveryEntity deliveryEntity4;
    private DeliveryEntity deliveryEntity5;

    private List<PatchOperationForm> patches;

    private AccountVo accountVo(String id, String firstName, String lastName, boolean active) {
        AccountVo accountVo = new AccountVo();
        accountVo.setActive(active);
        accountVo.setId(id);
        accountVo.setFirstName(firstName);
        accountVo.setLastName(lastName);
        return accountVo;
    }

    private DeliveryEntity deliveryEntity(String accountId, String sequence, String orderId, boolean active) {
        DeliveryEntity deliveryEntity = new DeliveryEntity();
        deliveryEntity.setSequence(sequence);
        deliveryEntity.setAccountId(accountId);
        deliveryEntity.setOrderId(orderId);
        deliveryEntity.setActive(active);
        return deliveryEntity;
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
         * Delivery
         */

        deliveryForm = new DeliveryForm();
        deliveryForm.setAccountId("1");
        deliveryForm.setSequence(UUID.randomUUID().toString());
        deliveryForm.setOrderId("11");

        patches = new ArrayList<>(Arrays.asList(new PatchOperationForm("replace", "/orderId", "patched orderId")));

        deliveryEntity1 = this.deliveryEntity(accountVo1.getId(), UUID.randomUUID().toString(), "222222222", true);
        deliveryEntity1 = deliveryRepository.save(deliveryEntity1);
        deliveryVo1 = this.deliveryEntity2VoConverter.convert(deliveryEntity1);

        deliveryEntity2 = this.deliveryEntity(accountVo2.getId(), UUID.randomUUID().toString(), "222222222", true);
        deliveryEntity2 = deliveryRepository.save(deliveryEntity2);
        deliveryVo2 = this.deliveryEntity2VoConverter.convert(deliveryEntity2);

        deliveryEntity3 = this.deliveryEntity(accountVo3.getId(), UUID.randomUUID().toString(), "111111111", true);
        deliveryEntity3 = deliveryRepository.save(deliveryEntity3);
        deliveryVo3 = this.deliveryEntity2VoConverter.convert(deliveryEntity3);

        deliveryEntity4 = this.deliveryEntity(accountVo4.getId(), UUID.randomUUID().toString(), "333333333", true);
        deliveryEntity4 = deliveryRepository.save(deliveryEntity4);
        deliveryVo4 = this.deliveryEntity2VoConverter.convert(deliveryEntity4);

        deliveryEntity5 = this.deliveryEntity(accountVo1.getId(), UUID.randomUUID().toString(), "555555555", false);
        deliveryEntity5 = deliveryRepository.save(deliveryEntity5);
        deliveryVo5 = this.deliveryEntity2VoConverter.convert(deliveryEntity5);

        deliveryVo6 = new DeliveryVo();
        deliveryVo6.setActive(true);
        deliveryVo6.setSequence(deliveryForm.getSequence());
        deliveryVo6.setAccount(accountVo4);
        deliveryVo6.setOrderId(deliveryForm.getOrderId());
        deliveryVo6.setId("6");
    }

    @AfterEach
    private void destroy() {
        deliveryRepository.deleteById(deliveryEntity1.getId());
        deliveryRepository.deleteById(deliveryEntity2.getId());
        deliveryRepository.deleteById(deliveryEntity3.getId());
        deliveryRepository.deleteById(deliveryEntity4.getId());
        deliveryRepository.deleteById(deliveryEntity5.getId());
    }

    @Test
    public void test_Delivery_Post_ShouldReturn_201Response_And_NewDeliveryId_WhenPosted_WithValidDeliveryForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(DELIVERY_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deliveryForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Delivery_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequested_WithEmptyAccountId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "accountId";
        deliveryForm.setAccountId("");

        mvcResult = mockMvc.perform(post(DELIVERY_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deliveryForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

    }

    @Test
    public void test_Delivery_Post_ShouldReturn_400Response_And_NewDeliveryId_WhenPosted_WithEmptyNameAndPhoneNo() throws Exception {
        MvcResult mvcResult = null;
        deliveryForm.setAccountId("2");
        deliveryForm.setOrderId("");

        mvcResult = mockMvc.perform(post(DELIVERY_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deliveryForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Delivery_Post_ShouldReturn_422Response_And_ErrorCode_RES_ENCTR_003_WhenPosted_WithNoDeliveryForm() throws Exception {
        String id = deliveryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldAccountId = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(DELIVERY_URI)
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
    public void test_Delivery_Get_ShouldReturn_200Response_And_DeliveryListNaturallyOrdered_WhenRequested_ForAllDeliveries() throws Exception {
        MvcResult mvcResult = null;
        List<DeliveryVo> deliveryList = Arrays.asList(deliveryVo3, deliveryVo1, deliveryVo2, deliveryVo4, deliveryVo5);

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deliveryList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Delivery_Get_Primary_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_EmptyAccountIdOnly(String accountId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String field = "filters";

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_PRIMARY_FILTER).queryParam("accountId", accountId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Delivery_Get_Primary_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_EmptyNamesOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String field = "filters";

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_PRIMARY_FILTER).queryParam("name", name))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Delivery_Get_Primary_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_EmptyPhoneNoOnly(String phoneNo) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String field = "filters";

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_PRIMARY_FILTER).queryParam("phoneNo", phoneNo))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Delivery_Get_Primary_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_EmptySequenceOnly(String sequence) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String field = "filters";

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_PRIMARY_FILTER).queryParam("sequence", sequence))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
    }

    @Test
    public void test_Delivery_Get_Primary_ShouldReturn_200Response_And_EmptyDeliveryList_WhenRequestedBy_AbsentAccountId() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_PRIMARY_FILTER).queryParam("accountId", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo[].class).length);
    }

    @Test
    public void test_Delivery_Get_Primary_ShouldReturn_200Response_And_EmptyDeliveryList_WhenRequestedBy_AbsentSequence() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_PRIMARY_FILTER).queryParam("sequence", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo[].class).length);
    }

    @Test
    public void test_Delivery_Get_Primary_ShouldReturn_200Response_And_DeliveryListNaturallyOrdered_WhenRequested_ForDeliveries_WithAccountId() throws Exception {
        MvcResult mvcResult = null;
        List<DeliveryVo> deliveryList = new ArrayList<>(Arrays.asList(deliveryVo5));

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_PRIMARY_FILTER)
                        .queryParam("accountId", deliveryVo5.getAccountId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo[].class).length);
    }

    @Test
    public void test_Delivery_Get_Primary_ShouldReturn_200Response_And_DeliveryListNaturallyOrdered_WhenRequested_ForDeliveries_WithSequence() throws Exception {
        MvcResult mvcResult = null;
        List<DeliveryVo> deliveryList = new ArrayList<>(Arrays.asList(deliveryVo2));

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_PRIMARY_FILTER)
                        .queryParam("sequence", deliveryVo2.getSequence()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo[].class).length);
    }


    @Test
    public void test_Delivery_Get_Primary_ShouldReturn_200Response_And_DeliveryListNaturallyOrdered_WhenRequested_ForDeliveries_WithAccountIdAndSequence() throws Exception {
        MvcResult mvcResult = null;
        Set<DeliveryVo> deliveryList = new TreeSet<>(Arrays.asList(deliveryVo1));

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_PRIMARY_FILTER)
                        .queryParam("accountId", deliveryVo1.getAccountId())
                        .queryParam("sequence", deliveryVo1.getSequence()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deliveryList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo[].class).length);
    }

    @Test
    public void test_Delivery_Get_Primary_ShouldReturn_200Response_And_EmptyDeliveryList_WhenRequested_ForDeliveries_WithAbsent_AccountIdAndSequence() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_PRIMARY_FILTER)
                        .queryParam("accountId", "4")
                        .queryParam("sequence", "Delivery 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Delivery_Get_Secondary_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_EmptyNameOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_SECONDARY_FILTER).queryParam("name", name))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Delivery_Get_Secondary_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_EmptyPhoneNoOnly(String phoneNo) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldPhoneNo = "filters";

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_SECONDARY_FILTER).queryParam("phoneNo", phoneNo))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldPhoneNo));
    }

    @Test
    public void test_Delivery_Get_Secondary_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_InvalidOrderId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "orderId";

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_SECONDARY_FILTER).queryParam("orderId", "@#$"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Delivery_Get_Secondary_ShouldReturn_200Response_And_EmptyDeliveryList_WhenRequestedBy_AbsentOrderId() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_SECONDARY_FILTER).queryParam("orderId", "2222222222222"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo[].class).length);
    }

    @Test
    public void test_Delivery_Get_Secondary_ShouldReturn_200Response_And_DeliveryListNaturallyOrdered_WhenRequested_ForDeliveries_WithName() throws Exception {
        MvcResult mvcResult = null;
        List<DeliveryVo> deliveryList = new ArrayList<>(Arrays.asList(deliveryVo5));

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_SECONDARY_FILTER).queryParam("orderId", deliveryVo5.getOrderId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deliveryList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo[].class).length);
    }

    @Test
    public void test_Delivery_Get_Secondary_ShouldReturn_200Response_And_EmptyDeliveryList_WhenRequested_ForDeliveries_WithAbsent_NameAndPhoneNo() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_SECONDARY_FILTER)
                        .queryParam("orderId", "99"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo[].class).length);
    }

    @Test
    public void test_Delivery_Get_ShouldReturn_200Response_And_DeliveryDetails_WhenRequested_ById() throws Exception {
        String id = deliveryEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(deliveryVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(deliveryVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Delivery_Get_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_EmptyId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Delivery_Get_ShouldReturn_404Response_And_ErrorCode_RES_ENCTR_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "9";
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Delivery_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = deliveryEntity3.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(DELIVERY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Delivery_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = deliveryEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deliveryVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo.class).getId());
        Assertions.assertEquals(deliveryVo1.getAccountId(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo.class).getAccountId());
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo.class).getCreatedBy()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo.class).getModifiedBy()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo.class).getCreatedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo.class).getModifiedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo.class).getActive()));
    }

    @Test
    public void test_Delivery_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = deliveryEntity1.getId().toString();
        MvcResult mvcResult = null;
        deliveryVo1.setAccount(accountVo1);

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deliveryVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo.class).getId());
        Assertions.assertNull(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo.class).getAccountId());
        Assertions.assertEquals(deliveryVo1.getAccount(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo.class).getAccount());
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo.class).getCreatedBy()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo.class).getModifiedBy()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo.class).getCreatedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo.class).getModifiedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo.class).getActive()));
    }

    @Test
    public void test_Delivery_Delete_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001__WhenDeleted_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(delete(DELIVERY_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Delivery_Delete_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_003_WhenDeleted_ByInvalidId() throws Exception {
        String id = " ";
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(delete(DELIVERY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Delivery_Delete_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = deliveryEntity5.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(DELIVERY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Delivery_Delete_ShouldReturn_404Response_And_ErrorCode_RES_ENCTR_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "6";
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(delete(DELIVERY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Delivery_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndDeliveryDetails() throws Exception {
        String id = deliveryEntity1.getId().toString();
        MvcResult mvcResult = null;
        deliveryForm.setAccountId(accountVo4.getId());

        mvcResult = this.mockMvc.perform(put(DELIVERY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deliveryForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Delivery_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenUpdatedBy_EmptyId_AndDeliveryDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(put(DELIVERY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deliveryForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Delivery_Put_ShouldReturn_404Response_And_ErrorCode_RES_ENCTR_002_WhenUpdated_ByAbsentId_AndDeliveryDetails() throws Exception {
        String id = "6";
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(put(DELIVERY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deliveryForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Delivery_Put_ShouldReturn_422Response_And_ErrorCode_RES_ENCTR_005_WhenUpdated_ByInactiveId_AndDeliveryDetails() throws Exception {
        String id = deliveryEntity5.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(DELIVERY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deliveryForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }
//
    @Test
    public void test_Delivery_Put_ShouldReturn_422Response_And_ErrorCode_RES_ENCTR_003_WhenUpdated_ById_AndNoDeliveryDetails() throws Exception {
        String id = deliveryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldAccountId = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(DELIVERY_URI_BY_ID, id)
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
    public void test_Delivery_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequested_ById_AndInvalidAccountId() throws Exception {
        String id = deliveryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "accountId";
        deliveryForm.setAccountId("");

        mvcResult = mockMvc.perform(put(DELIVERY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deliveryForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Delivery_Put_ShouldReturn_422Response_And_ErrorCode_RES_ENCTR_003_WhenUpdated_ById_AndEmptyDeliveryDetails() throws Exception {
        String id = deliveryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldAccountId = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(DELIVERY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(new DeliveryForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Delivery_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndDeliveryDetails() throws Exception {
        String id = deliveryEntity4.getId().toString();
        MvcResult mvcResult = null;
        /*patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", "5"),
                new PatchOperationForm("replace", "/notes", "patched notes"));*/

        mvcResult = this.mockMvc.perform(patch(DELIVERY_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Delivery_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenUpdated_ByEmptyId_AndDeliveryDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(patch(DELIVERY_URI_BY_ID, " ")
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
    public void test_Delivery_Patch_ShouldReturn_404Response_And_ErrorCode_RES_ENCTR_002_WhenUpdated_ByAbsentId_AndDeliveryDetails() throws Exception {
        String id = "6";
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", "2"));

        mvcResult = this.mockMvc.perform(patch(DELIVERY_URI_BY_ID, id)
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
    public void test_Delivery_Patch_ShouldReturn_422Response_And_ErrorCode_RES_ENCTR_003_WhenUpdated_ById_AndNoDeliveryDetails() throws Exception {
        String id = deliveryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldAccountId = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(DELIVERY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Delivery_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = deliveryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(DELIVERY_URI_BY_ID, id)
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
    public void test_Delivery_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidAccountId() throws Exception {
        String id = deliveryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", " "));

        mvcResult = mockMvc.perform(patch(DELIVERY_URI_BY_ID, id)
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
    public void test_Delivery_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyName() throws Exception {
        String id = deliveryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(DELIVERY_URI_BY_ID, id)
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
    public void test_Delivery_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequested_ById_AndInvalidDefinitionOfDeliveryAttribute() throws Exception {
        String id = deliveryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(DELIVERY_URI_BY_ID, id)
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
        String sequence = deliveryEntity1.getSequence();
        MvcResult mvcResult = null;
        String strDate = LocalDate.now().format(DateTimeFormatter.ofPattern(deliveryDateFormat));

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_BY_SEQUENCE, sequence)
                        .queryParam("date", strDate))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(deliveryVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(deliveryVo1.getSequence(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryVo.class).getSequence());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_PickUp_Get_Sequence_ShouldReturn_400Response_And_ErrorCode_RES_ENCTR_001_WhenRequestedBy_EmptySequence_AndDate(String sequence) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.getErrorCode();
        String fieldSequence = "sequence";
        String date = LocalDate.now().format(DateTimeFormatter.ofPattern(deliveryDateFormat));

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_BY_SEQUENCE, sequence).queryParam("date", date))
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

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_BY_SEQUENCE, sequence).queryParam("date", date))
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

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_BY_SEQUENCE, sequence).queryParam("date", date))
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
        String date = deliveryEntity1.getCreatedOn().format(DateTimeFormatter.ofPattern(deliveryDateFormat));

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_BY_SEQUENCE, sequence).queryParam("date", date))
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
        String sequence = deliveryEntity1.getSequence();
        String date = LocalDate.now().plusDays(2).format(DateTimeFormatter.ofPattern(deliveryDateFormat));

        mvcResult = this.mockMvc.perform(get(DELIVERY_URI_BY_SEQUENCE, sequence).queryParam("date", date))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldDate));
    }

}
